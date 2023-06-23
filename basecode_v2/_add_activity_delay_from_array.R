# This file has three generators
#
# 1 add_activity_delay_from_atts (base block)
# 2 add_breakdown_generator (base block)
# 3 add_activity_with_breakdown (hierarchical block level 1)


# Base Code
delay_from_array <- function(item,activity,trj_step,varpointer,from_fn){
  code <- paste0("
set_global('",item,"_",trj_step,"_delay', function()
  get_local_array_att(item = '",item,"',activity = '",activity,"',item_id = get_attribute(env, '",item,"_id'),
    varpointer = ",varpointer,",trj_step = ",trj_step,")
  ) %>% 
timeout_from_global('",item,"_",trj_step,"_delay') %>% 
",update_array(item=item,activity=activity,trj_step=trj_step,varpointer,
               value=from_fn,mod=""))
}


activity_delay_from_array <- function(item,activity,trj_step,TUM_text){
  # Change Status to status value- keep as internal
  # timeout
  # inc ute time -array
  # inc cap produced - array
  paste0("
",robs_log(item,activity,trj_step,'start activity_delay_from_atts',ret=FALSE),"
",
set_generic_attributes(
  item,
  activity,
  trj_step,
  to_name = paste0(item,"_",activity,"_status"),
  from_name = TUM_text,
  mod_code = '',
  from_type = 'fn',
  to_type = 'local'
)," %>% 
",delay_from_array(item=item,activity=activity,trj_step=trj_step,
          varpointer=paste0("ptr_",item,"_",activity,"_delay"),
          from_fn=paste0(item,"_",activity,"_delay_code()"))," %>% 
",robs_log(item,activity,trj_step,'finished delay',ret=FALSE),"
",update_local_array_from_array(item=item,activity=activity,trj_step=trj_step,arrayname=paste0(item,"_array"),
    from_ptr=paste0("ptr_",item,"_",activity,"_delay"),to_ptr=paste0("ptr_",item,"_ute_time"),mod='+' )," %>% 
",update_local_array_from_array(item=item,activity=activity,trj_step=trj_step,arrayname=paste0(item,"_array"),
    from_ptr=paste0("ptr_",item,"_unit_capacity"),to_ptr=paste0("ptr_",item,"_",activity,"_throughput"),mod='+' )
  )
}

breakdown_from_array <- function(item,activity,trj_step){
  trj_txt <- paste0("
  ### This is generic BD type code 
  ",robs_log(item,activity,trj_step,'test for item BD',ret=FALSE),"
  branch(
  #breakdown
  option = function() ifelse(
        get_local_array_att(
          item_id = get_attribute(env, '",item,"_id'),
          trj_step = ",trj_step,",
          item = '",item,"',
          activity = '",activity,"',
          varpointer = ptr_",item,"_next_bd
        ) < get_local_array_att(
          item_id = get_attribute(env, '",item,"_id'),
          trj_step = ",trj_step,",
          item = '",item,"',
          activity = '",activity,"',
          varpointer = ptr_",item,"_ute_time
        ),1,2),
  continue = c(TRUE, TRUE),
  trajectory('broken_down') %>%
    ",
    set_generic_attributes(
      item,
      activity,
      trj_step,
      to_name = paste0( item, "_", activity, "_status"),
      from_name = 's_breakdown',
      mod_code = '',
      from_type = 'fn',
      to_type = 'local'
    ), " %>% 
    ",
    robs_log(item,
             activity,
             trj_step = trj_step,
             text = 'status= s.breakdown',
             ret = FALSE), "
    ",
    delay_from_array(
      item = item,
      activity = activity,
      trj_step = trj_step,
      varpointer = paste0("ptr_", item, "_mttr"),
      from_fn = paste0(item, "_mttr_code()")
    )," %>%     
    ",
     robs_log(item,
              activity,
              trj_step = trj_step,
              text = 'continuing! after breakdown',
              ret = FALSE),"
     ",
     update_array(
       item = item,
       activity = activity,
       trj_step = trj_step,
       column = paste0("ptr_", item, "_next_bd"),
       value = paste0(item, "_mtbf_code()"),
       mod = "+"
     )," %>% 
    ",
   update_array(
     item = item,
     activity = activity,
     trj_step = trj_step,
     column = paste0("ptr_", item, "_mttr"),
     value = paste0(item, "_mttr_code()"),
     mod = "+"
   ),",
        trajectory('working') %>%
   ",
   robs_log(
     item,
     activity,
     trj_step = trj_step,
     text = 'not broken down',
     pipe = FALSE,
     ret = FALSE
   ),"
   )"
  )
}

get_or_put_activity_delay_from_array <-
  function(item,
           activity,
           trj_step,
           TUM_text,
           stockpile_id,
           type,
           access_val) {
 if(type=='put'){
   curr_mov='+'
   com_move='-'
 } else if (type=='get'){
   curr_move='-'
   com_move='+'
 } else {
   print("invalid type variable in get_or_put_activity_delay_from_array, only get or put allowed")
 }
 trj <-  paste0("z\n",
  robs_log(item,
           activity,
           trj_step,
           'start get_activity_delay_from_atts',
           ret = FALSE)
 ,"\n",
 update_stocks_from_item_build(item=item,activity=activity,trj_step=trj_step,
                               stockpile_id=stockpile_id,
                               stock_varpointer=varpointer_current_stocks,
                               item_varpointer=paste0("ptr_",item,"_unit_capacity"),
                               mod=curr_move)
 ," %>% \n", 
 update_stocks_from_item_build(item=item,activity=activity,trj_step=trj_step,
                               stockpile_id=stockpile_id,
                               stock_varpointer=varpointer_committed,
                               item_varpointer=paste0("ptr_",item,"_unit_capacity"),
                               mod=com_move)
 ," %>% \n", 
 update_stocks_from_val(item=item,activity=activity,trj_step=trj_step,
                        stockpile_id=stockpile_id,
                        stock_varpointer=varpointer_current_access_assigned,
                        access_val,
                        mod = "+")
 ," %>% \n", 
 activity_delay_from_array(item=item,activity=activity,trj_step=trj_step,TUM_text=TUM_text)
 ," %>% \n", 
 update_stocks_from_val(item=item,activity=activity,trj_step=trj_step,
                        stockpile_id=stockpile_id,
                        stock_varpointer=varpointer_current_access_assigned,
                        access_val,
                        mod = "-")
 )
}



add_activity_delay_from_array <- function(
                               mod_df,
                               item,
                               activity = 'activity_delay',
                               trj_step = -1,
                               next_trj_step=-1,
                               relative=FALSE,
                               breakdown=FALSE,
                               TUM_text = 's_working') {
  trj_step=check_trj_step(item=item,trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative=relative)
                             
  trj_txt <-
    paste0(
      start_code(
        item = item,
        activity = activity,
        trj_step = trj_step,
        next_trj_step = next_trj_step
      ),
      activity_delay_from_array(item, activity, trj_step, TUM_text)
    )
  if(breakdown){
    bd_txt <- breakdown_from_array(
      item
      , activity,
      trj_step)
    trj_txt <- paste0(trj_txt," %>% \n",bd_txt)
  }
  trj_txt <-
    paste0(trj_txt, end_code(item, activity, trj_step = trj_step))
  new_names <- c(paste0(item,"_",activity,"_throughput"))
  new_vals <<- c(0) 
  item_varnames <<- append(item_varnames,new_names)
  item_vals <<- append(item_vals,new_vals)
  var_txt <- ""
  env_txt <- ""
  signal_txt <- ""
  signal_dir <- ""
  decision_txt <- ""
  mod_df <- rbind(
    mod_df,
    data.frame(
      modelname = mod_df$modelname[1],
      item = item,
      trj_step = trj_step ,
      next_trj_step = next_trj_step,
      signal_txt=signal_txt,
      signal_dir=signal_dir,
      decision_txt=decision_txt,
      activity = activity ,
      var_txt = var_txt ,
      trj_txt = trj_txt ,
      env_txt = env_txt
    )
  )
  }    
# breakdown <- function(trj_step){
#   paste0("
#   ### This is generic BD type code 
#   ",robs_log(item,activity,trj_step,'test for item BD',ret=FALSE),"
#   ",robs_log_attribute(trj_step,text='ute time = ',attribute='item_ute_time',ret=FALSE),"
#   ",robs_log_attribute(trj_step,text='Next BD  = ',attribute='item_next_bd',ret=FALSE),"
#   branch(
#   #breakdown
#   option = function() ifelse(get_attribute(env, 'item_ute_time') >= get_attribute(env, 'item_next_bd'),1,2),
#   continue = c(TRUE, TRUE),
#   trajectory('broken_down') %>%
#     ",set_generic_attributes(trj_step,to_name='local_item_activity_status',from_name='s_breakdown',mod_code='',from_type='fn',to_type='local')," %>% 
#     ",robs_log(item,activity,trj_step=trj_step,text='status= s.breakdown',ret=FALSE),"
#     ",set_generic_attributes(trj_step,to_name='bd_delay',from_name='item_mttr_code',mod_code='',from_type='fn',to_type='local')," %>%
#     ",robs_log_attribute(trj_step,text='delay duration  = ',attribute='bd_delay',ret=FALSE),"
#     timeout_from_attribute('bd_delay') %>%
#     ",robs_log(item,activity,trj_step=trj_step,text='continuing! after breakdown',ret=FALSE),"
#     ",set_generic_attributes(trj_step,to_name='item_next_bd',from_name='item_mtbf_code',mod_code='+',from_type='fn',to_type='local')," %>%
#     ",robs_log_attribute(trj_step,text='update next bd  ',attribute='item_next_bd',pipe=FALSE,ret=FALSE),",
#   trajectory('working') %>%
#    ",robs_log(item,activity,trj_step=trj_step,text='not broken down',pipe=FALSE,ret = FALSE)," 
#               )"
# )
# }

# add_breakdown_generator <- function(modelname,
#                                     mod_df,
#                                     item,
#                                     activity = 'activity_breakdown',
#                                     trj_step = -1,
#                                     next_trj_step=-1,
#                                     relative =FALSE){
#   trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
#   next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step)
#   Block_code <- breakdown(trj_step=trj_step)
# 
#   trj_txt <- paste0(start_code(trj_step,next_trj_step),Block_code,end_code(trj_step))
#   var_txt <- ""
#   env_txt <- ""
#   mod_df <- rbind(mod_df,data.frame(
#     modelname=mod_df$modelname[1],
#     item =item,
#     trj_step=trj_step ,
#     next_trj_step=next_trj_step,
#     decision_txt = "yes",
#     activity=activity ,
#     var_txt=var_txt ,
#     trj_txt=trj_txt ,
#     env_txt=env_txt
#   )
#   )
# }
# 
# 
# add_activity_with_breakdown <- function(
#                                         mod_df,
#                                         item,
#                                         activity = 'activity_delay',
#                                         trj_step = -1,
#                                         next_trj_step=-1,
#                                         relative=FALSE,
#                                         TUM_text = 's_working',
#                                         delay_att_name = 'item_activity_delay_att',
#                                         unit_size_att = 'item_activity_unit_size_att'
#                                         ){
#   trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
#   next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative = relative)
#   trj_txt <-
#     paste0(start_code(trj_step,next_trj_step),
#            activity_delay_from_atts(item,activity,trj_step=trj_step,
#              TUM_text = TUM_text,
#              delay_att_name = delay_att_name,
#              unit_size_att = unit_size_att
#            )," %>% 
#     ",
#            breakdown(trj_step=trj_step),
#            end_code(trj_step)
#     )
#   env_txt=""
#   var_txt=""
#   mod_df <- rbind(mod_df,data.frame(
#     modelname=mod_df$modelname[1],
#     item =item,
#     trj_step=trj_step ,
#     next_trj_step=next_trj_step,
#     decision_txt = "yes",
#     activity=activity ,
#     var_txt=var_txt ,
#     trj_txt=trj_txt ,
#     env_txt=env_txt
#   )
#   )
# }
add_get_or_put_activity_delay_from_array <- function(
    item,
    activity,
    trj_step=-1,
    next_trj_step=-1,
    TUM_text = 's_working',
    delay_att_name,
    stockpile_id,
    type,
    access_val=1,
    breakdown_flag=TRUE)
  {
  trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative = relative)
  
  code <- get_or_put_activity_delay_from_array(
    item=item,
    activity=activity,
    trj_step=trj_step,
    TUM_text=TUM_text,
    stockpile_id=stockpile_id,
    type=type,
    access_val=access_val)
  if(breakdown){
    bd_txt <- breakdown_from_array(
      item
      ,activity,
      trj_step)
    trj_txt <- paste0(code," %>% \n",bd_txt)
  }
  trj_txt <-
    paste0(
      start_code(trj_step = trj_step, next_trj_step = next_trj_step),
      code,
      end_code(trj_step = trj_step)
    )
  var_txt <- ""
  env_txt <- ""
  mod_df <- rbind(mod_df,data.frame(
    modelname=mod_df$modelname[1],
    modeldf=mod_df,
    item =item,
    trj_step=trj_step ,
    next_trj_step=next_trj_step,
    activity=activity ,
    var_txt=var_txt ,
    stockpile = stockpile,
    trj_txt=trj_txt ,
    env_txt=env_txt
  )
  )
}
