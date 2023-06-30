# This file has three generators
#
# 1 add_activity_delay_from_atts (base block)
# 2 add_breakdown_generator (base block)
# 3 add_activity_with_breakdown (hierarchical block level 1)


# Base Code
delay_from_array <- function(item,activity,trj_step,varpointer,from_fn){
  code <- paste0("set_global('",item,"_",trj_step,"_delay', function()
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
  paste0(robs_log(item,activity,trj_step,'start activity_delay_from_atts',ret=FALSE),"
",
set_generic_attributes(
  item,
  activity,
  trj_step,
  to_name = paste0(activity,"_status"),
  from_name = TUM_text,
  mod_code = '',
  from_type = 'fn',
  to_type = 'local'
)," %>% 
",delay_from_array(item=item,activity=activity,trj_step=trj_step,
                   varpointer=paste0("ptr_",activity,"_delay"),
                   from_fn=paste0(activity,"_delay_code()"))," %>% 
",robs_log(item,activity,trj_step,'finished delay',ret=FALSE),"
",update_local_array_from_array(item=item,activity=activity,trj_step=trj_step,arrayname=paste0(item,"_array"),
                                from_ptr=paste0("ptr_",activity,"_delay"),to_ptr=paste0("ptr_",item,"_ute_time"),mod='+' )," %>% 
",update_local_array_from_array(item=item,activity=activity,trj_step=trj_step,arrayname=paste0(item,"_array"),
                                from_ptr=paste0("ptr_",item,"_unit_capacity"),to_ptr=paste0("ptr_",activity,"_throughput"),mod='+' )
  )
}

breakdown_from_array <- function(item,activity,trj_step){
  trj_txt <- paste0(" ### This is generic BD type code 
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



add_activity_delay_from_array <- function(
    mod_df,
    item,
    activity = 'activity_delay',
    trj_step = -1,
    next_trj_step=-1,
    relative=FALSE,
    breakdown=FALSE,
    TUM_text = 's_working',
    send_Signal_name="") {
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
  new_names <- c(paste0(activity,"_throughput"))
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


get_or_put_activity_delay_from_array <-
  function(item,
           activity,
           trj_step,
           TUM_text,
           stockpile_id,
           type,
           access_val,
           item_varpointer) {
    if(type=='put'){
      curr_move ='+'
      com_move ='-'
    } else if (type=='get'){
      curr_move ='-'
      com_move ='+'
    } else {
      print("invalid type variable in get_or_put_activity_delay_from_array, only get or put allowed")
    }
    if(is.null(item_varpointer)){
      item_varpointer = paste0("ptr_",item,"_unit_capacity")
    }

    trj <-  paste0("\n", robs_log(item,activity,trj_step,'start get_activity_delay_from_atts',ret = FALSE)
                   ,"\n",
                   update_stocks_from_item_build(item=item,activity=activity,trj_step=trj_step,
                                                 stockpile_id=stockpile_id,
                                                 stock_varpointer=get("varpointer_current_stocks"),
                                                 item_varpointer=item_varpointer,
                                                 mod=curr_move)
                   ," %>% \n", 
                   update_stocks_from_item_build(item=item,activity=activity,trj_step=trj_step,
                                                 stockpile_id=stockpile_id,
                                                 stock_varpointer=varpointer_committed,
                                                 item_varpointer=item_varpointer,
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
                   # ," %>% \n",
                   # update_array(
                   #   item = item,
                   #   activity = activity,
                   #   trj_step = trj_step,
                   #   column = paste0("ptr_", item, "_unit_capacity"),
                   #   value = paste0(item, "_unit_capacity_code()"),
                   #   mod = "+"
                   # )
    )
  }


add_get_or_put_activity_delay_from_array <- function(
    mod_df,
    item,
    activity,
    trj_step=-1,
    next_trj_step=-1,
    TUM_text = 's_working',
    stockpile_id,
    type,
    access_val=1,
    breakdown_flag=TRUE,
    relative = FALSE,
    send_Signal_name="",
    item_varpointer=NULL)
{
  trj_step <- check_trj_step(item,trj_step = trj_step,mod_df = mod_df)
  
  next_trj_step <-  check_next_trj_step(next_trj_step = next_trj_step,
                                        trj_step = trj_step,
                                        relative = relative)
  #next_trj_step <- trj_step+1
  
  code <- get_or_put_activity_delay_from_array(
    item=item,
    activity=activity,
    trj_step=trj_step,
    TUM_text=TUM_text,
    stockpile_id=stockpile_id,
    type=type,
    access_val=access_val,
    item_varpointer = paste0("ptr_",item,"_unit_capacity"))
  if (str_length(send_Signal_name) > 0) {
    code <- paste0(
      code,
      " %>% ",
      send_signal(
        item = item,
        activity = activity,
        signal_name = send_Signal_name,
        trj_step = trj_step
      )
    ) 
    signal_txt <- send_Signal_name
    signal_dir <- "send"
  } else {
    signal_txt <- ""
    signal_dir <- ""
  }
  if(breakdown_flag){
    bd_txt <- breakdown_from_array(
      item,
      activity,
      trj_step)
    trj_txt <- paste0(code," %>% ",bd_txt)
  }
  trj_txt <-
    paste0(
      start_code(item,activity,trj_step = trj_step, next_trj_step = next_trj_step),
      code,
      end_code(item,activity,trj_step = trj_step)
    )
  new_names <- c(paste0(activity,"_throughput"))
  new_vals <- c(0) 
  item_varnames <<- append(item_varnames,new_names)
  item_vals <<- append(item_vals,new_vals)
  var_txt <- ""
  env_txt <- ""
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
