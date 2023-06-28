# This file has three generators
#
# 1 add_activity_delay_from_atts (base block)
# 2 add_breakdown_generator (base block)
# 3 add_activity_with_breakdown (hierarchical block level 1)


# Debug inputs


# modelname <- 'develop'
# mod_df <- mod_df
# item <- 'truck'
# activity = 'activity_delay'
# trj_step = -1
# next_trj_step=-1
# TUM_text = 's_working'
# delay_att_name = 'item_activity_delay_att'
# unit_size_att = 'item_activity_unit_size_att'

# Base Code
put_activity_delay_from_atts <- function(trj_step,TUM_text,delay_att_name,unit_size_att,stockpile){
  paste0("
",robs_log(trj_step,'start get_activity_delay_from_atts',ret=FALSE),"
",set_generic_attributes(trj_step,to_name='stockpile_stocks_val',from_name=unit_size_att,mod_code='+',from_type='local',to_type='global')," %>% 
",set_generic_attributes(trj_step,to_name='stockpile_stocks_commited',from_name=unit_size_att,mod_code='-',from_type='local',to_type='global')," %>% 
",set_generic_attributes(trj_step,to_name='stockpile_current_activities',from_name=1,mod_code='+',from_type='fn',to_type='global')," %>% 
",activity_delay_from_atts(trj_step,TUM_text,delay_att_name,unit_size_att), " %>% 
",set_generic_attributes(trj_step,to_name='stockpile_current_activities',from_name=1,mod_code='-',from_type='fn',to_type='global')
  )
}

get_activity_delay_from_atts <- function(trj_step,TUM_text,delay_att_name,unit_size_att,stockpile){
  paste0("
",robs_log(trj_step,'start get_activity_delay_from_atts',ret=FALSE),"
",set_generic_attributes(trj_step,to_name='stockpile_stocks_val',from_name=unit_size_att,mod_code='-',from_type='local',to_type='global')," %>% 
",set_generic_attributes(trj_step,to_name='stockpile_stocks_commited',from_name=unit_size_att,mod_code='+',from_type='local',to_type='global')," %>% 
",set_generic_attributes(trj_step,to_name='stockpile_current_activities',from_name=1,mod_code='+',from_type='fn',to_type='global')," %>% 
",robs_log_attribute(trj_step,text=paste0("delay(attname=",delay_att_name,")  = "),attribute= delay_att_name,ret=FALSE),"
",activity_delay_from_atts(trj_step,TUM_text,delay_att_name,unit_size_att)," %>%
",set_generic_attributes(trj_step,to_name='stockpile_current_activities',from_name=1,mod_code='-',from_type='fn',to_type='global') 
  )
}
activity_delay_from_atts <- function(trj_step,TUM_text,delay_att_name,unit_size_att){
paste0("
",robs_log(trj_step,'start activity_delay_from_atts',ret=FALSE),"
",set_generic_attributes(trj_step,to_name='local_item_activity_status',from_name=TUM_text,mod_code='',from_type='fn',to_type='local')," %>% 
",robs_log(trj_step,'start activity_delay_from_atts',ret=FALSE),"
timeout_from_attribute('",delay_att_name,"') %>%
",robs_log(trj_step,'finished delay',ret=FALSE),"
",set_generic_attributes(trj_step,to_name='item_ute_time',from_name=delay_att_name,mod_code='+',from_type='local',to_type='local')," %>% 
",set_generic_attributes(trj_step,to_name='item_activity_cap_prod',from_name=unit_size_att,mod_code='+',from_type='local',to_type='local')
)
}




add_activity_delay_from_atts <- function(modelname,
                               mod_df,
                               item,
                               activity = 'activity_delay',
                               trj_step = -1,
                               next_trj_step=-1,
                               relative=FALSE,
                               TUM_text = 's_working',
                               delay_att_name = 'item_activity_delay_att',
                               unit_size_att = 'item_activity_unit_size_att') {
  
  trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative=relative)
                                    
  trj_txt <-
    paste0(
      start_code(trj_step = trj_step, next_trj_step = next_trj_step),
      activity_delay_from_atts(trj_step=trj_step,TUM_text = TUM_text,delay_att_name = delay_att_name,unit_size_att = unit_size_att),
      end_code(trj_step = trj_step)
    )
  var_txt <- paste0("item_unit_capacity <- ",item_unit_capacity,"\n",
                      "item_mttr_code <- ",item_mttr_txt,"\n",
                      "item_mtbf_code <- ",item_mtbf_txt,"\n")
  env_txt <- ""
  mod_df <- add_code_row(
    modelname=modelname,
    modeldf=mod_df,
    item =item,
    trj_step=trj_step ,
    next_trj_step=next_trj_step,
    activity=activity ,
    var_txt=var_txt ,
    trj_txt=trj_txt ,
    env_txt=env_txt
  )
  }    
breakdown <- function(trj_step){
  paste0("
  ### This is generic BD type code 
  ",robs_log(trj_step,'test for item BD',ret=FALSE),"
  ",robs_log_attribute(trj_step,text='ute time = ',attribute='item_ute_time',ret=FALSE),"
  ",robs_log_attribute(trj_step,text='Next BD  = ',attribute='item_next_bd',ret=FALSE),"
  branch(
  #breakdown
  option = function() ifelse(get_attribute(env, 'item_ute_time') >= get_attribute(env, 'item_next_bd'),1,2),
  continue = c(TRUE, TRUE),
  trajectory('broken_down') %>%
    ",set_generic_attributes(trj_step,to_name='local_item_activity_status',from_name='s_breakdown',mod_code='',from_type='fn',to_type='local')," %>% 
    ",robs_log(trj_step1=trj_step,text='status= s.breakdown',ret=FALSE),"
    ",set_generic_attributes(trj_step,to_name='bd_delay',from_name='item_mttr_code',mod_code='',from_type='fn',to_type='local')," %>%
    ",robs_log_attribute(trj_step,text='delay duration  = ',attribute='bd_delay',ret=FALSE),"
    timeout_from_attribute('bd_delay') %>%
    ",robs_log(trj_step1=trj_step,text='continuing! after breakdown',ret=FALSE),"
    ",set_generic_attributes(trj_step,to_name='item_next_bd',from_name='item_mtbf_code',mod_code='+',from_type='fn',to_type='local')," %>%
    ",robs_log_attribute(trj_step,text='update next bd  ',attribute='item_next_bd',pipe=FALSE,ret=FALSE),",
  trajectory('working') %>%
   ",robs_log(trj_step1=trj_step,text='not broken down',pipe=FALSE,ret = FALSE)," 
              )"
)
}

add_breakdown_generator <- function(modelname,
                                    mod_df,
                                    item,
                                    activity = 'activity_breakdown',
                                    trj_step = -1,
                                    next_trj_step=-1,
                                    relative =FALSE){
  trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step)
  Block_code <- breakdown(trj_step=trj_step)

  trj_txt <- paste0(start_code(trj_step,next_trj_step),Block_code,end_code(trj_step))
  var_txt <- ""
  env_txt <- ""
  mod_df <- add_code_row(
    modelname=modelname,
    modeldf=mod_df,
    item =item,
    trj_step=trj_step ,
    next_trj_step=next_trj_step,
    activity=activity ,
    var_txt=var_txt ,
    trj_txt=trj_txt ,
    env_txt=env_txt
  )
}


add_activity_with_breakdown <- function(modelname,
                                        mod_df,
                                        item,
                                        activity = 'activity_delay',
                                        trj_step = -1,
                                        next_trj_step=-1,
                                        relative=FALSE,
                                        TUM_text = 's_working',
                                        delay_att_name = 'item_activity_delay_att',
                                        unit_size_att = 'item_activity_unit_size_att'
                                        ){
  trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative = relative)
  trj_txt <-
    paste0(start_code(trj_step,next_trj_step),
           activity_delay_from_atts(trj_step=trj_step,
             TUM_text = TUM_text,
             delay_att_name = delay_att_name,
             unit_size_att = unit_size_att
           )," %>% 
    ",
           breakdown(trj_step=trj_step),
           end_code(trj_step)
    )
  env_txt=""
  var_txt=""
  mod_df <- add_code_row(
    modelname=modelname,
    modeldf=mod_df,
    item =item,
    trj_step=trj_step ,
    next_trj_step=next_trj_step,
    activity=activity ,
    var_txt=var_txt ,
    trj_txt=trj_txt ,
    env_txt=env_txt
  )
}

add_getorput_activity_delay_from_atts <- function(modelname,
                                         mod_df,
                                         item,
                                         activity = 'add_getorput_activity_delay_from_atts',
                                         trj_step = -1,
                                         next_trj_step=-1,
                                         relative=FALSE,
                                         TUM_text = 's_working',
                                         delay_att_name = 'item_activity_delay_att',
                                         unit_size_att = 'item_activity_unit_size_att',
                                         stockpile,
                                         getorput,
                                         breakdown_flag=TRUE){
  trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative = relative)
  code <- if(getorput=='get'){
    code <- get_activity_delay_from_atts(trj_step,TUM_text,delay_att_name,unit_size_att,stockpile)
  } else if(getorput=='put'){
    code <- put_activity_delay_from_atts(trj_step,TUM_text,delay_att_name,unit_size_att,stockpile)
  } else {
    print("Invalid get or put switch")
  }
  
  if (breakdown_flag) {
    code = paste0(code, " %>%
      ",breakdown(trj_step = trj_step))
  }
  trj_txt <-
    paste0(
      start_code(trj_step = trj_step, next_trj_step = next_trj_step),
      code,
      end_code(trj_step = trj_step)
    )
  var_txt <- ""
  env_txt <- ""
  mod_df <- add_code_row(
    modelname=modelname,
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
}
