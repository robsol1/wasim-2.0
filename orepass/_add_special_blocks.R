

loader_loads_item <- function(trj_step ,
                              TUM_text,
                              delay_att_name ,
                              unit_size_att,
                              stockpile,
                              getorput,
                              breakdown_flag=TRUE,
                              start_event_signal,
                              end_event_signal
                              ){
  Block_code <- paste0("
  ",send_signal(start_event_signal,trj_step)," %>% 
  ",if(getorput=='get'){
    get_activity_delay_from_atts(trj_step,TUM_text,delay_att_name,unit_size_att,stockpile)
  } else if(getorput=='put'){
    put_activity_delay_from_atts(trj_step,TUM_text,delay_att_name,unit_size_att,stockpile)
  } else {
    print("Invalid get or put switch")
  },  if (breakdown_flag) {
      paste0( " %>%
      ",breakdown(trj_step = trj_step))
  }," %>% 
  ",send_signal(signal_name=end_event_signal,trj_step=trj_step)
  
  )
}
add_loader_loads_item <- function(modelname,
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
                                  breakdown_flag=TRUE,
                                  start_event_signal,
                                  end_event_signal
) {
  trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative=relative)
  code <- loader_loads_item(
    trj_step =trj_step ,
    TUM_text = 's_working',
    delay_att_name = delay_att_name,
    unit_size_att = unit_size_att,
    stockpile=stockpile,
    getorput=getorput,
    breakdown_flag = TRUE,
    start_event_signal=start_event_signal,
    end_event_signal=end_event_signal
  )
    trj_txt <-
    paste0(
      start_code(trj_step, next_trj_step),
      code,
      end_code(trj_step)
    )
  var_txt <- ""
  env_txt <- ""
  mod_df <- add_code_row(
    modelname=modelname,
    modeldf=mod_df,
    item =item,
    trj_step=trj_step ,
    next_trj_step=next_trj_step,
    signal_txt = paste0(start_event_signal,",",end_event_signal),
    signal_dir = paste0("send",",","send"),
    activity=activity ,
    stockpile=stockpile,
    var_txt=var_txt ,
    trj_txt=trj_txt ,
    env_txt=env_txt
  )
}    
item_loaded_by_item <-
  function(trj_step,
           queue_name,
           working_status_text,
           wait_status_text,
           start_event_signal,
           end_event_signal,
           unit_size_att) {
    
  Block_code <- paste0("
        ",robs_log(trj_step,paste0('Item arrived for loading'),ret=FALSE,pipe=TRUE),"
        ",set_generic_attributes(trj_step,queue_name,1,mod_code="+",'fn','global')," %>% 
        ",wait_signal(start_event_signal,trj_step,wait_status_text)," %>% 
          set_attribute('start_load',  function() simmer::now(env)) %>%
        ",wait_signal(end_event_signal,trj_step,working_status_text), " %>% 
         set_attribute('item_activity_delay_att',function() simmer::now(env)-get_attribute(env,'start_load')) %>% 
        ",set_generic_attributes(trj_step,queue_name,-1,mod_code="+",'fn','global')," %>% 
        ",set_generic_attributes(trj_step,to_name='item_ute_time',from_name='item_activity_delay_att',mod_code='+',from_type='local',to_type='local')," %>% 
        ",set_generic_attributes(trj_step,to_name='item_activity_cap_prod',from_name=unit_size_att,mod_code='+',from_type='local',to_type='local')
        )
}
add_item_loaded_by_item <- 
  function(modelname,
           mod_df,
           item,
           activity = "add_item_loaded_by_item",
           trj_step = -1,
           next_trj_step = -1,
           relative=FALSE,
           queue_name,
           working_status_text,
           wait_status_text,
           start_event_signal,
           end_event_signal,
           unit_size_att) {
    
    
  
  trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative = relative)
  
  code <- item_loaded_by_item(trj_step,
             queue_name,
             working_status_text,
             wait_status_text,
             start_event_signal,
             end_event_signal,
             unit_size_att)

  
  trj_txt <-
    paste0(
      start_code(trj_step, next_trj_step),
      code,
      end_code(trj_step)
    )
  
  var_txt <- ""
  env_txt = paste0("env <-  env  %>%
  add_global('",queue_name,"',0)
                   ")
  mod_df <- add_code_row(
    modelname=modelname,
    modeldf=mod_df,
    item =item,
    trj_step=trj_step ,
    next_trj_step=next_trj_step,
    signal_txt = paste0(start_event_signal,",",end_event_signal),
    signal_dir = paste0("wait",",","wait"),
    activity=activity ,
    var_txt=var_txt ,
    trj_txt=trj_txt ,
    env_txt=env_txt
  )
} 