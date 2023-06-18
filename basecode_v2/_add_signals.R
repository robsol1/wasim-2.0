####
# block contains 2 functions for signals
# 1- add_send_signal (base block)
# 2 add_wait_signal (base block)



# modelname='develop'
# mod_df=mod_df
# item='LHD'
# trj_step = -1
# next_trj_step=-1
# activity = 'send_signal'
# signal_name='signal_name_txt'
# wait_status_text="s_blocked"
send_signal <- function(signal_name,trj_step){
  Block_code <- paste0(
    "
        ",robs_log(trj_step,paste0('sending signal ',signal_name),ret=FALSE,pipe=TRUE),"
        send('",signal_name,"') %>% 
        ",robs_log(trj_step,paste0('sent signal ',signal_name),ret=FALSE,pipe=FALSE),"
        "
  )
}
add_send_signal <- function(modelname,
                            mod_df,
                            item,
                            activity = 'send_signal',
                            trj_step = -1,
                            next_trj_step=-1,
                            relative=FALSE,
                            signal_name) {
  
  trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative = relative)
  
  
    trj_txt <-
    paste0(
      start_code(trj_step, next_trj_step),
      send_signal(signal_name = signal_name, trj_step = trj_step),
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
    signal_txt=signal_name,
    signal_dir = "send",
    activity=activity ,
    var_txt=var_txt ,
    trj_txt=trj_txt ,
    env_txt=env_txt
  )
}    
wait_signal <- function(signal_name,trj_step,wait_status_text){
  Block_code <- paste0("
        ",robs_log(trj_step,paste0('waiting signal ',signal_name),ret=FALSE,pipe=TRUE),"
        set_attribute('local_item_activity_status', ",wait_status_text,") %>%
        trap('",signal_name,"') %>%
        wait() %>%
        ",robs_log(trj_step,paste0('released from signal ;',signal_name),pipe=FALSE,ret=FALSE))
}
add_wait_signal <- function(modelname,
                            mod_df,
                            item,
                            activity = 'wait_signal',
                            trj_step = -1,
                            next_trj_step=-1,
                            relative=FALSE,
                            signal_name,
                            wait_status_text) {
  
  trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative=relative)
  

  
  trj_txt <-
    paste0(
      start_code(trj_step, next_trj_step),
      wait_signal(signal_name = signal_name, trj_step = trj_step,wait_status_text=wait_status_text),
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
    signal_txt=signal_name,
    signal_dir="wait",
    activity=activity ,
    var_txt=var_txt ,
    trj_txt=trj_txt ,
    env_txt=env_txt
  )
} 