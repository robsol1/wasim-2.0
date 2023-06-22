sched_branch <- function(item,activity,sched_name,trj_step,next_trj_step,continue){
  # Create Continue Text
  continuetext <- "continue=c("
  for (i in 1:(length(next_trj_step) - 1)) {
    continuetext = paste0(continuetext, continue[i], ',')
  }
  continuetext = paste0(continuetext, continue[length(next_trj_step)], '),')
  subtrj_txt=""
  for (i in 1:(length(next_trj_step) - 1)) {
    subtrj_txt <- paste0(subtrj_txt,
                         "\t\ttrajectory('",sched_name,"_trajectory_",i ,"') %>% 
    \t\t\t",robs_log(item,activity,trj_step,paste0('Entering branch trajectory ',i),ret=FALSE,pipe=TRUE),"
    \t\t\tset_attribute('item_next_block',",trj_step+ next_trj_step[i],"),
    ")  }
  subtrj_txt <- paste0(subtrj_txt,
"\t\ttrajectory('",sched_name,"_trajectory_",length(next_trj_step),"') %>% 
\t\t\t",robs_log(item,activity,trj_step,paste0('Entering branch trajectory ',length(next_trj_step)),ret=FALSE,pipe=TRUE),"
 \t\t\tset_attribute('item_next_block',", trj_step+next_trj_step[length(next_trj_step)],")
\t)")
  
  
  trj_txt <- paste0("
             ",robs_log(item,activity,trj_step,'Entering branch decision',ret=FALSE,pipe=TRUE),"
             ",robs_log(item,activity,trj_step,'enter schedule queue',ret=FALSE,pipe=TRUE),"
             seize('",paste0("item_sched_res_block_",as.character(trj_step)),"',1) %>% 
             timeout(",tiny,") %>% 
             ",robs_log(item,activity,trj_step,'finished tiny delay',ret=FALSE,pipe=TRUE),"
           branch( option = function() scheduler(sched_name='",sched_name,"',task='item:activity:',taskid=get_attribute(env,'item_id')),
                    ",continuetext,"\n",subtrj_txt," %>% 
                    release('",paste0("item_sched_res_block_",as.character(trj_step)),"',1)"
                    
                    ) 

  

}

add_schedule_branch <- function(mod_df,
                                item,
                                activity = 'sched_branch',
                                trj_step = -1,
                                sched_name = "sched",
                                next_trj_step = c(1, 2, 3),
                                continue = c(TRUE, TRUE, TRUE)){
  # Create next block id if not defined
  trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
  code <- sched_branch(item,activity,sched_name,trj_step,next_trj_step,continue=continue)
  trj_txt <-
    paste0(
      start_code(item,activiy,trj_step = trj_step, next_trj_step = next_trj_step[1]),
      code,
      end_code(item,activity,trj_step = trj_step)
    )
  env_txt <- paste0("env <-  env  %>%
  add_resource('",paste0("item_sched_res_block_",as.character(trj_step)),"', 1,preemptive = TRUE,preempt_order = 'fifo')")
  var_txt <- ""
  next_trj_step <- next_trj_step+trj_step
  mod_df <- full_join(mod_df,data.frame(
    modelname=mod_df$modelname[1],
    item =item,
    trj_step=trj_step ,
    next_trj_step=next_trj_step,
    decision_txt = "yes",
    activity=activity ,
    var_txt=var_txt ,
    trj_txt=trj_txt ,
    env_txt=env_txt
  )
  )
}







