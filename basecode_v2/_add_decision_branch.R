branch_start_txt <- function(item,activity,trj_step){
  paste0(" ",robs_log(item,activity,trj_step,'Entering branch decision',ret=FALSE,pipe=TRUE),"
           ",robs_log(item,activity,trj_step,'enter schedule queue',ret=FALSE,pipe=TRUE),"
           seize('",paste0(item,"_sched_res_block_",as.character(trj_step)),"',1) %>%
           timeout(",tiny,") %>%
           ",robs_log(item,activity,trj_step,'finished tiny delay',ret=FALSE,pipe=TRUE))
}

branch_end_txt <- function(item,activity,sched_name ,trj_step,next_trj_step,continue){
  # Create Continue Text
  continuetext <- "continue=c("
  for (i in 1:(length(continue) - 1)) {
    continuetext = paste0(continuetext, continue[i], ',')
  }
  continuetext = paste0(continuetext, continue[length(next_trj_step)], '),')
  subtrj_txt=""
  for (i in 1:(length(next_trj_step) - 1)) {
    subtrj_txt <- paste0(subtrj_txt,
                         "\t\ttrajectory('",sched_name,"_trajectory_",i ,"') %>% 
    \t\t\t",robs_log(item,activity,trj_step,paste0('Entering branch trajectory ',i),ret=FALSE,pipe=TRUE),"
    \t\t\tset_attribute('",item,"_next_block',",trj_step+ next_trj_step[i],"),
    ")  }
  subtrj_txt <- paste0(subtrj_txt,
                       "\t\ttrajectory('",sched_name,"_trajectory_",length(next_trj_step),"') %>% 
\t\t\t",robs_log(item,activity,trj_step,paste0('Entering branch trajectory ',length(next_trj_step)),ret=FALSE,pipe=TRUE),"
 \t\t\tset_attribute('",item,"_next_block',", trj_step+next_trj_step[length(next_trj_step)],")
\t)")
  paste0("
           ",continuetext,"\n",subtrj_txt," %>%
           release('",paste0(item,"_sched_res_block_",as.character(trj_step)),"',1)"
  )
}







sched_load_haul_dump_txt <- function(
    item,
    activity, 
    trj_step,
    from_pile_name, 
    to_pile_name, 
    commit_tonnes){
  
  paste0("branch( option = function() sched_load_haul_dump(
              sched_name = 'sched_load_haul_dump',
              item='",item,"',
              activity = '",activity,"',
              item_id=get_attribute(env,'",item,"_id'),
              trj_step = ",trj_step,",
              from_pile = '",from_pile_name,"',
              to_pile = '",to_pile_name,"',
              commit_tonnes = ",commit_tonnes,"              
            ),"
  ) 
}


add_schedule_Load_haul_dump <- function(mod_df,
                                item,
                                activity = 'sched_branch',
                                trj_step = -1,
                                next_trj_step = c(1, 2, 3),
                                continue = c(TRUE, TRUE, TRUE),
                                from_pile_name,
                                to_pile_name,
                                commit_tonnes
                                ){
  # Create next block id if not defined
  trj_step=check_trj_step(item,trj_step = trj_step,mod_df = mod_df)
  
  sched_name = "sched_load_haul_dump"
  
  code <- paste0("
           ",
                 branch_start_txt(item = item,
                                  activity = activity,
                                  trj_step = trj_step),
           "
           ",
                 sched_load_haul_dump_txt(
                   item = item,
                   activity = activity,
                   trj_step=trj_step,
                   from_pile_name = from_pile_name,
                   to_pile_name = to_pile_name,
                   commit_tonnes=commit_tonnes
                 ),
              branch_end_txt(item=item,
                  activity=activity,
                   sched_name = sched_name ,
                   trj_step = trj_step,
                   next_trj_step=next_trj_step,
                   continue = continue
                 )
  )
  trj_txt <-
    paste0(
      start_code(item,activity,trj_step = trj_step, next_trj_step = next_trj_step[1]),
      code,
      end_code(item,activity,trj_step = trj_step)
    )
  env_txt <- paste0("env <-  env  %>%
  add_resource('",paste0(item,"_sched_res_block_",as.character(trj_step)),"', 1,preemptive = TRUE,preempt_order = 'fifo')")
  var_txt <- ""
  decision_txt = "yes"
  signal_txt <- ""
  signal_dir <- ""
  next_trj_step <- next_trj_step+trj_step
  next_trj_step <- paste0(next_trj_step,collapse = ",")
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
##############################################################################################
##
############################################################################################
sched_access_to_stockpile_txt <- function(item,
                                          activity,
                                          trj_step,
                                          stockpile_name,
                                          access_val){
  
  paste0("branch( option = function() sched_access_to_stockpile(
              sched_name = 'sched_access_to_stockpile',
              item='",item,"',
              item_id=get_attribute(env,'",item,"_id'),
              activity = '",activity,"',
              trj_step = ",trj_step,",
              stockpile_name = '",stockpile_name,"' ,
              access_val = ",access_val,"
            ),"
  ) 
}

add_sched_access_to_stockpile <- function(mod_df,
                                        item,
                                        activity = 'sched_branch',
                                        trj_step = -1,
                                        next_trj_step = c(1, 2),
                                        continue = c(TRUE, TRUE),
                                        stockpile_name,
                                        access_val) {
  # Create next block id if not defined
  
  trj_step=check_trj_step(item,trj_step = trj_step,mod_df = mod_df)
  
  sched_name = "sched_access_to_stockpile"

  code <- paste0("
           ",
                 branch_start_txt(item = item,
                                  activity = activity,
                                  trj_step = trj_step),
                 "
           ",
                 sched_access_to_stockpile_txt(
                   item = item,
                   activity = activity,
                   trj_step=trj_step,
                   stockpile_name = stockpile_name,
                   access_val = access_val
                 ),
                 branch_end_txt(item=item,
                                activity=activity,
                                sched_name = sched_name ,
                                trj_step = trj_step,
                                next_trj_step=next_trj_step,
                                continue = continue
                 )
  )
  trj_txt <-
    paste0(
      start_code(item,activity,trj_step = trj_step, next_trj_step = next_trj_step[1]),
      code,
      end_code(item,activity,trj_step = trj_step)
    )
  env_txt <- paste0("env <-  env  %>%
  add_resource('",paste0(item,"_sched_res_block_",as.character(trj_step)),"', 1,preemptive = TRUE,preempt_order = 'fifo')")
  var_txt <- ""
  decision_txt = "yes"
  signal_txt <- ""
  signal_dir <- ""
  next_trj_step <- next_trj_step+trj_step
  next_trj_step <- paste0(next_trj_step,collapse = ",")
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
  return(mod_df)
}




