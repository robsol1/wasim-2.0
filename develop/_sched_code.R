
scheduler <- function(sched_name, task, taskid) {
  sched_log(sched_name,
            task,
            taskid,
            paste0("sched_name =", sched_name, "\n"))
  if (sched_name == "assign_LHD_task") {
    ret <- assign_LHD_task(sched_name, task, taskid)
  } else if (sched_name == "assign_LHD_to_Stockpile") {
    ret <- assign_LHD_to_Stockpile(sched_name, task, taskid)
  }
  else {
    sched_log(sched_name, task, taskid, "Invalid schedule name \n")
    ret = 100
  }
  sched_log(sched_name, task, taskid, paste0("result =", ret, "\n"))
  return(ret)
}


# assign_LHD_to_Stockpile <- function(sched_name,task,taskid) {
#   stockpile_Access_limit <- r_get_global(sched_name,task,taskid,'stope_stock_access_limit')
#   current_activities <- r_get_global(sched_name,task,taskid,'stope_stock_current_activities')
#   if(current_activities>stockpile_Access_limit){
#     sched_log(sched_name, task, taskid, "to many activities on stope_stocks \n")
#     ret = 100    
#   } else if(current_activities==stockpile_Access_limit) {
#     sched_log(sched_name, task, taskid, "no more activities available so wait to clear \n")
#     ret = 1
#   } else if(current_activities<stockpile_Access_limit){
#     sched_log(sched_name, task, taskid, "can put equipment on stockpile \n")
#     ret=2
#   }
#     else {
#     sched_log(sched_name, task, taskid, "too many activities on pile \n")
#       ret=-100
#   }
# }
assign_lhd_2_drawpoint <- function(sched_name,task,taskid,stope_stock){
  ret <- -1
  sched_log(sched_name,task,taskid,paste0("avg_lhd_Bucket_size_txt =", avg_lhd_Bucket_size_txt, "\n"))
  #sched_log(sched_name,task,taskid,paste0("drawpoint_stocks_max =", drawpoint_stocks_max, "\n"))
  if (stope_stock + committed_stope_stock + avg_lhd_Bucket_size_txt > stope_stocks_max) {
    ret = 1 #s_wait_downstream_stock
  } else if (drawpoint_stock + commited_drawpoint_stock < avg_lhd_Bucket_size_txt) {
    drawpoint_stock <- r_get_global(sched_name,task,taskid,'drawpoint_stocks_val')
    commited_drawpoint_stock <-r_get_global(sched_name,task,taskid,'drawpoint_stocks_commited')
    ret = 2 #s_wait_upstream_stock
  } else {
    ret = 5 # succede in assigning task to LHD
  }
}
assign_LHD_to_truck <- function(sched_name, task, taskid,stope_stock) {
  if (simmer::now(env) < 100) {
    truckwaiting = 0
  } else {
    truckwaiting <-
      r_get_global(sched_name, task, taskid, 'trucks_waiting_loader')
  }
  if (truckwaiting > 0) {
    sched_log(sched_name, task, taskid, paste0("and truck is waiting \n"))
    sched_log(sched_name,task,taskid,paste0("hoist_stocks_max =", hoist_stock_max_stock, "\n"))
    committed_stope_stock <-
      r_get_global(sched_name,task,taskid,'stope_stock_stocks_commited')
    hoist_stock <-
      r_get_global(sched_name, task, taskid, 'hoist_stock_stocks_val')
    sched_log(
      sched_name,
      task,
      taskid,
      paste0("truck_unit_capacity =", truck_unit_capacity, "\n")
    )
    committed_hoist_stock <-
      r_get_global(sched_name,task,taskid,'hoist_stock_stocks_commited')
    if (stope_stock + committed_stope_stock > truck_unit_capacity &
        hoist_stock_max_stock > truck_unit_capacity + hoist_stock + committed_hoist_stock) {
      sched_log(sched_name, task, taskid, paste0("all clear for truck so assign LHD \n"))
      ret = 4 # 
    } 
  } else {
    sched_log(sched_name, task, taskid, paste0("no truck waiting so skip \n"))
    ret <- -1
  }
  
  ret
}
#############################
##assign_LHD_task
#############################
assign_LHD_task <- function(sched_name, task, taskid) {
  ret <- -1
  stockpile_Access_limit <- r_get_global(sched_name, task,
                                         taskid,
                                         'stope_stock_access_limit')
  current_activities <- r_get_global(sched_name,
                                     task,
                                     taskid,
                                     'stope_stock_current_activities')
  if (stockpile_Access_limit > current_activities) {
    sched_log(sched_name,task,taskid,paste0("can access pile so look for task \n"))
    stope_stock <- 
      r_get_global(sched_name,task,taskid,'stope_stock_stocks_val')
    sched_log(sched_name,task,taskid,paste0("stope_stocks_max =", stope_stocks_max, "\n"))
    if (stope_stock > 0.5 * stope_stocks_max & truckwaiting > 0) {
      sched_log(sched_name,task,taskid,paste0("stope stocks > half full so prioritise truck \n"))
      ret <- assign_LHD_to_truck(sched_name, task, taskid,stope_stock)
      if (ret < 0) { # failed to assign truck so try to drawpoint
        ret <- assign_lhd_2_drawpoint(sched_name, task, taskid,stope_stock)
      }
    } else {
      sched_log(sched_name,task,taskid,paste0("stope stocks < half full so prioritise lhd \n"))
      # Try for LHD
      ret <- assign_lhd_2_drawpoint(sched_name, task, taskid,stope_stock)
      if (ret < 0 & truckwaiting > 0) { #failed LHD so try for truck
        ret <- assign_LHD_to_truck(sched_name, task, taskid,stope_stock)
      }
    }
  } else if (stockpile_Access_limit == current_activities){# no access to pile
    sched_log(sched_name,task,taskid,paste0("no access to pile so skip wait for pile release signal \n"))
    ret=3
  } else { #Error condition force break
    ret=-100
    sched_log(sched_name, task, taskid, "too many activities on pile \n")
  }
}

##### Schedular utility functions

sched_log <- function(sched_name,item_activity,item_id,message){
  cat(paste0(simmer::now(env),":",paste0(" ",unlist(strsplit(item_activity,":"))[1],item_id-1),":",item_activity,sched_name,":",message,"\n"))
}
r_get_global <- function(sched_name,task,taskid,att_name){
  ret <- get_global(env, att_name)
  sched_log(sched_name,task,taskid,paste0(att_name, " = ", ret, "\n"))
  ret
}