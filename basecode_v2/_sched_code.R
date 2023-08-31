sched_load_haul_dump <- function(sched_name,
                                 item,
                                 activity,
                                 item_id,
                                 trj_step,
                                 from_pile,
                                 to_pile,
                                 commit_tonnes){
  item_array <- get(paste0(item,"_array"))
  #check downstream stocks
  varp <- get(paste0("ptr_",item,"_unit_capacity"))
  from_pile <- get(paste0(from_pile,"_id"))
  to_pile <- get(paste0(to_pile,"_id"))
  unit_volume <- item_array[item_id,varp]
  sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0("sched_load_haul_dump check space available "))
  sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message= paste0("unit volume ", unit_volume))
  sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0("max avail ", stockpiles[to_pile,varpointer_maxstocks]))
  sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0("current stock ", stockpiles[to_pile,varpointer_current_stocks]))
  sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0("stocks committed ", stockpiles[to_pile,varpointer_committed]))
  space <- stockpiles[to_pile,varpointer_maxstocks] - stockpiles[to_pile,varpointer_current_stocks] - stockpiles[to_pile,varpointer_committed]
  sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0("space ", space))
  if(space > unit_volume) {
    sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, 
              message = paste0("space available - now check availability ")
    )
    sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, 
              message=paste0("stocks current ", stockpiles[from_pile, varpointer_current_stocks]))
    sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, 
              message=paste0("stocks committed ", stockpiles[from_pile, varpointer_committed]))
    av_stock <- stockpiles[from_pile, varpointer_current_stocks]
    + stockpiles[from_pile, varpointer_committed]
    if (av_stock > unit_volume) {
      ret = 3
      sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, 
                message=paste0("space available  and stock available so go ")
      )
      if(commit_tonnes){
        sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step,
                  message= paste0("committing tonnes")
        )
        set_array(item=item,item_id=item_id,trj_step=trj_step,activity=activity,"stockpiles",
                  row=from_pile,
                  column=varpointer_committed,
                  value=unit_volume,
                  mod = "-"
          )
        set_array(item=item,item_id=item_id,trj_step=trj_step,activity=activity,"stockpiles",
                  row=to_pile,
                  column=varpointer_committed,
                  value=unit_volume,
                  mod = "+"
        )
        set_array(item=item,item_id=item_id,trj_step=trj_step,activity=activity,paste0(item,"_array"),
                  row=item_id,
                  column=varp,
                  value=eval(parse(text=paste0(item,"_unit_capacity_code()"))),
                  mod = ""
        )
        
      }
    } else {
      sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step,message=paste0("starved "))
      ret = 2 #starved
    }
  } else {
    ret = 1 # block
    sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0("blocked "))
  }
  
  ret
}

sched_access_to_stockpile <- function(sched_name,
                                      item,
                                      item_id,
                                      activity,
                                      trj_step,
                                      stockpile_name,
                                      access_val){
  stockpile_id <- get(paste0(stockpile_name,"_id"))
  sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0("stockpile name is ",stockpile_name))
  
  sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0("stockpile id is ",stockpile_id))
  sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0("access_val is ",access_val))
  if(access_val==0){
    ret=2 #
    sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0(" access val=0 so status_working"))
  } else {
    current_activities = stockpiles[stockpile_id,varpointer_current_access_assigned]
    max_activities = stockpiles[stockpile_id,varpointer_access_limit]
    if(current_activities+access_val <= max_activities){
      ret=2 #
      sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0(" still room for access so status_working"))
    } else {
      ret=1 #
      sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0(" no access available"))
    }
  }
  ret
}

##### Schedular utility functions

sched_log <- function(item,item_id,sched_name,trj_step,message){
  # print(paste0("message1 = ",message))
  # print(paste0("item_id = ",item_id))
  # print(paste0("item_id-1 = ",item_id-1))
  cat(paste0(simmer::now(env),
             ":",paste0(item,item_id-1),
             ":",paste0(item,
             ":",sched_name,
             ":block_",trj_step,
             ":",message,"\n")
             )
  )
}


r_get_global <- function(item,item_id,sched_name,trj_step,att_name){
  # print(paste0("att name1 =",att_name))
  ret <- get_global(env, att_name)
  # print(paste0("att val =",ret))
  message <- paste0(att_name, " = ", ret, "\n")
  # print(paste0("message= ",message))
  sched_log(item=item,item_id=item_id,sched_name=sched_name,trj_step = trj_step,message = message)
  ret
}

