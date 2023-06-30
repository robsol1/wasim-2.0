server_loads_client <- function(
    server,
    client,
    trj_step,
    TUM_text,
    stockpile,
    breakdown_flag,
    access_val,
    start_event_signal,
    end_event_signal,
    activity=NULL,
    client_unit_size_ptr=NULL,
    event_duration_ptr = NULL
) {

  if(is.null(client_unit_size_ptr)) {
    client_unit_size_ptr <-  paste0("ptr_", client, "_unit_capacity")
  }
  if(is.null(event_duration_ptr)){
    event_duration_ptr <- paste0("ptr_",item,activity,"_delay")
  }
  type <- 'get'
  item <- server
  Block_code <- paste0("
  ",send_signal( item,activity,start_event_signal,trj_step)," %>%
  ",get_or_put_activity_delay_from_array(
    item=item,
    activity=activity,
    trj_step=trj_step,
    TUM_text=TUM_text,
    stockpile_id=eval(parse(text=paste0(stockpile,"_id"))),
    type,
    access_val,
    item_varpointer=client_unit_size_ptr
    #item_varpointer=paste0(client,"_unit_size_ptr") #old
  )
  
    ,
    if (breakdown_flag) {
      paste0( " %>%
      ",breakdown_from_array(item,activity,trj_step = trj_step))
  }," %>%
  ",send_signal( item,activity,signal_name=end_event_signal,trj_step=trj_step)

  )
}
add_server_loads_client <- function(
    mod_df,
    server,
    client,
    trj_step,
    next_trj_step=-1,
    relative=FALSE,
    TUM_text,
    stockpile,
    breakdown_flag,
    access_val,
    start_event_signal=NULL,
    end_event_signal=NULL,
    activity = NULL,
    client_unit_size_ptr = NULL,
    event_duration_ptr = NULL) {
  trj_step=check_trj_step(item,trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative=relative)
  if(is.null(activity)){
    activity  = paste0(item,"_loads_",client)
  }
  
  if(is.null(start_event_signal)){
    start_event_signal <- paste0(server,"_commence_load_",client,"_signal")
  }
  if(is.null(end_event_signal)){
    end_event_signal <- paste0(server,"_completes_load_",client,"_signal")
  }

  code <- server_loads_client(
    server,
    client,
    trj_step,
    TUM_text,
    stockpile,
    breakdown_flag,
    access_val,
    start_event_signal,
    end_event_signal,
    activity,
    client_unit_size_ptr,
    event_duration_ptr
  )
    trj_txt <-
    paste0(
      start_code(item=server,activity=activity,trj_step=trj_step, next_trj_step=next_trj_step),
      code,
      end_code(item=server,activity=activity,trj_step=trj_step)
    )
  new_names <- c(paste0(activity,"_throughput"))
  new_vals <<- c(0) 
  item_varnames <<- append(item_varnames,new_names)
  item_vals <<- append(item_vals,new_vals)
  var_txt <- ""
  env_txt <- ""
  signal_txt <- paste0(start_event_signal,",",end_event_signal)
  signal_dir <-  paste0("send",",","send")
  decision_txt <- ""
  item <- server
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


client_loaded_by_server <-
  function(client,activity,trj_step,
           queue_name,
           working_status_text,
           wait_status_text,
           start_event_signal,
           end_event_signal) {
  item <- client
  Block_code <- paste0("
        ",robs_log(item,activity,trj_step,paste0('Item arrived for loading'),ret=FALSE,pipe=TRUE),"
        ",set_generic_attributes(item,activity,trj_step,queue_name,1,mod_code="+",'fn','global')," %>%
        ",wait_signal(item,activity,start_event_signal,trj_step,wait_status_text)," %>%
          set_attribute('start_load',  function() simmer::now(env)) %>%
        ",wait_signal(item,activity,end_event_signal,trj_step,working_status_text), " %>%
         set_attribute('item_activity_delay_att',function() simmer::now(env)-get_attribute(env,'start_load')) %>%
        ",set_generic_attributes(item,activity,trj_step,queue_name,-1,mod_code="+",'fn','global')," %>%
        ",update_array_from_attribute(item=item,activity=activity,trj_step=trj_step,
                        column=paste0('ptr_',item,'_ute_time'),
                        att_name = 'item_activity_delay_att',mod='+')," %>% 
        ",update_local_array_from_array(item=item,activity=activity,trj_step=trj_step,arrayname=paste0(item,"_array"),
                from_ptr=paste0("ptr_",item,"_unit_capacity"),to_ptr=paste0("ptr_",activity,"_throughput"),mod='+' )
        )
}

add_client_loaded_by_server <-
  function(mod_df,
           client,
           server,
           activity = NULL,
           trj_step = -1,
           next_trj_step = -1,
           relative=FALSE,
           queue_name=NULL,
           working_status_text="s_working",
           wait_status_text = "s_wait_sec_eq",
           start_event_signal=NULL,
           end_event_signal=NULL) {

  item <- client

  trj_step=check_trj_step(item,trj_step = trj_step,mod_df = mod_df)
  next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative = relative)
  if(is.null(activity)){
    activity  = paste0(client,"_loaded_by_",server)
  }
  
  if(is.null(start_event_signal)){
    start_event_signal <- paste0(server,"_commence_load_",client,"_signal")
  }
  
  if(is.null(end_event_signal)){
    end_event_signal <- paste0(server,"_completes_load_",client,"_signal")
  }
  
  if(is.null(queue_name)){
    queue_name <- paste0(client,"_waiting_",server,"_queue_att")
  }
  
  code <- client_loaded_by_server(client=client,activity=activity,trj_step=trj_step,
             queue_name = queue_name,
             working_status_text = working_status_text,
             wait_status_text = wait_status_text,
             start_event_signal = start_event_signal,
             end_event_signal = end_event_signal
             )


  trj_txt <-
    paste0(
      start_code(item=item,activity=activity,trj_step=trj_step, next_trj_step=next_trj_step),
      code,
      end_code(item=item,activity=activity,trj_step=trj_step)
    )

  signal_txt <- paste0(start_event_signal,",",end_event_signal)
  signal_dir <-  paste0("wait",",","wait")
  decision_txt <- ""
  new_names <- c(paste0(activity,"_throughput"))
  new_vals <<- c(0) 
  item_varnames <<- append(item_varnames,new_names)
  item_vals <<- append(item_vals,new_vals)
  var_txt <- ""
  env_txt = paste0("env <-  env  %>%
  add_global('",queue_name,"',0)
                   ")
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

assign_server_to_client <- function(sched_name,
                                    client,
                                    server,
                                    activity,
                                    server_id,
                                    trj_step,
                                    from_stockpile_name,
                                    to_stockpile_name,
                                    client_queue_att_name,
                                    commit_tonnes) {
  item=server
  item_id=server_id
  # # commit_tonnes=TRUE #debug
  # print(paste0("commit tonnes",commit_tonnes)#debug

  clientwaiting <-
    r_get_global(item=item,item_id=item_id,sched_name=sched_name,trj_step=trj_step,att_name=client_queue_att_name)
    #r_get_global(item,item_id,sched_name,trj_step,client_queue_att_name, 'queue_waiting_server')#old
  # print(paste0("client waiting =",clientwaiting))#debug
  if (clientwaiting > 0) {
    sched_log(item,item_id,sched_name,trj_step, paste0("client is waiting \n"))
    sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0("check stocks and space"))
    ## get client array to determine unit volume
    item_array <- get(paste0(client,"_array"))
    varp <- get(paste0("ptr_",item,"_unit_capacity"))
    unit_volume <- item_array[item_id,varp]
    sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message= paste0("unit volume ", unit_volume))
    #check stocks
    from_pile <- get(paste0(from_stockpile_name,"_id"))
    to_pile <- get(paste0(to_stockpile_name,"_id"))
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
        ret = 2
        sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, 
                  message=paste0("space available stock available so go ")
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
        ret = 1 #starved just skip to sched haul route truck is waiting for server 
      }
    } else {
      ret = 1 # block just skip to sched haul route truck is waiting for server 
      sched_log(item=item, item_id=item_id,sched_name=sched_name,trj_step=trj_step, message=paste0("blocked "))
    }
  } else {
    ret=1 # no truck available
  }
  #print(paste0("return val = ",ret)) #debug
  ret
}
  

#add_loader_loads_item <- function(modelname,
#                                   mod_df,
#                                   item,
#                                   activity = 'add_getorput_activity_delay_from_atts',
#                                   trj_step = -1,
#                                   next_trj_step=-1,
#                                   relative=FALSE,
#                                   TUM_text = 's_working',
#                                   delay_att_name = 'item_activity_delay_att',
#                                   unit_size_att = 'item_activity_unit_size_att',
#                                   stockpile,
#                                   getorput,
#                                   breakdown_flag=TRUE,
#                                   start_event_signal,
#                                   end_event_signal
# ) {
#   trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
#   next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative=relative)
#   code <- loader_loads_item(
#     trj_step =trj_step ,
#     TUM_text = 's_working',
#     delay_att_name = delay_att_name,
#     unit_size_att = unit_size_att,
#     stockpile=stockpile,
#     getorput=getorput,
#     breakdown_flag = TRUE,
#     start_event_signal=start_event_signal,
#     end_event_signal=end_event_signal
#   )
#     trj_txt <-
#     paste0(
#       start_code(trj_step, next_trj_step),
#       code,
#       end_code(trj_step)
#     )
#   var_txt <- ""
#   env_txt <- ""
#   mod_df <- add_code_row(
#     modelname=modelname,
#     modeldf=mod_df,
#     item =item,
#     trj_step=trj_step ,
#     next_trj_step=next_trj_step,
#     signal_txt = paste0(start_event_signal,",",end_event_signal),
#     signal_dir = paste0("send",",","send"),
#     activity=activity ,
#     stockpile=stockpile,
#     var_txt=var_txt ,
#     trj_txt=trj_txt ,
#     env_txt=env_txt
#   )
# }    

# loader_loads_item <- function(trj_step ,
#                               TUM_text,
#                               delay_att_name ,
#                               unit_size_att,
#                               stockpile,
#                               getorput,
#                               breakdown_flag=TRUE,
#                               start_event_signal,
#                               end_event_signal
#                               ){
#   Block_code <- paste0("
#   ",send_signal(start_event_signal,trj_step)," %>% 
#   ",if(getorput=='get'){
#     get_activity_delay_from_atts(trj_step,TUM_text,delay_att_name,unit_size_att,stockpile)
#   } else if(getorput=='put'){
#     put_activity_delay_from_atts(trj_step,TUM_text,delay_att_name,unit_size_att,stockpile)
#   } else {
#     print("Invalid get or put switch")
#   },  if (breakdown_flag) {
#       paste0( " %>%
#       ",breakdown(trj_step = trj_step))
#   }," %>% 
#   ",send_signal(signal_name=end_event_signal,trj_step=trj_step)
#   
#   )
# }
# add_loader_loads_item <- function(modelname,
#                                   mod_df,
#                                   item,
#                                   activity = 'add_getorput_activity_delay_from_atts',
#                                   trj_step = -1,
#                                   next_trj_step=-1,
#                                   relative=FALSE,
#                                   TUM_text = 's_working',
#                                   delay_att_name = 'item_activity_delay_att',
#                                   unit_size_att = 'item_activity_unit_size_att',
#                                   stockpile,
#                                   getorput,
#                                   breakdown_flag=TRUE,
#                                   start_event_signal,
#                                   end_event_signal
# ) {
#   trj_step=check_trj_step(trj_step = trj_step,mod_df = mod_df)
#   next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative=relative)
#   code <- loader_loads_item(
#     trj_step =trj_step ,
#     TUM_text = 's_working',
#     delay_att_name = delay_att_name,
#     unit_size_att = unit_size_att,
#     stockpile=stockpile,
#     getorput=getorput,
#     breakdown_flag = TRUE,
#     start_event_signal=start_event_signal,
#     end_event_signal=end_event_signal
#   )
#     trj_txt <-
#     paste0(
#       start_code(trj_step, next_trj_step),
#       code,
#       end_code(trj_step)
#     )
#   var_txt <- ""
#   env_txt <- ""
#   mod_df <- add_code_row(
#     modelname=modelname,
#     modeldf=mod_df,
#     item =item,
#     trj_step=trj_step ,
#     next_trj_step=next_trj_step,
#     signal_txt = paste0(start_event_signal,",",end_event_signal),
#     signal_dir = paste0("send",",","send"),
#     activity=activity ,
#     stockpile=stockpile,
#     var_txt=var_txt ,
#     trj_txt=trj_txt ,
#     env_txt=env_txt
#   )
# }    
# item_loaded_by_item <-
#   function(trj_step,
#            queue_name,
#            working_status_text,
#            wait_status_text,
#            start_event_signal,
#            end_event_signal,
#            unit_size_att) {
#     
#   Block_code <- paste0("
#         ",robs_log(trj_step,paste0('Item arrived for loading'),ret=FALSE,pipe=TRUE),"
#         ",set_generic_attributes(trj_step,queue_name,1,mod_code="+",'fn','global')," %>% 
#         ",wait_signal(start_event_signal,trj_step,wait_status_text)," %>% 
#           set_attribute('start_load',  function() simmer::now(env)) %>%
#         ",wait_signal(end_event_signal,trj_step,working_status_text), " %>% 
#          set_attribute('item_activity_delay_att',function() simmer::now(env)-get_attribute(env,'start_load')) %>% 
#         ",set_generic_attributes(trj_step,queue_name,-1,mod_code="+",'fn','global')," %>% 
#         ",set_generic_attributes(trj_step,to_name='item_ute_time',from_name='item_activity_delay_att',mod_code='+',from_type='local',to_type='local')," %>% 
#         ",set_generic_attributes(trj_step,to_name='item_activity_cap_prod',from_name=unit_size_att,mod_code='+',from_type='local',to_type='local')
#         )
# }
# add_item_loaded_by_item <- 
#   function(modelname,
#            mod_df,
#            item,
#            activity = "add_item_loaded_by_item",
#            trj_step = -1,
#            next_trj_step = -1,
#            relative=FALSE,
#            queue_name,
#            working_status_text,
#            wait_status_text,
#            start_event_signal,
#            end_event_signal,
#            unit_size_att) {
#     
#     
#   
#   trj_step=check_trj_step(item,trj_step = trj_step,mod_df = mod_df)
#   next_trj_step=check_next_trj_step(next_trj_step=next_trj_step,trj_step = trj_step,relative = relative)
#   
#   code <- item_loaded_by_item(trj_step,
#              queue_name,
#              working_status_text,
#              wait_status_text,
#              start_event_signal,
#              end_event_signal,
#              unit_size_att)
# 
#   
#   trj_txt <-
#     paste0(
#       start_code(trj_step, next_trj_step),
#       code,
#       end_code(trj_step)
#     )
#   
#   var_txt <- ""
#   env_txt = paste0("env <-  env  %>%
#   add_global('",queue_name,"',0)
#                    ")
#   mod_df <- add_code_row(
#     modelname=modelname,
#     modeldf=mod_df,
#     item =item,
#     trj_step=trj_step ,
#     next_trj_step=next_trj_step,
#     signal_txt = paste0(start_event_signal,",",end_event_signal),
#     signal_dir = paste0("wait",",","wait"),
#     activity=activity ,
#     var_txt=var_txt ,
#     trj_txt=trj_txt ,
#     env_txt=env_txt
#   )
# } 