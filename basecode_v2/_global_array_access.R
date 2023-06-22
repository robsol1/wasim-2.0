set_array <-
  function(item,
           trj_step,
           activity,
           arrayname,
           row,
           column,
           value,
           mod = ""
  ){
  df <- get(arrayname)
  row <- as.numeric(row)
  column=as.numeric(column)
  curr_val=df[row,column]
  setval <- if (mod==""){
    value
  } else if (mod=='+'){
    curr_val+value
  } else if(mod=='-'){
    curr_val-value
  } else{
    cat(paste0("error - invalid mode code ",mod," in set local array att"))
  }
  df[row,column]=setval
  r_array_log(item,activity,trj_step,row,column,paste0(" was ",curr_val," is ",setval,"\n"))
  assign(arrayname,df,envir = .GlobalEnv)
  setval
  }
set_array_from_array <-
  function(item,
           trj_step,
           activity,
           arrayname,
           row,
           from_ptr,
           to_ptr,
           mod = "") {
    
    df <- get(arrayname)
    ret = set_array(
      item=item,
      trj_step=trj_step,
      activity=activity,
      arrayname = arrayname,
      row = row,
      column = to_ptr,
      value = df[row,from_ptr],
      mod = mod
    )
    
  }

set_local_array_att <-
  function(item,
           item_id,
           trj_step,
           arrayname,
           varpointer,
           value,
           mod = "") {
    ret = set_array(
      item,
      trj_step,
      activity,
      arrayname,
      row = item_id,
      column = varpointer,
      value = value,
      mod = ""
    )
  }

set_spile_array_att <-
  function(item,
           trj_step,
           activity,
           stockpile_id,
           varpointer,
           value,
           mod = "") {
    ret = set_array(
      item,
      trj_step,
      activity,
      arrayname = "stockpiles",
      row = stockpile_id,
      column = varpointer,
      value = value,
      mod = mod
    )
  }

get_array_att <- function(arrayname,activity,row,column,trj_step){
  df <- get(arrayname)
  row <- as.numeric(row)
  column=as.numeric(column)
  curr_val=df[row,column]
  r_array_log(arrayname,activity,trj_step,row,column,paste0(" is ",curr_val,"\n"))
  return(curr_val)
}
get_local_array_att <- function(item, activity,item_id, varpointer, trj_step) {
  curr_val <-
    get_array_att(
      arrayname = paste0(item,"_array"),
      activity = activity,
      row = item_id,
      column = varpointer,
      trj_step = trj_step
    )
  
}
get_stockpile_array_att <-
  function(item,
           item_id,
           activity,
           trj_step,
           stockpile_id,
           varpointer) {
    curr_val <-
      get_array_att(
        arrayname = "stockpiles",
        row = stockpile_id,
        column = varpointer,
        trj_step = trj_step,
        activity = activity
      )
  }
r_array_log <- 
  function(item, activity, trj_step, row, column, message){
  cat(paste0(item,":",activity,":block_",trj_step,":row ",row," col",column,message,"\n"))
}
# 
# env <- simmer()
# 
# item_trj <- trajectory() %>%
#   set_attribute('lhd_id', function()
#     get_global(env, 'lhd_count')) %>%
#   set_global('lhd_count', 1, mod = '+') %>%
#   log_("", tag = "loop") %>%
#   log_(function() {
#     paste0('lhd_id  ', get_attribute(env, 'lhd_id'))
#   }) %>%
#   log_('end init') %>%
#   log_('lhd:test_activity:block_1:end Init and start content') %>%
#   log_(function() {  paste0('setting global array  ', 
#                             set_spile_array_att(
#                               item = "lhd",
#                               activity = "thisactivity",
#                               trj_step=3,
#                               stockpile_id = 2,
#                               varpointer = varpointer_initstocks,
#                               value = -7
#                             ))
#   })
# 
# env %>% 
#   add_global('lhd_count',0) %>% 
#   add_generator('lhd',trajectory = item_trj,at(3), mon = 2)
# 
# sink(file = 'log.txt')
# env %>% run(100)
# sink()
# 
# 
# atts <- get_mon_attributes(env)
