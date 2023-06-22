
check_trj_step <- function(trj_step,mod_df) {
  if (trj_step < 0) {
    trj_step <- length(which(mod_df$item == item))# + 1
  } else{
    trj_step=trj_step
  }
}
check_next_trj_step <-
  function(next_trj_step, trj_step,relative) {
    if (relative) {
      next_trj_step = trj_step + next_trj_step
    } else {
      if (next_trj_step < 0) {
        next_trj_step <- trj_step + 1
      } else{
        next_trj_step = next_trj_step
      }
    }
  }

start_code <- function(item,activity,trj_step,next_trj_step){
paste0(item,"_trj <- ",item,"_trj %>%
  ## branch_if_not_activity start
  branch(
    option = function() ifelse(get_attribute(env, '",item,"_next_block') == ",trj_step,", 1, 2),
    continue = c(TRUE, TRUE),
    trajectory('",item,"_activity_stay_in_block') %>%
        set_attribute('",item,"_next_block',",next_trj_step,") %>% ")}

end_code <- function(item,activity,trj_step) {
  paste0(
    ",
    trajectory('",item,"_activity_skip_this_block') %>%
      ",robs_log(item,activity,trj_step, 'Block id is not next block so skip block', pipe = FALSE),
    ") %>%
  ",
    robs_log(item,activity,trj_step, 'End and go to next block', pipe = FALSE)
  )
}


update_stocks <- function(item,activity,trj_step,stockpile_id,varpointer,value,mod=""){
  paste0(
    "log_(function() {  
    paste0('setting global array  ', 
      set_spile_array_att(
        item = '",item,"',
        trj_step=",trj_step,",
        activity = '",activity,"',
        spile_id = ",stockpile_id,"
        varpointer = ",varpointer,",
        value = ",value,",
        mod = '",mod,"'
      ))
})"
  )
}

update_local_array_from_array <- function(item,activity,trj_step,arrayname,from_ptr,to_ptr,mod=""){
  paste0(
"log_(function() { as.character(
    set_array_from_array(
      item = '",item,"',
      activity='",activity,"',
      row = get_attribute(env, 'item_id'),
      trj_step = ",trj_step,",
      arrayname = '",paste0(item,"_array'"),",
      from_ptr = ",from_ptr,",
      to_ptr = ",to_ptr,",
      mod = '",mod,"'
    )
)})"
  )
}
