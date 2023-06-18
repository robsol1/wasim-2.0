
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

start_code <- function(trj_step,next_trj_step){
paste0("
item_trj <- item_trj %>%
  ## branch_if_not_activity start
  branch(
    option = function() ifelse(get_attribute(env, 'item_next_block') == ",trj_step,", 1, 2),
    continue = c(TRUE, TRUE),
    trajectory('item_activity_stay_in_block') %>%
        set_attribute('item_next_block',",next_trj_step,") %>% ")}

end_code <- function(trj_step) {
  paste0(
    ",
    trajectory('item_activity_skip_this_block') %>%
      ",robs_log(trj_step, 'Block id is not next block so skip block', pipe = FALSE),
    ") %>%
  ",
    robs_log(trj_step, 'End and go to next block', pipe = FALSE)
  )
}
