

access_stockpile <- function(mod_df,
                             item,
                             stockpiles_df,
                             stockpile_name,
                             putorget,
                             access_val) {
  
  if (putorget == "get") {
    sigval = "blocked"
    sigverb = "_from_"
  }  else if (putorget == "put") {
    sigval = "starved"
    sigverb = "_to_"
  } else {
    print("Invalid getorput command")
  }
  stockpile_id <- which(stockpiles_df$pilenames ==stockpile_name)
  mod_df <- add_sched_access_to_stockpile(
    mod_df,
    item = item,
    activity = paste0("sched_", item, "_to_", stockpile_name),
    trj_step = -1,
    next_trj_step = c(1, 2),
    continue = c(TRUE, TRUE),
    stockpile_name = stockpile_name,
    access_val= access_val
  )
  mod_df <- add_wait_signal(
    mod_df = mod_df,
    item = item,
    activity = paste0(item,"wait_release_", stockpile_name, "_access"),
    trj_step = -1,
    next_trj_step = -1,
    relative = TRUE,
    signal_name = paste0("release_", stockpile_name, "_access"),
    wait_status_text = 's_wait_stock_access'
  )
  mod_df <- add_get_or_put_activity_delay_from_array(
    mod_df=mod_df,
    item = item,
    activity = paste0(item,"_",putorget, sigverb ,stockpile_name),
    trj_step = -1,
    next_trj_step = -1,
    TUM_text = 's_working',
    stockpile_id = stockpile_id,
    type = putorget,
    access_val = 1,
    breakdown_flag = TRUE,
    relative = FALSE,
    send_Signal_name = paste0("release_", sigval, "_", stockpile_name)
  )
  mod_df <- add_send_signal(
    mod_df = mod_df,
    item = item,
    activity = paste0(item,"_send_release_", stockpile_name, "_access"),
    trj_step = -1,
    next_trj_step = -1,
    relative = FALSE,
    signal_name = paste0("release_", stockpile_name, "_access")
  )
  mod_df
}

add_load_haul_dump <-
  function(mod_df,
           item,
           stockpiles_df,
           from_stockpile_name,
           to_stockpile_name,
           access_val) {
    
    ## add schedular to determine if all stocks and space available
    mod_df <- add_schedule_Load_haul_dump(
      mod_df=mod_df,
      item=item,
      activity = paste0("sched_",item,"_haul_route"),
      trj_step = -1,
      next_trj_step = c(1, 2, 3),
      continue = c(TRUE, TRUE, TRUE),
      from_pile_name=from_stockpile_name,
      to_pile_name = to_stockpile_name,
      commit_tonnes =TRUE
    )
  
    mod_df <- add_wait_signal(
      mod_df = mod_df,
      item = item,
      activity = paste0(item,"_wait_release_blocked_",to_stockpile_name),
      trj_step = -1,
      next_trj_step = -1,
      relative = TRUE,
      signal_name = paste0("release_blocked_",to_stockpile_name),
      wait_status_text = 's_wait_downstream_stock'
    )
    mod_df <- add_wait_signal(
      mod_df = mod_df,
      item = item,
      activity = paste0(item,"_wait_release_starved_",from_stockpile_name),
      trj_step = -1,
      next_trj_step = -2,
      relative = TRUE,
      signal_name = paste0("release_starved_",from_stockpile_name),
      wait_status_text = 's_wait_upstream_stock'
    )
    
    mod_df <-
      access_stockpile(
        mod_df = mod_df,
        item = item,
        stockpiles_df=stockpiles_df,
        stockpile_name = from_stockpile_name,
        putorget = "get",
        access_val=access_val
      )
    mod_df <- add_activity_delay_from_array(
      mod_df = mod_df,
      item = item,
      activity = paste0(item,'_travel_loaded'),
      trj_step = -1,
      next_trj_step = -1,
      relative = FALSE,
      breakdown = TRUE,
      TUM_text = 's_working'
    )
  
    mod_df <- access_stockpile(
        mod_df = mod_df,
        item = item,
        stockpiles_df=stockpiles_df,
        stockpile_name = to_stockpile_name,
        putorget = "put",
        access_val=access_val
      )
    mod_df <- add_activity_delay_from_array(
      mod_df = mod_df,
      item = item,
      activity = paste0(item,'_travel_empty'),
      trj_step = -1,
      next_trj_step = -1,
      relative = FALSE,
      breakdown = TRUE,
      TUM_text = 's_working'
    )
    return(mod_df)
  }



