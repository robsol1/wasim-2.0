loglevel <- inputs$loglevel[run_id]
thisseed <- inputs$seed[run_id]

## define data inputs

stockpiles <- data.frame(
  pilenames = c(
    'drawpoint',
    'orepass_feed_hopper',
    'orepass_stocks',
    'hoist_stock'
  ),
  maxstocks = c(
    inputs$drawpoint_max_stock[run_id],
    inputs$orepass_feed_hopper_max_stock[run_id],
    inputs$orepass_stocks_max_stock[run_id],
    inputs$hoist_stock_max_stock[run_id]
  ),
  initstocks = c(
    inputs$drawpoint_init_stock[run_id],
    inputs$orepass_feed_hopper_init_stock[run_id],
    inputs$orepass_stocks_init_stock[run_id],
    inputs$hoist_stock_init_stock[run_id]
  ),
  access_limit = c(
    inputs$drawpoint_access_limit[run_id],
    inputs$orepass_feed_hopper_access_limit[run_id],
    inputs$orepass_stocks_access_limit[run_id],
    inputs$hoist_stock_access_limit[run_id]
  ),
  current_access_assigned=c(0,0,0,0),
  current_stocks=c(0,0,0,0),
  committed=c(0,0,0,0)
)

spnames <- names(stockpiles)
varpointer_spilenames <- which(spnames=='pilenames')
varpointer_maxstocks <- which(spnames=='maxstocks')
varpointer_initstocks <- which(spnames=='initstocks')
varpointer_access_limit <- which(spnames=='access_limit')
varpointer_current_access_assigned <- which(spnames=='current_access_assigned')
varpointer_current_stocks <- which(spnames=='current_stocks')
varpointer_committed <- which(spnames=='committed')
stockpile_id_txt <- ""
for(name in stockpiles$pilenames) {
  stockpile_id_txt <-
    paste0(stockpile_id_txt,
           name,
           '_id <- ',
           which(stockpiles$pilenames == name),
           '
')
}
eval(parse(text=stockpile_id_txt))

stockpiles <- stockpiles %>% 
  mutate(current_stocks=initstocks)
#################################################
mod_df <- init_model(model=modelname,level = loglevel)


#####################################################
item="lhd"
vardf <- inputs %>%
  select(starts_with(item))
mod_df <-
  add_trajectory_to_model(
    mod_df = mod_df,
    item = item,
    activity = paste0("init_",item,"_trj"),
    n_item = eval(parse(text=paste0("inputs$n_",item,"[run_id]"))),
    varnames = names(vardf),
    varlist = vardf[run_id, ]
  )

mod_df <- add_load_haul_dump(mod_df= mod_df,
                             item=item,
                             stockpiles_df=stockpiles,
                             from_stockpile_name="drawpoint",
                             to_stockpile_name ="orepass_feed_hopper",
                             access_val = 1)

mod_df <- create_close_trj(
  item = item,
  mod_df = mod_df,
  trj_step = -1,
  activity = 'loop_back_to_start'
)

######################################################
item="conveyor"
vardf <- inputs %>%
  select(starts_with(item))
mod_df <-
  add_trajectory_to_model(
    mod_df = mod_df,
    item = item,
    activity = paste0("init_",item,"_trj"),
    n_item = eval(parse(text=paste0("inputs$n_",item,"[run_id]"))),
    varnames = names(vardf),
    varlist = vardf[run_id, ]
  )
mod_df <- add_load_haul_dump(mod_df= mod_df,
                             item=item,
                             stockpiles_df=stockpiles,
                             from_stockpile_name="orepass_feed_hopper",
                             to_stockpile_name ="orepass_stocks",
                             access_val = 1)

mod_df <- create_close_trj(
  item = item,
  mod_df = mod_df,
  trj_step = -1,
  activity = 'loop_back_to_start'
)
item <- "truck"
vardf <- inputs %>%
  select(starts_with(item))
mod_df <-
  add_trajectory_to_model(
    mod_df = mod_df,
    item = item,
    activity = paste0("init_",item,"_trj"),
    n_item = eval(parse(text=paste0("inputs$n_",item,"[run_id]"))),
    varnames = names(vardf),
    varlist = vardf[run_id, ]
  )
mod_df <- add_load_haul_dump(mod_df= mod_df,
                             item=item,
                             stockpiles_df=stockpiles,
                             from_stockpile_name="orepass_stocks",
                             to_stockpile_name ="hoist_stock",
                             access_val = 1)

mod_df <- create_close_trj(
  item = item,
  mod_df = mod_df,
  trj_step = -1,
  activity = 'loop_back_to_start'
)




#####################################################