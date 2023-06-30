
source("fns.R")



modelname="no_orepass"
sequence_desc='initial suite'
input_xl_file="inputs.xlsx"
input_xl_sheet="inputs"
source(paste0(modelname,"/_add_special_blocks.R"))
run_duration =7*24*3600





model_path <- paste0(modelname,"/")
sequence_desc <- paste0(model_path,sequence_desc,"/")
inputs <- read_excel(paste0(sequence_desc,input_xl_file),sheet=input_xl_sheet)
inputs <- inputs[!is.na(inputs$Seq),]
runs <- nrow(inputs)


run_id=1

loglevel <- inputs$loglevel[run_id]
thisseed <- inputs$seed[run_id]

## define data inputs

stockpiles <- data.frame(
  pilenames = c(
    'drawpoint',
    'muckpile',
    'hoist_stock'
  ),
  maxstocks = c(
    inputs$drawpoint_max_stock[run_id],
    inputs$muckpile_max_stock[run_id],
    inputs$hoist_stock_max_stock[run_id]
  ),
  initstocks = c(
    inputs$drawpoint_init_stock[run_id],
    inputs$muckpile_init_stock[run_id],
    inputs$hoist_stock_init_stock[run_id]
  ),
  access_limit = c(
    inputs$drawpoint_access_limit[run_id],
    inputs$muckpile_access_limit[run_id],
    inputs$hoist_stock_access_limit[run_id]
  ),
  current_access_assigned=c(0,0,0),
  current_stocks=c(0,0,0),
  committed=c(0,0,0)
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

mod_df <- add_sched_server_loads_client(
  mod_df=mod_df,
  client="truck",
  server=item,
  trj_step = -1,
  next_trj_step = c(1, 3),
  from_stockpile_name="muckpile",
  to_stockpile_name="hoist_stock",
  client_queue_att_name = NULL)
## matched loader with truck - now seek access to stockpile
mod_df <- add_sched_access_to_stockpile(
  mod_df,
  item = item,
  activity = paste0("sched_", item, "_to_", "muckpile"),
  trj_step = -1,
  next_trj_step = c(1, 2),
  continue = c(TRUE, TRUE),
  stockpile_name = "muckpile",
  access_val= 1
)
## if no access then wait
mod_df <- add_wait_signal(
  mod_df = mod_df,
  item = item,
  activity = paste0(item,"wait_release_muckpile_access"),
  trj_step = -1,
  next_trj_step = -1,
  relative = TRUE,
  signal_name = paste0("release_muckpile_access"),
  wait_status_text = 's_wait_stock_access'
)
## now you can load the truck
mod_df <- add_server_loads_client(
    mod_df=mod_df,
    server='lhd',
    client='truck',
    trj_step=-1,
    breakdown_flag = TRUE,
    relative=FALSE,
    TUM_text='s_working',
    stockpile="muckpile",
    access_val = 1)

## finished loading truck now seek haulage task
mod_df <- add_load_haul_dump(mod_df=mod_df,
                             item=item,
                             stockpiles_df=stockpiles,
                             from_stockpile_name="drawpoint",
                             to_stockpile_name="muckpile",
                             access_val=1)
# close the \trajectory
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
mod_df <-add_client_loaded_by_server(mod_df=mod_df,
           client=item,
           server="lhd",
           activity = NULL,
           trj_step = -1,
           next_trj_step = -1,
           relative=FALSE,
           queue_name=NULL,
           working_status_text="s_working",
           wait_status_text = "s_wait_sec_eq",
           start_event_signal=NULL,
           end_event_signal=NULL)
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
  stockpiles_df=stockpiles,
  stockpile_name = "hoist_stock",
  putorget = "put",
  access_val=1
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
mod_df <- create_close_trj(
  item = item,
  mod_df = mod_df,
  trj_step = -1,
  activity = 'loop_back_to_start'
)


plot_blocks(mod_df, height = 22, width = 32)

#####################################################