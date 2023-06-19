source("fns.R")
source("develop/_add_special_blocks.R")
thisseed=2
modelname="develop"
loglevel=1

##############################################
#Assign Variables
#############################################
## stockpiles

drawpoint_stocks_max = 9999999
stope_stocks_max =1000
hoist_stocks_max = 9999999
pilenames=c('drawpoint','stope_stock','hoist_stock')
maxstocks=c(drawpoint_stocks_max,stope_stocks_max,hoist_stocks_max) 
initstocks=c(drawpoint_stocks_max,stope_stocks_max/2,0)
access_limit=c(1,1,1) # number of activities that can operate on pile at any one time


## Truck Inputs
n_trucks=1

truck_mttr_txt = 'function() max(1, rnorm(1, 3600, 360))'
truck_mtbf_txt = 'function() max(1, rexp(1, rate = 1/(24*3600)))'
truck_travel_empty_delay_txt= 'function() max(1, rnorm(1, 1800, 180))'
truck_travel_full_delay_txt= 'function() max(1, rnorm(1, 2400, 240))'
truck_dumps_to_hoist_delay_txt= 'function() max(1, rnorm(1, 90, 9))'

## LHD Inputs
n_lhd=1
lhd_mttr_txt = 'function() max(1, rnorm(1, 3600, 360))'
lhd_mtbf_txt = 'function() max(1, rexp(1, rate = 1/(1*3600)))'
lhd_travel_empty_delay_txt= 'function() max(1, rnorm(1, 180, 18))'
lhd_loads_from_drawpoint_Delay_txt='function() max(1, rnorm(1, 120, 18))'
lhd_loads_truck_Delay_txt='function() max(1, rnorm(1, 120, 18))'
lhd_travel_full_delay_txt= 'function() max(1, rnorm(1, 200, 5))'
lhd_dumps_to_stope_stock_delay_txt='function() max(1, rnorm(1, 60, 18))'

# LOading Inputs
avg_lhd_Bucket_size_txt=21
truck_passes=3

## Derived inputs
truck_unit_capacity_txt=avg_lhd_Bucket_size_txt*truck_passes

## Schedule Block



##############################################
#Build Code
#############################################


## Initialise model
mod_df <- init_model(model=modelname,level = loglevel)

## Add stockpiles
mod_df <-
  build_stockpiles(
    modeldf = mod_df,
    pilenames = pilenames,
    maxstocks = maxstocks,
    initstocks = initstocks,
    access_limit=access_limit
  )



item='lhd'


mod_df <- add_trajectory_to_model(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'start_lhd_trj',
  n_items = n_lhd,
  item_unit_capacity = avg_lhd_Bucket_size_txt,
  item_mttr_txt = lhd_mttr_txt,
  item_mtbf_txt = lhd_mtbf_txt
)
mod_df <- add_schedule_branch(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'sched_branch',
  trj_step = -1,
  sched_name = "assign_LHD_task",
  next_trj_step = c(1, 2, 3, 4 , 8), # 1= wait DS, 2= Wait US,3=Wait Access,4=Load Truck,5 Haul from DP
  continue = c(TRUE, TRUE, TRUE, TRUE, TRUE))
mod_df <- add_wait_signal(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'wait_release_blocked_lhd',
  trj_step = -1,
  next_trj_step = -1,
  relative = TRUE,
  signal_name = "release_blocked_lhd",
  wait_status_text = 's_wait_downstream_stock'
)
mod_df <- add_wait_signal(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'wait_release_starved_lhd',
  trj_step = -1,
  next_trj_step = -2,
  relative = TRUE,
  signal_name = "release_starved_lhd",
  wait_status_text = 's_wait_upstream_stock'
)
mod_df <- add_wait_signal(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'wait_stope_stock_access',
  trj_step = -1,
  next_trj_step = -3,
  relative = TRUE,
  signal_name = "release_stope_stock_access",
  wait_status_text = 's_wait_stock_access'
)

## LHD to load Truck
to_name <-
  c(
    'lhd_loads_truck_Delay',
    'truck_unit_capacity',
    'stope_stock_stocks_commited'
  )
from_name <- c(lhd_loads_truck_Delay_txt,
               truck_unit_capacity_txt,
               'avg_lhd_Bucket_size'
)
mod_code <- c("","",'+')
from_type <- c("fn","fn","local")
to_type <- c("local","local","global")
mod_df <- add_set_multiple_attributes(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'assigning_loader_to_truck',
  trj_step = -1,
  next_trj_step = -1,
  to_name = to_name,
  from_name = from_name,
  mod_code = mod_code,
  from_type = from_type,
  to_type = to_type
)
mod_df <- add_loader_loads_item(
  modelname=modelname,
  mod_df=mod_df,
  item=item,
  activity = 'loads_truck',
  trj_step = -1,
  next_trj_step = -1,
  TUM_text = 's_working',
  delay_att_name = 'lhd_loads_truck_Delay',
  unit_size_att = 'truck_unit_capacity',
  stockpile='stope_stock',
  getorput='get',
  breakdown_flag = TRUE,
  start_event_signal='loader_start_loading_truck',
  end_event_signal='loader_end_loading_truck'
)
mod_df <- add_send_signal(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'send_release_blocked_lhd',
  trj_step = -1,
  next_trj_step = -1,
  signal_name = "release_blocked_lhd"
)
mod_df <- add_send_signal(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'send_release_stope_stock_access',
  trj_step = -1,
  next_trj_step = -7,
  relative=TRUE,
  signal_name = "release_stope_stock_access"
)
## LHD to drawpoint
to_name <-
  c(
    'lhd_travel_empty_delay',
    'lhd_loads_from_drawpoint_Delay',
    'lhd_travel_full_delay',
    'lhd_dumps_to_stope_stock_delay',
    'avg_lhd_Bucket_size',
    'stope_stock_stocks_commited'
  )
from_name <- c(
  lhd_travel_empty_delay_txt,
  lhd_loads_from_drawpoint_Delay_txt,
  lhd_travel_full_delay_txt,
  lhd_dumps_to_stope_stock_delay_txt,
  avg_lhd_Bucket_size_txt,
  'avg_lhd_Bucket_size'
)
mod_code <- c("","","","","",'+')
from_type <- c("fn","fn","fn","fn","fn","local")
to_type <- c("local","local","local","local","local","global")
mod_df <- add_set_multiple_attributes(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'assign_lhd_to_drawpoint',
  trj_step = -1,
  next_trj_step = -1,
  to_name = to_name,
  from_name = from_name,
  mod_code = mod_code,
  from_type = from_type,
  to_type = to_type
)
mod_df <- add_activity_with_breakdown(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'travel_empty',
  trj_step = -1,
  next_trj_step = -1,
  TUM_text = 's_working',
  delay_att_name = 'lhd_travel_empty_delay',
  unit_size_att = 'avg_lhd_Bucket_size'
)
mod_df <- add_getorput_activity_delay_from_atts(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'load_from_drawpoint',
  trj_step = -1,
  next_trj_step = -1,
  TUM_text = 's_working',
  delay_att_name = 'lhd_loads_from_drawpoint_Delay',
  unit_size_att = 'avg_lhd_Bucket_size',
  stockpile = 'drawpoint',
  getorput = 'get',
  breakdown_flag = TRUE
)
mod_df <- add_activity_with_breakdown(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'travel_full',
  trj_step = -1,
  next_trj_step = -1,
  TUM_text = 's_working',
  delay_att_name = 'lhd_travel_full_delay',
  unit_size_att = 'avg_lhd_Bucket_size'
)
mod_df <- add_schedule_branch(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'seek_stope_stock_access',
  trj_step = -1,
  sched_name = "assign_LHD_to_Stockpile",
  next_trj_step = c(1, 2),
  # 1= Wait Access,4 = dump
  continue = c(TRUE, TRUE)
)
mod_df <- add_wait_signal(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'wait_for_stope_stock_access',
  trj_step = -1,
  next_trj_step = -1,
  relative = TRUE,
  signal_name = "release_stope_stock_access",
  wait_status_text = 's_wait_stock_access'
)
mod_df <- add_getorput_activity_delay_from_atts(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'dump_to_stope_stocks',
  trj_step = -1,
  next_trj_step = -1,
  TUM_text = 's_working',
  delay_att_name = 'lhd_dumps_to_stope_stock_delay',
  unit_size_att = 'avg_lhd_Bucket_size',
  stockpile = 'stope_stock',
  getorput = 'put',
  breakdown_flag = TRUE
)

mod_df <- add_send_signal(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'send_release_stope_stock_access',
  trj_step = -1,
  next_trj_step = -1,
  signal_name = "release_stope_stock_access"
)
mod_df <- add_send_signal( #not used in this model because schedular handles
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'send_release_starved_truck',
  trj_step = -1,
  next_trj_step = -1,
  signal_name = "release_starved_truck"
)
mod_df <- create_close_trj(modelname,mod_df,item,"end_lhd_trj_and_ret_to_start")


item='truck'
to_name <-
  c(
    'truck_travel_empty_delay',
    'lhd_loads_truck_Delay_txt',
    'truck_travel_full_delay',
    'truck_dumps_to_hoist_delay',
    'truck_unit_capacity'
  )
from_name <- c(truck_travel_empty_delay_txt,
               lhd_loads_truck_Delay_txt,
               truck_travel_full_delay_txt,
               truck_dumps_to_hoist_delay_txt,
               truck_unit_capacity_txt
)
mod_code <- c("","","","","",'-')
from_type <- c("fn","fn","fn","fn","fn")
to_type <- c("local","local","local","local","local")
mod_df <- add_trajectory_to_model(modelname=modelname,
                                  mod_df = mod_df,
                                  item = item,
                                  activity ='start_truck_trj',
                                  n_items = n_trucks,
                                  item_unit_capacity = avg_lhd_Bucket_size_txt,
                                  item_mttr_txt =lhd_mttr_txt,
                                  item_mtbf_txt =lhd_mtbf_txt)

mod_df <- add_send_signal(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'send_signal_when_truck_arrives',
  trj_step = -1,
  next_trj_step = -1,
  signal_name = "release_blocked_lhd"
)

mod_df <- add_item_loaded_by_item (
  modelname=modelname,
  mod_df=mod_df,
  item=item,
  activity = "truck_loaded_by_loader",
  trj_step = -1,
  next_trj_step = -1,
  queue_name="trucks_waiting_loader",
  working_status_text='s_working',
  wait_status_text='s_wait_sec_eq',
  start_event_signal='loader_start_loading_truck',
  end_event_signal='loader_end_loading_truck',
  unit_size_att='truck_unit_capacity'
)
mod_df <- add_set_multiple_attributes(
  modelname = modelname,
  mod_df = mod_df,
  item = item,
  activity = 'adding_mult_atts_truck',
  trj_step = -1,
  next_trj_step = -1,
  to_name = to_name,
  from_name = from_name,
  mod_code = mod_code,
  from_type = from_type,
  to_type = to_type
)


mod_df <- add_activity_with_breakdown(modelname=modelname,
                                      mod_df=mod_df,
                                      item=item,
                                      activity = 'truck_travel_full',
                                      trj_step = -1,
                                      next_trj_step=-1,
                                      TUM_text = 's_working',
                                      delay_att_name = 'truck_travel_full_delay',
                                      unit_size_att = 'truck_unit_capacity')
mod_df <- add_getorput_activity_delay_from_atts(modelname=modelname,
                                                mod_df=mod_df,
                                                item=item,
                                                activity = 'dump_to_stope_stocks',
                                                trj_step = -1,
                                                next_trj_step=-1,
                                                TUM_text = 's_working',
                                                delay_att_name = 'truck_dumps_to_hoist_delay_txt',
                                                unit_size_att = 'truck_unit_capacity',
                                                stockpile='stope_stock',
                                                getorput = 'put',
                                                breakdown_flag = TRUE)
mod_df <- add_activity_with_breakdown(modelname=modelname,
                                      mod_df=mod_df,
                                      item=item,
                                      activity = 'truck_travel_empty',
                                      trj_step = -1,
                                      next_trj_step=-1,
                                      TUM_text = 's_working',
                                      delay_att_name = 'truck_travel_empty_delay',
                                      unit_size_att = 'truck_unit_capacity')
mod_df <- create_close_trj(modelname,mod_df,item,activity="End_truck_traj_and_ret_to_start")


code <- join_code(mod_df)
path <- paste0(modelname,"/",modelname,"_code.R")
save_text_to_file(code,path)
t

source(path)


