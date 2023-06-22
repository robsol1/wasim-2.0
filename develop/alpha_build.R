source("fns.R")
modelname="develop"
scenario_desc='initial suite'
model_path <- paste0(modelname,"/")
scen_dir <- paste0(model_path,scenario_desc,"/")
source(paste0(modelname,"/_add_special_blocks.R"))
source(paste0(modelname,"/_sched_code.R"))
inputs <- read_excel(paste0(scen_dir,"inputs.xlsx"))
seq=2

loglevel <- inputs$loglevel[seq]
thisseed <- inputs$seed[seq]


## define data inputs

stockpiles <- data.frame(
  pilenames = c(
    'drawpoint_stock',
    'orepass_feed_hopper',
    'orepass_stocks',
    'hoist_stock'
  ),
  maxstocks = c(
    inputs$drawpoint_stock_max_stock[seq],
    inputs$orepass_feed_hopper_max_stock[seq],
    inputs$orepass_stocks_max_stock[seq],
    inputs$hoist_stock_max_stock[seq]
  ),
  initstocks = c(
    inputs$drawpoint_stock_init_stock[seq],
    inputs$orepass_feed_hopper_init_stock[seq],
    inputs$orepass_stocks_init_stock[seq],
    inputs$hoist_stock_init_stock[seq]
  ),
  access_limit = c(
    inputs$drawpoint_stock_access_limit[seq],
    inputs$orepass_feed_hopper_access_limit[seq],
    inputs$orepass_stocks_access_limit[seq],
    inputs$hoist_stock_access_limit[seq]
  ) 
)

n_lhds=inputs$n_lhds[seq]




# lhd_mttr_txt=inputs$lhd_mttr_code[seq]
# lhd_mtbf_txt=inputs$lhd_mtbf_code[seq]
# lhd_travel_empty_delay_txt= inputs$lhd_travel_empty_delay[seq]
# lhd_loads_from_drawpoint_Delay_txt=inputs$lhd_loads_from_drawpoint_Delay[seq]
# lhd_travel_full_delay_txt= inputs$lhd_travel_full_delay[seq]
# lhd_dumps_to_orepass_feed_hoppe_delay_txt=inputs$lhd_dumps_to_orepass_feed_hopper_delay[seq]

mod_df <- init_model(model=modelname,level = loglevel)
mod_df <- build_stockpiles(mod_df=mod_df,stockpile_df_name="stockpiles")
## lhd
item = 'lhd'
vardf <- inputs %>% 
  select(starts_with('lhd'))

mod_df <-
  add_trajectory_to_model(
    mod_df = mod_df,
    item = item,
    activity="init_lhd_trj",
    n_item = n_lhds,
    varnames = names(vardf),
    varlist = vardf[seq, ]
  )



mod_df <- add_activity_delay_from_array(
  mod_df=mod_df,
  item,
  activity = 'travel_empty',
  trj_step = -1,
  next_trj_step = -1,
  relative = FALSE,
  TUM_text = 's_working'
)




code <- join_code(mod_df)
path <- paste0(modelname,"/",modelname,"_code.R")
save_text_to_file(code,path)
source(path)



env <- env %>% run(1000)
atts<- get_mon_attributes(env)
