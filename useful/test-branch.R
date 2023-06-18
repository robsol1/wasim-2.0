source("fns.R")
thisseed=2
modelname="no_orepass"
loglevel=1


scheduler <- function(schedule,othertext){
  if(schedule=='schedule_loader'){
    gatt <- get_global(env, 'test_key')
    #set_global('globalatt1', gatt)
    trj=gatt
  } else {
    print("schedulenotfound")
    trj=-1
  }
  return(trj)
}


###
### build Code
mod_df <- init_model(model=modelname,level = loglevel)
item <- "LHD"


mod_df <- add_trajectory_to_model(modelname=modelname,
                                  modeldf = mod_df,
                                  item = item,
                                  actvity ='add_LHD_trj',
                                  n_items = n_LHD,
                                  item_unit_capacity = LHD_unit_capacity,
                                  item_mttr=LHD_mttr,
                                  item_mtbf=LHD_mtbf)


mod_df <-  add_schedule_branch(modelname,
                               mod_df,
                               item,
                               activity = 'sched_branch',
                               trj_step = -1,
                               sched_name = "schedule_loader",
                               next_trj_step = c(1, 2),
                               continue = c(TRUE, TRUE))


mod_df <- add_activity_delay(modelname=modelname,
                             mod_df=mod_df,
                             item=item,
                             activity = 'path1',
                             trj_step = -1,
                             number_of_resources = 1,
                             item_activity_delay = 30,
                             next_trj_step=5)


mod_df <- add_activity_delay(modelname=modelname,
                             mod_df=mod_df,
                             item=item,
                             activity = 'path2',
                             trj_step = -1,
                             number_of_resources = 1,
                             item_activity_delay = 30,
                             next_trj_step=5)

mod_df <- create_close_trj(modelname=modelname,
                           modeldf = mod_df,
                           item)








code <- join_code(mod_df)
path <- paste0(modelname,"/",modelname,"_code.R")
save_text_to_file(code,path)
thisseed=2
source(path)
env <- env %>% 
  add_global('test_key',2)
logfile <- paste0(modelname,"/",modelname,"_log1.log")
con <- file(logfile)
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")
env <- env %>% run(10000)
sink(type = "message")
sink()
close(con )
log <- read_log(logfile)
attributes <- get_attributes(env)
View(log)


arrivals <- get_mon_arrivals(env)
resources <- get_mon_resources(env)

#plot(resources, metric = "utilization")
#plot(arrivals, metric = "flow_time")
pltdata <- attributes %>% 
  filter(key=="truck_id")

pltdata %>% ggplot(aes(x=time,y=value)) +
  geom_step()+
  facet_wrap(facets=vars(name))


get_palette <- scales::brewer_pal(type = "qual", palette = 1)
plot(truck_trj, fill = get_palette,height=3000)
plot(LHD_trj, fill = get_palette,height=3000)

