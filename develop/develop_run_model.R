source("fns.R")
source("develop/_add_special_blocks.R")
source("develop/_sched_code.R")
# set sensitivity variables (make sure they are not overwritten by build code)
modelname = 'develop'
scenario_desc='initial suite'
n_trucks <- 1
n_trucks=1
runduration =  7*24 * 3600
loglevel = 1
reps = 1
verbose= TRUE
thisseed=2



## Run the model reps times
rm(summary_result)
sens_step=0
#run_id <-  makevalidname(as.character(Sys.time()))
run_id <- "debug"
model_path <- paste0(modelname,"/")
scen_dir <- paste0(model_path,scenario_desc,"/")
rundir <- paste0(scen_dir,run_id,"/") 
run_model(modelname=modelname,
           scenario_desc=scenario_desc,
           runduration=runduration,
           reps=reps,
           thisseed=thisseed ,
           loglevel=loglevel,
           verbose=verbose)

sink()


