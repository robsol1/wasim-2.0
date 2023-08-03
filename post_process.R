## Set up the pointers and details of the sequence and run you are interested in.

source("fns.R")
modelname="orepass"
sequence_desc='LongHaul'
source(paste0(modelname, "/build.R"))
run_id <- 1


# This code takes a close look at a single run

# Read in the data
run_dir <-
  paste0(sequence_desc, "run_id_", sprintf("%03.0f", run_id), "/")
log <- read_log(paste0(run_dir, "log_file.log"))
stocks <- get_stocks(log=log,pilenames=stockpiles$pilenames)
attributes <- get_attributes(paste0(run_dir, "attributes_file.csv"))

# collects information of when signals are sent during model run useful in debugging
signals <- get_signals(log)
# looks at the sequence that an item moves through - useful in debugging
block_seq <- get_block_seq(log)
# Looks at the last event recorded by an item, useful to ensure that items didn't get stuck during a run
last_event <- get_last_event(log)

#Plot the trend of stocks thgroughout the model
plot_stocks(log=log,pilenames=stockpiles$pilenames)

#plot the status of items
status <- get_status(attributes)
plot_status(status)

# Block flow of model- Useful for visualising the overall flow of the model
# The layout changes each time you plot it but the sequence is constant.
# Suggest re-running until you get a good visualisation of the pattern
plot_blocks(mod_df, height = 22, width = 32)

# This will summarise all the data from a suite of runs into a single dataframe.  Usefull for understanding the results of 
# a scenario with multiple runs.

tot_sum <- summarise_all_runs(sequence_desc)
