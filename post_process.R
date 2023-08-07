## Set up the pointers and details of the sequence and run you are interested in.


source("fns.R")

modelname="orepass"
sequence_desc='ShortHaul'
input_xl_file="Scenario_Inputs.xlsx"
input_xl_sheet="ShortHaul"

### Run to generate appropriate pointers
sequence_desc <- paste0(paste0(modelname,'/',sequence_desc, "/"))
inputs <- read_excel(paste0(sequence_desc,input_xl_file),sheet=input_xl_sheet)
inputs <- inputs[!is.na(inputs$Seq),]
source(paste0(modelname,"/_add_special_blocks.R"))
source(paste0(modelname, "/build.R"))



# This code takes a close look at a single run

# Read in the data
run_id <- 1
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

#Plot the trend of stocks throughout the model
plot_stocks(log=log,pilenames=stockpiles$pilenames)

#plot the status of items
status <- get_status(attributes)
plot_status(status)

# Block flow of model- Useful for visualizing the overall flow of the model
# The layout changes each time you plot it but the sequence is constant.
# Suggest re-running until you get a good visualization of the pattern
plot_blocks(mod_df, height = 22, width = 32)

# This will summaries all the data from a suite of runs into a single dataframe.  Useful for understanding the results of 
# a scenario with multiple runs.

tot_sum <- summarise_all_runs(sequence_desc)
