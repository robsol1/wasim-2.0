source("fns.R")
modelname="orepass"
sequence_desc='LongHaul'
source(paste0(modelname, "/build.R"))
run_id <- 1


run_dir <-
  paste0(sequence_desc, "run_id_", sprintf("%03.0f", run_id), "/")

log <- read_log(paste0(run_dir, "log_file.log"))
stocks <- get_stocks(log=log,pilenames=stockpiles$pilenames)
signals <- get_signals(log)
attributes <- get_attributes(paste0(run_dir, "attributes_file.csv"))
block_seq <- get_block_seq(log)
last_event <- get_last_event(log)
status <- get_status(attributes)
p <- plot_stocks(log=log,pilenames=stockpiles$pilenames)
print(p)
plot_status(status)
plot_blocks(mod_df, height = 22, width = 32)

tot_sum <- summarise_all_runs(sequence_desc)
