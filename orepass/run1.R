
source("fns.R")



modelname="orepass"
sequence_desc='initial suite'
input_xl_file="Scenario_Inputs.xlsx"
input_xl_sheet="LongHaul"
source(paste0(modelname,"/_add_special_blocks.R"))
run_duration =7*24*3600





model_path <- paste0(modelname,"/")
sequence_desc <- paste0(model_path,sequence_desc,"/")
inputs <- read_excel(paste0(sequence_desc,input_xl_file),sheet=input_xl_sheet)
inputs <- inputs[!is.na(inputs$Seq),]
runs <- nrow(inputs)
i=1
for(run_id in 1:runs) {
  ##
  # create sub directory for this run
  ##
  run_dir <-
    paste0(sequence_desc, "run_id_", sprintf("%03.0f", run_id), "/")
  if (!dir.exists(run_dir)) {
    dir.create(run_dir)
  }
  ##
  # Build model code and save to file
  ##
  source(paste0(modelname, "/build.R"))
  code <- join_code(mod_df)
  path <- paste0(run_dir,"code.R")
  save_text_to_file(code,path)
  ##
  # Compile the model
  ##
  source(path)
  ##
  # Divert output to log file
  log_file <- paste0(run_dir, "log_file.log")
  con <- file(log_file)
  sink(con, append = TRUE)
  sink(con, append = TRUE, type = "message")
  ##
  # Run Model
  env <- env %>% run(run_duration)
  ##
  # switch output back to module
  ##
  sink(type = "message")
  sink()
  close(con)
  ##
  # Extract attributes from "env"
  ##
  attributes <- get_mon_attributes(env)
  write.csv(attributes,paste0(run_dir, "attributes_file.csv"))
  write.csv(bogger_array,paste0(run_dir, "bogger_array.csv"))
  write.csv(truck_array,paste0(run_dir, "truck_array.csv"))
}







