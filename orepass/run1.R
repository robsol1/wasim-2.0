
source("fns.R")
t <- run_model(
  modelname = "orepass",
  sequence_desc = 'HighReliability',
  input_xl_file = "Scenario_Inputs.xlsx",
  input_xl_sheet = "HighReliability",
  run_duration = 7 * 24 * 3600
)