#install.packages('simmer', repos = c('https://r-simmer.r-universe.dev', 'https://cloud.r-project.org'))
# install.packages("tidyverse")
# install.packages('data.table')
# install.packages('ggplot')
# install.packages('simmer.plot')

library(simmer)
library(simmer.plot)
library(tidyverse)
library(data.table)
library(ggplot2)
library(stringr)
library(igraph)
library(readxl)

# load up the code to add specific activities to trajectories.

basecode <- 'basecode_v2'
source(paste0(basecode,"/build_fns.r"))
source(paste0(basecode,"/_add_activity_delay_from_array.R"))
source(paste0(basecode,"/_add_decision_branch.R"))
source(paste0(basecode,"/_add_signals.R"))
source(paste0(basecode,"/_post_process_utils.R"))
source(paste0(basecode,"/_set_attributes.R"))
source(paste0(basecode,"/_global_array_access.R"))
source(paste0(basecode,"/_add_lhd.R"))
source(paste0(basecode,"/_sched_code.R"))



stat_defn <- data.frame(stat_label =c(
  "s_wait_res",
  "s_wait_stock_access",
  "s_wait_downstream_stock",
  "s_wait_upstream_stock",
  "s_working",
  "s_breakdown",
  "s_tramming",
  "s_wait_sec_eq"
),
value=c(1:8))
s_wait_res <- 1
s_wait_stock_access <- 2
s_wait_downstream_stock <- 3
s_wait_upstream_stock <- 4
s_working <- 5
s_breakdown <- 6
s_tramming <- 7
s_wait_sec_eq <- 8


huge=999999999
tiny=0.00000001
robs_log <-
  function(item,
           activity,
           trj_step,
           text,
           pipe = TRUE,
           level = 1,
           tag = "",
           ret = TRUE) {
    if (pipe) {
      pipe <- ' %>% '
    } else {
      pipe <- ""
    }
    if (tag != "") {
      tag <- paste0(",tag = '", tag, "'")
    }
    if (ret) {
      ret <- '\n'
    } else {
      ret <- ""
    }
    paste0(
      "log_('",item,":",activity,":block_",
      trj_step,
      ":",
      text,
      "',level = ",
      level,
      tag,
      ")",
      pipe,
      ret
    )
  }


robs_log_global <-
  function(trj_step1,text,
           global,
           pipe = TRUE,
           level = 1,
           tag = "",
           env = 'env',
           ret=TRUE) {
    if (pipe) { pipe <- ' %>% ' } else {pipe <- ""}
    if(ret){ret <- '\n'} else {ret <- ""}
    if (tag != "") {tag <- paste0(",tag = '", tag, "'")
    }
    text <-
      paste0("log_(function() paste0('item:activity:block_",trj_step1,":", text,
             "',get_global(", env,",'", global,"')),level = ",level,tag,")",pipe, ret)  
  }
robs_log_attribute <-
  function(trj_step1,text,
           attribute,
           pipe = TRUE,
           level = 1,
           tag = "",
           env = 'env',
           ret=TRUE) {
    if (pipe) { pipe <- ' %>% ' } else {pipe <- ""}
    if(ret){ret <- '\n'} else {ret <- ""}
    if (tag != "") {tag <- paste0(",tag = '", tag, "'")
    }
    text <-
      paste0("log_(function() {paste0('item:activity:block_",trj_step1,":",text,
             "',get_attribute(", env,",'", attribute,"'))},level = ",level,tag,")",pipe, ret)  
  }

trimrandom <-
  function(val,
           fn = rnorm,
           varfrac = 0.1,
           min = tiny,
           max = huge,
           nvars = 1) {
    min(max(min, fn(nvars, val, val * varfrac))
        , huge)
  }

init_model <- function(model,
                       envname = "env",
                       level = 1) {
  model <- data.frame(
    modelname = model,
    item = '',
    trj_step = 0,
    next_trj_step = as.character(0),
    signal_txt="",
    signal_dir="",
    decision_txt="",
    activity = '_activate_model',
    var_txt = paste0("env <- simmer('", envname, "' , log_level = ", level, ")\nseed=thisseed\n"),
    trj_txt="",
    env_txt = paste0("env <-  env  %>%
    add_resource('schedule_queue', 1,preemptive = TRUE,preempt_order = 'fifo')")
  )
  
  
}
build_stockpiles <-
  function(mod_df,
           stockpile_df_name) {
    
    stockpile_vartext=paste0("
",stockpile_df_name," <- ",stockpile_df_name," %>% 
mutate(current_access_assigned=0,
current_stocks=initstocks,
committed=0)
spnames <- names(",stockpile_df_name,")
varpointer_spilenames <- which(spnames=='pilenames')
varpointer_maxstocks <- which(spnames=='maxstocks')
varpointer_initstocks <- which(spnames=='initstocks')
varpointer_access_limit <- which(spnames=='access_limit')
varpointer_current_access_assigned <- which(spnames=='current_access_assigned')
varpointer_current_stocks <- which(spnames=='current_stocks')
varpointer_committed <- which(spnames=='committed')
")
    df <- get(stockpile_df_name)
    varpointer_spilenames <- which(names(df)=="pilenames")
stockpile_names <- df[,varpointer_spilenames]
stockpile_id_txt='
'
for(name in stockpile_names){
  stockpile_id_txt <- paste0(stockpile_id_txt,name,'_id <- ',which(stockpile_names== name),'
')
}


stockpile_vartext=paste0(stockpile_vartext,stockpile_id_txt)
      df <- data.frame(
        modelname = mod_df$model[1],
        item = "",
        trj_step = 0,
        signal_txt="",
        signal_dir="",
        decision_txt="",
        next_trj_step = 0,
        activity = 'build_stockpiles',
        var_txt = stockpile_vartext,
        trj_txt = "",
        env_txt = "")
    mod_df = rbind(mod_df, df)
  }
add_trajectory_to_model <-
  function(mod_df, item,activity="init_trj", n_item, varnames, varlist) {
    trj_step=0
    next_trj_step=1
    signal_txt=""
    signal_dir=""
    decision_txt=""
    
    # Create list of variables
    varlist <- as.character(varlist)
    varlist[!str_detect(varlist, "function")] <-
      paste0("function() ", varlist[!str_detect(varlist, "function")])
    var_txt=''
    for(i in 1:length(varnames)){
      var_txt <- paste0(paste0(var_txt,varnames[i],"_code <- ",varlist[i],"\n"))
    }
    # build the item array
    linetext=paste0("data.frame(\n",varnames[1],"= ",varnames[1],"_code()")
    for(name in 2:length(varnames)){ 
      linetext=paste0(linetext,",\n",varnames[name]," = ",varnames[name],"_code()")
    }
    linetext <- paste0(linetext,"\n)")
    var_txt= paste0(var_txt,item,"_array <- ",linetext,"\n")
    if(n_item>1){
      for(i in 2:n_item){
        var_txt=paste0(var_txt,item,"_array <- rbind(",item,"_array,\n",linetext,")\n")
      }
    }

  ## generate var_pointers
    add_names <- c('_ute_time','_next_bd')
    add_names <- paste0(item,add_names)
    item_vals <<- c(0,paste0(item,"_mtbf")) 
    item_varnames <<- append(varnames,add_names)


    #environmentaL text
    env_txt <- paste0(
    "env <- env %>%
  add_generator('",item,"', trajectory = ",item,"_trj, at((1:",n_item,")), mon = 2)\n")
  activity <- paste0(item,"_setup_trj")
  trj_txt <- paste0(item,
      "_trj <- trajectory('",item,"_trj') %>%\n\t ",
      robs_log(item,activity,1,'init trajectory'),
      "\tset_global('",item,"_count', 1, mod = '+') %>%\n",
      "\tset_attribute('",item,"_id', function() get_global(env, '",item,"_count')) %>% \n",
     "\tset_attribute('",item,"_next_block',1) %>% \n ",
     "\t",robs_log(item,activity,1,'end Init and start content',tag=paste0(item,"_rollback_to_start"),pipe=FALSE)
  
  )
  df <- data.frame(
    modelname = mod_df$model[1],
    item = item,
    trj_step = trj_step,
    signal_txt=signal_txt,
    signal_dir=signal_dir,
    decision_txt=decision_txt,
    next_trj_step = next_trj_step,
    activity = activity,
    var_txt = var_txt,
    trj_txt = trj_txt,
    env_txt = env_txt)
  mod_df = rbind(mod_df, df)
}
create_close_trj <-
  function(modelname, mod_df, trj_step=-1,item, activity = 'close_item_trajectory'){
    trj_step = check_trj_step(item = item,
                              trj_step = trj_step,
                              mod_df = mod_df)
    #######
    
    trj_txt <- paste0(item,"_trj <- ",item,"_trj %>%
  #########################################
  ## Entering ",activity,"
  #########################################
  ## branch_if_not_activity start
  branch(
    option = function() ifelse(get_attribute(env, '",item,"_next_block') == ",trj_step,", 1, 2),
    continue = c(TRUE, TRUE),
    trajectory('",item,"_activity_stay_in_block') %>%
          ",robs_log(item,activity,trj_step, 'branch back to first block'),"
        set_attribute('",item,"_next_block', 1),
    trajectory() %>% 
      ",robs_log(item,activity,trj_step, 'continue skipping to next block', pipe = FALSE),
           ") %>%
      simmer::rollback(target = '",item,"_rollback_to_start') %>% 
           ",
      robs_log(item,activity,trj_step, 'End and go to next block', pipe = FALSE)
    )
    
    
    
    #####################################
  #next_trj_step <- 1
  signal_txt=""
  signal_dir=""
  decision_txt=""
  #trj_txt <-  paste0(item,"_trj <- ",item,"_trj %>%
  #set_attribute('",item,"_next_block',",next_trj_step,") %>%
  #simmer::rollback(target = '",item,"_rollback_to_start')")
  env_txt=""
  
  var_txt <- paste0('ptr_',item_varnames,' = ',1:length(item_varnames),collapse = '\n')
  var_txt <- paste0(var_txt,'\n')
  #####

  new_names <- item_varnames[(length(item_varnames) - length(item_vals)+1):length(item_varnames)]
  next_trj_step=1
  for(i in 1:length(new_names)){
    var_txt <- paste0(var_txt,item,"_array <- ",item,"_array %>% mutate(",new_names[i], " = ",item_vals[i],")\n")
  }
  #####
  df <- data.frame(
    modelname = mod_df$model[1],
    item = item,
    trj_step = trj_step,
    signal_txt=signal_txt,
    signal_dir=signal_dir,
    decision_txt=decision_txt,
    next_trj_step = next_trj_step,
    activity = activity,
    var_txt = var_txt,
    trj_txt = trj_txt,
    env_txt = env_txt)
  mod_df = rbind(mod_df, df)
  
}


save_text_to_file <- function(text,fname) {
  con <- file(
    description = fname,
    open = "w",
    blocking = TRUE,
    encoding = getOption("encoding"),
    raw = FALSE,
    method = getOption("url.method", "default")
  )
  writeLines(text, con = con)
  close(con = con)
}


join_code <- function(mod_df){
  full_vars <- paste(sep = '\n',paste(mod_df$var_txt[mod_df$var_txt!=""], collapse = '\n'))
  full_trj <-  paste(sep = '\n',paste(mod_df$trj_txt[mod_df$trj_txt!=""], collapse = '\n'))
  full_env <- paste(sep = '\n',paste(mod_df$env_txt[mod_df$env_txt!=""], collapse = '\n'))
  code <- paste(full_vars,'\n\n',full_trj,'\n\n',full_env)
}

get_mod_vars <- function(df) {
  vars <- df$var_txt
  var_items <- unlist(str_split(vars, '\n'))
  var_items <- str_replace_all(var_items, '=', ' <- ')
  var_items <-
    data.frame(varname = var_items[grepl(" <- ", var_items, fixed = TRUE)]) %>%
    mutate(
      varname = substr(varname, 1, str_locate(varname, " <- ")[, 1]),
      varname = gsub(" ", "", varname, fixed = TRUE),
    )
  
  var_items <-
    cbind(var_items, var_val = as.character(lapply(var_items$varname, get))) %>% 
    filter(!(varname %like% 'block_id')) %>% 
    filter(!(varname %like% 'last_block_in')) %>% 
    filter((varname !='env')) 
}
makevalidname <- function(text){
  invalid_chars <- c("/", "\\", ":", "*", "?", "\"", "<", ">", "|") # list of invalid filename characters
  # replace invalid characters with underscores
  valid_filename <- gsub(paste0("[", paste0(invalid_chars, collapse = ""), "]"), "_", text)
}


run_model <- function(modelname,
                      sequence_desc,
                      input_xl_file,
                      input_xl_sheet,
                      run_duration) {
  
  if (file.exists(paste0(modelname, "/_add_special_blocks.R"))) {
    source(paste0(modelname, "/_add_special_blocks.R"))
  }
  model_path <- paste0(modelname, "/")
  
  sequence_desc <- paste0(model_path, sequence_desc, "/")
  inputs <- read_excel(paste0(sequence_desc, input_xl_file), sheet = input_xl_sheet)
  inputs <- inputs[!is.na(inputs$Seq), ]
  assign("inputs",
         inputs,
         env = .GlobalEnv)
  assign("modelname",modelname,env = .GlobalEnv)
  runs <- nrow(inputs)
  for (run_id in 1:runs) {
    ##
    # create sub directory for this run
    ##
    
    print(paste0("run :", run_id, "  of :", runs))
    assign("run_id",run_id,env=.GlobalEnv)
    
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
    path <- paste0(run_dir, "code.R")
    save_text_to_file(code, path)
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
    write.csv(attributes, paste0(run_dir, "attributes_file.csv"))
    eqtypes <-
      mod_df %>% group_by(item) %>% summarise() %>% filter(str_length(item) >
                                                             0)
    for (eqtype in eqtypes$item) {
      if (exists(paste0(eqtype, "_array"))) {
        write.csv(get(paste0(eqtype, "_array")), paste0(eqtype, "_array.csv"))
      } else {
        print(paste0("Item array ", eqtype, "_array does not exist"))
      }
    }
    # write.csv(bogger_array,paste0(run_dir, "bogger_array.csv"))
    # write.csv(truck_array,paste0(run_dir, "truck_array.csv"))
    # write.csv(conveyor_array,paste0(run_dir, "conveyor_array.csv"))
  }
}
