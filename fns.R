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


# source(paste0(basecode,"/_activity_delay.R"))
# source(paste0(basecode,"/_delay_with_single_stock_action.r"))
# source(paste0(basecode,"/_LHD.R"))
# source(paste0(basecode,"/_item_bd_code_generator.R"))
# source(paste0(basecode,"/_add_decision_branch.R"))
# source(paste0(basecode,"/_target_seek_load_by_item.R"))
# source(paste0(basecode,"/_loader_loads_item.R"))
# source(paste0(basecode,"/_sched_branch.R"))
# source(paste0(basecode,"/_assign_task_to_item.R"))
# source(paste0(basecode,"/_send_signal.R"))
# source(paste0(basecode,"/_wait_signal.R"))

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
    next_trj_step = 0,
    signal_txt="",
    signal_dir="",
    decision_txt="",
    activity = '_activate_model',
    var_txt = paste0("env <- simmer('", envname, "' , log_level = ", level, ")\nseed=thisseed\n"),
    trj_txt="",
    env_txt=""
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
  add_generator('item', trajectory = ",item,"_trj, at((1:",n_item,")), mon = 2)\n")
  activity <- paste0(item,"_setup_trj")
  trj_txt <- paste0(item,
      "_trj <- trajectory('",item,"_trj') %>%\n\t ",
      robs_log(item,activity,1,'init trajectory'),
      "\tset_global('",item,"_count', 1, mod = '+') %>%\n",
      "\tset_attribute('",item,"_id', function() get_global(env, '",item,"_count')) %>% \n",
      # "\tset_attribute('",item,"_ute_time', 0) %>%\n",
      # "\tset_attribute('",item,"_next_bd', ",item,"_mtbf_code) %>%\n",
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
  print(activity)
    trj_step = check_trj_step(item = item,
                              trj_step = trj_step,
                              mod_df = mod_df)
  next_trj_step <- 1
  signal_txt=""
  signal_dir=""
  decision_txt=""
  trj_txt <-  paste0(item,"_trj <- ",item,"_trj %>%
  set_attribute('",item,"_next_block',",next_trj_step,") %>%
  simmer::rollback(target = '",item,"_rollback_to_start')")
  env_txt=""
  
  var_txt <- paste0('ptr_',item_varnames,' = ',1:length(item_varnames),collapse = '\n')
  var_txt <- paste0(var_txt,'\n')
  #####

  new_names <- item_varnames[(length(item_varnames) - length(item_vals)+1):length(item_varnames)]
  
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
# build_stockpiles_old <-
#   function(modeldf,
#            pilenames,
#            maxstocks = NULL,
#            initstocks = NULL,
#            access_limit = c(1, 1, 1)) {
#     npiles <- length(pilenames)
#     if (is.null(maxstocks)) {
#       maxstocks <- rep(999999, npiles)
#     }
#     if (is.null(initstocks)) {
#       initstocks <- maxstocks / 2
#     }
#     
#     df <-
#       data.frame(pilename = pilenames,
#                  maxstocks = maxstocks,
#                  initstocks = initstocks) %>%
#       mutate(
#         modelname = modeldf$model[1],
#         item = '',
#         trj_step = 0,
#         signal_txt="",
#         signal_dir="",
#         decision_txt="",
#         next_trj_step = 0,
#         activity = '',
#         var_txt = paste0(pilename, '_max_stock <- ', maxstocks,"\n",pilename,"_access_limit <- ", access_limit,"\n"),
#         trj_txt = "",
#         env_txt = paste0("env <-  env  %>%
#   add_global('",pilename,"_stocks_val',",initstocks,") %>%
#   add_global('",pilename,"_stocks_commited',",0,") %>%
#   add_global('",pilename,"_current_activities',",0,") %>%
#   add_global('",pilename,"_access_limit', ",access_limit,")
#   \n")
#         
#       ) %>%
#       dplyr::select(-pilename, -maxstocks, -initstocks)
#     
#     modeldf = rbind(modeldf, df)
#     
#   }
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




# create_init_trj <- function() {
#   item_trj <- paste0(
#     "item_trj <- trajectory('item_trj') %>%\n\t ",
#     robs_log(1,'init trajectory'),
#     "\tset_global('item_count', 1, mod = '+') %>%\n",
#     "\tset_attribute('item_id', function() get_global(env, 'item_count')) %>% \n",
#     "\tset_attribute('item_ute_time', 0) %>%\n",
#     "\tset_attribute('item_next_bd', item_mtbf_code) %>%\n",
#    "\tset_attribute('item_next_block',2) %>% \n ",
#    "\t",robs_log(1,'end Init and start content',tag='item_rollback_to_start',pipe=FALSE)
#   )
# }
# 
# 
# 
# create_init_item_vars <- function(item_unit_capacity,item_mttr_txt,item_mtbf_txt){
#   item_vars <- paste0("item_unit_capacity <- ",item_unit_capacity,"\n",
#                       "item_mttr_code <- ",item_mttr_txt,"\n",
#                       "item_mtbf_code <- ",item_mtbf_txt,"\n")
# }
# 
# create_init_item_env <- function(n_item) {
#   item_env <- paste0(
#     "env <- env %>%\n",
#     "\tadd_generator('item', trajectory = item_trj, at((1:",n_item,")), mon = 2)\n")
# }


# add_trajectory_to_model_old <-
#   function(modelname,
#            mod_df,
#            item,
#            activity='setup_trj',
#            n_items=1,
#            item_unit_capacity,
#            item_mttr_txt,
#            item_mtbf_txt) {
#     var_txt <- create_init_item_vars(item_unit_capacity,
#                                      item_mttr_txt,
#                                      item_mtbf_txt)
#     trj_txt <- create_init_trj()
#     env_txt = create_init_item_env(n_items)
#     trj_step = 0
#     item=item
#     mod_df=add_code_row(
#       modelname = modelname,
#       modeldf = mod_df,
#       item = item,
#       trj_step ,
#       next_trj_step=1,
#       signal_txt="",
#       signal_dir="",
#       decision_txt="",
#       activity=paste0(".initialise.") ,
#       var_txt="" ,
#       trj_txt="" ,
#       env_txt=""
#     )
#     add_code_row(
#       modelname = modelname,
#       modeldf = mod_df,
#       item = item,
#       trj_step=1 ,
#       next_trj_step=2,
#       signal_txt="",
#       signal_dir="",
#       decision_txt="",
#       activity =activity,
#       var_txt =var_txt,
#       trj_txt =trj_txt,
#       env_txt =env_txt
#     )
#   }
# create_close_trj <-
#   function(modelname, modeldf, item, activity = 'close_item_trajectory'
#   ){
#   trj_txt <-  "item_trj <- item_trj %>% 
#   set_attribute('item_next_block',2) %>% 
#   simmer::rollback(target = 'item_rollback_to_start')"
#   env_txt=""
#   trj_step <- length(which(modeldf$item == item))
# 
# ptr <- paste0('ptr_',item_varnames,' = ',1:length(item_varnames),collapse = '\n')
# var_txt <- paste0(var_txt,ptr)
#   var_txt=paste0("last_block_in_",item,"_trj=",trj_step)
#   
#   add_code_row(
#     modelname = modelname,
#     modeldf = modeldf,
#     item = item,
#     trj_step ,
#     next_trj_step=1,
#     signal_txt="",
#     signal_dir="",
#     decision_txt="",
#     activity ,
#     var_txt ,
#     trj_txt ,
#     env_txt
#   )
# }
# 
# add_code_row <-
#   function(modelname,
#            modeldf,
#            item,
#            trj_step,
#            next_trj_step,
#            signal_txt="",
#            signal_dir="",
#            decision_txt="",
#            activity,
#            var_txt,
#            trj_txt,
#            env_txt,
#            stockpile = NULL,
#            secondary_unit_name = NULL) {
#     # sometimes next_trj_step is a vector so need to convert to a atomic character variable
#     if (length(next_trj_step) > 1) {
#       next_trj_step_txt <- next_trj_step[1]
#       for (i in 2:length(next_trj_step)) {
#         next_trj_step_txt <-
#           paste0(next_trj_step_txt, ",", next_trj_step[i])
#       }
#     } else {
#       next_trj_step_txt <- as.character(next_trj_step)
#     }
#     df = data.frame(
#       modelname = modelname,
#       item = item,
#       trj_step = trj_step,
#       next_trj_step = next_trj_step_txt,
#       signal_txt=signal_txt,
#       signal_dir=signal_dir,
#       decision_txt=decision_txt,
#       activity = activity,
#       var_txt = var_txt,
#       trj_txt = trj_txt,
#       env_txt = env_txt
#     )
#     df <- add_unique_vars(
#       df,
#       item = item,
#       activity = activity,
#       stockpile = stockpile,
#       secondary_unit_name = secondary_unit_name
#     )
#     if (df$trj_step < 0) {
#       df$trj_step <- length(which(modeldf$item == item)) + 1
#     }
#     model <- rbind(modeldf,
#                    df)
#   }
# add_unique_vars <- function(df,
#                             item = NULL,
#                             activity = NULL,
#                             stockpile=NULL,
#                             secondary_unit_name=NULL) {
#   
#   df$var_txt <- create_code_text(
#     text=df$var_txt,
#     item = item,
#     activity = activity,
#     stockpile = stockpile,
#     secondary_unit_name=secondary_unit_name
#   )
#   df$trj_txt <- create_code_text(
#     text=df$trj_txt,
#     item = item,
#     activity = activity,
#     stockpile = stockpile,
#     secondary_unit_name=secondary_unit_name
#   )
#   df$env_txt <- create_code_text(
#     text=df$env_txt,
#     item = item,
#     activity = activity,
#     stockpile = stockpile,
#     secondary_unit_name=secondary_unit_name
#   )
#   df
# }
# create_code_text <-
#   function(text,
#            item = item,
#            activity = activity,
#            stockpile = stockpile,
#            secondary_unit_name
#   ) {
#     text <- fix(text, 'item', item)
#     text <- fix(text, 'activity', activity)
#     text <- fix(text, 'secondary_unit_name', secondary_unit_name)
#     stockpile <- fix(text, 'stockpile', stockpile)
#   }
# fix <- function(text, searchstring, repstring) {
#   if (!is.null(repstring)) {
#     text <- str_replace_all(text, searchstring, repstring)
#   }
#   text
# }

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



run_model <-
  function(modelname,
           scenario_desc,
           runduration,
           reps = 1,
           thisseed = NULL,
           loglevel = 0,
           verbose=FALSE) {
    if(!dir.exists(scen_dir)){
      dir.create(scen_dir)
    }
    
    if(!dir.exists(rundir)){
      dir.create(rundir)
    }
    
    full_model_filename <- paste0(model_path,modelname,"_code.R")
    source(paste0(model_path,modelname,"_build.R")) # build and save code
    save_model_str(scen_dir,
                   mod_df)
    for (i in 1:reps) {
      if (loglevel > 0)
        sink(file = paste0(rundir, 'run_', sprintf("%03d", i), '_log.txt'))
      if (is.null(thisseed))
        thisseed <<- as.integer(runif(1, 1, 10000))
      source(full_model_filename) #load model
      env <- env %>% run(runduration)
      print(i)
      if (loglevel > 0) {
        sink()
      }
      save_results(
        modelname,
        scenario_desc,
        rundir,
        run_number = i,
        df = mod_df,
        resource = get_mon_resources(env),
        attributes = get_mon_attributes(env),
        #arrive = get_mon_arrivals(env, per_resource = FALSE),
        verbose
      )
      rm(env)
    }
  }


#get basic data 
addid <- function(df,
                  modelname,
                  scenario_desc,
                  run_number) {
  ncols = ncol(df)
  df$modelname = modelname
  df$scenario = scenario_desc
  df$sens_step = sens_step
  df$run_number = run_number
  
  if ("name" %in% colnames(df)) {
    df <- df %>%
      mutate(
        name = ifelse(name == "", 'global0', name),
        item_id = as.numeric(gsub("[^0-9.-]", "", name)),
        item_type = gsub("[0-9.]", "", name)
      )
  }
  df
}

protosave <-
  function(df,
           modelname,
           scenario_desc,
           run_number,
           path,
           dfname) {
    if (nrow(df) > 0) {
      
      df <- addid(df,modelname,scenario_desc,run_number)
      
      write.csv(df,
                paste0(
                  rundir,
                  '/',
                  dfname,
                  '_sens_',
                  sprintf("%03d", sens_step),
                  '_run_',
                  sprintf("%03d", run_number),
                  '.csv'
                ),
                row.names = FALSE)
    } else {
      print(paste0('Warning : dataframe ', dfname, ' has zero rows'))
    }
    df
  }
save_results <- function(modelname,
                         scenario_desc,
                         path,
                         run_number,
                         df,
                         resource,
                         attributes,
                         #arrive,
                         verbose) {
  varlist <- get_mod_vars(df = df)
  
  
  varlist <- protosave(
    df = varlist,
    modelname = modelname,
    scenario_desc = scenario_desc,
    run_number = run_number,
    path = path,
    dfname = 'vars'
  )
  if (verbose) {
    attributes <-
      protosave(
        df = attributes,
        modelname = modelname,
        scenario_desc = scenario_desc,
        run_number = run_number,
        path = path,
        dfname = 'attributes'
      )
    protosave(
      df = resource,
      modelname = modelname,
      scenario_desc = scenario_desc,
      run_number = run_number,
      path = path,
      dfname = 'resource'
    )

  }

  if (!exists("summary_result")) {
    summary_result <<- get_run_sum(attributes = attributes)
  } else{
    summary_result <<-
      full_join(summary_result, get_run_sum(attributes = attributes))
  }
}
combine_csv <- function(path,target){
  d <- dir(path)
  files <- d[grep(target, d)]
  if (length(files) > 0) {
    df <- read.csv(paste0(seq_path,'/', files[1]))
    if (length(files) > 1) {
      files <- files[2:length(files)]
      for (file in files) {
        df <- rbind(df, read.csv(paste0(seq_path,'/', file)))
      }
    }
  } else {
    print(paste0('Warning  no files match',target,'.'))
    df <- NULL
  }
  
  df
}
scenario_summary <- function(path, target) {
  d <- dir(path)
  files <- d[grep(target, d)]
  if (length(files) > 0) {
    df <- get_run_sum(read.csv(paste0(seq_path,'/', files[1])))
    if (length(files) > 1) {
      files <- files[2:length(files)]
      for (file in files) {
        df <- rbind(df, get_run_sum(read.csv(paste0(seq_path,'/', file))))
      }
    }
  } else {
    print(paste0('Warning  no files match',target,'.'))
    df <- NULL
  }
  
  df
}

save_model_str <- function(path,df){
  write.csv(df,paste0(path,'/structure.csv'))
}

get_run_sum <- function(attributes) {
  attributes <- attributes %>%
    mutate(item_id = as.numeric(gsub("[^0-9.-]", "", name)),
           item_type = gsub("[0-9.]", "", name))
  status <- attributes %>%
    filter(key %like% 'status' & name != "") %>%
    arrange(replication, name, time) %>%
    group_by(replication, name) %>%
    mutate(
      event_end = lead(time),
      event_end = ifelse(is.na(event_end), runduration, event_end),
      event_duration = event_end - time,
      hrs = time / 3600
    ) %>%
    filter(event_duration > 0)
  status <- left_join(status, read.csv('statuscodes.csv'))
  
  eqnumbers <- status %>%
    group_by(item_type, name) %>%
    summarise() %>%
    group_by(item_type) %>%
    summarise(fleetsize = n())
  
  status_by_fleet <- status %>%
    group_by(modelname, scenario, sens_step, run_number, item_type, tum, ID) %>%
    summarise(
      n = n(),
      duration = sum(event_duration, na.rm = TRUE),
      lastevent_start = max(time)
    ) %>%
    mutate(mtbe = lastevent_start / n, mtoe = duration / n)
  
  
  tonnes_moved_fleet <- attributes %>%
    filter(!is.na(item_type) & key %like% 'cap.prod')  %>%
    group_by(modelname, scenario, sens_step, run_number, item_type) %>%
    summarise(tot_moved = max(value))
  
  
  run_summary <- status_by_fleet %>% ungroup() %>%
    select(modelname,
           scenario,
           sens_step,
           run_number,
           item_type,
           ID,
           duration) %>%
    pivot_wider(
      names_from = ID,
      values_from = duration,
      values_fill = 0
    ) %>%
    left_join(., tonnes_moved_fleet) %>%
    left_join(., eqnumbers)
}

