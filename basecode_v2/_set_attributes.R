# to_att_name <- c('att_1_name','att_2_name','att_3_name')
# from_name <- c('function() 6','from_att2','from_att3')
# mod_code <- c('','+','-')
# type <- c('fn','att','att')


translate_att_modecode <- function(mod_code){
  if(is.null(mod_code)){
    pref=''
    mod_code=''
  } else if(mod_code==''){
    pref=''
    mod_code=''
  } else if(mod_code == '/'){
    pref='1/'
    mod_code=", mod = '*'"
  } else if(mod_code =='-'){
    pref='-'
    mod_code=", mod = '+'"
  } else if (mod_code=='+' | mod_code =='*'){
    pref=''
    mod_code=paste0(", mod = '",mod_code,"'")
  } else {
    print("error invalid modification code.")
  }
  list(prefix=pref,mod_code=mod_code)
}


set_generic_attributes <- function(trj_step,to_name,from_name,mod_code="",from_type,to_type){
  ret <- translate_att_modecode(mod_code)
  pref <- ret$prefix
  mod_code=ret$mod_code
  if(from_type=='local') {
    fromtext=paste0("get_attribute(env, '",from_name,"')")
    fun="', function() "
    } else if(from_type== 'global'){
    fromtext=paste0("get_global(env, '",from_name,"')")
    fun="', function() "
  } else if(from_type=='fn'){
    fromtext=from_name
    fun="', "
  } else {
    print("Invalid from code when setting attribute")
  }
  if(to_type=="global"){
    totext="set_global('"
  } else if (to_type=="local"){
    totext="set_attribute('"
  } else {
    print("invalid to code when setting attribute")
  }
  code <- robs_log(trj_step,paste0("setting attribute ",to_name," from ",from_name))
  code <- paste0(code,totext,to_name,fun,pref,fromtext,mod_code,")")

    
}

set_multiple_attributes <- function(trj_step,to_name,from_name,mod_code,from_type,to_type){
  code <- "
 "
  for (i in 1:length(to_type)) {
    code <- paste0(code,set_generic_attributes(trj_step,to_name[i],from_name[i],mod_code[i],from_type[i],to_type[i])," %>% \n ")
  }
  code <- substring(code,1,nchar(code)-6)
}
add_set_multiple_attributes <- function(modelname,
                                    mod_df,
                                    item,
                                    activity = 'activity_delay',
                                    trj_step = -1,
                                    next_trj_step = -1,
                                    relative=FALSE,
                                    to_name,
                                    from_name,
                                    mod_code,
                                    from_type,
                                    to_type) {
  trj_step = check_trj_step(trj_step = trj_step, mod_df = mod_df)
  next_trj_step = check_next_trj_step(next_trj_step = next_trj_step, trj_step = trj_step,relative = relative)
  
  code <-
    set_multiple_attributes(trj_step,to_name, from_name, mod_code ,from_type,to_type)
  
  trj_txt <-
    paste0(
      start_code(trj_step = trj_step, next_trj_step = next_trj_step),
      code,
      end_code(trj_step = trj_step)
    )
  var_txt <- ""
  env_txt <- ""
  mod_df <- add_code_row(
    modelname=modelname,
    modeldf=mod_df,
    item =item,
    trj_step=trj_step ,
    next_trj_step=next_trj_step,
    activity=activity ,
    var_txt=var_txt ,
    trj_txt=trj_txt ,
    env_txt=env_txt
  )
}

