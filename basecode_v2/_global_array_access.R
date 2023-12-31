set_array <-
  function(item,
           item_id,
           trj_step,
           activity,
           arrayname,
           row,
           column,
           value,
           mod = ""
  ){
  df <- get(arrayname)
  row <- as.numeric(row)
  column=as.numeric(column)
  curr_val=df[row,column]
  setval <- if (mod==""){
    value
  } else if (mod=='+'){
    curr_val+value
  } else if(mod=='-'){
    curr_val-value
  } else{
    cat(paste0("error - invalid mode code ",mod," in set local array att"))
  }
  df[row,column]=setval
  r_array_log(
    item = item,
    item_id = item_id,
    activity = activity,
    trj_step = trj_step,
    array = arrayname,
    row = row,
    column = column,
    paste0("was =", curr_val, "% is =", setval)
  )

  assign(arrayname,df,envir = .GlobalEnv)
  setval
  }


set_array_from_array <-
  function(item,
           item_id,
           trj_step,
           activity,
           arrayname,
           row,
           from_ptr,
           to_ptr,
           mod = "") {
    
    df <- get(arrayname)
    ret = set_array(
      item=item,
      item_id=item_id,
      trj_step=trj_step,
      activity=activity,
      arrayname = arrayname,
      row = row,
      column = to_ptr,
      value = df[row,from_ptr],
      mod = mod
    )
    
  }

update_stocks_from_item <-
  function(item,
           activity,
           trj_step,
           item_id,
           stockpile_id,
           stock_varpointer,
           item_varpointer,
           mod = "") {
    item_df <- get(paste0(item, "_array"))
    ret <- set_array(
      item = item,
      item_id,
      trj_step = trj_step,
      activity = activity,
      arrayname = 'stockpiles',
      row = stockpile_id,
      column = stock_varpointer,
      value = item_df[item_id, item_varpointer],
      mod = mod
    )
  }



get_array_att <- function(item,item_id,arrayname,activity,row,column,trj_step){
  df <- get(arrayname)
  row <- as.numeric(row)
  column=as.numeric(column)
  curr_val=df[row,column]
  r_array_log(item=item,item_id=item_id,activity=activity,trj_step=trj_step,array=arrayname,row=row,column=column,paste0("% was =% is=",curr_val))
  return(curr_val)
}

get_local_array_att <- function(item, activity,item_id, varpointer, trj_step) {
  curr_val <-
    get_array_att(
      item,
      item_id=item_id,
      arrayname = paste0(item,"_array"),
      activity = activity,
      row = item_id,
      column = varpointer,
      trj_step = trj_step
    )
  
}
get_stockpile_array_att <-
  function(item,
           item_id,
           activity,
           trj_step,
           stockpile_id,
           varpointer) {
    curr_val <-
      get_array_att(
        item=item,
        item_id=Item_id,
        arrayname = "stockpiles",
        row = stockpile_id,
        column = varpointer,
        trj_step = trj_step,
        activity = activity
      )
  }
r_array_log <-
  function(item,
           item_id,
           activity,
           trj_step,
           array,
           row,
           column,
           message) {
    cat(
      paste0(
        simmer::now(env),
        ":",
        paste0(item, item_id-1),
        ":",
        item,
        ":",
        activity,
        ":block_",
        trj_step,
        ":array =",
        array,
        "%row =",
        row,
        "%col =",
        column,
        "%",
        message,
        "\n"
      )
    )
  }

