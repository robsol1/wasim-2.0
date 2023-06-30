fnames <- dir(path=paste0(sequence_desc, "run_id_", sprintf("%03.0f", run_id), "/"))
fnames <- dir(path=paste0(sequence_desc, "/"),recursive = T)
itemsarrays <- fnames[str_detect(fnames,'array')]


#remove existing df if they exist
for(filename in itemsarrays){
  dfName <-
    str_replace(substr(
      filename,
      str_locate(filename, "/")[1] + 1,
      str_length(filename)
    ),
    '_array.csv',
    "")
  print(dfName)
  if (exists(dfName)){
    eval(parse(text=paste0("rm(",dfName,")"))) 
  }
}

for(filename in itemsarrays) {
  run_id <-
    as.numeric(str_replace(substr(
      filename, 1, str_locate(filename, "/")[1] - 1
    ), "run_id_", ""))
  dfName <-
    str_replace(substr(
      filename,
      str_locate(filename, "/")[1] + 1,
      str_length(filename)
    ),
    '_array.csv',
    "")
  if (!exists(dfName)) {
    eval(parse(
      text = paste0(
        dfName,
        " <- read_csv('",
        sequence_desc,
        filename,
        "',show_col_types = FALSE) %>% mutate(run_id=",
        run_id,
        ")"
      )
    ))
  } else {
    eval(parse(
      text = paste0(
        dfName,
        " <- rbind(",
        dfName,
        ",read_csv('",
        sequence_desc,
        filename,
        "',show_col_types = FALSE) %>% mutate(run_id=",
        run_id,
        "))"
      )
    ))
  }
}

