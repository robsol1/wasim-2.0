runcode <- function(runduration, logswitch) {
  if (logswitch) {
    logfile <- paste0(modelname, "/", modelname, "_log.log")
    con <- file(logfile)
    sink(con, append = TRUE)
    sink(con, append = TRUE, type = "message")
  }
  source(path)
  runduration = 60000
  env <- env %>% run(runduration)
  if (logswitch) {
    sink(type = "message")
    sink()
    close(con)
  }
}
runcode(runduration=60000,logswitch=T)
log <- read_log(logfile)

attributes <- get_attributes(env)




