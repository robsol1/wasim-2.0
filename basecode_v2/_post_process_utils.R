### Log manipulation functions
read_log <- function(logfile) {
  log <- read.delim(logfile, header = F, sep = ":")
  names(log) <- c("time", "item_Id", "item_type", "activity","block" ,"message")
  log <- log %>% mutate(time=as.numeric(time),
                        activity = str_trim(activity),
                        item_Id =str_trim(item_Id),
                        item_type=str_trim(item_type),
                        seq = as.numeric(seq_along(time))) %>% 
    filter(!is.na(time))
}
get_stocks <- function(log,pilenames) {
  colnames <- c("pilenames","maxstock","initstocks","access_limit","current_access_assigned","current_stocks","committed")
  stocks <- log %>%
    filter(str_detect(message, "%")) %>% 
    mutate(message = strsplit(message, '%')) %>%
    unnest(message) %>%
    mutate(
      varname = substr(message, 1, str_locate(message, "=")),
      varname = str_replace(varname, "=", ""),
      varname = str_trim(varname),
      val = str_replace(message, varname, ""),
      val = str_replace(val, '=', ""),
      val = str_trim(val)
    ) %>%
    select(-message) %>%
    pivot_wider(names_from = varname, values_from = val) %>%
    select(-'NA')
  
  names(stocks)[7] <- "arrayname"
  
  stocks <- stocks %>%
    filter(str_detect(arrayname, "stockpile")) %>% 
    mutate(
      was = as.numeric(was),
      is = as.numeric(is),
      row = as.numeric(row),
      col = as.numeric(col),
      rowname = pilenames[row],
      varname = colnames[col]) %>% 
    rename(stockpile=rowname)
}

get_signals <- signals <- function(log) {
  log %>%
    filter(
      str_detect(message, "waiting signal") |
        str_detect(message, "released from signal") |
        str_detect(message, "sending signal") |
        str_detect(message, "sent signal ")
      
    )
}

get_attributes <- function(path){
  attributes <- read_csv(path) %>%
    mutate(seq = as.numeric(seq_along(time)),
           item_id = as.numeric(gsub("[^0-9.-]", "", name)),
           item_type = gsub("[0-9.]", "", name)) %>% 
    dplyr::select(replication,seq,time,item_type,name,key,value)
}
get_status <- function(attributes) {
  status <- attributes %>%
    filter(str_detect(key, "_status"))
  status <- left_join(status, stat_defn) %>% 
    group_by(name) %>% 
    arrange(name,time) %>% 
    mutate(end_time =lead(time),
           duration = end_time-time)
}

get_block_seq <- function(log) {
  log %>%
    filter(message != 'End and go to next block') %>%
    filter(message != 'Block id is not next block so skip block') %>%
    filter(message != 'continue skipping to next block') %>%
    arrange(item_Id, seq) %>%
    group_by(item_Id) %>%
    mutate(
      new_event = ifelse(lag(block) != block, 1, 0),
      new_event = ifelse(is.na(new_event), 1, new_event),
      event = cumsum(new_event)
    ) %>%
    group_by(item_Id, event, activity, block) %>%
    summarise(last_seq=max(seq),start = min(time), end = max(time)) %>%
    mutate(duration = round(end - start, 2))
}

get_last_event <- function(log){
  block_seq <- get_block_seq(log)
  last_event <- block_seq %>% 
    group_by(item_Id) %>%
    summarise(last_event =max(end),last_seq =max(last_seq)) %>% 
    mutate(event_type="any")
  t <- left_join(last_event,block_seq)
  last_sig_event <- block_seq %>% 
    filter(duration > 1)%>% 
    group_by(item_Id) %>%
    summarise(last_event =max(end),last_seq =max(last_seq))%>% 
    mutate(event_type="significant")
  s <- left_join(last_sig_event,block_seq)
  rbind(t,s) %>% 
    arrange(item_Id,last_seq)
}




## Plotting fns

plot_blocks <- function(df,filename=NULL,
                        textsize = 0.2,
                        arrowsize = 0.2,
                        height=11,
                        width=16
                        ) {
  txtwidth = 15
  distinct_mod_df <- mod_df %>%
    filter(next_trj_step != 0)
  
  distinct_mod_df <- distinct_mod_df %>%
    mutate(
      next_trj_step = strsplit(next_trj_step, ","),
      activity = paste0("block_",trj_step,"_",item, "_", activity),
      activity = (stringr::str_wrap(gsub(
        "[_]", " ", activity
      ), width = txtwidth)),
      signal_txt = (stringr::str_wrap(gsub(
        "[_]", " ", signal_txt
      ), width = txtwidth))
    )
  
  distinct_mod_df <- distinct_mod_df %>%
    unnest(next_trj_step) %>%
    mutate(
      trj_step = paste0(item, trj_step),
      next_trj_step = paste0(item, next_trj_step)
    )
  
  
  dist_nodes <- distinct_mod_df %>%
    mutate(node_colour = ifelse(
      decision_txt == "yes",
      3,
      ifelse(signal_txt != "" &
               signal_dir == "send", 2, 1)
    )) %>%
    select(trj_step, activity, node_colour) %>%
    group_by(activity, trj_step) %>%
    summarise(node_colour = max(node_colour))
  distinct_mod_df <- left_join(
    distinct_mod_df,
    dist_nodes %>% rename(next_activity = activity, next_trj_step =
                            trj_step) %>%
      select(next_activity, next_trj_step)
  )
  links <-  distinct_mod_df %>%
    select(activity,
           next_activity,) %>%
    mutate(
      link_name = "",
      link_colour = ifelse(activity %like% "activate\nmodel", "blue", 'black')
    )
  
  
  signals <- distinct_mod_df %>% filter(signal_txt != "") %>%
    mutate(
      signal_txt = strsplit(signal_txt, ","),
      signal_dir = strsplit(signal_dir, ","),
      link_colour = "red"
    ) %>%
    unnest(signal_txt, signal_dir) %>%
    rename(link_name = signal_txt) %>%
    select(activity,
           #next_activity,
           link_name,
           signal_dir, link_colour, signal_dir)
  
  from <- signals %>%
    filter(signal_dir == "send") %>%
    select(-signal_dir)
  to <- signals %>%
    filter(signal_dir == "wait") %>%
    select(-signal_dir) %>%
    rename(next_activity = activity)
  signals <- full_join(from, to) %>%
    select(activity,
           next_activity,
           link_name,
           link_colour)
  
  links <- rbind(links, signals) %>%
    filter(activity != "NA") %>%
    filter(next_activity != "NA") %>%
    filter(activity != "") %>%
    filter(next_activity != "")
  
  
  
  plt <- links %>%
    select(activity, next_activity)
  #https://r-graphics.org/recipe-miscgraph-graph-label
  
  network <- graph_from_data_frame(d = plt)
  E(network)$label <- links$link_name
  E(network)$color <- links$link_colour
  E(network)$label.cex = textsize
  E(network)$arrow.size = arrowsize
  E(network)$curved = TRUE
  V(network)$label.cex = textsize
  V(network)$shape="rectangle"
  V(network)$size=10
  V(network)$size2=10
  df <- data.frame(activity = names(V(network)))
  t <-
    dist_nodes %>% group_by(activity) %>% summarise(node_colour = max(node_colour)) %>%
    mutate(
      node_colour = ifelse(
        node_colour == 1,
        "white",
        ifelse(node_colour == 2, "pink", "red")
      ),
      node_colour = ifelse(activity %like% ".initialise.", "lightgrey", node_colour)
    )
  vnodes <- left_join(df, t)
  V(network)$color = vnodes$node_colour
  if(!is.null(filename)){
    pdf(filename,width=16,height=11)
    plot(network)
    dev.off()
  }
  p <- plot(network)

}

plot_trajectory <- function(trj,height=3000,filename=NULL){
  get_palette <- scales::brewer_pal(type = "qual", palette = 1)
  plot <- plot(trj, fill = get_palette,height=3000)
  if(!is.null(filename)){
    htmltools::save_html(plot, file = paste0(filename,".html"))
  }
  print(plot)
  plot
}

plot_stocks <- function(log,pilenames){
  stocks <- get_stocks(log,pilenames)
  current <- stocks %>% 
    filter(varname=='current_stocks') %>% 
    select(time,seq,varname,stockpile,was,is) %>% 
    pivot_longer(cols = c(was,is)) %>% 
    mutate(order=ifelse(name=='was',1,2)) %>% 
    arrange(stockpile,seq,order)
  
  current %>% ggplot(aes(x=time,y=value)) + 
    geom_path()+
    facet_wrap(vars(stockpile),scales = "free")+
    labs(title="Stockpile Trends")
}
plot_status <- function(status){
  status <- status %>%
    arrange(name, seq) %>%
    group_by(name) %>% 
    mutate(
      was = lag(value),
      was = ifelse(is.na(was), lead(was), was)
    ) %>% 
    select(time,seq,name,stat_label,was,value,) %>% 
    rename(is=value,item_id=name,status=value) %>% 
    # filter(was!=is) %>% 
    # pivot_longer(cols = c(is,was),names_repair = "check_unique",values_to = "Status") %>% 
   # mutate(order=ifelse(name=='was',1,2)) %>% 
    arrange(item_id,seq)

  status %>% ggplot(aes(x=time,y=status,colour=stat_label)) + 
    geom_point()+
    facet_wrap(vars(item_id))+
    labs(title="Status Trends")
  
  
  }
summarise_log <- function(df){
  names <- names(df)
  df = if (length(names[str_detect(names, "message")])) {
    get_sequence_from_log(df)
  }
  df <- df %>% 
    filter(lastevent > 0) %>% 
    group_by(item_type,item_Id,activity) %>% 
    summarise(events=n(),tot_duration=sum(duration),avg_duration=mean(duration),sd_duration=sd(duration))
}

summarise_all_runs <- function(sequence_desc) {
  files <- dir(sequence_desc, recursive = TRUE)
  files <- files[str_detect(files, '.csv')]
  attributefiles <- files[str_detect(files, 'attributes')]
  equipment <-
    data.frame(fullpath = files[!str_detect(files, 'attributes')]) %>%
    mutate(
      run = as.numeric(substr(fullpath, 8, 10)),
      name = basename(fullpath),
      equip_type = str_replace(name, "_array.csv", "")
    )
  # summarise run TUM by equip
  for (run_id in 1:length(attributefiles)) {
    status <-
      get_status(get_attributes(paste0(sequence_desc, attributefiles[run_id])))
    status_summary <- status %>%
      group_by(item_type, name, stat_label) %>%
      summarise(Stat_duration = sum(duration, na.rm = T)) %>%
      pivot_wider(
        names_from = stat_label,
        values_from = Stat_duration,
        values_fill = 0
      ) %>%
      mutate(run_id = run_id)
    
    # run through all equipment in that run
    runeq <- equipment %>%
      filter(run == run_id)
    for (eq_id in 1:nrow(runeq)) {
      equip_data <- read.csv(paste0(sequence_desc, runeq$fullpath[eq_id]))
      names <- names(equip_data)
      names <-
        str_replace(names, paste0(runeq$equip_type[eq_id], "_"), "")
      names(equip_data) <- names
      names(equip_data)[1] <- "name"
      equip_data <- equip_data %>%
        mutate(name = paste0(runeq$equip_type[eq_id], name - 1),
               run_id  = run_id)
      if (eq_id == 1) {
        toteq = equip_data
      } else {
        toteq <- full_join(toteq, equip_data)
      }
    }
    toteq[is.na(toteq)] <- 0
    if (run_id == 1) {
      total_summary <- full_join(toteq, status_summary)
    } else {
      total_summary <-
        full_join(total_summary, full_join(toteq, status_summary))
    }
    
  }
  total_summary
}
