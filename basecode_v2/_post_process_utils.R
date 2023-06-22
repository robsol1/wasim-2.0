### Log manipulation functions
read_log <- function(logfile) {
  log <- read.delim(logfile, header = F, sep = ":")
  names(log) <- c("time", "item_Id", "item_type", "activity","block" ,"message")
  log <- log %>% mutate(activity = str_trim(activity),
                        item_Id =str_trim(item_Id),
                        item_type=str_trim(item_type),
                        seq = as.numeric(seq_along(time)))
}
get_sequence_from_log <- function(logdf){
  seq <- log %>% group_by(item_type,item_Id, time, activity) %>%
  summarise(seq = min(seq)) %>%
  arrange(item_Id, time, seq) %>%
  group_by(item_type,item_Id) %>%
  mutate(
    activity_seq = ifelse(lag(activity) == activity, 0, 1),
    activity_seq = ifelse(is.na(activity_seq), 0, activity_seq),
    activity_seq = cumsum(activity_seq),
    lastevent = ifelse(is.na(lead(activity)), 0, 1)
  ) %>%
  group_by(item_type,item_Id, activity, activity_seq) %>%
  summarise(
    n = n(),
    start = min(time),
    end = max(time),
    seq = min(activity_seq),
    lastevent = min(lastevent)
  ) %>%
  mutate(duration = end - start) %>%
  arrange(item_type,item_Id, seq)
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

get_attributes <- function(env){
  attributes <- get_mon_attributes(env) %>%
    mutate(seq = as.numeric(seq_along(time)),
           item_id = as.numeric(gsub("[^0-9.-]", "", name)),
           item_type = gsub("[0-9.]", "", name)) %>% 
    dplyr::select(replication,seq,time,item_type,name,key,value)
}

get_stock_trend <- function(df=attributes){
  df <- df %>% 
    filter(key %like% "stocks_val") %>% 
    arrange(replication,key,time)
}

## Plotting fns

plot_blocks <- function(df,filename=NULL,
                        textsize = 0.3,
                        arrowsize = 0.3,
                        height=11,
                        width=16
                        ) {
  txtwidth = 15
  distinct_mod_df <- mod_df %>%
    filter(next_trj_step != 0)
  
  distinct_mod_df <- distinct_mod_df %>%
    mutate(
      next_trj_step = strsplit(next_trj_step, ","),
      activity = paste0(item, "_", activity),
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
  V(network)$shape="crectangle"
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
  plot(network)

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


