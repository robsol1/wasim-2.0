
txtwidth=15
distinct_mod_df <- mod_df %>% 
  filter(next_trj_step!=0)

distinct_mod_df <- distinct_mod_df %>% 
  mutate(
  next_trj_step = strsplit(next_trj_step, ","),
  activity = paste0(item, "_", activity),
  activity=(stringr::str_wrap(gsub("[_]", " ", activity), width = txtwidth)),
            signal_txt=(stringr::str_wrap(gsub("[_]", " ", signal_txt), width = txtwidth))
  )

distinct_mod_df <- distinct_mod_df %>% 
    unnest(next_trj_step) %>% 
  mutate( trj_step = paste0(item, trj_step),
  next_trj_step = paste0(item, next_trj_step)
) 


dist_nodes <- distinct_mod_df %>%
  mutate(
    node_colour = ifelse(
      decision_txt == "yes",
      3,
      ifelse(signal_txt != "" &
               signal_dir == "send", 2, 1)
    )
  ) %>% 
  select(trj_step,activity,node_colour) %>% 
  group_by(activity,trj_step) %>% 
  summarise(node_colour=max(node_colour))
distinct_mod_df <- left_join(distinct_mod_df,
                   dist_nodes %>% rename(next_activity=activity,next_trj_step=trj_step) %>% 
                     select(next_activity,next_trj_step))
links <-  distinct_mod_df %>% 
  select(
         activity,
         next_activity,
         ) %>% 
  mutate(link_name="",link_colour=ifelse(activity %like% "activate\nmodel","blue",'black'))


signals <- distinct_mod_df %>% filter(signal_txt != "") %>%
  mutate(signal_txt = strsplit(signal_txt, ","),
         signal_dir = strsplit(signal_dir, ","),
         link_colour="red") %>%
  unnest(signal_txt, signal_dir) %>% 
  rename(link_name=signal_txt) %>% 
  select(activity,
         #next_activity,
         link_name,
         signal_dir,link_colour,signal_dir)

from <- signals %>% 
  filter(signal_dir=="send") %>% 
  select(-signal_dir)
to <- signals %>% 
  filter(signal_dir=="wait") %>% 
  select(-signal_dir) %>% 
  rename(next_activity=activity)
signals <- full_join(from,to) %>% 
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
  select(activity,next_activity)
#https://r-graphics.org/recipe-miscgraph-graph-label
textsize=0.3
network <- graph_from_data_frame(d=plt)
E(network)$label <- links$link_name
E(network)$color <- links$link_colour
E(network)$label.cex = textsize
E(network)$arrow.size=0.3
E(network)$curved=TRUE
V(network)$label.cex = textsize
V(network)$shape="crectangle"
df <- data.frame(activity=names(V(network)))
t <- dist_nodes %>% group_by(activity) %>% summarise(node_colour=max(node_colour)) %>% 
  mutate(node_colour=ifelse(node_colour==1,"white",ifelse(node_colour==2,"pink","red")),
         node_colour=ifelse(activity %like% ".initialise.","lightgrey",node_colour)
         )
vnodes <- left_join(df,t)
V(network)$color=vnodes$node_colour
plot(network)

