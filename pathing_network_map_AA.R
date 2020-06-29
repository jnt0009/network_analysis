source('./Network Importance.R')  ## Reference to the functions held in network importance UPDATE THIS

if (!require("pacman"))
  install.packages("pacman")
  pacman::p_load(
              RSiteCatalyst, # How we actually get the Adobe Analytics data
               tidyverse, # Includes dplyr, ggplot2, and others; very key!
               visNetwork, # For network diagrams
               )

# Set the view ID and the date range.
uw <- ## Adobe API Username
pw <- ## Adobe API Password
start_date <- Sys.Date() - 372          # 30 days back from yesterday
end_date <- Sys.Date() - 365            # Yesterday
SCAuth(uw, pw)                        # Authorize AA. 
rs <- GetReportSuites()               # List of possible report suites
rsid <- rs$rsid[71]                   # focused Reporstsuite ID
GetEvars(rsid) %>% View()             # List of Evars that the prev and current pages could be

report.data <- QueueDataWarehouse(
  rsid,
  start_date,
  end_date,
  c("visits"),
  c('page', 'evar7'), # current page and previous page
  enqueueOnly = FALSE
)
# this step takes a while

df <- report.data # rename the report suite data to a local df
colnames(df) <- c('drop', 'page', 'prev_page', 'visits') # clean col names
df$page <- df$page %>% as.character()
df$prev_page <- df$prev_page %>% as.character()

df <- df %>%
  filter(visits > 5) %>% # Get rid of some pairs that aren't frequent
  mutate(page = case_when(
    page == '' ~ 'exit', # if no page then the user exited
    page != '' ~ page
  )) %>% 
  mutate(prev_page = case_when(
    prev_page == '' ~ 'entrance', # if no previous page likely an entrance
    prev_page != '' ~ prev_page
  )) %>%
  filter(page != prev_page) %>% 
  filter(prev_page != 'entrance' & page != "entrance") %>% # remove bounces
  filter(prev_page != 'entrance') %>% # remove previous page info
  group_by(page, prev_page) %>% # Flatten out the duplicates
  summarise(visits = sum(visits)) %>% # Flatten out the duplicates pt2
  arrange(desc(visits))
df <- df %>% top_n(5) # only the top x pairs 
ndf <- df[1:10,]
# Nodes creation #
nodes <-  data.frame(id = unique(append(ndf$prev_page,ndf$page)),
                     label = unique(append(ndf$prev_page,ndf$page)),
                     title = unique(append(ndf$prev_page,ndf$page))
)
nodes$group <- nodes$id %>% as.character() %>% strsplit(':') %>% sapply(tail, 1)
nodes$label <- nodes$label %>% as.character() %>% strsplit(':') %>% sapply(tail, 1)
# for(i in 1:nrow(nodes)){
#   nodes$group[i] <- unlist(strsplit(unique(append(data_top_100$previousPagePath,data_top_100$pagePath))[i],'/'))[2]
# }  ## Adds group to nodes 
# df_density <- Density(data_top_100) ## Get density
# nodes <- add_Density(nodes,df_density) ## Adds Density

# Edges creation
edges <- data.frame(
  to = df$page,
  from = df$prev_page,
  title = df$visits,
  value = df$visits # rnorm(df$visits,10) 
)

nodes %>%
  visNetwork(., edges,width = "100%",
             height = "700px") %>%
    visEdges(arrows = 'to','from') %>%
    visGroups(groupname = 'Careers',
              color = "darkblue", 
              shadow = list(enabled = TRUE)) %>%
    visLayout(improvedLayout = TRUE, 
              randomSeed = 123) %>%
    visOptions(selectedBy = 'group',
               highlightNearest = TRUE,
               nodesIdSelection = TRUE) %>%
    visPhysics(solver = 'forceAtlas2Based')

## Network importance Viz
high <- max(df_density$density) + 5
low <- min(df_density$density) - 1
df_density$density <- rescale(df_density$density,c(0,100))
df_density %>%
  top_n(5) %>%
  ggplot() +
  geom_text(aes(-3, density, label =density )) +
  geom_text(aes(2, density, label = Page)) +
  coord_cartesian(xlim = c(low, high)) +
  labs(x = 'Page Importance',
       y = NULL) +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())









