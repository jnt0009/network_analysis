source('./Network Importance.R')  ## Reference to the functions held in network importance UPDATE THIS


if (!require("pacman")) install.packages("pacman")
pacman::p_load(googleAnalyticsR,  # How we actually get the Google Analytics data
               tidyverse,         # Includes dplyr, ggplot2, and others; very key!
               visNetwork,        # For network diagrams
               igraph,            # Also for network diagrams
               networkD3,         # Yet another network diagram option
               scales,
               rvest)            # Useful for some number formatting in the visualizations

# Authorize GA.
ga_auth()

# Set the view ID and the date range. 
view_id <- # Replace with a hardcoded GA view Id
start_date <- Sys.Date() - 30            # 30 days back from yesterday
end_date <- Sys.Date() - 1               # Yesterday


# Pull the pages data
data_previous_pages <- google_analytics(viewId = view_id,
                                        date_range = c(start_date, end_date),
                                        metrics = "pageviews",
                                        dimensions = c("pagePath", "previousPagePath"),
                                        anti_sample = TRUE)

# If the previous page is the same as the page, then nix the row
data_previous_pages <- data_previous_pages %>% 
  filter(previousPagePath != pagePath)

# Pull the exits by page. We'll have "(exited)" as another node
data_exit_pages <- google_analytics(viewId = view_id,
                                    date_range = c(start_date, end_date),
                                    metrics = "exits",
                                    dimensions = "pagePath",
                                    anti_sample = TRUE)

# Turn this into a "previous page" view
data_exit_pages <- data_exit_pages %>% 
  mutate(previousPagePath = pagePath,
         pagePath = "(exit)",
         pageviews = exits) %>% 
  select(pagePath, previousPagePath, pageviews)

# Combine the true previous pages and the exits "page"
data_combined <- data_previous_pages %>% 
  rbind(data_exit_pages)
data_combined$pagePath <- gsub("\\?.*","",data_combined$pagePath)

data_combined$previousPagePath <- gsub("\\?.*","",data_combined$previousPagePath)

hold <- aggregate(data_combined$pageviews, by=list(data_combined$pagePath,data_combined$previousPagePath),FUN=sum)
colnames(hold) <- colnames(data_combined)
data_combined <-hold
data_combined
## Entrance and exit table ##
exits <- data_combined %>%
  filter(pagePath == "(exit)") %>%  
  group_by(previousPagePath)  ## Filter for only exits

entrances <- data_combined %>%
  filter(previousPagePath == "(entrance)") %>%
  group_by(pagePath)        ## Filter for only entrances


entrance_exit <- data_combined %>%
  select(pagePath) %>%
  unique() %>%
  left_join(exits,
            by = c("pagePath" = "previousPagePath") ) %>%
  left_join(entrances,
            by = c("pagePath" = "pagePath")) %>%
  select(pagePath,pageviews.x,pageviews.y)

colnames(hold) <- c("pagePath","exits","entrances")
hold[is.na(hold)]<- 0



# Top 100 Relation ships # 
data_top_100 <- data_combined %>%
  filter(previousPagePath != "(exit)" & pagePath != "(exit)") %>%
  filter(previousPagePath != "(entrance)") %>%
  top_n(100,pageviews)

## Network Density 
df_density <- data_combined %>%
  filter(previousPagePath != "(exit)" & pagePath != "(exit)") %>%
  filter(previousPagePath != "(entrance)") %>%
  filter(pageviews > 0) %>%
  Density()


# Nodes creation #
nodes <-  data.frame(id = unique(append(data_top_100$previousPagePath,data_top_100$pagePath)),
                     label = unique(append(data_top_100$previousPagePath,data_top_100$pagePath)),
                     title = unique(append(data_top_100$previousPagePath,data_top_100$pagePath))
                     
)

for(i in 1:nrow(nodes)){
  nodes$group[i] <- unlist(strsplit(unique(append(data_top_100$previousPagePath,data_top_100$pagePath))[i],'/'))[2]
}  ## Adds group to nodes 
df_density <- Density(data_top_100) ## Get density
nodes <- add_Density(nodes,df_density) ## Adds Density

# Edges creation
edges <- data.frame(
  to = data_top_100$pagePath,
  from = data_top_100$previousPagePath,
  title = data_top_100$pageviews,
  value = data_top_100$pageviews #rnorm(data_top_100$pageviews,10)
)



visNetwork(nodes,edges,width = "125%",height = "100%") %>%
  visEdges(arrows = 'to','from') %>%
  visGroups(groupname = "solutions", color = "darkblue", 
            shadow = list(enabled = TRUE)) %>%
  visLayout(improvedLayout = TRUE) %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)


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


