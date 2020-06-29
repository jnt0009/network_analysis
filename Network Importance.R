##potential Connections  PC =  (n*(n-1))/2
potentialConnection <- function(df){
  a <- df %>%
    filter(previousPagePath != "(exit)" & pagePath != "(exit)") %>%
    filter(previousPagePath != "(entrance)") %>%
    filter(pageviews > 0) %>%
    select(pageviews) %>%
    sum()
    ## Add column sum
    
  
  b <- a - 1
  
  c <- a*b

  PC <- c/2
  
  return(PC)
}
## Network Density  actual Connections / Potential connections

actualConnection <- function(df,page_){
  if(page_ %in% df$pagePath){ 
    LH <- df %>%
      filter(previousPagePath != "(exit)" & pagePath != "(exit)") %>%
      filter(previousPagePath != "(entrance)") %>%
      filter(pageviews > 0) %>%
      filter(pagePath == page_) %>%
      select(pageviews) %>%
      sum()
  } else {
    LH <- 0
  } ## Coming
  if(page_ %in% df$previousPagePath){
    RH <- df %>%
      filter(previousPagePath != "(exit)" & pagePath != "(exit)") %>%
      filter(previousPagePath != "(entrance)") %>%
      filter(pageviews > 0) %>%
      filter(previousPagePath == page_) %>%
      select(pageviews) %>%
      sum()
  } else {
    RH <- 0
  } ## Going
  ## Total
  AC <- LH + RH
  return(AC)
}


Density <- function(data_){
  hold <- as.data.frame(unique(append(data_$previousPagePath,data_$pagePath)))  ## Provision empty list
  for(i in 1:nrow(hold)){
    AC <- actualConnection(data_,hold[i,1])
    PC <- potentialConnection(data_)
    hold$density[i] <- (AC/PC)
    
  }
  colnames(hold)[1] = "Page"
  
  return(hold)
}

add_Density <- function(nodes,dense){
  hold <- merge(nodes,dense,
                 by.x = 'id',
                 by.y = 'Page')
  return(hold)
}





