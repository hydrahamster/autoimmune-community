# Load packages
library(Hmisc)
library(plyr)
library(dplyr)
library(tidyverse)
library(magrittr)
library(readr)
library(httr)
library(lubridate)
library(sjlabelled)
library(janitor)
library(RColorBrewer)
library(wesanderson)
library(ggrepel)
library(arsenal) 
library(ggbeeswarm)
library(wordcloud2)
library(readxl)
library(viridis)
library(naniar)
library(ComplexUpset)
library(gridExtra)
library(tm)
library(countrycode)
library(ggstatsplot)
library(GGally)
library(paletteer)

#Functions
count_comp <- function(x) sum(as.numeric(x))

### symptoms heatmap & network
sympt.heat <- function(input){
  ###males: don't have full compliment of responses, need to work around
  symp_list = lapply(1:dim(input)[2], function(i) cbind(i,plyr::count(input[,i])) )
  for(i in 1:length(symp_list)) { colnames(symp_list[[i]]) = c("i", "j", "k" ) }
  symp_temp = do.call(rbind,symp_list )
  symp_mat = tidyr::spread(symp_temp, key=2, value=3, fill=0  )
  symp_mat = t(symp_mat[,-1])
  # Since all the symptoms have similar scales, get those from the response
  symps = as.character(plyr::count(data.clean$fatigue.factor)[,1]) 
  missing = which(is.na(symps)) 
  symps[missing] = "Missing"
  
  # Get the symptom labels 
  sympt = Hmisc::label(data.clean %>% select(fatigue:bruise))
  
  # Label the symptoms matrix 
  rownames(symp_mat) = symps
  colnames(symp_mat) = sympt
  
  
  # Plot the tally (removing missing results row)
  
  x <- gplots::heatmap.2(t(symp_mat[-missing,]), 
                         Colv=F, 
                         col=plasma(100),
                         ColSideCol = rev(cols2[2:7]), 
                         density="none", trace="none", 
                         margins=c(8,8),
                         keysize=1,
                         key.xlab="Tally",
                         key.title="NULL") 
  x
}
# call: sympt.heat(input)
#ggsave("images/-heatmap.png")

sympt.network <- function(input){
  # Generate co-occurence network 
  ### Version 1 - room to try other methods here 
  symp_num <- mapply(input, FUN=as.numeric)
  symp_net = cor(symp_num, m="s", use="pair") # get correlations 
  n= dim(symp_net)[1] 
  # rank vlaues and standardize 
  symp_net <- matrix(rank(symp_net, na.last = "keep", ties.method = "average"),
                     nrow = n, ncol = n) 
  rownames(symp_net) <- rownames(symp_net)
  colnames(symp_net) <- colnames(symp_net)
  symp_net <- symp_net/max(symp_net, na.rm = TRUE)
  
  
  ## Setup igraph network 
  sub_net =  symp_net 
  diag(sub_net) <-  0
  upper <- row(sub_net) < col(sub_net)
  pairs <- which(upper, arr.ind = T )
  gene_names <-  Hmisc::label(input) 
  weights <- sub_net[upper]
  pairs <- data.frame( p1 = gene_names[pairs[,1]], p2 = gene_names[pairs[,2]] , weights = weights )
  inet <- igraph::graph_from_data_frame(pairs, directed=F)
  
  ## Stylize edges
  igraph::E(inet)$weight <- rank(weights)/length(weights)
  igraph::E(inet)$width <- (igraph::E(inet)$weight^2 * 10)
  igraph::E(inet)$color <- viridis(101)[ round(igraph::E(inet)$weight * 100) + 1  ]
  
  ## Stylize nodes/vertices 
  igraph::V(inet)$size <- 5
  igraph::V(inet)$color <- "white"
  
  ## To remove weak edges for clarity 
  threshold = 0.5
  inet_sub <-  igraph::delete_edges(inet, igraph::E(inet)[weights <= threshold])
  x <- plot(inet_sub)
  x
}
#call: sympt.network(sexy_m)
#ggsave("images/-network.png")

#colours
cols2 = brewer.pal(8, "Spectral")

