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
library(igraph)
#library(utilities)
#library(cooccur) damn it, specific for species co-occurence

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

sympt.network <- function(input, title){
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
  coords <- layout_as_star(diff_inet) 
  igraph::E(inet)$weight <- rank(weights)/length(weights)
  igraph::E(inet)$width <- (igraph::E(inet)$weight^2 * 10)
  igraph::E(inet)$color <- viridis(101)[ round(igraph::E(inet)$weight * 100) + 1  ]
  
  ## Stylize nodes/vertices 
  igraph::V(inet)$size <- 5
  igraph::V(inet)$color <- "white"
  
  ## To remove weak edges for clarity 
  threshold = 0.5
  inet_sub <-  igraph::delete_edges(inet, igraph::E(inet)[weights <= threshold])
  x <- plot(inet_sub,
            vertex.label.color=c("black"),
            vertex.label.font=c(2), 
            vertex.label.cex=c(1.5),                 
            vertex.label.dist=1,
            layout=coords,
            main = title)
  x
}
#call: sympt.network(sexy_m)
#ggsave("images/-network.png")
# sympt.network(gender.f, "title")

network.differences <- function(input_a, input_b, title) {
  #weights for input A
  symp_num_a <- mapply(input_a, FUN = as.numeric)
  symp_net_a = cor(symp_num_a, m = "s", use = "pair") # get correlations
  n_a = dim(symp_net_a)[1]
  # rank vlaues and standardize
  symp_net_a <-
    matrix(
      rank(symp_net_a, na.last = "keep", ties.method = "average"),
      nrow = n_a,
      ncol = n_a
    )
  rownames(symp_net_a) <- rownames(symp_net_a)
  colnames(symp_net_a) <- colnames(symp_net_a)
  symp_net_a <- symp_net_a / max(symp_net_a, na.rm = TRUE)
  ## Setup igraph network
  sub_net_a =  symp_net_a
  diag(sub_net_a) <-  0
  upper_a <- row(sub_net_a) < col(sub_net_a)
  pairs_a <- which(upper_a, arr.ind = T)
  gene_names_a <-  Hmisc::label(input_a)
  weights_a <- sub_net_a[upper_a]
  
  
  #weights for input B
  symp_num_b <- mapply(input_b, FUN = as.numeric)
  symp_net_b = cor(symp_num_b, m = "s", use = "pair") # get correlations
  n_b = dim(symp_net_b)[1]
  # rank vlaues and standardize
  symp_net_b <-
    matrix(
      rank(symp_net_b, na.last = "keep", ties.method = "average"),
      nrow = n_b,
      ncol = n_b
    )
  rownames(symp_net_b) <- rownames(symp_net_b)
  colnames(symp_net_b) <- colnames(symp_net_b)
  symp_net_b <- symp_net_b / max(symp_net_b, na.rm = TRUE)
  ## Setup igraph network
  sub_net_b =  symp_net_b
  diag(sub_net_b) <-  0
  upper_b <- row(sub_net_b) < col(sub_net_b)
  pairs_b <- which(upper_b, arr.ind = T)
  gene_names_b <-  Hmisc::label(input_b)
  weights_b <- sub_net_b[upper_b]
  
  # Rank and standardize the weights
  weights_a_ranked = rank(weights_a)
  weights_a_ranked = weights_a_ranked / max(weights_a_ranked, na.rm = T)
  
  weights_b_ranked = rank(weights_b)
  weights_b_ranked = weights_b_ranked / max(weights_b_ranked, na.rm = T)
  
  # Difference
  diff_weights = weights_a_ranked - weights_b_ranked
  
  # fill in the network matrix
  symp_net_diff = diag(n_a)
  symp_net_diff[upper_a] = diff_weights
  symp_net_diff = symp_net_diff + t(symp_net_diff)
  diag(symp_net_diff) = 0
  
  ##test
  diff_pairs <-
    data.frame(p1 = gene_names_a[pairs_a[, 1]],
               p2 = gene_names_a[pairs_a[, 2]] ,
               weights = diff_weights)
  diff_inet <- igraph::graph_from_data_frame(diff_pairs, directed = F)
  
  ## Stylize nodes/vertices 
  igraph::V(diff_inet)$size <- 5
  igraph::V(diff_inet)$color <- "white"
  
  ## Stylize edges
  pgcols <-
    colorRampPalette(brewer.pal(11, "PRGn"))(201) # generate new palette for the differences
  coords <- layout_as_star(diff_inet)
  igraph::E(diff_inet)$color <-
    pgcols[round((igraph::E(diff_inet)$weights + 1) * 100) + 1]  # since the "weights" can be negative, transpose by 1
  igraph::E(diff_inet)$width <-
    ((igraph::E(diff_inet)$weights + 1) ^ 2 * 5) # play around with this to make sure it looks okay
  x <- plot(diff_inet,
            vertex.label.color=c("black"),
            vertex.label.font=c(2), 
            vertex.label.cex=c(1.5),                 
            vertex.label.dist=1,
            layout = coords,
            main = title)
  x
}
# network.differences(gender.m, gender.f, "Network difference, male & female symptoms")

#colours
cols2 = brewer.pal(8, "Spectral")


# #### trying to work out mdx other gender issues in dropping zeroes during plyr::count
# symp_list = lapply(1:dim(scale.mdx.o)[2], function(i) cbind(i,plyr::count(scale.mdx.o[,i]))) #%>% complete(i, .[,], fill = list(freq=0))) )
# for(i in 1:length(symp_list)) { colnames(symp_list[[i]]) = c("i", "j", "k" ) }
# #symp_list <- symp_list %>% complete()
# symp_temp = do.call(rbind,symp_list )
# symp_mat = tidyr::spread(symp_temp, key=2, value=3, fill=0  )
# symp_mat = t(symp_mat[,-1])
# # Since all the symptoms have similar scales, get those from the response
# symps = as.character(plyr::count(data.clean$fatigue.factor)[,1]) 
# missing = which(is.na(symps)) 
# symps[missing] = "Missing"
# 
# # Get the symptom labels 
# sympt = Hmisc::label(data.clean %>% select(fatigue:bruise))
# 
# # Label the symptoms matrix 
# rownames(symp_mat) = symps
# colnames(symp_mat) = sympt
# 
# 
# # Plot the tally (removing missing results row)
# 
# x <- gplots::heatmap.2(t(symp_mat[-missing,]), 
#                        Colv=F, 
#                        col=plasma(100),
#                        ColSideCol = rev(cols2[2:7]), 
#                        density="none", trace="none", 
#                        margins=c(8,8),
#                        keysize=1,
#                        key.xlab="Tally",
#                        key.title="NULL") 
# x