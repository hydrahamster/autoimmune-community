# EXtras 

#load helper file
source("0_pachages-function.R") 
# source("0_packages-function.R") # typo in name changed 

#load data
data.clean <- readRDS("AD-data-clean.rds")
df.sumstats <- readRDS("AD-sumstats.rds")
df.ad <- readRDS("df-ad.rds")

# load types (saved from 4_*.R)
type.local.blood <- readRDS( file = "type-local-blood.rds")
type.local.cardiac <- readRDS( file = "type-local-cardiac.rds")
type.local.ear <- readRDS( file = "type-local-ear.rds")
type.local.endocrine <- readRDS( file = "type-local-endocrine.rds")
type.local.gastrointestinal <- readRDS( file = "type-local-gastrointestinal.rds")
type.local.genitourinary <- readRDS( file = "type-local-genitourinary.rds")
type.local.skeletal <- readRDS( file = "type-local-skeletal.rds")
type.local.kidney <- readRDS( file = "type-local-kidney.rds")
type.local.liver <- readRDS( file = "type-local-liver.rds")
type.local.lymphatic <- readRDS( file = "type-local-lymphatic.rds")
type.local.muscular <- readRDS( file = "type-local-muscular.rds")
type.local.nervous <- readRDS( file = "type-local-nervous.rds")
type.local.ocular <- readRDS( file = "type-local-ocular.rds")
type.local.pancreas <- readRDS( file = "type-local-pancreas.rds")
type.local.respiratory <- readRDS( file = "type-local-respiratory.rds")
type.local.skin <- readRDS( file = "type-local-skin.rds")
type.systemic.general <- readRDS( file = "type-systemic-general.rds")
type.systemic.rheum <- readRDS( file = "type-systemic-rheum.rds")
type.systemic.vasc <- readRDS( file = "type-systemic-vas")
autoimmune.yes    <- readRDS(file = "classification-autoimmune-yes.rds")
autoimmune.no <- readRDS(file = "classification-autoimmune-no.rds")
autoimmune.unknown <- readRDS(file = "classification-autoimmune-unknown")



types.list = list(type.local.blood,
                  type.local.cardiac,
                  type.local.ear,
                  type.local.endocrine,
                  type.local.gastrointestinal,
                  type.local.genitourinary,
                  type.local.skeletal,
                  type.local.kidney,
                  type.local.liver,
                  type.local.lymphatic,
                  type.local.muscular,
                  type.local.nervous,
                  type.local.ocular,
                  type.local.pancreas,
                  type.local.respiratory,
                  type.local.skin,
                  type.systemic.general,
                  type.systemic.rheum,
                  type.systemic.vasc) 

names(types.list) = c("Local blood",
                      "Local cardiac",
                      "Local ear",
                      "Local endocrine",
                      "Local gastrointestinal system",
                      "Local genitourinary system",
                      "Local joint/skeletal",
                      "Local kidney",
                      "Local liver",
                      "Local lymphatic",
                      "Local muscular",
                      "Local nervous system",
                      "Local occular",
                      "Local pancreas",
                      "Local respiratory",
                      "Local skin",
                      "Systemic general",
                      "Systemic rheumatological",
                      "Systemic vasculitis") 


 save( autoimmune.unknown, autoimmune.no, autoimmune.yes, types.list, file="classification-autoimmune.Rdata" )

# make palette - max number of overlap is ~173 
cols <- colorRampPalette(brewer.pal(9, "BuPu")[3:9] )(175)
cols[1] = "white"


# build ad type table  
labels1 = colnames(df.ad)
labels2 = get_label(df.ad)
labels3 = str_trim(gsub("\\(.*", "", labels2)) 
df.ad.labels = cbind(labels1, labels2, labels3 )
rownames(df.ad.labels) = labels1


# flag based on AD/yes/no/unk
temp = rep(0,length(labels1)) 
names(temp) = labels1
temp[autoimmune.yes] = 1 
temp[autoimmune.no] = 2
temp[autoimmune.unknown] = 3 
df.ad.labels = cbind(df.ad.labels, temp)

# colors based on AD/yes/no/unk
temp = rep("grey", length(labels1))
names(temp) = labels1
temp[autoimmune.yes] =  brewer.pal(8, "Dark2")[3]
temp[autoimmune.no] = brewer.pal(8, "Dark2")[1] 
temp[autoimmune.unknown] = brewer.pal(8, "Dark2")[4] 
df.ad.labels = cbind(df.ad.labels, temp)


# colors based on types, using turbo 
temp =  rep("grey", length(labels1))
i = 1 
for(types in types.list){ 
  m = match(types, labels1) 
  temp[ m[!is.na(m)] ] = turbo(19)[i] 
  i = i + 1 
}
# flag based on types 
temp2  =  rep(0, length(labels1))
i = 1 
for(types in types.list){ 
  m = match(types, labels1) 
  temp2[ m[!is.na(m)] ] = i  
  i = i + 1 
}
df.ad.labels = cbind(df.ad.labels, temp2, temp)

# remove top and bottom rows (disease start, cohort)
df.ad.labels = df.ad.labels[-c(1,2),]
df.ad.labels = df.ad.labels[-c(201,202),]


colnames(df.ad.labels) = c("columnID", "label", "label_edit", "AD", "AD_col", "type", "type_col" ) 
df.ad.labels <- as.data.frame(df.ad.labels)

save(df.ad.labels, cols, file="ad.labels.Rdata")


 

## AD + co co-occurence matrix (copied from 10_AD-coccurences.R)
ad.com.co <- df.ad %>%
  select(record_id, contains("autoimmune_id___"), contains("chronic___"), -c(autoimmune_id___44, chronic___8)) 
ad.com.labels <- ad.com.co %>%
  get_label %>% gsub("\\(.*", "", .) %>%
  str_trim 
ad.com.co <- ad.com.co %>%
  mutate(across(autoimmune_id___1:autoimmune_id___172, .fns = as.numeric))
colnames(ad.com.co) <- ad.com.labels

ad.com.co <- ad.com.co %>% select(-`Record ID`)
ad.com.co.10 <- ad.com.co[, colSums(ad.com.co)>10]
ad.com.co.10 <- ad.com.co.10 %>% t()
k <- as.matrix( ad.com.co %>% t() )
l <- k %*% t(k)
symp_co <- l %>% as.data.frame() %>% mutate(across(.fns =  ~case_when(. != 0 ~ log2(. + 1),
                                                                . == 0 ~ log2(1), 
                                                                TRUE ~ 1000)))                                         


#TODO is there a difference between this chunk and the one above?
## AD + co significance co-occurence matrix  
ad.com.co <- df.ad %>%
  select(record_id, contains("autoimmune_id___"), contains("chronic___"), -c(autoimmune_id___44, chronic___8)) 
ad.com.labels <- ad.com.co %>%
  get_label %>% gsub("\\(.*", "", .) %>%
  str_trim 
ad.com.co <- ad.com.co %>%
  mutate(across(autoimmune_id___1:autoimmune_id___172, .fns = as.numeric))
colnames(ad.com.co) <- ad.com.labels

ad.com.co <- ad.com.co %>% select(-`Record ID`)
k <- as.matrix( ad.com.co %>% t() )
l <- k %*% t(k)
symp_co <- l %>% as.data.frame() %>% mutate(across(.fns =  ~case_when(. != 0 ~ log2(. + 1),
                                                                      . == 0 ~ log2(1), 
                                                                      TRUE ~ 1000)))                                         


## Setup igraph network 
sub_net =  symp_co 
upper <- row(sub_net) < col(sub_net)
pairs <- which(upper, arr.ind = T )
node_names <-  rownames(symp_co)
weights <- sub_net[upper]
pairs <- data.frame( p1 = node_names[pairs[,1]], p2 = node_names[pairs[,2]] , weights = weights )
inet <- igraph::graph_from_data_frame(pairs, directed=F) 



# match symp_co to labels to get colors/types
m = match( node_names, df.ad.labels$label_edit  ) 
vertices_col = df.ad.labels$AD_col[m]
vertices_col2 = df.ad.labels$type_col[m] 

# make filters 
f1 = df.ad.labels$AD[m] != 0            # filter for only AD  
f2 = (2^diag(as.matrix(symp_co))-1) <  10   # filter for those less than 10 (note, take complement/NOT(!) to get those greater)   

# color and adjust graph
igraph::E(inet)$width <-  igraph::E(inet)$weights  
igraph::E(inet)$color <- cols[ (2^igraph::E(inet)$weights  )+1 ]  
igraph::V(inet)$size <-  diag(as.matrix(symp_co)) + 1  * 2  
igraph::V(inet)$color <- vertices_col

# filter for plots 
threshold = 1 # remove overlaps threshold (ie greater than 1, mostly visual)

# plot only those with more than threshold 
inet_sub <-  igraph::delete_edges(inet, igraph::E(inet)[weights <= threshold])
# plot with labels 
  plot(inet_sub ) 
# plot without labels 
  plot(inet_sub ,  vertex.label=NA, layout=layout_with_fr(inet_sub) )  

  
# plot only larger overlaps (ie remove f2 filter) and threshold 
inet_sub <-  igraph::delete_vertices(inet, igraph::V(inet)[ f2] )
inet_sub <-  igraph::delete_edges(inet_sub, igraph::E(inet_sub)[E(inet_sub)$weights <= threshold])
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
vcount(inet_sub) # find number of vertices in inet_sub object
lab.locs <- radian.rescale(x=1:56, direction=-1, start=0) #insert number of vertices as upper limit for x
plot(inet_sub, 
     layout=layout_in_circle(inet_sub),
     vertex.label.color=c("black"),
     vertex.label.degree=lab.locs,
     vertex.label.font=c(2), 
     vertex.label.cex=c(0.8),
     vertex.label.dist=1.5)  

# plot only larger overlaps + co (ie remove !f1 and f2 filter) and threshold 
inet_sub_2 <-  igraph::delete_vertices(inet, igraph::V(inet)[ !(f1 & !f2)  ] )
inet_sub_2 <-  igraph::delete_edges(inet_sub_2, igraph::E(inet_sub_2)[E(inet_sub_2)$weights <= threshold])
#lab.locs <- radian.rescale(x=1:43, direction=-1, start=0)
plot(inet_sub_2 ,
     layout=layout_with_kk(inet_sub_2),
     vertex.label.color=c("black"),
     vertex.label.degree=-pi/2,
     vertex.label.font=c(2), 
     vertex.label.cex=c(0.8))#,
     #vertex.label.dist=1.5)    


# make legends for AD and types  
leg1 = unique(df.ad.labels[,6:7])
o = order(as.numeric(leg1[,1])  ) 
leg1 = leg1[o,]
legend = cbind(leg1[,1], c("none", names(types.list)) , leg1[,2])

leg2 = unique(df.ad.labels[,4:5]) 
o = order(as.numeric(leg2[,1])  ) 
leg2 = leg2[o,]
legend2 = cbind(leg2[,1], c("Comorbid", "Autoimmune", "Other", "Unknown") , leg2[,2])

# save legend for figure  
plot(0,0)
legend( "top", leg=legend[,2], col=legend[,3], pch=15)
legend( "bottom", leg=legend2[,2], col=legend2[,3], pch=15)


# Co-occurence heatmaps 
# Full (ad + co)
symp_co <- as.matrix(symp_co)
gplots::heatmap.2((symp_co[,]), 
                  col=cols,
                  ColSideCol = vertices_col[], 
                  RowSideCol = vertices_col2[], 
                  density="none", trace="none", 
                  margins=c(8,8),
                  keysize=1,
                  # cexRow= 1.1,
                  key.xlab="Tally",
                  key.title="NULL")

# Only AD 
gplots::heatmap.2((symp_co[f1,f1]), 
                  col=cols,
                  ColSideCol = vertices_col[f1], 
                  RowSideCol = vertices_col2[f1], 
                  density="none", trace="none", 
                  margins=c(8,8),
                  keysize=1,
                  key.xlab="Tally",
                  key.title="NULL")

# AD + co & >= 10 
gplots::heatmap.2((symp_co[!f2,!f2]), 
                  col=cols,
                  ColSideCol = vertices_col[!f2], 
                  RowSideCol = vertices_col2[!f2], 
                  density="none", trace="none", 
                  margins=c(8,8),
                  keysize=1,
                  key.xlab="Tally",
                  key.title="NULL")



# AD only  & >= 10 
gplots::heatmap.2( (symp_co[f1&!f2,f1&!f2]), 
                  col=cols,
                  ColSideCol = vertices_col[f1&!f2], 
                  RowSideCol = vertices_col2[f1&!f2], 
                  density="none", trace="none", 
                  margins=c(8,8),
                  keysize=1,
                  key.xlab="Tally",
                  key.title="NULL")



#TODO up to here
# borrowing code from outdeco, adjust for this work (https://github.com/ballouzlab/OutDeCo_lite/blob/master/R/cluster_coexp.R)
clust_net = OutDeCo::cluster_coexp(symp_co[f1,f1], flag_plot = T, flag_med = F, flag_dist = T, col=cols, method="complete")
# look through clusters to see which illnesses all cooccur 
clust_net$clusters 


# joint df to get scatter plots 
m = match( data.clean$record_id, df.ad$record_id)
f.d2 = !is.na(m)
f.a2 = m[f.d2]

m = match( df.sumstats$record_id, df.ad$record_id)
f.d = !is.na(m)
f.a = m[f.d]

df.temp =  cbind( data.clean[f.d2,],  df.sumstats[f.d,], df.ad[f.a2,])

## color by misdiag
tempcols = c(magma(10)[4], viridis(5)[3])
tempcols2 = as.numeric(df.temp$misdiag.factor)
tempcols3 = tempcols[tempcols2]
tempcols3[is.na(tempcols3)]  = "grey"
misd= unique(tempcols3)

filt = df.temp$cohort.id!="control"; 
plot( jitter(df.temp$ad.sum[filt]) , df.temp$age[filt], pch=19, col = tempcols3[filt] , xlab="Number of chronic illnesses", ylab="Age", cex=2)
for(i in misd) { 
  filt2 = tempcols3==i 
  zlm = lm( df.temp$age[filt][filt2] ~  df.temp$ad.sum[filt][filt2]  )
  abline(zlm, col=i, lwd=2)
} 

## color by nqscore 
nqscore = as.numeric(df.temp$neuroqol_bank_v10_fatigueol_tscore )
nqscore[is.na(nqscore)] = 0
temp = nqscore[nqscore > 200  ]/10
nqscore[nqscore > 200  ] = temp
nqscore[nqscore == 0  ] = NA

tempcols4 = plasma(100)[round(nqscore)] 
tempcols3[is.na(tempcols3)]  = "grey"
plot( jitter(df.temp$ad.sum[filt]) , df.temp$age[filt], pch=19, col = tempcols4[filt] , xlab="Number of chronic illnesses", ylab="Age", cex=2)
 

plot( jitter(df.temp$ad.sum[filt]) , nqscore[filt], pch=19, col = tempcols3[filt] , xlab="Number of chronic illnesses", ylab="nqol", cex=2)



# pvals of overlap, also using outdeco stuff https://github.com/ballouzlab/OutDeCo_lite/blob/master/R/gene_set_enrichment.R
voc = cbind(rownames(k), rownames(k), rowSums(k)  )
overlaps = lapply(1:dim(k)[2], function(i) OutDeCo::gene_set_enrichment (rownames(t(k))[which(t(k)[,i] > 0  )] , t(k), voc) )
pp = sapply(1:length(overlaps), function(i) as.numeric(overlaps[[i]][,3])+1  )
pval = sapply(1:length(overlaps), function(i) as.numeric(overlaps[[i]][,5])  )
pval[pval==0] = 1e-300 # 
padj = pval * 0 
bottom = row(pval) > col(pval)
padj[bottom] = p.adjust(pval[bottom])
padj = padj + t(padj)
diag(padj) = 1 

gplots::heatmap.2( log2(pp+1), 
                  col=cols,
                  ColSideCol = vertices_col, 
                  RowSideCol = vertices_col2, 
                  density="none", trace="none", 
                  margins=c(8,8),
                  keysize=1,
                  key.xlab="Tally",
                  key.title="NULL")



gplots::heatmap.2( -log10(pval), 
                   col=cols,
                   ColSideCol = vertices_col, 
                   RowSideCol = vertices_col2, 
                   density="none", trace="none", 
                   margins=c(8,8),
                   keysize=1,
                   key.xlab="Tally",
                   key.title="NULL")


f3 = colSums(padj <= 0.05 ) > 0
gplots::heatmap.2( -log10(padj[f3,f3]), 
                   col=cols,
                   ColSideCol = vertices_col[f3], 
                   RowSideCol = vertices_col2[f3], 
                   density="none", trace="none", 
                   margins=c(8,8),
                   keysize=1,
                   key.xlab="Tally",
                   key.title="NULL")





