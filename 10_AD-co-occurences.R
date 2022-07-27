#load helper file
source("0_pachages-function.R")

#load data
data.clean <- readRDS("AD-data-clean.rds")
df.sumstats <- readRDS("AD-sumstats.rds")

df.ad <- readRDS("df-ad.rds")

## AD co-occurence matrix

ad.co <- df.ad %>%
  select(record_id, contains("autoimmune_id___"), -autoimmune_id___44) 
ad.labels <- ad.co %>%
  get_label %>% gsub("\\(.*", "", .) %>%
  str_trim 
ad.co <- ad.co %>%
  mutate(across(autoimmune_id___1:autoimmune_id___172, .fns = as.numeric))
colnames(ad.co) <- ad.labels

# test <- ad.co %>%
  # pivot_longer(-`Record ID`, names_to = "illnesses", values_to = "count")

#THIS WORKED
ad.co <- ad.co %>% select(-`Record ID`)

## log transformation
ad.co.log <- ad.co %>% t()
i <- as.matrix(ad.co.log[,])
j <- i %*% t(i)
z <- j %>% as.data.frame() %>% mutate(across(.fns =  ~case_when(. != 0 ~ log2(. + 1),
                                                                      . == 0 ~ log2(1), 
                                                                      TRUE ~ 1000)))
y <- as.matrix(z[,])
diag(y) <- 0
# pdf("images/AD-cooc-log2.pdf")
#manually save 30 x 35 inches
gplots::heatmap.2(t(y), 
                  Colv=T, 
                  col=plasma(100),
                  #ColSideCol = rev(cols2[2:7]), 
                  density="none", trace="none", 
                  margins=c(8,8),
                  keysize=1,
                  key.xlab="Tally",
                  key.title="NULL")
# dev.off()

## with more than 10 entries
# ad.co.10 <- ad.co[, colSums(ad.co)>10]
# ad.co.10 <- ad.co.10 %>% t()
# m <- as.matrix(ad.co.10[,])
# n <- m %*% t(m)
# diag(n) <- 0
# png("images/AD-cooccurence-matrix.png")
# gplots::heatmap.2(t(n), 
#                   Colv=F, 
#                   col=plasma(100),
#                   #ColSideCol = rev(cols2[2:7]), 
#                   density="none", trace="none", 
#                   margins=c(8,8),
#                   keysize=1,
#                   key.xlab="Tally",
#                   key.title="NULL")
# dev.off()

#plus co-morbs
ad.com.co <- df.ad %>%
  select(record_id, contains("autoimmune_id___"), contains("chronic___"), -c(autoimmune_id___44, chronic___8)) 
ad.com.labels <- ad.com.co %>%
  get_label %>% gsub("\\(.*", "", .) %>%
  str_trim 
ad.com.co <- ad.com.co %>%
  mutate(across(autoimmune_id___1:autoimmune_id___172, .fns = as.numeric))
colnames(ad.com.co) <- ad.com.labels

ad.com.co <- ad.com.co %>% select(-`Record ID`)
#log transformation
ad.com.co.log <- ad.com.co %>% t()
k <- as.matrix(ad.com.co.log[,])
l <- k %*% t(k)
l <- l %>% as.data.frame() %>% mutate(across(.fns =  ~case_when(. != 0 ~ log2(. + 1),
                                                                . == 0 ~ log2(1), 
                                                                TRUE ~ 1000)))
l <- as.matrix(l[,])
diag(l) <- 0
# pdf("images/AD-comorb-cooc-log2.pdf")
#manually save 30 x 35 inches
gplots::heatmap.2(t(l), 
                  Colv=T, 
                  col=plasma(100),
                  #ColSideCol = rev(cols2[2:7]), 
                  density="none", trace="none", 
                  margins=c(8,8),
                  keysize=1,
                  key.xlab="Tally",
                  key.title="NULL")
# dev.off()

## just more than 10 responses
# ad.com.co <- ad.com.co[, colSums(ad.com.co)>10]
# ad.com.co <- ad.com.co %>% t()
# k <- as.matrix(ad.com.co[,])
# l <- k %*% t(k)
# diag(l) <- 0
# png("images/AD-comorb-cooccurence-matrix.png")
# gplots::heatmap.2(t(l), 
#                   Colv=F, 
#                   col=plasma(100),
#                   #ColSideCol = rev(cols2[2:7]), 
#                   density="none", trace="none", 
#                   margins=c(8,8),
#                   keysize=1,
#                   key.xlab="Tally",
#                   key.title="NULL")
# dev.off()

## upset plot conectivity
