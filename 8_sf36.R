#load helper file
source("0_pachages-function.R")

#load data
data.clean <- readRDS("AD-data-clean.rds")
df.sumstats <- readRDS("AD-sumstats.rds")
#NOTE: df.ad too large ot load on desktop
df.ad <- readRDS("df-ad.rds")

#SF36
df.sf36 <- data.clean %>%
  select(record_id,
         contains("rand_36_"),
         contains("rand36")) %>%
  filter(rand_36_item_sf_health_survey_instrument_version_1_complete == 2) %>%
  select( -rand_36_item_sf_health_survey_instrument_version_1_complete,
          -rand_36_item_sf_health_survey_instrument_version_1_timestamp,
          -contains(".factor")) %>%
  mutate(across(record_id:rand36_36, .fns = as.numeric))
#already in the right scale from 0 (bad) to 100 (good)
#set up scale columns
scale.physfunc <- c("rand36_3", "rand36_4", "rand36_5", "rand36_6", "rand36_7", "rand36_8", "rand36_9", "rand36_10", "rand36_11", "rand36_12")
scale.rolephys <- c("rand36_13", "rand36_14", "rand36_15", "rand36_16")
scale.roleemot <- c("rand36_17", "rand36_18", "rand36_19")
scale.fatigue <- c("rand36_23", "rand36_27", "rand36_29", "rand36_31")
scale.emotwell <- c("rand36_24", "rand36_25", "rand36_26", "rand36_28", "rand36_30")
scale.socfunct <- c("rand36_20", "rand36_32")
scale.pain <- c("rand36_21", "rand36_22")
scale.genhealth <- c("rand36_1", "rand36_33", "rand36_34", "rand36_35", "rand36_36")

#set up summary stats columns
broad.physsum <- c("scale.physfunc", "scale.rolephys", "scale.pain", "scale.genhealth")
broad.mentsum <- c("scale.roleemot", "scale.fatigue", "scale.emotwell", "scale.socfunct")

#sf36 df
sf36.score <- df.sf36 %>%
  mutate(scale.physfunc = rowMeans(.[,c(scale.physfunc)])) %>%
  mutate(scale.rolephys = rowMeans(.[,c(scale.rolephys)])) %>%
  mutate(scale.roleemot = rowMeans(.[,c(scale.roleemot)])) %>%
  mutate(scale.fatigue = rowMeans(.[,c(scale.fatigue)])) %>%
  mutate(scale.emotwell = rowMeans(.[,c(scale.emotwell)])) %>%
  mutate(scale.socfunct = rowMeans(.[,c(scale.socfunct)])) %>%
  mutate(scale.pain = rowMeans(.[,c(scale.pain)])) %>%
  mutate(scale.genhealth = rowMeans(.[,c(scale.genhealth)])) %>%
  mutate(broad.physsum = rowMeans(.[,c(broad.physsum)])) %>%
  mutate(broad.mentsum = rowMeans(.[,c(broad.mentsum)])) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))
saveRDS(sf36.score, file = "AD_sf36-score.rds") 


ad.short <- df.ad %>%
  #filter(autoimmune_id___55 != 1) %>%
  subset(., select = -c(autoimmune_id___44, # "none" predominantly logged ADs in other section so this category is a moot
                        #autoimmune_id___45, #other ADs
                        #autoimmune_id___120, #following are encompassed in hypermobility, aAD 172
                          #autoimmune_id___123,
                          #autoimmune_id___124,
                          #autoimmune_id___135,
                        #autoimmune_id_other,
                        #otherchronic,
                        diseasestat)) %>%
  select(-contains("chronic___"))
## overview ADs
plot.ad <- ad.short %>%
  select(contains("autoimmune_id")) %>%
  label_to_colnames(.) %>%
  summarise_all(sum) %>%
  transpose()
plot.autdis <- as.data.frame(do.call(cbind, plot.ad)) %>% 
  rownames_to_column(.) %>%
  rename("autoimmune.disorder" = "rowname") %>%
  rename("count" = "V1") %>%
  #mutate_all(~str_replace_all(., "infection.", "")) %>%
  mutate(count = as.numeric(count))
plot.autdis %>%
  #filter(infection != "none") %>%
  filter(count > 9) %>%
  mutate(autoimmune.disorder = fct_reorder(autoimmune.disorder, count)) %>%
  ggplot(aes(x = autoimmune.disorder, y = count, fill = autoimmune.disorder)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 220),
                     breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200, 220)) +
  scale_fill_viridis_d() +
  coord_flip() +
  theme_bw() +
  theme(plot.title=element_text(size=15, face="bold"),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15, face="bold"))


## interconnextivity is nuts. So need to look at how many ADs people have in total per top 5
ad.short <- ad.short %>%
  mutate(ad.sum = rowSums(select(., contains("autoimmune_id"))))

## join SF36 scores and sumstats
sf36.domain <- sf36.score %>%
  select(record_id,
         broad.physsum,
         broad.mentsum)
ad.info <- ad.short %>%
  left_join(sf36.domain, by = "record_id") %>%
  left_join(df.sumstats, by = "record_id")
##there are zero AD entries, with diseasestat == 1.. why?
#TODO up to here need to re-do HC and disease cohort classification


# ad.query <- data.clean  %>%
#   select(record_id,
#          autoimmune_id_other,
#          otherchronic)
# query <- ad.info %>%
#   filter(ad.sum == 0) %>%
#   left_join(ad.query, by = as.numeric("record_id")) %>%
#   select(record_id,
#          autoimmune_id_other,
#          otherchronic,
#          contains("autoimmune_id___"))

saveRDS(ad.info, file = "ad-sf36-sumstats.rds")

### all ADs
ggscatterstats(
  data = ad.info,
  x = ad.sum,
  y = broad.mentsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Mental QoL score per number of ADs in all patients"
)
ggsave("ad-sf36/allADs-mental-qol.png")

ggscatterstats(
  data = ad.info,
  x = ad.sum,
  y = broad.physsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Physical QoL score per number of ADs in all patients"
)
ggsave("ad-sf36/allADs-physical-qol.png")
#split into sub dataframes of top 5 diseases
### coeliac
df.coeliac <- ad.info %>%
  filter(autoimmune_id___6 == 1)

ggscatterstats(
  data = df.coeliac,
  x = ad.sum,
  y = broad.qolall,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Overall QoL score per number of ADs in Coeliac patients"
)
ggsave("ad-sf36/coeliac-overall-qol.png")

ggscatterstats(
  data = df.coeliac,
  x = ad.sum,
  y = broad.mentsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Mental QoL score per number of ADs in Coeliac patients"
)
ggsave("ad-sf36/coeliac-mental-qol.png")

ggscatterstats(
  data = df.coeliac,
  x = ad.sum,
  y = broad.physsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Physical QoL score per number of ADs in Coeliac patients"
)
ggsave("ad-sf36/coeliac-physical-qol.png")

### hashi
df.hashi <- ad.info %>%
  filter(autoimmune_id___17 == 1)

ggscatterstats(
  data = df.hashi,
  x = ad.sum,
  y = broad.qolall,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Overall QoL score per number of ADs in Hashimoto's patients"
)
ggsave("ad-sf36/hashi-overall-qol.png")

ggscatterstats(
  data = df.hashi,
  x = ad.sum,
  y = broad.mentsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Mental QoL score per number of ADs in Hashimoto's patients"
)
ggsave("ad-sf36/hashi-mental-qol.png")

ggscatterstats(
  data = df.hashi,
  x = ad.sum,
  y = broad.physsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Physical QoL score per number of ADs in Hashimoto's patients"
)
ggsave("ad-sf36/hashi-physical-qol.png")

### lupus
df.lupus <- ad.info %>%
  filter(autoimmune_id___22 == 1)

ggscatterstats(
  data = df.lupus,
  x = ad.sum,
  y = broad.qolall,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Overall QoL score per number of ADs in SLE patients"
)
ggsave("ad-sf36/lupus-overall-qol.png")

ggscatterstats(
  data = df.lupus,
  x = ad.sum,
  y = broad.mentsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Mental QoL score per number of ADs in SLE patients"
)
ggsave("ad-sf36/lupus-mental-qol.png")

ggscatterstats(
  data = df.lupus,
  x = ad.sum,
  y = broad.physsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Physical QoL score per number of ADs in SLE patients"
)
ggsave("ad-sf36/lupus-physical-qol.png")

### RA
df.rheum <- ad.info %>%
  filter(autoimmune_id___35 == 1)

ggscatterstats(
  data = df.rheum,
  x = ad.sum,
  y = broad.qolall,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Overall QoL score per number of ADs in RA patients"
)
ggsave("ad-sf36/rheum-overall-qol.png")

ggscatterstats(
  data = df.rheum,
  x = ad.sum,
  y = broad.mentsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Mental QoL score per number of ADs in RA patients"
)
ggsave("ad-sf36/rheum-mental-qol.png")

ggscatterstats(
  data = df.rheum,
  x = ad.sum,
  y = broad.physsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Physical QoL score per number of ADs in RA patients"
)
ggsave("ad-sf36/rheum-physical-qol.png")

### sjoegren's
df.sjog <- ad.info %>%
  filter(autoimmune_id___38 == 1)

ggscatterstats(
  data = df.sjog,
  x = ad.sum,
  y = broad.qolall,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Overall QoL score per number of ADs in Sjoegren's patients"
)
ggsave("ad-sf36/sjog-overall-qol.png")

ggscatterstats(
  data = df.sjog,
  x = ad.sum,
  y = broad.mentsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Mental QoL score per number of ADs in Sjoegren's patients"
)
ggsave("ad-sf36/sjog-mental-qol.png")

ggscatterstats(
  data = df.sjog,
  x = ad.sum,
  y = broad.physsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Physical QoL score per number of ADs in Sjoegren's patients"
)
ggsave("ad-sf36/sjog-physical-qol.png")

# check how 6th to 8th most common ADs are fareing
### fibro
df.fibro <- ad.info %>%
  filter(autoimmune_id___64 == 1)

ggscatterstats(
  data = df.fibro,
  x = ad.sum,
  y = broad.qolall,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Overall QoL score per number of ADs in fibro patients"
)
ggsave("ad-sf36/fibro-overall-qol.png")

ggscatterstats(
  data = df.fibro,
  x = ad.sum,
  y = broad.mentsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Mental QoL score per number of ADs in fibro patients"
)
ggsave("ad-sf36/fibro-mental-qol.png")

ggscatterstats(
  data = df.fibro,
  x = ad.sum,
  y = broad.physsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Physical QoL score per number of ADs in fibro patients"
)
ggsave("ad-sf36/fibro-physical-qol.png")


###scleroderma
df.sclero <- ad.info %>%
  filter(autoimmune_id___37 == 1)

ggscatterstats(
  data =df.sclero,
  x = ad.sum,
  y = broad.qolall,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Overall QoL score per number of ADs in scleroderma patients"
)
ggsave("ad-sf36/sclero-overall-qol.png")

ggscatterstats(
  data = df.sclero,
  x = ad.sum,
  y = broad.mentsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Mental QoL score per number of ADs in scleroderma patients"
)
ggsave("ad-sf36/sclero-mental-qol.png")

ggscatterstats(
  data = df.sclero,
  x = ad.sum,
  y = broad.physsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Physical QoL score per number of ADs in scleroderma patients"
)
ggsave("ad-sf36/sclero-physical-qol.png")

###hypermobility
df.hyper <- ad.info %>%
  filter(autoimmune_id___172 == 1)

ggscatterstats(
  data = df.hyper,
  x = ad.sum,
  y = broad.qolall,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Overall QoL score per number of ADs in hypermobile patients"
)
ggsave("ad-sf36/hyper-overall-qol.png")

ggscatterstats(
  data = df.hyper,
  x = ad.sum,
  y = broad.mentsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Mental QoL score per number of ADs in hypermobile patients"
)
ggsave("ad-sf36/hyper-mental-qol.png")

ggscatterstats(
  data = df.hyper,
  x = ad.sum,
  y = broad.physsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Physical QoL score per number of ADs in hypermobile patients"
)
ggsave("ad-sf36/hyper-physical-qol.png")

### look at ME and number of ADs specifically
# remove ME/CFS cohort and useless columns
me.short <- df.ad %>%
  filter(autoimmune_id___55 == 1) %>%
  subset(., select = -c( autoimmune_id___44, #healthy controls, NA issues from disease stat problematic downstream
                        autoimmune_id___45, #other ADs
                        autoimmune_id___120, #following are encompassed in hypermobility, aAD 172
                        autoimmune_id___123,
                        autoimmune_id___124,
                        autoimmune_id___135,
                        autoimmune_id_other,
                        otherchronic,
                        diseasestat)) %>%
  select(-contains("chronic___"))

#look at interconnextivity
me.co <- me.short %>%
  select(contains("autoimmune_id"))%>%
  names()
upset(me.short, 
      me.co, 
      name = "Autoimmune disorders with ME",
      min_degree=1) #del comma before ")" shouldn't affect running though

## interconnextivity is nuts. So need to look at how many ADs people have in total per top 5
me.short <- me.short %>%
  mutate(ad.sum = rowSums(select(., contains("autoimmune_id"))))

## join SF36 scores and sumstats
me.info <- me.short %>%
  left_join(sf36.domain, by = "record_id") %>%
  left_join(df.sumstats, by = "record_id")
saveRDS(me.info, file = "me-sf36-gen.rds")

ggscatterstats(
  data = me.info,
  x = ad.sum,
  y = broad.qolall,
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  #label.var = ad.sum > 8,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))), #, limits = (c(1, 14)) fo uniform scale on x-axis
  # package = "viridis",
  # palette = "viridis",
  title = "Overall QoL score per number of ADs in ME patients"
)
ggsave("ad-sf36/me-overall-qol.png")

ggscatterstats(
  data = me.info,
  x = ad.sum,
  y = broad.mentsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Mental QoL score per number of ADs in ME patients"
)
ggsave("ad-sf36/me-mental-qol.png")

ggscatterstats(
  data = me.info,
  x = ad.sum,
  y = broad.physsum,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "QoL score (0 = poor, 100 = excellent)",
  title = "Physical QoL score per number of ADs in ME patients"
)
ggsave("ad-sf36/me-physical-qol.png")

### compare QoL betweeen illnesses by how many ADs

df.coeliac <- df.coeliac %>%
  mutate(disorder = "coeliac")

df.hashi <- df.hashi %>%
  mutate(disorder = "hashimotos")

df.lupus <- df.lupus %>%
  mutate(disorder = "SLE")

df.rheum <- df.rheum %>%
  mutate(disorder = "RA")

df.sjog <- df.sjog %>%
  mutate(disorder = "sjoegrens")

me.info <- me.info %>%
  mutate(disorder = "ME") 

top5.me <- bind_rows(list(df.coeliac, df.hashi, df.lupus, df.rheum, df.sjog, me.info))
saveRDS(top5.me, "sf36-top5-me.rds")

grouped_ggbetweenstats(
  data = top5.me %>% dplyr::filter(ad.sum < 5),
  x = disorder,
  y = broad.qolall,
  grouping.var = ad.sum,
  annotation.args = list(title = "Differences in overall quality of life between illnesses for different number of overall disorders")
)
ggsave("ad-sf36/top5-me-overall-qol.png")

grouped_ggbetweenstats(
  data = top5.me %>% dplyr::filter(ad.sum < 5),
  x = disorder,
  y = broad.physsum,
  grouping.var = ad.sum,
  annotation.args = list(title = "Differences in physical health QoL between illnesses for different number of overall disorders")
)
ggsave("ad-sf36/top5-me-physical-qol.png")

grouped_ggbetweenstats(
  data = top5.me %>% dplyr::filter(ad.sum < 5),
  x = disorder,
  y = broad.mentsum,
  grouping.var = ad.sum,
  annotation.args = list(title = "Differences in mental health QoL between illnesses for different number of overall disorders")
)
ggsave("ad-sf36/top5-me-mental-qol.png")

#scatterplot top 5 vs. number of ADs
#function for plots and saving
# qol.plot <- function(domain, title){
#   x <- ggplot(df, aes(x=ad.sum, y=domain)) + #, color=gender.id)) + #, y=broad.qolall, color=demog_x1..biological.sex)) +
#     geom_jitter()+
#     geom_smooth(method=lm) +#, aes(fill=gender))+ #, aes(fill=demog_x1..biological.sex) #se=FALSE) +#, fullrange=TRUE) +
#     scale_color_viridis_d(na.value = "grey")+
#     scale_x_continuous(breaks = seq(0, 15, by = 1))+
#     # scale_y_continuous(breaks = seq(0, 100, by = 10)) +
#     theme_bw() +
#     labs(title = title,
#          y = "QoL score",
#          x = "Number of conditions") +
#   #fill = NULL,
#   #color = "Sex") +
#   theme(text = element_text(family = "Helvetica-Narrow", face = "bold")) +
#     theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
#           plot.title=element_text(size=15),
#           axis.title=element_text(size=15),
#           axis.text=element_text(size=10),
#           legend.text = element_text(size=10),
#           legend.title = element_text(size=15))
#   print(x)
# }
# domain.types <- c("broad.qolall", "broad.physsum", "broad.mentsum")
# for (dom in domain.types) {
# #  print(dom)
#   print(qol.plot(ad.info, dom))
#   ggsave(paste0("images/sans-ME/", df, "-", dom, ".png"))
# }

# qol.plot <- function(domain){
#      %>% ggplot(aes(x=ad.sum, y=domain)) + #, color=gender.id)) + #, y=broad.qolall, color=demog_x1..biological.sex)) +
#     geom_jitter()+ 
#     geom_smooth(method=lm) +#, aes(fill=gender))+ #, aes(fill=demog_x1..biological.sex) #se=FALSE) +#, fullrange=TRUE) +
#     scale_color_viridis_d(na.value = "grey")+
#     scale_x_continuous(breaks = seq(0, 15, by = 1))+
#     # scale_y_continuous(breaks = seq(0, 100, by = 10)) +
#     theme_bw() +
#     labs(title = domain,
#          y = "QoL score", 
#          x = "Number of conditions") +
#     #fill = NULL,
#     #color = "Sex") +
#     theme(text = element_text(family = "Helvetica-Narrow", face = "bold")) +
#     theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
#           plot.title=element_text(size=15),
#           axis.title=element_text(size=15),
#           axis.text=element_text(size=10),
#           legend.text = element_text(size=10),
#           legend.title = element_text(size=15)) 
# }
# domain.types <- c("broad.qolall", "broad.physsum", "broad.mentsum")
# for (dom in domain.types) {
#   sjog.test %>% qol.plot(dom)
#   # print(qol.plot(ad.info, dom))
#   # ggsave(paste0("images/sans-ME/", df, "-", dom, ".png"))
# }

#ad.info

ad.info %>%
  filter(ad.sum != 0) %>%
  ggplot(aes(x=ad.sum, y=broad.qolall)) + #, color=gender.id)) + #, y=broad.qolall, color=demog_x1..biological.sex)) +
  geom_jitter()+ 
  geom_smooth(method=lm) +#, aes(fill=gender))+ #, aes(fill=demog_x1..biological.sex) #se=FALSE) +#, fullrange=TRUE) +
  scale_color_viridis_d(na.value = "grey")+
  scale_x_continuous(breaks = seq(0, 15, by = 1))+
  # scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  labs(title = "All respondents with ADs",
       y = "SF36 QoL score", 
       x = "Number of conditions") +
#fill = NULL,
#color = "Sex") +
theme(text = element_text(family = "Helvetica-Narrow", face = "bold")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
        plot.title=element_text(size=15),
        axis.title=element_text(size=15),
        axis.text=element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=15)) 
ad.info %>%
  filter(ad.sum != 0) %>%
  ggplot(aes(x=ad.sum, y=broad.physsum)) + #, color=gender.id)) + #, y=broad.qolall, color=demog_x1..biological.sex)) +
  geom_jitter()+ 
  geom_smooth(method=lm) +#, aes(fill=gender))+ #, aes(fill=demog_x1..biological.sex) #se=FALSE) +#, fullrange=TRUE) +
  scale_color_viridis_d(na.value = "grey")+
  scale_x_continuous(breaks = seq(0, 15, by = 1))+
  # scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  labs(title = "All respondents with ADs",
       y = "SF36 QoL score", 
       x = "Number of conditions")
#fill = NULL,
#color = "Sex") +
theme(text = element_text(family = "Helvetica-Narrow", face = "bold")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
        plot.title=element_text(size=15),
        axis.title=element_text(size=15),
        axis.text=element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=15)) 
ad.info %>%
  filter(ad.sum != 0) %>%
  ggplot(aes(x=ad.sum, y=broad.mentsum)) + #, color=gender.id)) + #, y=broad.qolall, color=demog_x1..biological.sex)) +
  geom_jitter()+ 
  geom_smooth(method=lm) +#, aes(fill=gender))+ #, aes(fill=demog_x1..biological.sex) #se=FALSE) +#, fullrange=TRUE) +
  scale_color_viridis_d(na.value = "grey")+
  scale_x_continuous(breaks = seq(0, 15, by = 1))+
  # scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  labs(title = "All respondents with ADs",
       y = "SF36 QoL score", 
       x = "Number of conditions")
#fill = NULL,
#color = "Sex") +
theme(text = element_text(family = "Helvetica-Narrow", face = "bold")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
        plot.title=element_text(size=15),
        axis.title=element_text(size=15),
        axis.text=element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=15)) 

df.coeliac %>%
  ggplot(aes(x=ad.sum, y=broad.qolall)) + #, color=gender.id)) + #, y=broad.qolall, color=demog_x1..biological.sex)) +
  geom_jitter()+ 
  geom_smooth(method=lm) +#, aes(fill=gender))+ #, aes(fill=demog_x1..biological.sex) #se=FALSE) +#, fullrange=TRUE) +
  scale_color_viridis_d(na.value = "grey")+
  scale_x_continuous(breaks = seq(0, 15, by = 1))+
  # scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  theme_bw() +
  labs(title = "Coeliac respondents",
       y = "SF36 QoL score", 
       x = "Number of conditions")
#fill = NULL,
#color = "Sex") +
theme(text = element_text(family = "Helvetica-Narrow", face = "bold")) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.5),
        plot.title=element_text(size=15),
        axis.title=element_text(size=15),
        axis.text=element_text(size=10),
        legend.text = element_text(size=10),
        legend.title = element_text(size=15)) 

# #upset plot of co-occurence of ADs
# ad.10 <- ad.short %>%
#   select(contains("autoimmune_id")) %>%
#   rbind(., colSums(.)) %>%
#   select(1080)
# test <- ad.short %>%
#   select(contains("autoimmune_id"))# %>%
#   label_to_colnames(.) #%>%
# test2 <- which(colSums(test) > 60)
#   # .[ , (colSums > 9)]
#   #select(where( ~ colSums(.) > 9))
#   
#   # label_to_colnames(.) %>%
#   subset(vars(.col), any_vars(colSums > 9))
#   subset(colSums(. > 9))
#   
#   summarise_all(sum) %>%
#   subset(. > 9)
# ads <- ad.short %>%
#   select(contains("autoimmune_id")) %>%
#   names()
# # comorbs <- ad.short %>%
# #   select(infection.none,general.viral:general.unknown) %>%
# #   names()
# upset(ad.short, ads, name = "Autoimmune disorders")
