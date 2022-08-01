#load helper file
source("0_pachages-function.R")

#load data
df.sumstats <- readRDS("AD-sumstats.rds")
df.ad <- readRDS("df-ad.rds")

# data cleaning step removed decimal points, so go back to raw data for neuroqol extraction
# script 1_data-clean.R comment out line 25: mutate_all(~str_replace_all(., "\\.", "")) %>%
#re-run script, extract neuroqol scores, then uncomment line 25

df.neuroqol <- data.clean %>%
  select(record_id, contains("neuroqol")) %>%
  filter(neuroqol_bank_v10_fatigue_complete.factor != "Incomplete") #%>%
saveRDS(df.neuroqol, "df-neuroqol-all.rds")

df.nqol <- df.neuroqol %>%
  select(record_id,
         neuroqol_bank_v10_fatigueol_tscore,
         neuroqol_bank_v10_fatigueol_std_error) %>%
  mutate(across(neuroqol_bank_v10_fatigueol_tscore:neuroqol_bank_v10_fatigueol_std_error, .fns = as.numeric))
saveRDS(df.nqol, "df-neuroqol.rds")

df.nqol <- df.nqol %>%
  left_join(sf36.domain, by = "record_id") %>%
  left_join(df.sumstats, by = "record_id") %>%
  left_join((df.ad %>% select(-c(ad.sum, cohort.id))), by = "record_id")

ggbetweenstats(
  data = df.nqol,
  x = cohort.id,
  y = neuroqol_bank_v10_fatigueol_tscore,
  #ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Chronic vs. control cohort",
  ylab = "Neuro-QoL score (0 = excellent, 100 = poor)",
  title = "Neuro-QoL score between chronic and control cohorts"
)

ggscatterstats(
  data = df.nqol,
  x = ad.sum,
  y = neuroqol_bank_v10_fatigueol_tscore,
  ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(1, 14, 1)), ggplot2::scale_y_continuous(breaks = seq(0, 100, 25), limits = (c(0, 100)))),
  xlab = "Number of ADs",
  ylab = "Neuro-QoL score (0 = excellent, 100 = poor)",
  title = "Neuro-QoL score per number of ADs in all patients"
)

nqol.t10 <- top10.ad %>%
  left_join((df.nqol %>% select(record_id, neuroqol_bank_v10_fatigueol_tscore, neuroqol_bank_v10_fatigueol_std_error)), by = "record_id")

ggbetweenstats(
  data = nqol.t10,
  x = disorder,
  y = neuroqol_bank_v10_fatigueol_tscore,
  annotation.args = list(title = "Differences in Neuro-QoL fatigue score between illnesses"),
  ggplot.component =
    ## modify further with `{ggplot2}` functions
    list(
      scale_color_manual(values = paletteer::paletteer_c("viridis::turbo", 10)),
      theme(axis.text.x = element_text(angle = 90))
    )
)
##############
# removing any double dipping between 10 most common ADs 
##############

nqol.t10.unique <- nqol.t10 %>%
  group_by(record_id) %>%
  filter(n()<2) %>%
  ungroup()

ggbetweenstats(
  data = nqol.t10.unique,
  x = disorder,
  y = neuroqol_bank_v10_fatigueol_tscore,
  type = "robust",
  xlab = "Disorder", ## label for the x-axis
  ylab = "QoL score (0 = excellent, 100 = poor)", ## label for the y-axis
  annotation.args = list(title = "Differences in Neuro-QoL fatigue score between illnesses"),
  ggplot.component =
    ## modify further with `{ggplot2}` functions
    list(
      scale_color_manual(values = paletteer::paletteer_c("viridis::turbo", 10)),
      theme(axis.text.x = element_text(angle = 90))
    )
)  + ## modifying the plot further
  ggplot2::scale_y_continuous(
    limits = c(0, 140),
    breaks = seq(from = 0, to = 100, by = 25)
  )
#ggsave("images/nqol-top10AD-unique-entries.png")

grouped_ggbetweenstats(
  data = nqol.t10.unique %>% dplyr::filter(ad.sum < 4), # %>% dplyr::filter(disorder != "SLE") %>% dplyr::filter(disorder != "hsd")  %>% dplyr::filter(disorder != "pots"),
  x = disorder,
  y = neuroqol_bank_v10_fatigueol_tscore,
  grouping.var = ad.sum,
  type = "robust",
  xlab = "Disorder", ## label for the x-axis
  ylab = "QoL score (0 = excellent, 100 = poor)", ## label for the y-axis
  #title = "Comparison of QoL between 10 most reported illnesses, by number of overall illnesses",
  annotation.args = list(title = "Differences in Neuro-QoL fatigue score between illnesses per number of illnesses"),
  ggplot.component =
    ## modify further with `{ggplot2}` functions
    list(
      scale_color_manual(values = paletteer::paletteer_c("viridis::turbo", 10)),
      theme(axis.text.x = element_text(angle = 90))
    )
)  + ## modifying the plot further /// doesn't work for all faceted plots
  ggplot2::scale_y_continuous(
    limits = c(0, 140),
    breaks = seq(from = 0, to = 100, by = 25)
  )
#ggsave("images/nqol-top10AD-unique-entries-faceted-numADs.png")

#SF36 w/o double dipping
ggbetweenstats(
  data = nqol.t10.unique,
  x = disorder,
  y = broad.mentsum,
  type = "robust",
  xlab = "Disorder", ## label for the x-axis
  ylab = "QoL score (0 = poor, 100 = excellent)", ## label for the y-axis
  title = "Mental QoL score for 10 most reported illnesses",
  annotation.args = list(title = "Differences in RAND36 score between illnesses"),
  outlier.tagging = TRUE, ## whether outliers should be flagged
  outlier.coef = 1.5, ## coefficient for Tukey's rule
  outlier.label = employment.id,
  ggplot.component =
    ## modify further with `{ggplot2}` functions
    list(
      scale_color_manual(values = paletteer::paletteer_c("viridis::turbo", 10)),
      theme(axis.text.x = element_text(angle = 90))
    )
) + ## modifying the plot further
  ggplot2::scale_y_continuous(
    limits = c(0, 140),
    breaks = seq(from = 0, to = 100, by = 25)
  )
#ggsave("images/sf36-mental-top10AD-unique-entries.png")

ggbetweenstats(
  data = nqol.t10.unique,
  x = disorder,
  y = broad.physsum,
  type = "robust",
  xlab = "Disorder", ## label for the x-axis
  ylab = "QoL score (0 = poor, 100 = excellent)", ## label for the y-axis
  title = "Physical QoL score for 10 most reported illnesses",
  annotation.args = list(title = "Differences in RAND36 score between illnesses"),
  outlier.tagging = TRUE, ## whether outliers should be flagged
  outlier.coef = 1.5, ## coefficient for Tukey's rule
  outlier.label = employment.id,
  ggplot.component =
    ## modify further with `{ggplot2}` functions
    list(
      scale_color_manual(values = paletteer::paletteer_c("viridis::turbo", 10)),
      theme(axis.text.x = element_text(angle = 90))
    )
) + ## modifying the plot further
  ggplot2::scale_y_continuous(
    limits = c(0, 140),
    breaks = seq(from = 0, to = 100, by = 25)
  )


#################
# single vs. multi illnesses
#################

#### singles with 20 or more entries vs. 2, 3 etc ADs
 test <- df.nqol %>%
  mutate(illness.grouped = case_when(
    ad.sum == 0 ~ "control",
    ad.sum == 1 & autoimmune_id___6 == 1 ~ "coeliac",
    ad.sum == 1 & autoimmune_id___55 == 1 ~ "ME",
    ad.sum == 1 & autoimmune_id___17 == 1 ~ "hashimotos",
    ad.sum == 1 & autoimmune_id___35 == 1 ~ "RA",
    ad.sum == 1 & autoimmune_id___26 == 1 ~ "MS",
    ad.sum == 1 & autoimmune_id___34 == 1 ~ "PsA",
    ad.sum == 1 & autoimmune_id___1 == 1 ~ "AS",
    ad.sum == 1 ~ "1 other illnesses",
    ad.sum == 2 ~ "2 illnesses",
    ad.sum == 3 ~ "3 illnesses",
    ad.sum == 4 ~ "4 illnesses",
    ad.sum > 4 ~ "5+ illnesses",
    TRUE ~ NA_character_
  ))

test %>%
  tabyl(illness.grouped) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "rear") %>%
  adorn_title(
    row_name = "Illness group"
  )

#### singles vs. blanket multi ADs

#   select(-contains(".factor"), -contains("_tscore"), -contains ("_stderror"), -contains("qposition"), -contains("complete"), -c(neuroqol_bank_v10_fatigue_timestamp,neuroqol_bank_v10_fatigueol_std_error )) %>%
#   mutate(across(neuroqol_nqftg13:neuroqol_nqftg20, .fns = as.numeric)) %>%
#   rowwise() %>%
#   mutate(neuro.sum = sum(!is.na(c_across(where(is.numeric)))))
#   mutate(neuro.sum = rowSums(select(., -record_id), na.rm = TRUE))
#  # filter(!if_all(fatigue:bruise, is.na)) %>%
# 
# df.neuroqol.short %>%
#   tabyl(neuro.sum) %>%
#   adorn_totals(where = "row") %>%
#   adorn_percentages(denominator = "col") %>%
#   adorn_pct_formatting() %>%
#   adorn_ns(position = "rear") %>%
#   adorn_title(
#     row_name = "# Qs answered"
#   )
# # problem: 95% of answers are around 4 questions out of 19
# query <- df.neuroqol.short %>% filter(neuro.sum == 4)
