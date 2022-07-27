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
      scale_color_manual(values = paletteer::paletteer_c("viridis::turbo", 7)),
      theme(axis.text.x = element_text(angle = 90))
    )
)

grouped_ggbetweenstats(
  data = nqol.t10 %>% dplyr::filter(ad.sum < 4) %>% dplyr::filter(disorder != "SLE") %>% dplyr::filter(disorder != "hsd")  %>% dplyr::filter(disorder != "pots"),
  x = disorder,
  y = neuroqol_bank_v10_fatigueol_tscore,
  grouping.var = ad.sum,
  annotation.args = list(title = "Differences in Neuro-QoL fatigue score between illnesses per number of illnesses"),
  ggplot.component =
    ## modify further with `{ggplot2}` functions
    list(
      scale_color_manual(values = paletteer::paletteer_c("viridis::turbo", 7)),
      theme(axis.text.x = element_text(angle = 90)),
      ylim(20, 100) # set y-axis
    )
)

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
