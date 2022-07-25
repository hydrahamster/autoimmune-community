#load helper file
source("0_pachages-function.R")

#load data
data.clean <- readRDS("AD-data-clean.rds")
df.sumstats <- readRDS("AD-sumstats.rds")

df.ad <- readRDS("df-ad.rds")

df.neuroqol.short <- data.clean %>%
  select(record_id, contains("neuroqol")) %>%
  filter(neuroqol_bank_v10_fatigue_complete.factor != "Incomplete") %>%
  select(-contains(".factor"), -contains("_tscore"), -contains ("_stderror"), -contains("qposition"), -contains("complete"), -c(neuroqol_bank_v10_fatigue_timestamp,neuroqol_bank_v10_fatigueol_std_error )) %>%
  mutate(across(neuroqol_nqftg13:neuroqol_nqftg20, .fns = as.numeric)) %>%
  rowwise() %>%
  mutate(neuro.sum = sum(!is.na(c_across(where(is.numeric)))))
  mutate(neuro.sum = rowSums(select(., -record_id), na.rm = TRUE))
 # filter(!if_all(fatigue:bruise, is.na)) %>%

df.neuroqol.short %>%
  tabyl(neuro.sum) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "rear") %>%
  adorn_title(
    row_name = "# Qs answered"
  )
# problem: 95% of answers are around 4 questions out of 19
query <- df.neuroqol.short %>% filter(neuro.sum == 4)
