#load helper file
source("0_pachages-function.R")

#load data
df.ad <- readRDS("df-ad.rds")
data.clean <- readRDS("AD-data-clean.rds")
df.sumstats <- readRDS("AD-sumstats.rds")

### count completion per section
query.completion <- data.clean %>%
  select(contains(".factor") & contains("_complete"))
query.completion %>%
  tabyl(demographics_complete.factor) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "rear") %>%
  adorn_title(
    row_name = "Completion"
  )
query.completion %>%
  tabyl(disease_and_diagnosis_complete.factor) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "rear") %>%
  adorn_title(
    row_name = "Completion"
  )
query.completion %>%
  tabyl(symptom_management_complete.factor) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "rear") %>%
  adorn_title(
    row_name = "Completion"
  )
query.completion %>%
  tabyl(mental_health_complete.factor) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "rear") %>%
  adorn_title(
    row_name = "Completion"
  )
query.completion %>%
  tabyl(neuroqol_bank_v10_fatigue_complete.factor) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "rear") %>%
  adorn_title(
    row_name = "Completion"
  )
query.completion %>%
  tabyl(alcohol_dependence_scale_complete.factor) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "rear") %>%
  adorn_title(
    row_name = "Completion"
  )
query.completion %>%
  tabyl(rand_36_item_sf_health_survey_instrument_version_1_complete.factor) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "rear") %>%
  adorn_title(
    row_name = "Completion"
  )

### Responses by state pie chart
state.count <- df.sumstats %>%
  count(state) #%>% 

#png("state-summary.png")
ggplot(state.count, aes(x = "", y = n, fill = state)) +
  geom_col(color = "black") +
  geom_label(aes(label = NA),
             colour = "white",
             size = 10,
             position = position_stack(vjust = 0.5),
             show.legend = F) +
  coord_polar(theta = "y") +
  labs(title = "Responses by state") +
  guides(fill = guide_legend(title = "State")) +
  scale_fill_viridis_d() + #discrete palette, _c for continuous
  theme_void() +
  theme(text = element_text(family = "Helvetica-Narrow", face = "bold")) +
theme(plot.title=element_text(size=20, hjust = 0.5),
      #axis.title=element_text(size=15),
      #axis.text=element_text(size=15),
      legend.text = element_text(size=15),
      legend.title = element_text(size=15))

#alternative method
ggpiestats(
  data         = df.sumstats,
  x            = state,
  #y            = gender.id,
  package      = "RColorBrewer",
  palette = "Spectral",
  title        = "Responses by state",
  legend.title = "State"
)
ggsave("images/sumstats/AD-state-responses-pie.png")

ggplot(df.sumstats, aes(x = age, fill = gender.id, color = gender.id)) +
  geom_histogram(binwidth = 5,
                 alpha = 0.5) +
  scale_x_continuous(breaks = seq(20, 90, by = 10)) +
  scale_y_continuous(breaks = seq(0, 200, by = 10)) +
  theme_bw() +
  labs(title = "Respondents by age and gender",
       y = NULL, 
       x = "Age of respondents",
       fill = "Gender",
       color = NULL) +
  guides(color = "none") +
  theme(text = element_text(family = "Helvetica-Narrow", face = "bold")) +
theme(plot.title=element_text(size=20),
      axis.title=element_text(size=10),
      axis.text=element_text(size=10),
      legend.text = element_text(size=10),
      legend.title = element_text(size=15))
ggsave("images/AD-gender-age-hist.png")

df.sumstats %>%
  tabyl(gender.id, cohort.id) %>%
  adorn_totals(where = "row") %>%
  adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting() %>%
  adorn_ns(position = "rear") %>%
  adorn_title(
    row_name = "Sex"
  )

### Beeswarm of age, gender and cohort
# ggplot(data = df.sumstats, mapping = aes(cohort.id, age, color = gender.id)) +
#   geom_quasirandom(method = 'pseudorandom') +
#   theme_bw() +
#   labs(title = "Distribution of responses by age and disease status",
#        x = "Chronically ill vs. control cohort",
#        y = "Age of respondents",
#        color = "Gender") +
#   scale_y_continuous(breaks = seq(0, 100, by = 10)) +
#   #scale_color_manual(values=c("#cf3232", "#221d1d", "#c3b7b7")) +
#   theme(text = element_text(family = "Helvetica-Narrow", face = "bold")) +
#   scale_colour_viridis_d(option = "turbo",
#                          begin = 0.1 ,
#                          direction = -1,
#                          na.value = "black")
# ggsave("images/AD-gender-age-cohort-bees.png")

ggbetweenstats(
  ## arguments relevant for ggscatterstats
  data = df.sumstats,
  x = cohort.id,
  y = age,
  ylab = "Age of respondents",
  xlab = "Chronically ill vs. control cohort",
  #label.expression = rating < 5 & budget > 80,
  # type = "r",
   # ggtheme = ggthemes::theme_calc(),
  ## arguments relevant for combine_plots
) +
  ggplot2::scale_color_viridis_d(end = 0.7)
ggsave("images/AD-cohort-age-violin.png")

grouped_ggbetweenstats(
  ## arguments relevant for ggscatterstats
  data = df.sumstats,
  x = cohort.id,
  y = age,
  grouping.var = gender.group,
  ylab = "Age of respondents",
  xlab = "Chronically ill vs. control cohort",
  #package = "viridis",
  #palette = "turbo",
  #label.expression = rating < 5 & budget > 80,
  # type = "r",
  # ggtheme = ggthemes::theme_calc(),
  ## arguments relevant for combine_plots
) #+
   #ggplot2::scale_color_manual(values = paletteer::paletteer_c("viridis::mako", n = 10))
  # ggplot2::scale_color_viridis_d(option = "turbo")
ggsave("images/AD-cohort-age-gender-violin.png")

### misdignosis pie
ggpiestats(
  data         = df.sumstats,
  x            = misdiag.id,
  #y            = gender.id,
  package      = "RColorBrewer",
  palette = "Accent",
  title        = "Rate of misdiagnosis",
  legend.title = "Have you been misdiagnosed at some stage"
)
ggsave("images/misdiag-pie.png")

### misdiagnosis len dx bar
ggplot((df.sumstats %>% filter(cohort.id != "control") %>% filter(!is.na(dx.group))), aes(dx.group, fill = misdiag.id)) +
  geom_bar(stat = "count", position = position_dodge2(width = 0.9, preserve = "single")) +
  labs(title = "Length of time to receive a diagnosis",
       x = NULL, 
       y = NULL,
       fill = "Misdiagnosed") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d(option = "mako",
                       na.value = "grey") +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/mdx-length-dx-bar.png")

#mdx by length and 3ads, doesn't quite work
grouped_ggbetweenstats(
  data = (df.sumstats %>% filter(cohort.id != "control")),
  x = dia.length, #misdiag.id,
  y =  ad.sum,
  grouping.var = misdiag.id ,
  pairwise.comparisons = TRUE,
  ylab = "Number of chronic illnesses",
  xlab = "Misdiagnosis received",
) +
  ggplot2::scale_color_viridis_d(end = 0.7)

### ethnicity
ggpiestats(
  data         = df.sumstats,
  x            = ethnicity.id,
  #y            = gender.id,
  package      = "RColorBrewer",
  palette = "Spectral",
  title        = "Ethnicity",
  legend.title = "Ethnicity respondent identifies with"
)
#ggsave("images/ethnicity-pie.png")

### Income
ggpiestats(
  data         = df.sumstats,
  x            = employment.id,
  #y            = gender.id,
  package      = "RColorBrewer",
  palette = "Spectral",
  title        = "Type of employment",
  legend.title = "Type of employment of respondents"
)
#ggsave("images/employment-pie.png")

### Education level
ggpiestats(
  data         = df.sumstats,
  x            = education.id,
  #y            = gender.id,
  package      = "RColorBrewer",
  palette = "Spectral",
  title        = "Type of education",
  legend.title = "Highest level of educatin of respondents"
)
#ggsave("images/education-pie.png")


### Relationship status
ggpiestats(
  data         = df.sumstats,
  x            = relationship.id,
  #y            = gender.id,
  package      = "RColorBrewer",
  palette = "Spectral",
  title        = "Type of relationship",
  legend.title = "Relationship status of respondents"
)
#ggsave("images/relationship-pie.png")

# ADs reported
ad.cloud <- df.ad %>%
  select(contains("autoimmune_id__"), -c(autoimmune_id___44)) #rm none & other 
ad.labels <- ad.cloud %>%
  get_label 
ad.labels <- gsub("\\(.*", "", ad.labels) %>% #TODO this removes the brackets in the AD labels
  str_trim
ad.cloud <- ad.cloud %>%
  mutate_if(is.character, as.numeric) #for some reason initial ADs were character columns, so convert back
colnames(ad.cloud) <- ad.labels
ad.count <- ad.cloud %>% 
  summarise_if(is.numeric, sum) %>% 
  transpose()
ad.count.df <- as.data.frame(do.call(cbind, ad.count)) %>% 
  rownames_to_column(.) %>% 
  rename(., word = rowname, freq = V1)
ad.count.df$freq <- as.numeric(as.character(ad.count.df$freq))


wordcloud2(ad.count.df, shape = 'diamond', color = "random-light", backgroundColor = "grey", size = .4, widgetsize =c("1000","1000"))
#save alternatively, ggsave doesn't recognise the wordcloud as a plot
# AD barplot (use ad.count.df from wordcloud )

ad.count.ord <- ad.count.df %>%
  filter(freq > 9)%>%
  rename(., Autoimmune_disorder = word) %>%
  arrange(., desc(freq))

ggplot(ad.count.ord, aes(x = freq, y = Autoimmune_disorder)) +
  geom_col(fill = viridis(43)) +
  geom_text(aes(label = freq, color = "white", face="bold"), size = 6, hjust = 0, nudge_x = -5, show.legend = FALSE) +
  scale_y_discrete(limits=rev(ad.count.ord$Autoimmune_disorder)) +
  labs(title = NULL,
       x = NULL, 
       y = NULL,
       label = NULL) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 300, by = 20)) +
  theme_bw() +
  theme(axis.text=element_text(size=18, face="bold"))
#save through export due to sizing issue

# co-morbs
##wordcloud comorbidities
como.df <- df.ad %>%
  select(contains("chronic_"), -contains("factor"))  #record_id, 
como.labels <- como.df %>%
  get_label 
# como.df <- como.df %>%
#  mutate_if(is.character, as.numeric) #for some reason initial ADs were character columns, so convert back
colnames(como.df) <- como.labels
como.count <- como.df %>% 
  mutate_if(is.character, as.numeric) %>%
  #  select(como.df, -contains("ecord")) %>%
  summarise_if(is.numeric, sum) %>% 
  transpose()
como.count.df <- as.data.frame(do.call(cbind, como.count)) %>% 
  rownames_to_column(.) %>% 
  rename(., word = rowname, freq = V1)
como.count.df$freq <- as.numeric(as.character(como.count.df$freq))

wordcloud2(como.count.df, shape = 'diamond', color = "random-dark", backgroundColor = "white", size = .4)
#not informative
como.count.ord <- como.count.df %>%
  #filter(freq > 9)%>%
  rename(., Comorbidity = word) %>%
  arrange(., desc(freq))

ggplot(como.count.ord, aes(x = freq, y = Comorbidity)) +
  geom_col(fill = viridis(24)) +
  geom_text(aes(label = freq, color = "white", face="bold"), size = 6, hjust = 0, nudge_x = -9, show.legend = FALSE) +
  scale_y_discrete(limits=rev(como.count.ord$Comorbidity)) +
  labs(title = NULL,
       x = NULL, 
       y = NULL,
       label = NULL) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 500, by = 20)) +
  theme_bw() +
  theme(axis.text=element_text(size=18, face="bold"))

### Controls with co-morbs?
control.comorb <- df.sumstats %>%
  filter(cohort.id == "control") %>%
  left_join((df.ad %>% select(record_id, contains("chronic_"))), by="record_id")

conco.df <- control.comorb %>%
  select(contains("chronic_"))
conco.labels <- conco.df %>%
   get_label 
# conco.df <- conco.df %>%
#   mutate_if(is.character, as.numeric) #for some reason initial ADs were character columns, so convert back
colnames(conco.df) <- conco.labels
conco.count <- conco.df %>% 
  summarise_if(is.numeric, sum) %>% 
  transpose()

conco.count.df <- as.data.frame(do.call(cbind, conco.count)) %>% 
  rownames_to_column(.) %>% 
  rename(., word = rowname, freq = V1)
conco.count.df$freq <- as.numeric(as.character(conco.count.df$freq))
conco.count.ord <- conco.count.df %>%
  rename(., Comorbidity = word) %>%
  filter(freq > 1) %>%
  arrange(., desc(freq))

ggplot(conco.count.ord, aes(x = freq, y = Comorbidity)) +
  geom_col(fill = viridis(9)) +
  geom_text(aes(label = freq, color = "white", face="bold"), size = 6, hjust = 0, nudge_x = -2, show.legend = FALSE) +
  scale_y_discrete(limits=rev(conco.count.ord$Comorbidity)) +
  labs(title = NULL,
       x = NULL, 
       y = NULL,
       label = NULL) +
  scale_x_continuous(expand = c(0, 0),
                     breaks = seq(0, 500, by = 20)) +
  theme_bw() +
  theme(axis.text=element_text(size=18, face="bold"))
ggsave("images/Control-comorb-bar.png")


############
### Looking at top 10 ADs and interconnectivity through upset plot
############

# choose top 10 represented illnesses and look at interconnextivity
#TODO where the fuck did AD short come from
top.10 <- df.ad %>%
  select(contains("autoimmune_id"))# %>%
# label_to_colnames(.) #use to see labels instead of column header
top.10 <- which(colSums(top.10) > 60)

ad.10 <- df.ad %>%
  select(contains("autoimmune_id"))%>%
  .[ ,top.10] %>%
  labels()
ad.upset <- df.ad %>%
  rename("Coeliac disease" = "autoimmune_id___6") %>%
  rename("Hashimoto's disease" = "autoimmune_id___17") %>%
  rename("Systemic lupus erythematosus" = "autoimmune_id___22") %>%
  rename("Psoriatic arthritis" = "autoimmune_id___34") %>%
  rename("Rheumatoid arthritis" = "autoimmune_id___35") %>%
  rename("Sjögren's syndrome" = "autoimmune_id___38") %>%
  rename("ME/CFS" = "autoimmune_id___55") %>%
  rename("Fibromyalgia" = "autoimmune_id___64") %>%
  rename("Postural orthostatic tachycardia syndrome" = "autoimmune_id___116") %>%
  rename("Hypermobility disorders" = "autoimmune_id___172") %>%
  left_join(df.sumstats, by = "record_id") %>%
  mutate(misdx.written = case_when(
    misdiag.id == "Yes" ~ "Misdiagnosed",
    misdiag.id == "No" ~ "Not misdiagnosed",
    is.na(misdiag.id) ~ NA_character_
  ))
upset.10 <- c("Coeliac disease", "Hashimoto's disease", "Systemic lupus erythematosus", "Psoriatic arthritis", "Rheumatoid arthritis", "Sjögren's syndrome", "ME/CFS", "Fibromyalgia", "Postural orthostatic tachycardia syndrome", "Hypermobility disorders")
upset(ad.upset, 
      upset.10, 
      name = "Autoimmune disorders",
      min_degree=1,
      min_size=3,
      themes=upset_default_themes(text=element_text(size=20, face='bold')),
      set_sizes=(
        upset_set_size(
          geom=geom_bar(
            aes(fill=employment.id, x=group), #misdiag.id
            width=0.8
          )
        )+
          scale_fill_viridis_d(option = "mako",
                               na.value = "grey")
      ),
      base_annotations = list(
        'Intersection size' = intersection_size(
          #counts = F,
          mapping = aes(fill = misdx.written) #dia.length, gender.group, employment.id
        ) +
          scale_fill_viridis_d(na.value = "grey") +
          theme(legend.title=element_blank())
      ),
      guides= "over") #del comma before ")" shouldn't affect running though

#########chosen illnesses
final.upset <- df.ad %>%
  rename("Coeliac disease" = "autoimmune_id___6") %>%
  rename("Hashimoto's disease" = "autoimmune_id___17") %>%
  rename("Multiple sclerosis" = "autoimmune_id___26") %>%
  rename("Psoriatic arthritis" = "autoimmune_id___34") %>%
  rename("Rheumatoid arthritis" = "autoimmune_id___35") %>%
  rename("Ankylosing spondylitis" = "autoimmune_id___1") %>%
  rename("ME/CFS" = "autoimmune_id___55") %>%
  left_join(df.sumstats, by = "record_id") %>%
  mutate(misdx.written = case_when(
    misdiag.id == "Yes" ~ "Misdiagnosed",
    misdiag.id == "No" ~ "Not misdiagnosed",
    is.na(misdiag.id) ~ NA_character_
  ))
upset.final <- c("Coeliac disease", "Hashimoto's disease", "Multiple sclerosis", "Psoriatic arthritis", "Rheumatoid arthritis", "Ankylosing spondylitis", "ME/CFS")
upset(final.upset, 
      upset.final, 
      name = "Chronic illnesses",
      min_degree=1,
      min_size=3,
      themes=upset_default_themes(text=element_text(size=20, face='bold')),
      set_sizes=(
        upset_set_size(
          geom=geom_bar(
            aes(fill=employment.id, x=group), #misdiag.id
            width=0.8
          )
        )+
          scale_fill_viridis_d(option = "mako",
                               na.value = "grey")
      ),
      base_annotations = list(
        'Intersection size' = intersection_size(
          #counts = F,
          mapping = aes(fill = misdx.written) #dia.length, gender.group, employment.id
        ) +
          scale_fill_viridis_d(na.value = "grey") +
          theme(legend.title=element_blank())
      ),
      guides= "over") #del comma before ")" shouldn't affect running though

# correlation matrix of ADs and comorbs
 ad.cor <-df.ad %>% select(-record_id, -diseasestat)
# ggcorr( data = NULL, cor_matrix = cor(ad.cor, use = "everything"))
# ggcorrmat(
#   data     = df.ad %>% select(-record_id),
#   colors   = c("#B2182B", "white", "#4D4D4D"),
#   title    = "Correlalogram for disorders and comorbidities"
# )
cormat <- cor(ad.cor)
#TODO the problem here is that it isn't a co-occurence thing with just the ADs

##################
### Symptoms
#################

### Symptoms overview

#TODO most of the code is in 5_symptoms.R
#plot of symptoms all together by severity
sympt.long <- symptoms.factor %>%
  select(-c(record_id))
colnames(sympt.long)<-gsub(".factor","",colnames(sympt.long))
sympt.long<- sympt.long %>%
  pivot_longer(!cohort.id, names_to = "symptom", values_to = "severity") 

sympt.count <- sympt.long %>%
  count(cohort.id, symptom, severity)
ggplot(sympt.count, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip() +
  facet_wrap( ~ cohort.id, scales = "free") 
ggsave("images/Symptoms-summary.png")

### Individual symptoms
#fatigue
ggplot(symptoms.factor, aes(fatigue.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "Fatigue severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 450, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-fatigue.png")

#joint
ggplot(symptoms.factor, aes(joint.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "Joint pain and swelling severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 450, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-joint.png")

#skin
ggplot(symptoms.factor, aes(skin.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "Skin problems severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 450, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-skin.png")

#fever
ggplot(symptoms.factor, aes(fever.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "Recurring fever severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-fever.png")

#glands
ggplot(symptoms.factor, aes(glands.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "Swollen glands severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-glands.png")

#chills
ggplot(symptoms.factor, aes(chills.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "chills severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-chills.png")
#memory
ggplot(symptoms.factor, aes(memory.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "Memory loss severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-memory.png")
#conc
ggplot(symptoms.factor, aes(conc.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "Concentration loss severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-conc.png")
#hair
ggplot(symptoms.factor, aes(hair.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "Hair loss severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-hair.png")
#skin2
ggplot(symptoms.factor, aes(skin2.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "Peculiar skin sensation severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-skin2.png")
#weak
ggplot(symptoms.factor, aes(weak.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "General weakness severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-weak.png")
#pain
ggplot(symptoms.factor, aes(pain.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "Muscle aches and pain severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-pain.png")
#bruise
ggplot(symptoms.factor, aes(bruise.factor, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "Easy bruising severity",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
ggsave("images/Sympt-bar-bruise.png")

ggplot(df.sumstats, aes(dia.length, fill = cohort.id)) +
  geom_bar(stat = "count") +
  labs(title = "dx length",
       x = NULL, 
       y = NULL,
       fill = "Chronically Ill") +
  scale_y_continuous(expand = c(0, 0),
                     breaks = seq(0, 950, by = 50)) +
  theme_bw() +
  scale_fill_viridis_d() +
  theme(plot.title=element_text(size=20, hjust = 0.5),
        axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        legend.text = element_text(size=15),
        legend.title = element_text(size=15)) +
  coord_flip()
