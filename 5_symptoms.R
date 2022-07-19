#load helper file
source("0_pachages-function.R")

#load data
data.clean <- readRDS("AD-data-clean.rds")
df.ad <- readRDS("df-ad.rds")
df.sumstats <- readRDS("AD-sumstats.rds")
#TODO cohort.id doesn't live here
symptoms.factor <- data.clean %>%
  select(record_id,
         fatigue.factor:bruise.factor) %>%
  left_join((df.ad %>% select(record_id, cohort.id)), by="record_id")
symptoms.scale <- data.clean %>%
  select(record_id,
         fatigue:bruise) %>%
  left_join((df.ad %>% select(record_id, cohort.id)), by="record_id") %>%
  mutate(across(fatigue:bruise, .fns = as.numeric))

heattest <- symptoms.scale %>%
  select(-c(record_id, diseasestat)) %>%
  as.matrix()
heatmap(heattest) #doesn't work with NA

#plot of symptoms all together by severity
#TODO already copied over
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
### copied till here

#TODO some weird interaction going on down there with dx.id and cohort.id
### top 10 illnesses symptoms
#coeliac
id.coeliac <- df.ad %>%
  filter(autoimmune_id___6 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "coeliac") 
sympt.coeliac <- id.coeliac %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  select(-c(record_id))
colnames(sympt.coeliac)<-gsub(".factor","",colnames(sympt.coeliac))
sympt.coeliac<- sympt.coeliac %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.coeliac <- sympt.coeliac %>%
  count(dx.id, symptom, severity)
ggplot(count.coeliac, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Coeliac symptoms",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()  
ggsave("images/coeliac-symptoms.png")

### hashi
id.hashi <- df.ad %>%
  filter(autoimmune_id___17 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "hashimotos") 
sympt.hashi <- id.hashi %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  select(-c(record_id))
colnames(sympt.hashi)<-gsub(".factor","",colnames(sympt.hashi))
sympt.hashi<- sympt.hashi %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.hashi <- sympt.hashi %>%
  count(dx.id, symptom, severity)
ggplot(count.hashi, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Hashimoto's symptoms",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/hashi-symptoms.png")

### lupus
id.lupus <- df.ad %>%
  filter(autoimmune_id___22 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "lupus") 
sympt.lupus <- id.lupus %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  select(-c(record_id))
colnames(sympt.lupus)<-gsub(".factor","",colnames(sympt.lupus))
sympt.lupus<- sympt.lupus %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.lupus <- sympt.lupus %>%
  count(dx.id, symptom, severity)
ggplot(count.lupus, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Lupus symptoms",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/lupus-symptoms.png")

### me
id.me <- df.ad %>%
  filter(autoimmune_id___55 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "mecfs") 
sympt.me <- id.me %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  select(-c(record_id))
colnames(sympt.me)<-gsub(".factor","",colnames(sympt.me))
sympt.me<- sympt.me %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.me <- sympt.me %>%
  count(dx.id, symptom, severity)
ggplot(count.me, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "ME/CFS symptoms",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/mecfs-symptoms.png")

### ra
id.ra <- df.ad %>%
  filter(autoimmune_id___35 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "ra") 
sympt.ra <- id.ra %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  select(-c(record_id))
colnames(sympt.ra)<-gsub(".factor","",colnames(sympt.ra))
sympt.ra<- sympt.ra %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.ra <- sympt.ra %>%
  count(dx.id, symptom, severity)
ggplot(count.ra, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Rheumatoid arthritis symptoms",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/ra-symptoms.png")

### sjog
id.sjog <- df.ad %>%
  filter(autoimmune_id___38 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "sjog") 
sympt.sjog <- id.sjog %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  select(-c(record_id))
colnames(sympt.sjog)<-gsub(".factor","",colnames(sympt.sjog))
sympt.sjog<- sympt.sjog %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.sjog <- sympt.sjog %>%
  count(dx.id, symptom, severity)
ggplot(count.sjog, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Sjogren's symptoms",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/sjog-symptoms.png")


### PsA
id.psa <- df.ad %>%
  filter(autoimmune_id___34 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "psa") 
sympt.psa <- id.psa %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  select(-c(record_id))
colnames(sympt.psa)<-gsub(".factor","",colnames(sympt.psa))
sympt.psa<- sympt.psa %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.psa <- sympt.psa %>%
  count(dx.id, symptom, severity)
ggplot(count.psa, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Psoriatic arthritis symptoms",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/psa-symptoms.png")

### fibro
id.fibro <- df.ad %>%
  filter(autoimmune_id___64 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "fibro") 
sympt.fibro <- id.fibro %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  select(-c(record_id))
colnames(sympt.fibro)<-gsub(".factor","",colnames(sympt.fibro))
sympt.fibro<- sympt.fibro %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.fibro <- sympt.fibro %>%
  count(dx.id, symptom, severity)
ggplot(count.fibro, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Fibromyalgia symptoms",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/fibro-symptoms.png")

### pots
id.pots <- df.ad %>%
  filter(autoimmune_id___116 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "pots") 
sympt.pots <- id.pots %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  select(-c(record_id))
colnames(sympt.pots)<-gsub(".factor","",colnames(sympt.pots))
sympt.pots<- sympt.pots %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.pots <- sympt.pots %>%
  count(dx.id, symptom, severity)
ggplot(count.pots, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Postural orthostatic tachycardia syndrome symptoms",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/pots-symptoms.png")

### hsd
id.hsd <- df.ad %>%
  filter(autoimmune_id___172 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "hsd") 
sympt.hsd <- id.hsd %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  select(-c(record_id))
colnames(sympt.hsd)<-gsub(".factor","",colnames(sympt.hsd))
sympt.hsd<- sympt.hsd %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.hsd <- sympt.hsd %>%
  count(dx.id, symptom, severity)
ggplot(count.hsd, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Hypermobility disorders symptoms",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/hsd-symptoms.png")
