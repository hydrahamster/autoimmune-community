# Symptoms analysis 
## A variety of symptoms are shared across autoimmune diseases and other chronic illnesses. 
## We would like to know what symptoms, if any, you suffer from. 

# Fatigue
# Joint pain and swelling
# Skin problems
# Abdominal pain or digestive
# issues
# Recurring fever
# Swollen glands
# Chills
# Memory loss
# Concentration loss
# Hair loss
# Peculiar skin sensation
# General weakness
# Muscle aches and pain
# Easy bruising

#load helper file
source("0_pachages-function.R")
#includes heatmap and network functions: sympt.heat() & sympt.network()

#load data
data.clean <- readRDS("AD-data-clean.rds")
df.ad <- readRDS("df-ad.rds")
df.sumstats <- readRDS("AD-sumstats.rds")

################################################################################################################

###################
# Basic bar plots
#################

#factor with written answers, removing rows with all NAs
symptoms.factor <- data.clean %>%
  select(record_id,
         fatigue.factor:bruise.factor) %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  left_join((df.ad %>% select(record_id, cohort.id)), by="record_id")

#scale from 0 to 5 for severity, rm rows with NA only
symptoms.scale <- data.clean %>%
  select(record_id,
         fatigue:bruise) %>%
  filter(!if_all(fatigue:bruise, is.na)) %>%
  left_join((df.ad %>% select(record_id, cohort.id)), by="record_id") %>%
  mutate(across(fatigue:bruise, .fns = as.numeric))


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
  scale_fill_viridis_d(name = "Severity",
                       na.value = "grey")+
  coord_flip() +
 # theme(axis.text=element_text(size=18, face="bold")) +
    facet_wrap( ~ cohort.id, scales = "free") 
pdf("images/symptoms/symptoms-summary.pdf")
ggsave("images/symptoms/symptoms-summary.png")

### top 10 illnesses symptoms
#coeliac
id.coeliac <- df.ad %>%
  filter(autoimmune_id___6 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "coeliac") 
sympt.coeliac <- id.coeliac %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  select(-c(record_id))
colnames(sympt.coeliac)<-gsub(".factor","",colnames(sympt.coeliac))
sympt.coeliac.long <- sympt.coeliac %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.coeliac <- sympt.coeliac.long %>%
  count(dx.id, symptom, severity)
s.coeliac <- ggplot(count.coeliac, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Coeliac",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()  
ggsave("images/symptoms/coeliac-symptoms.png")

### hashi
id.hashi <- df.ad %>%
  filter(autoimmune_id___17 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "hashimotos") 
sympt.hashi <- id.hashi %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  select(-c(record_id))
colnames(sympt.hashi)<-gsub(".factor","",colnames(sympt.hashi))
sympt.hashi.long <- sympt.hashi %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.hashi <- sympt.hashi.long %>%
  count(dx.id, symptom, severity)
s.hashi <- ggplot(count.hashi, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Hashimoto's",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/symptoms/hashi-symptoms.png")

### lupus
id.lupus <- df.ad %>%
  filter(autoimmune_id___22 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "lupus") 
sympt.lupus <- id.lupus %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  select(-c(record_id))
colnames(sympt.lupus)<-gsub(".factor","",colnames(sympt.lupus))
sympt.lupus.long <- sympt.lupus %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.lupus <- sympt.lupus.long %>%
  count(dx.id, symptom, severity)
s.lupus <-ggplot(count.lupus, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Lupus",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/symptoms/lupus-symptoms.png")

### me
id.me <- df.ad %>%
  filter(autoimmune_id___55 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "mecfs") 
sympt.me <- id.me %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  select(-c(record_id))
colnames(sympt.me)<-gsub(".factor","",colnames(sympt.me))
sympt.me.long <- sympt.me %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.me <- sympt.me.long %>%
  count(dx.id, symptom, severity)
s.me <- ggplot(count.me, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "ME/CFS",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/symptoms/mecfs-symptoms.png")

### ra
id.ra <- df.ad %>%
  filter(autoimmune_id___35 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "ra") 
sympt.ra <- id.ra %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  select(-c(record_id))
colnames(sympt.ra)<-gsub(".factor","",colnames(sympt.ra))
sympt.ra.long <- sympt.ra %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.ra <- sympt.ra.long %>%
  count(dx.id, symptom, severity)
s.ra <- ggplot(count.ra, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Rheumatoid arthritis",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/symptoms/ra-symptoms.png")

### sjog
id.sjog <- df.ad %>%
  filter(autoimmune_id___38 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "sjog") 
sympt.sjog <- id.sjog %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  select(-c(record_id))
colnames(sympt.sjog)<-gsub(".factor","",colnames(sympt.sjog))
sympt.sjog.long <- sympt.sjog %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.sjog <- sympt.sjog.long %>%
  count(dx.id, symptom, severity)
s.sjog <- ggplot(count.sjog, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Sjogren's",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/symptoms/sjog-symptoms.png")


### PsA
id.psa <- df.ad %>%
  filter(autoimmune_id___34 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "psa") 
sympt.psa <- id.psa %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  select(-c(record_id))
colnames(sympt.psa)<-gsub(".factor","",colnames(sympt.psa))
sympt.psa.long <- sympt.psa %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.psa <- sympt.psa.long  %>%
  count(dx.id, symptom, severity)
s.psa <- ggplot(count.psa, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Psoriatic arthritis",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/symptoms/psa-symptoms.png")

### fibro
id.fibro <- df.ad %>%
  filter(autoimmune_id___64 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "fibro") 
sympt.fibro <- id.fibro %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  select(-c(record_id))
colnames(sympt.fibro)<-gsub(".factor","",colnames(sympt.fibro))
sympt.fibro.long <- sympt.fibro %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.fibro <- sympt.fibro.long %>%
  count(dx.id, symptom, severity)
s.fibro <- ggplot(count.fibro, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Fibromyalgia",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/symptoms/fibro-symptoms.png")

### pots
id.pots <- df.ad %>%
  filter(autoimmune_id___116 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "pots") 
sympt.pots <- id.pots %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  select(-c(record_id))
colnames(sympt.pots)<-gsub(".factor","",colnames(sympt.pots))
sympt.pots.long <- sympt.pots %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.pots <- sympt.pots.long %>%
  count(dx.id, symptom, severity)
s.pots <- ggplot(count.pots, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "POTS",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/symptoms/pots-symptoms.png")

### hsd
id.hsd <- df.ad %>%
  filter(autoimmune_id___172 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "hsd") 
sympt.hsd <- id.hsd %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  select(-c(record_id))
colnames(sympt.hsd)<-gsub(".factor","",colnames(sympt.hsd))
sympt.hsd.long <- sympt.hsd %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.hsd <- sympt.hsd.long %>%
  count(dx.id, symptom, severity)
s.hsd <- ggplot(count.hsd, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Hypermobility disorders",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()
ggsave("images/symptoms/hsd-symptoms.png")

## figure of all the symptoms and the controls together
id.cont <- df.ad %>%
  filter(autoimmune_id___44 == 1) %>%
  select(record_id) %>%
  mutate(dx.id = "control") 
sympt.cont <- id.cont %>%
  left_join((symptoms.factor %>% select(-cohort.id)), by = "record_id") %>%
  filter(!if_all(fatigue.factor:bruise.factor, is.na)) %>%
  select(-c(record_id))
colnames(sympt.cont)<-gsub(".factor","",colnames(sympt.cont))
sympt.cont.long <- sympt.cont %>%
  pivot_longer(!dx.id, names_to = "symptom", values_to = "severity") 
count.cont <- sympt.cont.long %>%
  count(dx.id, symptom, severity)
s.cont <- ggplot(count.cont, aes(symptom, n, fill= forcats::fct_rev(severity))) +
  labs(title = "Controls",
       x = "Symptom", 
       y = "Number of respondents",
       fill = "Severity") +
  geom_col() +
  theme_bw() +
  scale_fill_viridis_d(na.value = "grey")+
  coord_flip()

ggarrange(s.cont, s.coeliac, s.fibro, s.hashi, s.hsd, s.lupus, s.me, s.pots, s.psa, s.ra, s.sjog,
          labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"),
          common.legend = TRUE, legend = "right")

################################################################################################################

###########################
# Networks & heatmaps
###########################


###
# All symptoms
###

sympt.all <- data.clean %>% select(fatigue:bruise)
sympt.heat(sympt.all)
#ggsave("images/sympt-heatmap-all.png")
sympt.network(sympt.all, "Symptoms of all respondents")
#ggsave("images/sympt-network-all.png")


###
# Gender differences
###

gender.data <- data.clean %>% select(record_id, fatigue:bruise) %>% left_join((df.sumstats %>% select(record_id, gender.group)), by = "record_id")

 gender.m <- gender.data %>% filter(gender.group == "Male") %>% select(!c(record_id, gender.group))
 sympt.heat(gender.m)
 #ggsave("images/sympt-heatmap-male.png")
 sympt.network(gender.m, "Symptoms of males")
 #ggsave("images/sympt-network-male.png")
 
 gender.f <- gender.data %>% filter(gender.group == "Female") %>% select(!c(record_id, gender.group))
 sympt.heat(gender.f)
 #ggsave("images/sympt-heatmap-female.png")
 sympt.network(gender.f, "Symptoms of females")
 #ggsave("images/sympt-network-female.png")
 
 gender.o <- gender.data %>% filter(gender.group == "Other") %>% select(!c(record_id, gender.group))
 sympt.heat(gender.o)
 # ggsave("images/sympt-heatmap-other-gender.png")
 sympt.network(gender.o, "Symptoms of other genders")
 # ggsave("images/sympt-network-other-gender.png")
 
 network.differences(gender.m, gender.f, "Network difference between male and female")
 # ggsave("images/sympt-network-diff-male-female.png")
 
###
# control vs. chronic
###

 cohort.data <- data.clean %>% select(record_id, fatigue:bruise) %>% left_join((df.sumstats %>% select(record_id, cohort.id, gender.group)), by = "record_id")
 
 cohort.control <- cohort.data %>% filter(cohort.id == "control") %>% select(!c(record_id, cohort.id, gender.group))
 sympt.heat(cohort.control)
 # ggsave("images/sympt-heatmap-control-cohort.png")
 sympt.network(cohort.control, "Symptoms of control cohort")
 # ggsave("images/sympt-network-control-cohort.png")
 
 cohort.chronic <- cohort.data %>% filter(cohort.id == "chronically ill") %>% select(!c(record_id, cohort.id, gender.group))
 sympt.heat(cohort.chronic)
 # ggsave("images/sympt-heatmap-chronic-cohort.png")
 sympt.network(cohort.chronic, "Symptoms of chronic cohort")
 # ggsave("images/sympt-network-chronic-cohort.png")
 
 network.differences(cohort.chronic, cohort.control, "Network difference between chronic and control cohorts")
 # ggsave("images/sympt-network-diff-chron-control.png")
 
 ### gender difference control vs chronic
 cohort.control.m <- cohort.data %>% filter(cohort.id == "control" & gender.group == "Male") %>% select(!c(record_id, cohort.id, gender.group))
 
 # png("images/sympt-heatmap-male-control-cohort.png")
 sympt.heat(cohort.control.m)
 # dev.off()
 
 # png("images/sympt-network-male-control-cohort.png")
 sympt.network(cohort.control.m, "Symptoms of male control cohort")
 # dev.off()
 
 cohort.chronic.m <- cohort.data %>% filter(cohort.id == "chronically ill" & gender.group == "Male") %>% select(!c(record_id, cohort.id, gender.group))
 
 # png("images/sympt-heatmap-male-chronic-cohort.png")
 sympt.heat(cohort.chronic.m)
 # dev.off()
 
 # png("images/sympt-network-male-chronic-cohort.png")
 sympt.network(cohort.chronic.m, "Symptoms of male chronic cohort")
 # dev.off()
 
 cohort.control.f <- cohort.data %>% filter(cohort.id == "control" & gender.group == "Female") %>% select(!c(record_id, cohort.id, gender.group))
 
 # png("images/sympt-heatmap-female-control-cohort.png")
 sympt.heat(cohort.control.f)
 # dev.off()
 
 # png("images/sympt-network-female-control-cohort.png")
 sympt.network(cohort.control.f, "Symptoms of female control cohort")
 # dev.off()
 
 cohort.chronic.f <- cohort.data %>% filter(cohort.id == "chronically ill" & gender.group == "Female") %>% select(!c(record_id, cohort.id, gender.group))
 
 # png("images/sympt-heatmap-female-chronic-cohort.png")
 sympt.heat(cohort.chronic.f)
 # dev.off()
 
 # png("images/sympt-network-female-chronic-cohort.png")
 sympt.network(cohort.chronic.f, "Symptoms of female chronic cohort")
 # dev.off()
 
 # png("images/sympt-network-diff-male-chron-control.png")
 network.differences(cohort.chronic.m, cohort.control.m, "Network difference between male chronic and control cohorts")
 # dev.off()
 
 # png("images/sympt-network-diff-female-chron-control.png")
 network.differences(cohort.chronic.f, cohort.control.f, "Network difference between female chronic and control cohorts")
 # dev.off()
 
 # png("images/sympt-network-diff-female-male-chron.png")
 network.differences(cohort.chronic.f, cohort.chronic.m, "Network difference between female and male chronic cohorts")
 # dev.off()
 
 
###
# Top 10 ADs
####
 
 scale.fibro <- id.fibro %>%
   select(-c(dx.id)) %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id))
  sympt.heat(scale.fibro)
 # ggsave("images/sympt-heatmap-fibro.png")
 sympt.network(scale.fibro, "x")
 # ggsave("images/sympt-network-fibro.png")
 #TODO probs need to use single AD input rathter than all
 scale.pots <- id.pots %>%
   select(-c(dx.id)) %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id))
  sympt.heat(scale.pots)
 ggsave("images/sympt-heatmap-pots.png")
 pots.test <- sympt.pots %>% select(-dx.id) 
 sympt.network(scale.pots, "x")
 ggsave("images/sympt-network-pots.png")
 
 scale.psa <- id.psa %>%
   select(-c(dx.id)) %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id))
 sympt.heat(scale.psa)
 ggsave("images/sympt-heatmap-psa.png")
 sympt.network(scale.psa, "x")
 ggsave("images/sympt-network-psa.png")
 
 scale.sjog <- id.sjog %>%
   select(-c(dx.id)) %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id))
 sympt.heat(scale.sjog)
 ggsave("images/sympt-heatmap-sjog.png")
 sympt.network(scale.sjog, "x")
 ggsave("images/sympt-network-sjog.png")
 
 scale.ra <- id.ra %>%
   select(-c(dx.id)) %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id))
 sympt.heat(scale.ra)
 ggsave("images/sympt-heatmap-ra.png")
 sympt.network(scale.ra)
 ggsave("images/sympt-network-ra.png")
 
 scale.me <- id.me %>%
   select(-c(dx.id)) %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id))
 sympt.heat(scale.me)
 ggsave("images/sympt-heatmap-me.png")
 sympt.network(scale.me, "x")
 ggsave("images/sympt-network-me.png")
 
 scale.lupus <- id.lupus %>%
   select(-c(dx.id)) %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id))
 sympt.heat(scale.lupus)
 ggsave("images/sympt-heatmap-lupus.png")
 sympt.network(scale.lupus)
 ggsave("images/sympt-network-lupus.png")
 
 scale.hashi <- id.hashi %>%
   select(-c(dx.id)) %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id))
 sympt.heat(scale.hashi)
 ggsave("images/sympt-heatmap-hashi.png")
 sympt.network(scale.hashi)
 ggsave("images/sympt-network-hashi.png")
 
 scale.coeliac <- id.coeliac %>%
   select(-c(dx.id)) %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id))
 sympt.heat(scale.coeliac)
 ggsave("images/sympt-heatmap-coeliac.png")
 sympt.network(scale.coeliac, "x")
 ggsave("images/sympt-network-coeliac.png")
 network.differences(scale.psa, scale.ra, "psa vs ra")
 scale.hsd <- id.hsd %>%
   select(-c(dx.id)) %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id))
 sympt.heat(scale.hsd)
 ggsave("images/sympt-heatmap-hsd.png")
 sympt.network(scale.hsd)
 ggsave("images/sympt-network-hsd.png")
 
 ###
 # Misdiag
 ###
 scale.mdx <- df.sumstats %>%
   select(record_id, misdiag.id) %>%
   filter(misdiag.id == "Yes") %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id, misdiag.id))
 sympt.heat(scale.mdx)
 # ggsave("images/sympt-heatmap-misdiagnosed.png")
 sympt.network(scale.mdx, "Symptoms of misdiagnosed cohort")
 # ggsave("images/sympt-network-misdiagnosed.png")
 
 scale.cdx <- df.sumstats %>%
   select(record_id, misdiag.id) %>%
   filter(misdiag.id == "No") %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id, misdiag.id))
 sympt.heat(scale.cdx)
 # ggsave("images/sympt-heatmap-correct-diagnosed.png")
 sympt.network(scale.cdx, "Symptoms of correctly diagnosed cohort")
 # ggsave("images/sympt-network-correct-diagnosed.png")
 
 network.differences(scale.mdx, scale.cdx, "Network difference between correct and misdiagnoses")
 # ggsave("images/sympt-network-diff-mdx-cdx.png")
 
 ## misdiag by sex as well as mdx
 scale.mdx.m <- df.sumstats %>%
   select(record_id, misdiag.id, gender.group) %>%
   filter(misdiag.id == "Yes" & gender.group == "Male") %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id, misdiag.id, gender.group))
 # png("images/sympt-heatmap-misdiagnosed-male.png")
 sympt.heat(scale.mdx.m)
 # dev.off()
 # png("images/sympt-network-misdiagnosed-male.png")
 sympt.network(scale.mdx.m, "Symptoms of misdiagnosed male cohort")
 # dev.off()
 scale.mdx.f <- df.sumstats %>%
   select(record_id, misdiag.id, gender.group) %>%
   filter(misdiag.id == "Yes" & gender.group == "Female") %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id, misdiag.id, gender.group))
 # png("images/sympt-heatmap-misdiagnosed-female.png")
 sympt.heat(scale.mdx.f)
 # dev.off()
 # png("images/sympt-network-misdiagnosed-female.png")
 sympt.network(scale.mdx.f, "Symptoms of misdiagnosed female cohort")
 # dev.off()
 #### need to fix missing symptoms issue
 # scale.mdx.o <- df.sumstats %>%
 #   select(record_id, misdiag.id, gender.group) %>%
 #   filter(misdiag.id == "Yes" & gender.group == "Other") %>%
 #   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
 #   filter(!if_all(fatigue:bruise, is.na)) %>%
 #   select(-c(record_id, misdiag.id, gender.group))
 # png("images/sympt-heatmap-misdiagnosed-other-gender.png")
 # sympt.heat(scale.mdx.o)
 # dev.off()
 # png("images/sympt-network-misdiagnosed-other-gender.png")
 # sympt.network(scale.mdx.o, "Symptoms of misdiagnosed other gender cohort")
 # dev.off()
 
 scale.cdx.m <- df.sumstats %>%
   select(record_id, misdiag.id, gender.group) %>%
   filter(misdiag.id == "No" & gender.group == "Male") %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id, misdiag.id, gender.group))
 # png("images/sympt-heatmap-correct-diagnosed-male.png")
 sympt.heat(scale.cdx.m)
 # dev.off()
 # png("images/sympt-network-correct-diagnosed-male.png")
 sympt.network(scale.cdx.m, "Symptoms of correctly diagnosed male cohort")
 # dev.off()
 scale.cdx.f <- df.sumstats %>%
   select(record_id, misdiag.id, gender.group) %>%
   filter(misdiag.id == "No" & gender.group == "Female") %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id, misdiag.id, gender.group))
 # png("images/sympt-heatmap-correct-diagnosed-female.png")
 sympt.heat(scale.cdx.f)
 # dev.off()
 # png("images/sympt-network-correct-diagnosed-female.png")
 sympt.network(scale.cdx.f, "Symptoms of correctly diagnosed female cohort")
 # dev.off()
 # scale.cdx.o <- df.sumstats %>%
 #   select(record_id, misdiag.id, gender.group) %>%
 #   filter(misdiag.id == "No" & gender.group == "Other") %>%
 #   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
 #   filter(!if_all(fatigue:bruise, is.na)) %>%
 #   select(-c(record_id, misdiag.id, gender.group))
 # png("images/sympt-heatmap-misdiagnosed-other-gender.png")
 # sympt.heat(scale.cdx.o)
 # dev.off()
 # png("images/sympt-network-misdiagnosed-other-gender.png")
 # sympt.network(scale.cdx.o, "Symptoms of correctly diagnosed other gender cohort")
 # dev.off()
 
 # png("images/sympt-network-diff-mdx-cdx-males.png")
 network.differences(scale.mdx.m, scale.cdx.m, "Network difference between correct and misdiagnoses in males")
# dev.off()
# png("images/sympt-network-diff-mdx-cdx-females.png")
  network.differences(scale.mdx.f, scale.cdx.f, "Network difference between correct and misdiagnoses in females")
 # dev.off()
 # png("images/sympt-network-diff-mdx-cdx-other-gender.png")
 # network.differences(scale.mdx.o, scale.cdx.o, "Network difference between correct and misdiagnoses in other genders")
 # dev.off()
 # png("images/sympt-network-diff-mdx-males-females.png")
 network.differences(scale.mdx.m, scale.mdx.f, "Network difference of misdiagnoses between males and females")
 # dev.off()
 ###
 # Length dx
 ###
 #TODO this section not yet saved
 scale.less6m <- df.sumstats %>%
   select(record_id, dx.group) %>%
   filter(dx.group == "< 6 months") %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id, dx.group))
 sympt.heat(scale.less6m)
 ggsave("images/sympt-heatmap-dx-less6m.png")
 sympt.network(scale.less6m)
 ggsave("images/sympt-network-dx-less6m.png")
 
 scale.6mto1 <- df.sumstats %>%
   select(record_id, dx.group) %>%
   filter(dx.group == ">6 months - 1 year") %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id, dx.group))
 sympt.heat(scale.6mto1)
 ggsave("images/sympt-heatmap-dx-6mto1.png")
 sympt.network(scale.6mto1)
 ggsave("images/sympt-network-dx-6mto1.png")
 
 scale.1to5 <- df.sumstats %>%
   select(record_id, dx.group) %>%
   filter(dx.group == ">1 - 5 years") %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id, dx.group))
 sympt.heat(scale.1to5)
 ggsave("images/sympt-heatmap-dx-1to5.png")
 sympt.network(scale.1to5)
 ggsave("images/sympt-network-dx-1to5.png")
 
 scale.5to9 <- df.sumstats %>%
   select(record_id, dx.group) %>%
   filter(dx.group == ">5 - 9 years") %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id, dx.group))
 sympt.heat(scale.5to9)
 ggsave("images/sympt-heatmap-dx-5to9.png")
 sympt.network(scale.5to9)
 ggsave("images/sympt-network-dx-5to9.png")
 
 scale.9plus <- df.sumstats %>%
   select(record_id, dx.group) %>%
   filter(dx.group == "9+ years") %>%
   left_join((data.clean %>% select(record_id, fatigue:bruise)), by = "record_id") %>%
   filter(!if_all(fatigue:bruise, is.na)) %>%
   select(-c(record_id, dx.group))
 sympt.heat(scale.9plus)
 ggsave("images/sympt-heatmap-dx-9plus.png")
 sympt.network(scale.9plus)
 ggsave("images/sympt-network-dx-9plus.png")
 
#TODO next question: how does this pan out between AD vs. non-ad chron cohort? Would need to class as AD, other, both depending on combo
 # Other/continuation/questions 
 
 ## Symptom co-occurences (network edges) 
 ## Symptoms as a classifier 
 ## Severity and other associations 