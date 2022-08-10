#load helper file
source("0_pachages-function.R")

#load data
data.clean <- readRDS("AD-data-clean.rds")
df.ad <- readRDS("df-ad.rds")
df.sumstats <- readRDS("AD-sumstats.rds")

### Mental health df
df.mental <- data.clean %>%
  select(record_id,
         selfmental:formalmental,
         mental_diagn___1:mental_diagn___6,
         ptsd___1:ptsd___6,
         selffamment:formalfamment,
         mental_diagn_fam___1:mental_diagn_fam___7,
         ptsd_fam___1:ptsd_fam___8) %>%
  mutate(across(selfmental:ptsd_fam___8, .fns = as.numeric)) %>%
  left_join(data.clean %>% select(record_id,mental_diagn_other, ptsd_other, mental_diagn_fam_other, ptsd_fam_other), by = "record_id") %>%
  mutate(mental_diagn___8 = case_when(
    str_detect(mental_diagn_other, "adhd") ~ 1,
    str_detect(mental_diagn_other, "attention deficit") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate_at(vars(c(mental_diagn___2, mental_diagn___3)), ~ case_when(
    str_detect(mental_diagn_other, "personality disorder and eating disordered behaviour") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(ptsd___1)), ~ case_when(
    str_detect(ptsd_other, "work accident") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(ptsd___2)), ~ case_when(
    str_detect(ptsd_other, "attempted murder") ~ 1,
    str_detect(ptsd_other, "sexual abuse") ~ 1,
    str_detect(ptsd_other, "sexual assault") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(ptsd___5)), ~ case_when(
    str_detect(ptsd_other, "miscarriage") ~ 1,
    str_detect(ptsd_other, "from autism") ~ 1,
    str_detect(ptsd_other, "medical neglect") ~ 1,
    str_detect(ptsd_other, "medical negligence") ~ 1,
    TRUE ~ .
  )) %>%
  #child abuse
  mutate(ptsd___8 = case_when( 
    str_detect(ptsd_other, "childhood neglect") ~ 1,
    str_detect(ptsd_other, "was molested") ~ 1,
    str_detect(ptsd_other, "as a child") ~ 1,
    str_detect(ptsd_other, "childhood emotional") ~ 1,
    str_detect(ptsd_other, "childhood sexual abuse") ~ 1,
    str_detect(ptsd_other, "childhood abuse") ~ 1,
    str_detect(ptsd_other, "raised in an abusive household") ~ 1,
    str_detect(ptsd_other, "child abuse") ~ 1,
    str_detect(ptsd_other, "adverse childhood experience") ~ 1,
    str_detect(ptsd_other, "severe family trauma") ~ 1,
    str_detect(ptsd_other, "childhood and inter generational trauma") ~ 1,
    str_detect(ptsd_other, "growing up in") ~ 1,
    str_detect(ptsd_other, "toxic family") ~ 1,
    str_detect(ptsd_other, "childhood trauma") ~ 1,
    str_detect(ptsd_other, "throughout childhood") ~ 1,
    str_detect(ptsd_other, "alcoholic father") ~ 1,
    str_detect(ptsd_other, "(childhood) trauma") ~ 1,
    str_detect(ptsd_other, "abuse in childhood") ~ 1,
    str_detect(ptsd_other, "childhood domestic violence") ~ 1,
    str_detect(ptsd_other, "childhood sexual assault") ~ 1,
    str_detect(ptsd_other, "family trauma") ~ 1,
    str_detect(ptsd_other, "sexual harassment during school") ~ 1,
    str_detect(ptsd_other, "childhood neglect") ~ 1,
    str_detect(ptsd_other, "father is a vietnam veteran") ~ 1,
    TRUE ~ 0
  )) %>%
  #emotional abuse
  mutate(ptsd___9 = case_when( 
    str_detect(ptsd_other, "emotional abuse") ~ 1,
    str_detect(ptsd_other, "psychological abuse") ~ 1,
    str_detect(ptsd_other, "stalking") ~ 1,
    str_detect(ptsd_other, "lifetime of abuse") ~ 1,
    str_detect(ptsd_other, "verbal abuse") ~ 1,
    str_detect(ptsd_other, "estrangement from family") ~ 1,
    str_detect(ptsd_other, "death threats") ~ 1,
    str_detect(ptsd_other, "problems with relationships") ~ 1,
    str_detect(ptsd_other, "homeless") ~ 1,
    str_detect(ptsd_other, "workplace") ~ 1, 
    TRUE ~ 0
  )) %>%
  #domestic abuse
  mutate(ptsd___10 = case_when( 
    str_detect(ptsd_other, "domestic violence") ~ 1,
    str_detect(ptsd_other, "leaving violence") ~ 1,
    str_detect(ptsd_other, "physical and psychological abuse") ~ 1,
    str_detect(ptsd_other, "dv") ~ 1, 
    str_detect(ptsd_other, "adverse childhood experience due") ~ 0,
    str_detect(ptsd_other, "grew up in a house with domestic violence") ~ 0,
    str_detect(ptsd_other, "relationship") ~ 1, 
    str_detect(ptsd_other, "extremely toxic family") ~ 0,
    str_detect(ptsd_other, "domestic abuse") ~ 1,
    TRUE ~ 0
  )) %>%
  #witnessing violence\death
  mutate(ptsd___11 = case_when( 
    str_detect(ptsd_other, "armed hold up") ~ 1,
    str_detect(ptsd_other, "death") ~ 1,
    str_detect(ptsd_other, "death threats") ~ 0, 
    str_detect(ptsd_other, "car-jacking") ~ 1,
    str_detect(ptsd_other, "police officer") ~ 1,
    str_detect(ptsd_other, "son physical violence") ~ 1,
    str_detect(ptsd_other, "shopfront robbery") ~ 1,
    str_detect(ptsd_other, "suicide attempts") ~ 1,
    str_detect(ptsd_other, "long term missing person") ~ 1,
    str_detect(ptsd_other, "policing") ~ 1,
    str_detect(ptsd_other, "kill them self") ~ 1,
    str_detect(ptsd_other, "were killed") ~ 1,
    str_detect(ptsd_other, "thoroughbred horse studs") ~ 1,
    str_detect(ptsd_other, "commit suicide") ~ 1,
    str_detect(ptsd_other, "world treats your communities") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate_at(vars(c(mental_diagn_fam___1)), ~ case_when(
    str_detect(mental_diagn_fam_other, "anxiety") ~ 1,
    str_detect(mental_diagn_fam_other, "ocd") ~ 1,
    str_detect(mental_diagn_fam_other, "hoarding") ~ 1,
    str_detect(mental_diagn_fam_other, "ocpd") ~ 1,
    str_detect(mental_diagn_fam_other, "dissociative") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(mental_diagn_fam___2)), ~ case_when(
    str_detect(mental_diagn_fam_other, "depression") ~ 1,
    str_detect(mental_diagn_fam_other, "bipolar") ~ 1,
    str_detect(mental_diagn_fam_other, "bpd") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(mental_diagn_fam___4)), ~ case_when(
    str_detect(mental_diagn_fam_other, "schizophrenia") ~ 1,
    str_detect(mental_diagn_fam_other, "not schizophrenia and paranoia") ~ 0,
    str_detect(mental_diagn_fam_other, "narcissi") ~ 1,
    str_detect(mental_diagn_fam_other, "psychotic") ~ 1,
    mental_diagn_fam_other == "did" ~ 1, #check
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(mental_diagn_fam___5)), ~ case_when(
    str_detect(mental_diagn_fam_other, "post traumatic stress") ~ 1,
    TRUE ~ .
  )) %>%
  #mental_diagn_fam___7 ADHD family
  mutate(mental_diagn_fam___7 = case_when(    
    str_detect(mental_diagn_fam_other, "adhd") ~ 1, 
    str_detect(mental_diagn_fam_other, "add") ~ 1, 
    TRUE ~ 0
  )) %>%
#mental_diagn_fam___8 Autism family
  mutate(mental_diagn_fam___8 = case_when(    
    str_detect(mental_diagn_fam_other, "autism") ~ 1,     
    str_detect(mental_diagn_fam_other, "asd") ~ 1,
    str_detect(mental_diagn_fam_other, "aspergers") ~ 1,
    str_detect(mental_diagn_fam_other, "autistic") ~ 1,
    TRUE ~ 0
  )) %>%
  #fam child abuse
  mutate(ptsd_fam___9 = case_when(    
    str_detect(ptsd_fam_other, "childhood trauma") ~ 1, 
    str_detect(ptsd_fam_other, "childhood verbal abuse") ~ 1, 
    str_detect(ptsd_fam_other, "childhood abuse") ~ 1, 
    str_detect(ptsd_fam_other, "family history of abuse") ~ 1, 
    str_detect(ptsd_fam_other, "family violence") ~ 1, 
    TRUE ~ 0
  )) %>%
  #dv
  mutate(ptsd_fam___10 = case_when(   
    str_detect(ptsd_fam_other, "domestic violence") ~ 1, 
    str_detect(ptsd_fam_other, "domestic abuse") ~ 1, 
    str_detect(ptsd_fam_other, "physically abusive to me") ~ 1, 
    TRUE ~ 0
  )) %>%
  #bemotional abuse
  mutate(ptsd_fam___11 = case_when(    
    str_detect(ptsd_fam_other, "bullying") ~ 1, 
    TRUE ~ 0
  )) %>%
  #witnessing violence/death
  mutate(ptsd_fam___12 = case_when(    
    str_detect(ptsd_fam_other, "death of all") ~ 1, 
    str_detect(ptsd_fam_other, "was a police officer") ~ 1, 
    str_detect(ptsd_fam_other, "lost mother and wife") ~ 1, 
    str_detect(ptsd_fam_other, "witness to suicide") ~ 1, 
    str_detect(ptsd_fam_other, "grand father murdered my grandmother") ~ 1, 
    str_detect(ptsd_fam_other, "life can be a more general bastard") ~ 1, 
    TRUE ~ 0
  )) %>%
  #mental_diagn_other was entirely co-morbidities, no re-coding needed
  select(-c(ptsd_other, mental_diagn_other, mental_diagn_fam_other, ptsd_fam_other,
            mental_diagn_fam___7, ptsd___6)) # recoded 'other' sections so not needed
label(df.mental$mental_diagn___1) = "Anxiety disorders"
label(df.mental$mental_diagn___2) = "Mood disorders"
label(df.mental$mental_diagn___3) = "Eating disorders"
label(df.mental$mental_diagn___4) = "Personality disorders"
label(df.mental$mental_diagn___5) = "Post-traumatic stress disorder"
label(df.mental$mental_diagn___6) = "None"
label(df.mental$mental_diagn___8) = "ADHD"
label(df.mental$ptsd___1) = "Accident"
label(df.mental$ptsd___2) = "Assault"
label(df.mental$ptsd___3) = "War/torture"
label(df.mental$ptsd___4) = "Natural disaster"
label(df.mental$ptsd___5) = "Medical trauma"
label(df.mental$ptsd___7) = "None"
label(df.mental$ptsd___8) = "Child abuse"
label(df.mental$ptsd___9) = "Emotional abuse"
label(df.mental$ptsd___10) = "Domestic abuse"
label(df.mental$ptsd___11) = "Witnessing death/violence"
label(df.mental$mental_diagn_fam___1) = "Anxiety disorders family"
label(df.mental$mental_diagn_fam___2) = "Mood disorders family"
label(df.mental$mental_diagn_fam___3) = "Eating disorders family"
label(df.mental$mental_diagn_fam___4) = "Personality disorders family"
label(df.mental$mental_diagn_fam___5) = "Post-traumatic stress disorder family"
label(df.mental$mental_diagn_fam___6) = "None family"
label(df.mental$mental_diagn_fam___7) = "ADHD family"
label(df.mental$mental_diagn_fam___8) = "Autism family"
label(df.mental$ptsd_fam___1) = "PTSD diagnosed family"
label(df.mental$ptsd_fam___2) = "Accident family"
label(df.mental$ptsd_fam___3) = "Assault family"
label(df.mental$ptsd_fam___4) = "War/torture family"
label(df.mental$ptsd_fam___5) = "Natural disaster family"
label(df.mental$ptsd_fam___6) = "Medical trauma family"
label(df.mental$ptsd_fam___8) = "None family"
label(df.mental$ptsd_fam___9) = "Child abuse family"
label(df.mental$ptsd_fam___10) = "Domestic abuse family"
label(df.mental$ptsd_fam___11) = "Emotional abuse family"
label(df.mental$ptsd_fam___12) = "Witnessing death/violence family"
label(df.mental$selfmental) = "Self-diagnosed mental health"
label(df.mental$formalmental) = "Formally diagnosed mental health"
label(df.mental$selffamment) = "Family self-diagnosed mental health"
label(df.mental$formalfamment) = "Family formally diagnosed mental health"
saveRDS(df.mental, file = "df-mental.rds")

### plots

df.ment <- df.mental %>%
  select(contains("mental_diagn___")) 
ment.labels <- df.ment %>%
  get_label 
colnames(df.ment) <- ment.labels
ment.count <- df.ment %>% 
  summarise_if(is.numeric, sum) %>% 
  transpose()
ment.count.df <- as.data.frame(do.call(cbind, ment.count)) %>% 
  rownames_to_column(.) %>% 
  rename(., word = rowname, freq = V1)
ment.count.df$freq <- as.numeric(as.character(ment.count.df$freq))         
ment.count.ord <- ment.count.df %>%
  rename(., Illness = word) %>%
  arrange(., desc(freq))
ggplot(ment.count.ord, aes(x = freq, y = Illness)) +
  geom_col(fill = viridis(6)) +
  scale_y_discrete(limits=rev(ment.count.ord$Illness)) +
  labs(title = NULL,
       x = NULL, #"Number of respondents",
       y = NULL) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 470)) +
  theme_bw() +
  theme(plot.title=element_text(size=35, face="bold"),
        axis.title=element_text(size=35),
        axis.text=element_text(size=35, face="bold"))

df.ptsd <- df.mental %>%
  select(contains("ptsd___")) 
ptsd.labels <- df.ptsd %>%
  get_label 
colnames(df.ptsd) <- ptsd.labels
ptsd.count <- df.ptsd %>% 
  summarise_if(is.numeric, sum) %>% 
  transpose()
ptsd.count.df <- as.data.frame(do.call(cbind, ptsd.count)) %>% 
  rownames_to_column(.) %>% 
  rename(., word = rowname, freq = V1)
ptsd.count.df$freq <- as.numeric(as.character(ptsd.count.df$freq))         
ptsd.count.ord <- ptsd.count.df %>%
  rename(., Illness = word) %>%
  arrange(., desc(freq))
ggplot(ptsd.count.ord, aes(x = freq, y = Illness)) +
  geom_col(fill = viridis(10)) +
  scale_y_discrete(limits=rev(ptsd.count.ord$Illness)) +
  labs(title = NULL,
       x = NULL, #"Number of respondents",
       y = NULL) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 170)) +
  theme_bw() +
  theme(plot.title=element_text(size=35, face="bold"),
        axis.title=element_text(size=35),
        axis.text=element_text(size=35, face="bold"))

ggpiestats(
  data         = data.clean,
  x            = selfmental.factor,
  #y            = gender.id,
  package      = "RColorBrewer",
  palette = "Spectral",
  title        = "Respondents with self diagnosed mental health illness",
  legend.title = "Self diagnosed"
)

ggpiestats(
  data         = data.clean,
  x            = formalmental.factor,
  #y            = gender.id,
  package      = "RColorBrewer",
  palette = "Spectral",
  title        = "Respondents with formally diagnosed mental health illness",
  legend.title = "Formally diagnosed"
)
# is there an overlap with mental health formal and self dx?
df.mental %>% filter(selfmental == 1 & formalmental == 1)
# 122 respondents both self and formally dx mental health


#selected chronic illness groupings, from Sara's trig figure code
# trigger responses 
ment.score <- df.mental %>%
  select(record_id  , 
         contains("mental_diagn___"))
# Get labels for responses 
ment_labels =  get_label(ment.score)
#ment_responses =  c(levels("0", "1")) 

df.ment.score <- ment.score %>%
  left_join(df.ad %>% select(record_id, illness.grouped, cohort.id), by = "record_id") %>%  filter(cohort.id != "control")

# make long table, need a cleaner way to do this :/ 
freq_ment_long = rbind( 
  data.frame(x=ment_labels[1], plyr::count( cbind( as.character(df.ment.score$illness.grouped), as.character(df.ment.score$mental_diagn___1)))), 
  data.frame(x=ment_labels[2],plyr::count( cbind( as.character(df.ment.score$illness.grouped), as.character(df.ment.score$mental_diagn___2)))), 
  data.frame(x=ment_labels[3],plyr::count( cbind( as.character(df.ment.score$illness.grouped), as.character(df.ment.score$mental_diagn___3)))), 
  data.frame(x=ment_labels[4],plyr::count( cbind( as.character(df.ment.score$illness.grouped), as.character(df.ment.score$mental_diagn___4)))), 
  data.frame(x=ment_labels[5],plyr::count( cbind( as.character(df.ment.score$illness.grouped), as.character(df.ment.score$mental_diagn___5.score)))), 
  data.frame(x=ment_labels[6],plyr::count( cbind( as.character(df.ment.score$illness.grouped), as.character(df.ment.score$mental_diagn___6))))#, 
  # data.frame(x=ment_labels[7],plyr::count( cbind( as.character(df.ment.score$illness.grouped), as.character(df.ment.score$mental_diagn___7)))) 
) 
#test for an easier way to generate the long table
# ment_labels =  get_label(df.ment.score)
# test.long <- df.ment.score# %>%
# colnames(test.long) <- ment_labels
# test.count <- test.long %>%
#   select(-c('Record ID', '')) %>%
#   mutate(across('Anxiety disorders':None, .fns = as.numeric)) %>%
#   pivot_longer(!'Illness grouped', names_to = "mental", values_to = "count")

# extract yes reponses
freq_ment_long_yes =  freq_ment_long  %>% filter(x.2 == "Checked") 
freq_ment_long_yes = freq_ment_long_yes[,-3]
freq_ment_yes_mat = spread(freq_ment_long_yes, key=2, value = 3, fill=0) 

# plot 
#TODO no idea why this is introducing NAs and not working
barplot( t((freq_ment_yes_mat)) , col=turbo(10), hori=T) 
text(200, bp[,1], ment_labels, adj=1, cex=2)