#load helper file
source("0_pachages-function.R")

#load data
data.clean <- readRDS("AD-data-clean.rds")

df.trigs.base <- data.clean %>%
  dplyr::select(record_id, viral1:otherchronic_2)
saveRDS(df.trigs, "df-triggers.rds")

##wordcloud treatment
trig.cloud <- df.trigs %>%
  select(-c(record_id)) 
trig.labels <- trig.cloud %>%
  get_label 
#check if column labels are duplicated
# if_else(duplicated(tr.labels) == TRUE, print(tr.labels), "NA")
colnames(trig.cloud) <- trig.labels

#TODO needs to change to numeric to be counted
trig.count <- trig.cloud %>% 
  summarise_if(is.numeric, sum) %>% 
  transpose()
trig.count.df <- as.data.frame(do.call(cbind, trig.count)) %>% 
  rownames_to_column(.) #%>% 
  rename(., word = rowname, freq = V1)
trig.count.df$freq <- as.numeric(as.character(trig.count.df$freq))

wordcloud2(trig.count.df, shape = 'diamond', color = "random-dark", backgroundColor = "white")

### need to trawl through otherchronic_2

df.trigs <- df.trigs.base %>%
  mutate(across(viral1:otherlife, ~  case_when(.x == 2 ~ 0,
                                                .x == 1 ~ 1,
                                                .x == 3 ~ NA_integer_,
                                                is.na(.x) ~ NA_integer_,
                                                TRUE ~ 999999))) %>%
  mutate(trig_stress = case_when(
    str_detect(otherchronic_2, "gp indicated it was stress") ~ 0,
    str_detect(otherchronic_2, "distress") ~ 0,
    str_detect(otherchronic_2, "stress") ~ 1,
               TRUE ~ 0
  )) %>%
  mutate(trig_trauma = case_when(
    str_detect(otherchronic_2, "brain injury") ~ 0,
    str_detect(otherchronic_2, "major emotional trauma") ~ 0,
    str_detect(otherchronic_2, "trauma and the finger") ~ 0,
    str_detect(otherchronic_2, "traumatic injury") ~ 0,
    str_detect(otherchronic_2, "trauma") ~ 1,
    str_detect(otherchronic_2, "death") ~ 1,
    str_detect(otherchronic_2, "died") ~ 1,
    str_detect(otherchronic_2, "traumatic incident") ~ 1,
    str_detect(otherchronic_2, "torture") ~ 1,
    str_detect(otherchronic_2, "childhood trauma") ~ 1,
    str_detect(otherchronic_2, "shock") ~ 1,
    str_detect(otherchronic_2, "2yr old passed away") ~ 1,
    str_detect(otherchronic_2, "miscarriage") ~ 1,
    str_detect(otherchronic_2, "was dying") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(trig_toxins = case_when(
    str_detect(otherchronic_2, "wasps") ~ 1,
    str_detect(otherchronic_2, "weed killer") ~ 1,
    str_detect(otherchronic_2, "mould") ~ 1,
    str_detect(otherchronic_2, "roundup") ~ 1,
    str_detect(otherchronic_2, "drink spiked") ~ 1,
    str_detect(otherchronic_2, "poison in the ceiling") ~ 1,
    str_detect(otherchronic_2, "toxic chemical poisoning") ~ 1,
    str_detect(otherchronic_2, "chemicals and radiation") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(trig_emotional = case_when(
    str_detect(otherchronic_2, "relationship breakup") ~ 1,
    str_detect(otherchronic_2, "mental health breakdown") ~ 1,
    str_detect(otherchronic_2, "family problems") ~ 1,
    str_detect(otherchronic_2, "emotional") ~ 1,
    str_detect(otherchronic_2, "marriage breakdown") ~ 1,
    str_detect(otherchronic_2, "emotional relationship") ~ 1,
    str_detect(otherchronic_2, "lost career") ~ 1,
    str_detect(otherchronic_2, "locked down") ~ 1,
    str_detect(otherchronic_2, "divorce") ~ 1,
    str_detect(otherchronic_2, "anxiety") ~ 1,
    str_detect(otherchronic_2, "bullying") ~ 1,
    str_detect(otherchronic_2, "mental distress") ~ 1,
    str_detect(otherchronic_2, "hardship") ~ 1,
    str_detect(otherchronic_2, "post natal depression") ~ 1,
    str_detect(otherchronic_2, "major life change") ~ 1,
    str_detect(otherchronic_2, "an affair") ~ 1,
    str_detect(otherchronic_2, "major emotional trauma") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(trig_parasite = case_when(
    str_detect(otherchronic_2, "parasitic") ~ 1,
    str_detect(otherchronic_2, "giardia") ~ 1,
    str_detect(otherchronic_2, "parasite") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(trig_vaccine = case_when(
    str_detect(otherchronic_2, "vaccina") ~ 1,
    str_detect(otherchronic_2, "vaccine") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(trig_physicalinjury = case_when(
    str_detect(otherchronic_2, "torn achilles") ~ 1,
    str_detect(otherchronic_2, "superspinatus tendon") ~ 1,
    str_detect(otherchronic_2, "mv accident") ~ 1,
    str_detect(otherchronic_2, "brain injury") ~ 1,
    str_detect(otherchronic_2, "physical injury") ~ 1,
    str_detect(otherchronic_2, "surgery") ~ 1,
    str_detect(otherchronic_2, "cholecystectomy") ~ 1,
    str_detect(otherchronic_2, "osteomyelitis") ~ 1,
    str_detect(otherchronic_2, "fracture") ~ 1,
    str_detect(otherchronic_2, "heart attack") ~ 1,
    str_detect(otherchronic_2, "hitting ribs") ~ 1,
    str_detect(otherchronic_2, "appendectomy") ~ 1,
    str_detect(otherchronic_2, "trauma and the finger") ~ 1,
    str_detect(otherchronic_2, "accident") ~ 1,
    str_detect(otherchronic_2, "colonoscopy") ~ 1,
    str_detect(otherchronic_2, "car accident") ~ 1,
    str_detect(otherchronic_2, "sore back") ~ 1,
    str_detect(otherchronic_2, "shift work") ~ 1,
    str_detect(otherchronic_2, "traumatic injury") ~ 1,
    str_detect(otherchronic_2, "mva") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(trig_dv = case_when(
    str_detect(otherchronic_2, "violent partner") ~ 1,
    str_detect(otherchronic_2, "dv") ~ 1,
    str_detect(otherchronic_2, "violent marriage") ~ 1,
    #str_detect(otherchronic_2, "") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(trig_childabuse = case_when(
    str_detect(otherchronic_2, "childhood abuse") ~ 1,
    str_detect(otherchronic_2, "childhood trauma") ~ 1,
    str_detect(otherchronic_2, "abusive family situation") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(trig_hormonal = case_when(
    str_detect(otherchronic_2, "at puberty") ~ 1,
    str_detect(otherchronic_2, "ivf") ~ 1,
    str_detect(otherchronic_2, "puberty") ~ 1,
    str_detect(otherchronic_2, "peri-menopause") ~ 1,
    str_detect(otherchronic_2, "pregnancy") ~ 1,
    str_detect(otherchronic_2, "perimenopause") ~ 1,
    str_detect(otherchronic_2, "birth control pills") ~ 1,
    str_detect(otherchronic_2, "perimenapause") ~ 1,
    str_detect(otherchronic_2, "menopause") ~ 1,
    str_detect(otherchronic_2, "excessive periods") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(trig_medicine = case_when(
    str_detect(otherchronic_2, "chemotherapy") ~ 1,
    str_detect(otherchronic_2, "immunotherapy") ~ 1,
    str_detect(otherchronic_2, "reaction to medication") ~ 1,
    str_detect(otherchronic_2, "iron infusion") ~ 1,
    str_detect(otherchronic_2, "starting treatment for") ~ 1,
    str_detect(otherchronic_2, "steroid") ~ 1,
    str_detect(otherchronic_2, "sulfa drugs") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(trig_tick = case_when(
    str_detect(otherchronic_2, "tick bite") ~ 1,
    str_detect(otherchronic_2, "biting vector") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(viral1 = case_when(
    record_id == "374" ~ 1,
    record_id == "1039" ~ 1 ,
    record_id == "15" ~ 1 ,
    record_id == "465" ~ 1,
    record_id == "541" ~ 0,
    TRUE ~ viral1
  )) %>%
  mutate(ill1 = case_when(
    record_id == "738" ~ 0,
    record_id == "1668" ~ 0 ,
    record_id == "15" ~ 1,
    record_id == "136"~ 0,
    record_id == "314"~ 1,
    record_id == "1382"~ 1,
    record_id == "541"~ 1,
    record_id == "1015"~ 1,
    TRUE ~ ill1
  ))
saveRDS(df.trigs, "df-triggers.rds")

#labels for plotting
label(df.trigs$viral1 )="Virus"
label(df.trigs$preg1)="During pregnancy"
label(df.trigs$preg2)="After pregnancy"
label(df.trigs$meno1)="Menopause"
label(df.trigs$ill1)="Other illness"
label(df.trigs$trig_stress)="Stress"
label(df.trigs$trig_trauma)="Trauma"
label(df.trigs$trig_toxins)="Toxins"
label(df.trigs$trig_emotional)="Emotional"
label(df.trigs$trig_parasite)="Parasite"
label(df.trigs$trig_vaccine)="Vaccination"
label(df.trigs$trig_physicalinjury)="Physical injury"
label(df.trigs$trig_dv) = "Domestic violence"
label(df.trigs$trig_childabuse)="Child abuse"
label(df.trigs$trig_hormonal)="Hormonal"
label(df.trigs$trig_medicine)="Medication"
label(df.trigs$trig_tick)="Tick bite"

#plot
df.trigs.plot <- df.trigs %>%
  dplyr::select(-c(record_id,
            otherlife,
            otherchronic_2)) 
trig.labels <- df.trigs.plot %>%
  get_label 
colnames(df.trigs.plot) <- trig.labels
trig.count <- df.trigs.plot %>% 
  summarise_if(is.numeric, sum, na.rm = T) %>% 
  transpose()
trig.count.df <- as.data.frame(do.call(cbind, trig.count)) %>% 
  rownames_to_column(.) %>% 
  rename(., word = rowname, freq = V1)
trig.count.df$freq <- as.numeric(as.character(trig.count.df$freq))         
trig.count.ord <- trig.count.df %>%
  rename(., Trigger = word) %>%
  arrange(., desc(freq))
ggplot(trig.count.ord, aes(x = freq, y = Trigger)) +
  geom_col(fill = viridis(17)) +
  geom_text(aes(label = freq), hjust = -1) +
  scale_y_discrete(limits=rev(trig.count.ord$Trigger)) +
  labs(title = NULL,
       x = NULL, #"Number of respondents",
       y = NULL) +
   scale_x_continuous(expand = c(0, 0),
                      limits = c(0, 250)) +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"),
        axis.title=element_text(size=20),
        axis.text=element_text(size=20, face="bold"))
                  