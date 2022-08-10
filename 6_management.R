#treatments, diet, specialists, triggers, trigers
#load helper file
source("0_pachages-function.R")

#load data
data.clean <- readRDS("AD-data-clean.rds")
df.ad <- readRDS("df-ad.rds")
df.sumstats <- readRDS("AD-sumstats.rds")

### to check which str_detect other sections aren't picked up yet
test.flares <- data.clean %>%
  select(flares_other) %>%
  mutate(flares_other = as.character(flares_other))%>%
  mutate_at(vars(c(flares_other)), ~ case_when(
    str_detect(flares_other, "chemotherapy") ~ NA_character_,
    str_detect(flares_other, "food") ~ NA_character_,
    str_detect(flares_other, "eating badly") ~ NA_character_,
    str_detect(flares_other, "eating") ~ NA_character_,
    str_detect(flares_other, "running/ overheating") ~ NA_character_,
    str_detect(flares_other, "had a pacemaker inserted") ~ NA_character_,
    str_detect(flares_other, "garlic") ~ NA_character_,
    str_detect(flares_other, "sugar intake") ~ NA_character_,
    str_detect(flares_other, "ingestion") ~ NA_character_,
    str_detect(flares_other, "diet") ~ NA_character_,
    str_detect(flares_other, "alcohol") ~ NA_character_,
    str_detect(flares_other, "mental emtional activity") ~ NA_character_,
    str_detect(flares_other, "over-exertion") ~ NA_character_,
    str_detect(flares_other, "mental stress") ~ NA_character_,
    str_detect(flares_other, "increased activity") ~ NA_character_,
    str_detect(flares_other, "post exertion") ~ NA_character_,
    str_detect(flares_other, "exercise") ~ NA_character_,
    str_detect(flares_other, "doing to much") ~ NA_character_,
    str_detect(flares_other, "movement, walking") ~ NA_character_,
    str_detect(flares_other, "over extension") ~ NA_character_,
    str_detect(flares_other, "energy expenditure") ~ NA_character_,
    str_detect(flares_other, "running") ~ NA_character_,
    str_detect(flares_other, "emotion") ~ NA_character_,
    str_detect(flares_other, "get a cold") ~ NA_character_,
    str_detect(flares_other, "illness") ~ NA_character_,
    str_detect(flares_other, "virus") ~ NA_character_,
    str_detect(flares_other, "infection") ~ NA_character_,
    str_detect(flares_other, "changes") ~ NA_character_,
    str_detect(flares_other, "heat intolerance, competing sensory") ~ NA_character_,
    str_detect(flares_other, "humidity") ~ NA_character_,
    str_detect(flares_other, "sleep") ~ NA_character_,
    str_detect(flares_other, "insomnia") ~ NA_character_,
    str_detect(flares_other, "smells") ~ NA_character_,
    str_detect(flares_other, "sound") ~ NA_character_,
    str_detect(flares_other, "allergens") ~ NA_character_,
    str_detect(flares_other, "mould") ~ NA_character_,
    str_detect(flares_other, "insect") ~ NA_character_,
    str_detect(flares_other, "smoke") ~ NA_character_,
    str_detect(flares_other, "dust") ~ NA_character_,
    str_detect(flares_other, "preg") ~ NA_character_,
    str_detect(flares_other, "menst") ~ NA_character_,
str_detect(flares_other, "lot of drugs") ~ NA_character_,
str_detect(flares_other, "drugs/medications") ~ NA_character_,
str_detect(flares_other, "prescribed drugs") ~ NA_character_,
str_detect(flares_other, "physical") ~ NA_character_,
str_detect(flares_other, "fragrance") ~ NA_character_,
str_detect(flares_other, "weather") ~ NA_character_,
str_detect(flares_other, "too much") ~ NA_character_,
str_detect(flares_other, "chemical") ~ NA_character_,
str_detect(flares_other, "overdoing") ~ NA_character_,
str_detect(flares_other, "too long") ~ NA_character_,
    TRUE ~ .)) %>%
  unique()

df.management <- data.clean %>%
  select(record_id,
    contains("diet__"),
         contains("flares___"),
         contains("treatments___"),
         contains("drug___"),
         drug_other,
         contains("hormone___"),
         contains("physical___"),
         contains("specialist___"),
         contains("therapy___"),
         -contains("factor")) %>%
  mutate(across(diet___1:therapy___8, .fns = as.numeric)) %>%
  left_join(data.clean %>% select(record_id, symptoms_other, flares_other, treatments_other_other, drug_other_other, hormone_other, diet_other, physical_other, specialist_other, treatments_other), by = "record_id") %>%
  #diet
  # mutate(treatments___6 = case_when(   
  #   str_detect(symptoms_other, "diet") ~ 1, 
  #   str_detect(symptoms_other, "") ~ 1, 
  #   str_detect(symptoms_other, "") ~ 1, 
  #   TRUE ~ 0
  # )) %>%
  # #life style change
  # mutate(treatments___7 = case_when(   
  #   str_detect(symptoms_other, "lifestyle changes") ~ 1, 
  #   str_detect(symptoms_other, "") ~ 1, 
  #   str_detect(symptoms_other, "") ~ 1, 
  #   TRUE ~ 0
  # )) %>%
  # #exercise
  # mutate(treatments___8 = case_when(   
  #   str_detect(symptoms_other, "exercise") ~ 1, 
  #   str_detect(, "") ~ 1, 
  #   str_detect(, "") ~ 1, 
  #   TRUE ~ 0
  # )) %>%
mutate_at(vars(c(flares___3)), ~ case_when(
  str_detect(flares_other, "chemotherapy") ~ 1,
  str_detect(flares_other, "lot of drugs") ~ 1,
  str_detect(flares_other, "drugs/medications") ~ 1,
  str_detect(flares_other, "prescribed drugs") ~ 1,
  str_detect(flares_other, "large chemical doses") ~ 1,
  flares_other == "medication" ~ 1,
  str_detect(flares_other, "i was on them for too ling") ~ 1,
  str_detect(flares_other, "medication needing adjusting") ~ 1,
  str_detect(flares_other, "anaesthetics") ~ 1,
  TRUE ~ .
)) %>%
  mutate_at(vars(c(flares___4)), ~ case_when(
    str_detect(flares_other, "sometimes dont know") ~ 1,
    str_detect(flares_other, "not figured out why") ~ 1,
    str_detect(flares_other, "unknown") ~ 1,
    str_detect(flares_other, "or nothing at all") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(flares___1)), ~ case_when(
    flares_other == "stress" ~ 1,
    str_detect(flares_other, "worsened by stress") ~ 1,
    str_detect(flares_other, "stress") ~ 1,
    TRUE ~ .
  )) %>%
#food
  mutate(flares___7 = case_when(   
    str_detect(flares_other, "food") ~ 1, 
    str_detect(flares_other, "eating badly") ~ 1, 
    str_detect(flares_other, "eating") ~ 1, 
    str_detect(flares_other, "running/ overheating") ~ 0,
    str_detect(flares_other, "had a pacemaker inserted") ~ 0,
    str_detect(flares_other, "garlic") ~ 1, 
    str_detect(flares_other, "sugar intake") ~ 1, 
    str_detect(flares_other, "ingestion") ~ 1, 
    str_detect(flares_other, "diet") ~ 1,  
    str_detect(flares_other, "gluten") ~ 1, 
    str_detect(flares_other, "dairy products") ~ 1, 
    str_detect(flares_other, "caffeine") ~ 1, 
    str_detect(flares_other, "red meat") ~ 1,
    str_detect(flares_other, "wheat") ~ 1,
    TRUE ~ 0
  )) %>%
  #alcohol
  mutate(flares___8 = case_when(   
    str_detect(flares_other, "alcohol") ~ 1,    
    str_detect(flares_other, "red wine ") ~ 1, 
    TRUE ~ 0
  )) %>%
  #exertion, physical/mental
  mutate(flares___9 = case_when(   
    str_detect(flares_other, "mental emtional activity") ~ 1, 
    str_detect(flares_other, "over-exertion") ~ 1, 
    str_detect(flares_other, "mental stress") ~ 1, 
    str_detect(flares_other, "increased activity") ~ 1, 
    str_detect(flares_other, "post exertion") ~ 1, 
    str_detect(flares_other, "exercise") ~ 1, 
    str_detect(flares_other, "doing to much") ~ 1,
    str_detect(flares_other, "movement, walking") ~ 1,
    str_detect(flares_other, "over extension") ~ 1,
    str_detect(flares_other, "energy expenditure") ~ 1,
    str_detect(flares_other, "running") ~ 1,
    str_detect(flares_other, "emotion") ~ 1,
    str_detect(flares_other, "physical") ~ 1,
    str_detect(flares_other, "too much") ~ 1,
    str_detect(flares_other, "how to get better the neuro issues") ~ 0,
    str_detect(flares_other, "time in the sun") ~ 0,
    str_detect(flares_other, "a lot of drugs") ~ 0,
   str_detect(flares_other, "too much alcohol") ~ 0,
   str_detect(flares_other, "overdoing") ~ 1,
   str_detect(flares_other, "too long") ~ 1,
   str_detect(flares_other, "periods of business") ~ 1,
   str_detect(flares_other, "hard work") ~ 1, 
   str_detect(flares_other, "daily tasks any extra tasks") ~ 1,
   str_detect(flares_other, "learn or concentrate") ~ 1, 
   str_detect(flares_other, "pushing myself") ~ 1,
   str_detect(flares_other, "manual work") ~ 1, 
   str_detect(flares_other, "over exerti") ~ 1,
   str_detect(flares_other, "exercising or cleaning") ~ 1,
   str_detect(flares_other, "not pacing myself") ~ 1, 
   str_detect(flares_other, "beyond my energy") ~ 1, 
   str_detect(flares_other, "walking, talking, thinking") ~ 1, 
   str_detect(flares_other, "exertion") ~ 1, 
   str_detect(flares_other, "outside my energy") ~ 1, 
   str_detect(flares_other, "activity/stress") ~ 1, 
   str_detect(flares_other, "doing things at times") ~ 1, 
   str_detect(flares_other, "prolonged high effort") ~ 1, 
   str_detect(flares_other, "trying to be active") ~ 1, 
   str_detect(flares_other, "caring commitments") ~ 1,
   str_detect(flares_other, "excess activity") ~ 1,
   str_detect(flares_other, "pushing activities") ~ 1,
   str_detect(flares_other, "overuse/fatigue") ~ 1,
   str_detect(flares_other, "being upright") ~ 1,
   str_detect(flares_other, "workload") ~ 1,
  flares_other =="activity" ~ 1, 
  str_detect(flares_other, "daily activities") ~ 1,
  str_detect(flares_other, "not pacing") ~ 1,
    TRUE ~ 0
  )) %>%
  #infection
  mutate(flares___10 = case_when(   
    str_detect(flares_other, "get a cold") ~ 1, 
    str_detect(flares_other, "illness") ~ 1, 
    str_detect(flares_other, "virus") ~ 1, 
    str_detect(flares_other, "infection") ~ 1, 
    str_detect(flares_other, "sinusitis") ~ 1,
    TRUE ~ 0
  )) %>%
  #seasonal/weather changes
  mutate(flares___11 = case_when(   
    str_detect(flares_other, "changes") ~ 1, 
    str_detect(flares_other, "heat intolerance, competing sensory") ~ 0, 
    str_detect(flares_other, "humidity") ~ 1, 
    str_detect(flares_other, "weather") ~ 1, 
    str_detect(flares_other, "drops in air pressure") ~ 1, 
    str_detect(flares_other, "temperature") ~ 1, 
    str_detect(flares_other, "cold triggers") ~ 1,
    str_detect(flares_other, "barometric pressure") ~ 1,
    str_detect(flares_other, "tiredness and cold") ~ 1,
    str_detect(flares_other, "june so heat") ~ 1,
    TRUE ~ 0
  )) %>%
  #lack of sleep
  mutate(flares___12 = case_when(   
    str_detect(flares_other, "sleep") ~ 1, 
    str_detect(flares_other, "insomnia") ~ 1, 
    str_detect(flares_other, "tiredness") ~ 1, 
    TRUE ~ 0
  )) %>%
  #environmental triggers
  mutate(flares___13 = case_when(   
    str_detect(flares_other, "smells") ~ 1, 
    str_detect(flares_other, "sound") ~ 1, 
    str_detect(flares_other, "allergens") ~ 1, 
    str_detect(flares_other, "mould") ~ 1,
    str_detect(flares_other, "insect") ~ 1,
    str_detect(flares_other, "smoke") ~ 1,
    str_detect(flares_other, "dust") ~ 1,
    str_detect(flares_other, "fragrance") ~ 1,
    str_detect(flares_other, "chemical") ~ 1,
    str_detect(flares_other, "large chemical doses") ~ 0,
    str_detect(flares_other, "irritated") ~ 1,
    str_detect(flares_other, "becoming cold") ~ 1,
    str_detect(flares_other, "wasp bites") ~ 1,
    str_detect(flares_other, "crowds, noise") ~ 1,
    str_detect(flares_other, "heat and cold") ~ 1,
    str_detect(flares_other, "heat or cold") ~ 1,
    str_detect(flares_other, "heat & cold") ~ 1,
    flares_other == "heat" ~ 1,
    str_detect(flares_other, "heat/heating") ~ 1,
    str_detect(flares_other, "viruses heat hormones") ~ 1,
    str_detect(flares_other, "/ overheating") ~ 1,
    TRUE ~ 0
  )) %>%
  #pregnancy
  mutate(flares___14 = case_when(   
    str_detect(flares_other, "preg") ~ 1, 
    TRUE ~ 0
  )) %>%
  #menstrualtion
  mutate(flares___15 = case_when(   
    str_detect(flares_other, "menst") ~ 1, 
    str_detect(flares_other, "periods certain kinds") ~ 1, 
    TRUE ~ 0
  )) %>%
  #vaccination
  mutate(flares___16 = case_when(   
    str_detect(flares_other, "vaccin") ~ 1, 
    str_detect(flares_other, "jab") ~ 1, 
    TRUE ~ 0
  ))%>%
  #hormonal
  mutate(flares___17 = case_when(  
    str_detect(flares_other, "hormon") ~ 1,
    str_detect(flares_other, "estradiol") ~ 1, 
    TRUE ~ 0
  ))

label(df.management$flares___7) = "Food"
label(df.management$flares___8) = "Alcohol"
label(df.management$flares___9) = "Exertion physical/mental"
label(df.management$flares___10) = "Infection"
label(df.management$flares___11) = "Seasonal/weather changes"
label(df.management$flares___12) = "Lack of sleep"
label(df.management$flares___13) = "Environmental triggers"
label(df.management$flares___14) = "Pregnancy"
label(df.management$flares___15) = "Menstrual cycle"
label(df.management$flares___16) = "Vaccination"
label(df.management$flares___17) = "Hormones"




# label(df.diet$diet___1)="Nutritional supplements"
# label(df.diet$diet___2)="Gluten free"
# label(df.diet$diet___3)="Low FODMAP"
# label(df.diet$diet___4)="Vegan"
# label(df.diet$diet___5)="Sugar free or low carbohydrate"
# label(df.diet$diet___6)="Ketogenic"
# label(df.diet$diet___7)="None"
# label(df.diet$diet___8)="Other"
diet.labels <- df.diet %>%
  get_label 
colnames(df.diet) <- diet.labels
diet.count <- df.diet %>% 
  summarise_if(is.numeric, sum) %>% 
  transpose()
diet.count.df <- as.data.frame(do.call(cbind, diet.count)) %>% 
  rownames_to_column(.) %>% 
  rename(., word = rowname, freq = V1)
diet.count.df$freq <- as.numeric(as.character(diet.count.df$freq))         


diet.count.ord <- diet.count.df %>%
  #filter(word != "None") %>%
  #  filter(freq > 5)%>%
  rename(., Diet = word) %>%
  arrange(., desc(freq))
ggplot(diet.count.ord, aes(x = freq, y = Diet)) +
  geom_col(fill = viridis(8)) +
  scale_y_discrete(limits=rev(diet.count.ord$Diet)) +
  labs(title = "Types of diets employed",
       x = NULL, #"Number of respondents",
       y = NULL) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 520)) +
  theme_bw() +
  theme(plot.title=element_text(size=35, face="bold"),
        axis.title=element_text(size=35),
        axis.text=element_text(size=35, face="bold"))
