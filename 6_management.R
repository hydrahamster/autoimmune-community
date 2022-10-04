#treatments, diet, specialists, triggers, trigers
#load helper file
source("0_pachages-function.R")

#load data
data.clean <- readRDS("AD-data-clean.rds")
df.ad <- readRDS("df-ad.rds")
df.sumstats <- readRDS("AD-sumstats.rds")

### to check which str_detect other sections aren't picked up yet
# test.flares <- data.clean %>%
#   select(flares_other) %>%
#   mutate(flares_other = as.character(flares_other))%>%
#   mutate_at(vars(c(flares_other)), ~ case_when(
#     str_detect(flares_other, "chemotherapy") ~ NA_character_,
#     str_detect(flares_other, "food") ~ NA_character_,
#     str_detect(flares_other, "eating badly") ~ NA_character_,
#     str_detect(flares_other, "eating") ~ NA_character_,
#     str_detect(flares_other, "running/ overheating") ~ NA_character_,
#     str_detect(flares_other, "had a pacemaker inserted") ~ NA_character_,
#     str_detect(flares_other, "garlic") ~ NA_character_,
#     str_detect(flares_other, "sugar intake") ~ NA_character_,
#     str_detect(flares_other, "ingestion") ~ NA_character_,
#     str_detect(flares_other, "diet") ~ NA_character_,
#     str_detect(flares_other, "alcohol") ~ NA_character_,
#     str_detect(flares_other, "mental emtional activity") ~ NA_character_,
#     str_detect(flares_other, "over-exertion") ~ NA_character_,
#     str_detect(flares_other, "mental stress") ~ NA_character_,
#     str_detect(flares_other, "increased activity") ~ NA_character_,
#     str_detect(flares_other, "post exertion") ~ NA_character_,
#     str_detect(flares_other, "exercise") ~ NA_character_,
#     str_detect(flares_other, "doing to much") ~ NA_character_,
#     str_detect(flares_other, "movement, walking") ~ NA_character_,
#     str_detect(flares_other, "over extension") ~ NA_character_,
#     str_detect(flares_other, "energy expenditure") ~ NA_character_,
#     str_detect(flares_other, "running") ~ NA_character_,
#     str_detect(flares_other, "emotion") ~ NA_character_,
#     str_detect(flares_other, "get a cold") ~ NA_character_,
#     str_detect(flares_other, "illness") ~ NA_character_,
#     str_detect(flares_other, "virus") ~ NA_character_,
#     str_detect(flares_other, "infection") ~ NA_character_,
#     str_detect(flares_other, "changes") ~ NA_character_,
#     str_detect(flares_other, "heat intolerance, competing sensory") ~ NA_character_,
#     str_detect(flares_other, "humidity") ~ NA_character_,
#     str_detect(flares_other, "sleep") ~ NA_character_,
#     str_detect(flares_other, "insomnia") ~ NA_character_,
#     str_detect(flares_other, "smells") ~ NA_character_,
#     str_detect(flares_other, "sound") ~ NA_character_,
#     str_detect(flares_other, "allergens") ~ NA_character_,
#     str_detect(flares_other, "mould") ~ NA_character_,
#     str_detect(flares_other, "insect") ~ NA_character_,
#     str_detect(flares_other, "smoke") ~ NA_character_,
#     str_detect(flares_other, "dust") ~ NA_character_,
#     str_detect(flares_other, "preg") ~ NA_character_,
#     str_detect(flares_other, "menst") ~ NA_character_,
# str_detect(flares_other, "lot of drugs") ~ NA_character_,
# str_detect(flares_other, "drugs/medications") ~ NA_character_,
# str_detect(flares_other, "prescribed drugs") ~ NA_character_,
# str_detect(flares_other, "physical") ~ NA_character_,
# str_detect(flares_other, "fragrance") ~ NA_character_,
# str_detect(flares_other, "weather") ~ NA_character_,
# str_detect(flares_other, "too much") ~ NA_character_,
# str_detect(flares_other, "chemical") ~ NA_character_,
# str_detect(flares_other, "overdoing") ~ NA_character_,
# str_detect(flares_other, "too long") ~ NA_character_,
#     TRUE ~ .)) %>%
#   unique()

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
  #
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
  )) %>%
  # Other triggers
  mutate(flares___18 = case_when(
    flares___5 == 1 ~ 1,
    str_detect(flares_other, "vaccin") ~ 0, 
    str_detect(flares_other, "jab") ~ 0, 
    str_detect(flares_other, "menst") ~ 0, 
    str_detect(flares_other, "periods certain kinds") ~ 0, 
    str_detect(flares_other, "preg") ~ 0, 
    str_detect(flares_other, "irritated") ~ 0,
    str_detect(flares_other, "becoming cold") ~ 0,
    str_detect(flares_other, "wasp bites") ~ 0,
    str_detect(flares_other, "crowds, noise") ~ 0,
    str_detect(flares_other, "heat and cold") ~ 0,
    str_detect(flares_other, "heat or cold") ~ 0,
    str_detect(flares_other, "heat & cold") ~ 0,
    flares_other == "heat" ~ 0,
    str_detect(flares_other, "heat/heating") ~ 0,
    str_detect(flares_other, "viruses heat hormones") ~ 0,
    str_detect(flares_other, "/ overheating") ~ 0,
    str_detect(flares_other, "smells") ~ 0, 
    str_detect(flares_other, "sound") ~ 0, 
    str_detect(flares_other, "allergens") ~ 0, 
    str_detect(flares_other, "mould") ~ 0,
    str_detect(flares_other, "insect") ~ 0,
    str_detect(flares_other, "smoke") ~ 0,
    str_detect(flares_other, "dust") ~ 0,
    str_detect(flares_other, "fragrance") ~ 0,
    str_detect(flares_other, "chemical") ~ 0,
    str_detect(flares_other, "sleep") ~ 0, 
    str_detect(flares_other, "insomnia") ~ 0, 
    str_detect(flares_other, "tiredness") ~ 0, 
    str_detect(flares_other, "humidity") ~ 0, 
    str_detect(flares_other, "weather") ~ 0, 
    str_detect(flares_other, "drops in air pressure") ~ 0, 
    str_detect(flares_other, "temperature") ~ 0, 
    str_detect(flares_other, "cold triggers") ~ 0,
    str_detect(flares_other, "barometric pressure") ~ 0,
    str_detect(flares_other, "tiredness and cold") ~ 0,
    str_detect(flares_other, "june so heat") ~ 0,
    str_detect(flares_other, "changes") ~ 0, 
    str_detect(flares_other, "get a cold") ~ 0, 
    str_detect(flares_other, "illness") ~ 0, 
    str_detect(flares_other, "virus") ~ 0, 
    str_detect(flares_other, "infection") ~ 0, 
    str_detect(flares_other, "sinusitis") ~ 0,
    str_detect(flares_other, "overdoing") ~ 0,
    str_detect(flares_other, "too long") ~ 0,
    str_detect(flares_other, "periods of business") ~ 0,
    str_detect(flares_other, "hard work") ~ 0, 
    str_detect(flares_other, "daily tasks any extra tasks") ~ 0,
    str_detect(flares_other, "learn or concentrate") ~ 0, 
    str_detect(flares_other, "pushing myself") ~ 0,
    str_detect(flares_other, "manual work") ~ 0, 
    str_detect(flares_other, "over exerti") ~ 0,
    str_detect(flares_other, "exercising or cleaning") ~ 0,
    str_detect(flares_other, "not pacing myself") ~ 0, 
    str_detect(flares_other, "beyond my energy") ~ 0, 
    str_detect(flares_other, "walking, talking, thinking") ~ 0, 
    str_detect(flares_other, "exertion") ~ 0, 
    str_detect(flares_other, "outside my energy") ~ 0, 
    str_detect(flares_other, "activity/stress") ~ 0, 
    str_detect(flares_other, "doing things at times") ~ 0, 
    str_detect(flares_other, "prolonged high effort") ~ 0, 
    str_detect(flares_other, "trying to be active") ~ 0, 
    str_detect(flares_other, "caring commitments") ~ 0,
    str_detect(flares_other, "excess activity") ~ 0,
    str_detect(flares_other, "pushing activities") ~ 0,
    str_detect(flares_other, "overuse/fatigue") ~ 0,
    str_detect(flares_other, "being upright") ~ 0,
    str_detect(flares_other, "workload") ~ 0,
    flares_other =="activity" ~ 0, 
    str_detect(flares_other, "daily activities") ~ 0,
    str_detect(flares_other, "not pacing") ~ 0,
    str_detect(flares_other, "mental emtional activity") ~ 0, 
    str_detect(flares_other, "over-exertion") ~ 0, 
    str_detect(flares_other, "mental stress") ~ 0, 
    str_detect(flares_other, "increased activity") ~ 0, 
    str_detect(flares_other, "post exertion") ~ 0, 
    str_detect(flares_other, "exercise") ~ 0, 
    str_detect(flares_other, "doing to much") ~ 0,
    str_detect(flares_other, "movement, walking") ~ 0,
    str_detect(flares_other, "over extension") ~ 0,
    str_detect(flares_other, "energy expenditure") ~ 0,
    str_detect(flares_other, "running") ~ 0,
    str_detect(flares_other, "emotion") ~ 0,
    str_detect(flares_other, "physical") ~ 0,
    str_detect(flares_other, "too much") ~ 0,
    str_detect(flares_other, "alcohol") ~ 0,    
    str_detect(flares_other, "red wine ") ~ 0, 
    str_detect(flares_other, "garlic") ~ 0, 
    str_detect(flares_other, "sugar intake") ~ 0, 
    str_detect(flares_other, "ingestion") ~ 0, 
    str_detect(flares_other, "diet") ~ 0,  
    str_detect(flares_other, "gluten") ~ 0, 
    str_detect(flares_other, "dairy products") ~ 0, 
    str_detect(flares_other, "caffeine") ~ 0, 
    str_detect(flares_other, "red meat") ~ 0,
    str_detect(flares_other, "wheat") ~ 0,
    str_detect(flares_other, "food") ~ 0, 
    str_detect(flares_other, "eating badly") ~ 0, 
    str_detect(flares_other, "eating") ~ 0, 
    flares_other == "stress" ~ 0,
    str_detect(flares_other, "worsened by stress") ~ 0,
    str_detect(flares_other, "stress") ~ 0,
    str_detect(flares_other, "sometimes dont know") ~ 0,
    str_detect(flares_other, "not figured out why") ~ 0,
    str_detect(flares_other, "unknown") ~ 0,
    str_detect(flares_other, "or nothing at all") ~ 0,
    str_detect(flares_other, "chemotherapy") ~ 0,
    str_detect(flares_other, "lot of drugs") ~ 0,
    str_detect(flares_other, "drugs/medications") ~ 0,
    str_detect(flares_other, "prescribed drugs") ~ 0,
    str_detect(flares_other, "large chemical doses") ~ 0,
    flares_other == "medication" ~ 0,
    str_detect(flares_other, "i was on them for too ling") ~ 0,
    str_detect(flares_other, "medication needing adjusting") ~ 0,
    str_detect(flares_other, "anaesthetics") ~ 0,
    TRUE ~ 0
  )) %>%
  mutate_at(vars(c(specialist___9)), ~ case_when(
    specialist_other == "no treatment other than gluten free diet"~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(specialist___2, specialist___6, specialist___8)), ~ case_when(
    specialist_other == "ive had diagnosis but not further treatment from gastroenterologist, rheumatologist, and immunologist i asked to be referred to an endocrinologist for review of menory and bowel problems caused by hashimotos and menopause, but my gp said it wasnt necessary"~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(specialist___4)), ~ case_when(
    str_detect(specialist_other, "neurosurgeon") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(specialist___5)), ~ case_when(
    str_detect(specialist_other,"haematologist") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(specialist___2)), ~ case_when(
    str_detect(specialist_other,"rheumatoid specialist") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(specialist___3)), ~ case_when(
    specialist_other =="being managed by endocrinolgy nurse practitioner in public health"~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(specialist___2)), ~ case_when(
    str_detect(specialist_other,"rheumatologist") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(specialist___6)), ~ case_when(
    specialist_other =="gastrointestinal and liver specialist"~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(specialist___3)), ~ case_when(
    str_detect(specialist_other,"hormone specialist") ~ 1,
    TRUE ~ .
  )) %>%
  #cardiologist
  mutate(specialist___11 = case_when(  
    str_detect(specialist_other, "cardio") ~ 1,
    str_detect(specialist_other, "cardiovascular and autonomic issues") ~ 0, 
    str_detect(specialist_other, "seeing a cardiologist is not drug seeking") ~ 0,
    str_detect(specialist_other, "heart specialist") ~ 0,
    str_detect(specialist_other, "cardiac") ~ 0,
    TRUE ~ 0
  )) %>%
  #Allergy specialist
  mutate(specialist___12 = case_when(  
    str_detect(specialist_other, "allerg") ~ 1,
    TRUE ~ 0
  )) %>%
  #Otolaryngologist
  mutate(specialist___13 = case_when(  
    str_detect(specialist_other, "ent") ~ 1,
    str_detect(specialist_other, "dentist") ~ 0,
    str_detect(specialist_other, "orthomolecular") ~ 0,
    str_detect(specialist_other, "lung function specialist for") ~ 0,
    str_detect(specialist_other, "currently seeing an integrative gp") ~ 0,
    str_detect(specialist_other, "potential sleep apnoea") ~ 0,
    str_detect(specialist_other, "colonics") ~ 0,
    str_detect(specialist_other, "central sleep apnoea") ~ 0,
    str_detect(specialist_other, "prosthodontist") ~ 0,
    str_detect(specialist_other, "appointment with") ~ 0,
    str_detect(specialist_other, "a gastroenterologist for") ~ 0,
    str_detect(specialist_other, "ear nose") ~ 1, 
    str_detect(specialist_other, "ear, nose") ~ 1,
    str_detect(specialist_other, "otolaryngologist") ~ 1,
    TRUE ~ 0
  )) %>%
  #Gynocologist
  mutate(specialist___14 = case_when(  
    str_detect(specialist_other, "gyn") ~ 1,
    str_detect(specialist_other, "endometriosis specialist") ~ 1, 
    TRUE ~ 0
  )) %>%
  #Ophthalmologist
  mutate(specialist___15 = case_when(  
    str_detect(specialist_other, "eye") ~ 1,
    str_detect(specialist_other, "ophtha") ~ 1, 
    str_detect(specialist_other, "opthal") ~ 1,
    TRUE ~ 0
  )) %>%
  #Pain specialist
  mutate(specialist___16 = case_when(  
    str_detect(specialist_other, "pain") ~ 1,
    str_detect(specialist_other, "bertolotti") ~ 0, 
    str_detect(specialist_other, "psychiatrist for pain medication") ~ 0,
    str_detect(specialist_other, "attended the rpa") ~ 0,
    str_detect(specialist_other, "a medication for pain management") ~ 0,
    str_detect(specialist_other, "neurologist is for brain tumours") ~ 0,
    str_detect(specialist_other, "ketamine") ~ 1,
    TRUE ~ 0
  )) %>%
  #Sleep specialist
  mutate(specialist___17 = case_when(  
    str_detect(specialist_other, "sleep") ~ 1,
    TRUE ~ 0
  )) %>%  
  #Pulmonologist
  mutate(specialist___18 = case_when(  
    str_detect(specialist_other, "pulmonol") ~ 1,
    str_detect(specialist_other, "lung") ~ 1, 
    str_detect(specialist_other, "respir") ~ 1,
    str_detect(specialist_other, "thoracic") ~ 1,
    TRUE ~ 0
  )) %>%  
  #Urologist
  mutate(specialist___19 = case_when(  
    str_detect(specialist_other, "urol") ~ 1,
    str_detect(specialist_other, "neurol") ~ 0, 
    TRUE ~ 0
  )) %>%  
    #Psychologist
    mutate(specialist___20 = case_when(  
      str_detect(specialist_other, "psychologist") ~ 1,
      str_detect(specialist_other, "psycologist") ~ 1, 
      str_detect(specialist_other, "phycologist") ~ 1,
      TRUE ~ 0
    )) %>%  
    #Psychiatrist
    mutate(specialist___21 = case_when(  
      str_detect(specialist_other, "psychiatrist") ~ 1,
      TRUE ~ 0
    )) %>%  
  #Other
  mutate(specialist___22 = case_when(  
    specialist___10 == 1 ~ 1,
    str_detect(specialist_other, "cardio") ~ 0,
    str_detect(specialist_other, "allerg") ~ 0,
    str_detect(specialist_other, "ent") ~ 0,
    str_detect(specialist_other, "ear nose") ~ 0, 
    str_detect(specialist_other, "ear, nose") ~ 0,
    str_detect(specialist_other, "otolaryngologist") ~ 0,
    str_detect(specialist_other, "gyn") ~ 0,
    str_detect(specialist_other, "endometriosis specialist") ~ 0, 
    str_detect(specialist_other, "eye") ~ 0,
    str_detect(specialist_other, "ophtha") ~ 0, 
    str_detect(specialist_other, "opthal") ~ 0,
    str_detect(specialist_other, "pain") ~ 0,
    str_detect(specialist_other, "ketamine") ~ 0,
    str_detect(specialist_other, "sleep") ~ 0,
    str_detect(specialist_other, "pulmonol") ~ 0,
    str_detect(specialist_other, "lung") ~ 0, 
    str_detect(specialist_other, "respir") ~ 0,
    str_detect(specialist_other, "thoracic") ~ 0,
    str_detect(specialist_other, "urol") ~ 0,
    str_detect(specialist_other, "psychologist") ~ 0,
    str_detect(specialist_other, "psycologist") ~ 0, 
    str_detect(specialist_other, "phycologist") ~ 0,
    str_detect(specialist_other, "psychiatrist") ~ 0,
    TRUE ~ 0
  )) %>%
  # Low/no dairy/lactose
  mutate(diet___9 = case_when(  
    str_detect(diet_other, "dairy") ~ 1,
    str_detect(diet_other, "without any real improvement") ~ 0,
    str_detect(diet_other, "dairy as it didnt") ~ 0,
    TRUE ~ 0
  )) %>%
  # Vegetarian
  mutate(diet___10 = case_when(  
    str_detect(diet_other, "vegetar") ~ 1,
    TRUE ~ 0
  )) %>%  
  # Low processed foods
  mutate(diet___11 = case_when(  
    str_detect(diet_other, "process") ~ 1,
    str_detect(diet_other, "manufactured") ~ 1,
    str_detect(diet_other, "refined") ~ 1,
    str_detect(diet_other, "junk") ~ 1,
    TRUE ~ 0
  )) %>%  
  # Low/no meat
  mutate(diet___12 = case_when(  
    str_detect(diet_other, "meat") ~ 1,
    str_detect(diet_other, "organic meat, ") ~ 0,
    str_detect(diet_other, "carnivore (meat and eggs)") ~ 0,
    str_detect(diet_other, "mammalian meat") ~ 0,
    TRUE ~ 0
  )) %>%  
  # Other
  mutate(diet___13 = case_when(  
    diet___8 == 1 ~ 1,
    str_detect(diet_other, "dairy") ~ 0,
    str_detect(diet_other, "meat") ~ 0,
    str_detect(diet_other, "process") ~ 0,
    str_detect(diet_other, "manufactured") ~ 0,
    str_detect(diet_other, "refined") ~ 0,
    str_detect(diet_other, "junk") ~ 0,
    str_detect(diet_other, "vegetar") ~ 0,
    TRUE ~ 0
  )) #%>%
#   therapy_other:
#   physi
#   osteo
# podi
# chiro
# diet
# myo
# accup/acup
# pilate
# naturo

#%>%  #
  mutate(diet___ = case_when(  
    str_detect(diet_other, "") ~ 1,
    str_detect(diet_other, "") ~ 1,
    str_detect(diet_other, "") ~ 1,
    TRUE ~ 0
  )) %>%  #
  mutate(diet___ = case_when(  
    str_detect(diet_other, "") ~ 1,
    str_detect(diet_other, "") ~ 1,
    str_detect(diet_other, "") ~ 1,
    TRUE ~ 0
  )) %>%
# specialist_other To therapy section: "audiologist", "exercise physiologist", "physiotherapist", "physio", "speech pathologist", "sports physician", "osteopath", "physical therapy", "physio", "sports medicine doctor"
  #drop other sections and old other options
  select(-c(diet_other, diet___8, specialist_other, specialist___10, flares_other, flares___5))

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
label(df.management$flares___18) = "Other"
label(df.management$specialist___11) = "Cardiologist"
label(df.management$specialist___12) = "Allergy specialist"
label(df.management$specialist___13 ) = "Otolaryngologist"
label(df.management$specialist___14 ) = "Gynocologist"
label(df.management$specialist___15 ) = "Ophthalmologist"
label(df.management$specialist___16 ) = "Pain specialist"
label(df.management$specialist___17 ) = "Sleep specialist"
label(df.management$specialist___18 ) = "Pulmonologist"
label(df.management$specialist___19 ) = "Urologist"
label(df.management$specialist___20 ) = "Psychologist"
label(df.management$specialist___21 ) = "Psychiatrist"
label(df.management$specialist___22 ) = "Other"
label(df.management$specialist___11) = "Cardiologist"
label(df.management$specialist___12) = "Allergy specialist"
label(df.management$specialist___13 ) = "Otolaryngologist"
label(df.management$specialist___14 ) = "Gynocologist"
label(df.management$specialist___15 ) = "Ophthalmologist"
label(df.management$specialist___16 ) = "Pain specialist"
label(df.management$specialist___17 ) = "Sleep specialist"
label(df.management$specialist___18 ) = "Pulmonologist"
label(df.management$specialist___19 ) = "Urologist"
label(df.management$specialist___20 ) = "Psychologist"
label(df.management$specialist___21 ) = "Psychiatrist"
label(df.management$specialist___22 ) = "Other specialist"
label(df.management$diet___9) = "Low/no dairy/lactose"
label(df.management$diet___10) = "Vegetarian"
label(df.management$diet___11) = "Low processed foods"
label(df.management$diet___12) = "Low/no meat"
label(df.management$diet___13) = "Other"



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
