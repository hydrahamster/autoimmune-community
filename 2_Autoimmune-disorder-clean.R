#load helper file
source("0_pachages-function.R")

#load data
data.clean <- readRDS("AD-data-clean.rds")


############# uncomment when needed dealing with 'other' AD stuff
#extract a list of unique other ADs to replace
# other.ad <- distinct(data.clean, autoimmune_id_other)
# other.ad <- arrange(other.ad, autoimmune_id_other)
# #read in prev unique AD descriptions and work out which are new
# other.ad.old <- read.csv("/home/anna/postdoc/spoonie/survey-analysis/AD-other-R.txt", sep = "\t") 
# other.ad <- other.ad %>%
#   filter(!other.ad$autoimmune_id_other %in% other.ad.old$autoimmune_id_other)
# #add to AD code file at /home/anna/postdoc/spoonie/autoimmunity_community/bin/other-AD-codes.xlsx
# write.table(x = other.ad,
#            file = "1-22_AD-other-R.xlsx",
#            sep = "\t")

#### update AD with new entries
 other.autoimmune <- data.clean %>%
    select(autoimmune_id_other) %>%
    filter_all(any_vars(str_detect(., pattern = ""))) %>%
   arrange(., autoimmune_id_other) %>%
    distinct(.)
write.table(x = other.autoimmune,
             file = "ADs-other-all_final.txt",
             row.names = FALSE,
             sep = "\t")
# then run new-AD-search.sh

#### update comorbs with new entries
other.chron <- data.clean %>%
   select(otherchronic) %>%
   filter_all(any_vars(str_detect(., pattern = ""))) %>%
  arrange(., otherchronic) %>%
   distinct(.)
write.table(x = other.chron,
             file = "chronic-other-all_final.txt",
             row.names = FALSE,
             sep = "\t")
# then run new-chronic-search.sh

#### updatedrugs with new entries
#  other.drug <- data.clean %>%
#     select(drug_other_other) %>%
#     filter_all(any_vars(str_detect(., pattern = ""))) %>%
#    arrange(., drug_other_other) %>%
#     distinct(.)
# write.table(x = other.drug,
#              file = "drugs-other-all_1-22.txt",
#              row.names = FALSE,
#              sep = "\t")
# then run new-?-search.sh

df.ad <- data.clean %>% 
  select(record_id, diseasestat, autoimmune_id_other, otherchronic, chronic___1:chronic___8, autoimmune_id___1:autoimmune_id___45) %>%
  mutate(diseasestat = as.numeric(diseasestat)) %>%
  mutate(across(chronic___1:autoimmune_id___45, .fns = as.numeric))
#check if things should Actually be in control or not if 'other' doesn't match
# Q <- df.ad %>%
#   filter(autoimmune_id_other == "self diagnosed that 'accepted'/recognised by medical professionals ")

df.ad <- df.ad %>% 
  mutate(autoimmune_id___40 = ifelse(autoimmune_id_other == "sporadic inclusion body myositis", 1, autoimmune_id___40)) %>%
  mutate(autoimmune_id___6 = ifelse(autoimmune_id_other == "mixed connective tissue disease - formally diagnosed coeliac gene and antibody positive but biopsy negative so its a grey area but cannot have gluten at all, like a coeliac possible pernicious anaemia also but not properly tested due to doctors ignorance", 1, autoimmune_id___6)) %>%
  mutate(autoimmune_id___34 = ifelse(autoimmune_id_other == "survey wont let me continue without filling this in diagnosed with psoriatic arthritis", 1, autoimmune_id___34)) %>%
  mutate(autoimmune_id___15 = ifelse(autoimmune_id_other == "motor axonal neuropathy; treated & now in remission" |
                                       autoimmune_id_other ==  "acute axonal neuropath due to c-anca vasculitis", 1, autoimmune_id___15)) %>%
  mutate(autoimmune_id___19 = ifelse(autoimmune_id_other == "bergers disease", 1, autoimmune_id___19)) %>%
  mutate(autoimmune_id___31 = ifelse(autoimmune_id_other == "polymyositis", 1, autoimmune_id___31)) %>%
  mutate(autoimmune_id___22 = ifelse(autoimmune_id_other == "sle lupus", 1, autoimmune_id___22)) %>%
  mutate(autoimmune_id___6 = ifelse(autoimmune_id_other == "was also diagnosed at one point with autoimmune hypophysitis - 2005 by a specialist in endocrinology will mostly refer to coeliac disease in answers to questions", 1, autoimmune_id___6)) %>%
  mutate(autoimmune_id___38 = ifelse(autoimmune_id_other == "i havent been formally diagnosed but my rheum suspects either sjogrenns or heds my maternal grandmother had sjogrenns and my mother has fibro but i have been waiting for lockdown to lessen before i can go back to chase the diag", 1, autoimmune_id___38)) %>%
  mutate(autoimmune_id___46 = ifelse(autoimmune_id_other == "addisions disease (adrenal insufficiency)" | 
                                       autoimmune_id_other == "addisons" | 
                                       autoimmune_id_other == "addisons diease" | 
                                       autoimmune_id_other == "addisons disease"| 
                                       autoimmune_id_other == "addisons disease ( pai )" | 
                                       autoimmune_id_other == "addisons disease- primary adrenal insufficiency" | 
                                       autoimmune_id_other == "addisonss disease" | 
                                       autoimmune_id_other == "adisons desease" , 1, 0)) %>%
  mutate(autoimmune_id___47 = ifelse(autoimmune_id_other == "alopecia areata", 1, 0)) %>%
  mutate(autoimmune_id___48 = ifelse(autoimmune_id_other == "alopecia areata universalis" | 
                                       autoimmune_id_other == "alopecia universalis" | 
                                       autoimmune_id_other == "alopecia universalis have been tested and told i have the lupus gene and hemachromatosis gene", 1, 0)) %>%
  mutate(autoimmune_id___49 = ifelse(autoimmune_id_other == "alopecia totalis", 1, 0)) %>%
  mutate(autoimmune_id___50 = ifelse(autoimmune_id_other == "antiphospholipid syndrome" | 
                                       autoimmune_id_other == "antiphospholipid syndrome i havent heard of it being called primary antiphospholipid syndrome - although i did just look it up and that seems right", 1, 0)) %>%
  mutate(autoimmune_id___94 = ifelse(autoimmune_id_other == "autoimmune atrophic gastritis" |
                                       autoimmune_id_other == "autoimmune gastritis", 1, 0)) %>%
  mutate(autoimmune_id___51 = 0 ) %>%
  mutate(autoimmune_id___52 = ifelse(autoimmune_id_other == "autoimmune hepatitis" | 
                                       autoimmune_id_other == "autoimmune hepatitis (aih)" | 
                                       autoimmune_id_other == "autoimmune liver disease" |
                                       autoimmune_id_other == "auto immune hepatitis  barrett's disease", 1, 0)) %>% 
  mutate(autoimmune_id___53 = 0) %>% 
  mutate(autoimmune_id___54 = ifelse(autoimmune_id_other == "central nervous system vasculitis" | 
                                       autoimmune_id_other == "cerebral nervous vasculitis", 1, 0)) %>%
  mutate(autoimmune_id___55 = ifelse(autoimmune_id_other == "cfs" | 
                                       autoimmune_id_other == "cfs/me" | 
                                       autoimmune_id_other == "cfs/me (i dont consider it autoimmune but see you include it)" | 
                                       autoimmune_id_other == "chronic fatigue sydrome" | 
                                       autoimmune_id_other == "chronic fatigue syndrome" | 
                                       autoimmune_id_other == "chronic fatigue syndrome/ myalgic encephalomyelitis" | 
                                       autoimmune_id_other == "me / cfs" | 
                                       autoimmune_id_other == "me / cfs myalgic encephalomyelitis" | 
                                       autoimmune_id_other == "me, myalgic encephalomyelitis" | 
                                       autoimmune_id_other == "me/cfs" | 
                                       autoimmune_id_other == "me/cfs - neuroimmune as studied at the national centre for neuroimmune and emerging diseases (ncned) latest brain scan research indicates that it is neuroinflamatory" | 
                                       autoimmune_id_other == "me/cfs (its still unclear as to its exact nature, but there seems to be an autoimmune element in at least part of the patient population)" | 
                                       autoimmune_id_other == "me/cfs i put physical impairment earlier but it referred to this i wasnt sure whether you meant this or something else" | 
                                       autoimmune_id_other == "me/chronic fatigue syndrome" | 
                                       autoimmune_id_other == "mecfs" | 
                                       autoimmune_id_other == "myalgic encephalomyelitis" | 
                                       autoimmune_id_other == "myalgic encephalomyelitis (international consensus criteria 2011)" | 
                                       autoimmune_id_other == "myalgic encephalomyelitis is now considered to be an auto immune disease" | 
                                       autoimmune_id_other == "myalgic encephalomyelitis/ chronic fatigue syndrome" | 
                                       autoimmune_id_other == "myalgic encephalomylitis" , 1, 0)) %>%
  mutate(autoimmune_id___56 = ifelse(autoimmune_id_other == "hereditary angioedema type 3 normal c1", 1, 0)) %>%
  mutate(autoimmune_id___57 =  0) %>%
  mutate(autoimmune_id___58 =  0) %>%
  mutate(autoimmune_id___59 =  0) %>%
  mutate(autoimmune_id___60 =  0) %>%
  mutate(autoimmune_id___61 = ifelse(autoimmune_id_other == "diverticulitis", 1, 0)) %>%#
  mutate(autoimmune_id___62 = ifelse(autoimmune_id_other == "egpa" |  
                                       autoimmune_id_other == "churg-strauss syndrome: or egpa or hyper-eosinophilic syndrome still unclear after 12 years", 1, 0)) %>%
  mutate(autoimmune_id___63 = ifelse(autoimmune_id_other == "dysautonomia", 1, 0)) %>%
  mutate(autoimmune_id___64 = ifelse(autoimmune_id_other == "fibromyalgia" | 
                                       autoimmune_id_other == "fibromyalgia symptoms that led to the diagnosis: 1 hair loss (not attributed to alopecia) 2 unexplained pain throughout body 3 emotional trauma through marriage breakup and divorce 4 stress from demanding job as a high school teacher 5 fatigue 6 multiple medical conditions that led to several surgical procedures (gall bladder removal, ankle reconstruction, uterine ablation, carpel tunnel surgery, removal of several recurring ganglions) 7 severe allergic reactions resulting in anaphylactic reactions and increased food sensitivity", 1, 0)) %>%
  mutate(autoimmune_id___65 = ifelse(autoimmune_id_other == "endometriosis", 1, 0)) %>%
  mutate(autoimmune_id___66 = ifelse(autoimmune_id_other == "raynaud phenomenon (primary) an autoimmune aetiology has been suggested, but other non autoimmune aetiologies has also been suggested, so unsure how to catalogue it!" | 
                                       autoimmune_id_other == "raynauds" | 
                                       autoimmune_id_other == "raynauds phenomena (secondary)" | 
                                       autoimmune_id_other == "raynauds - each of the 5 autoimmune diagnosed at differetnt times since 2000 by gastroenterologists, blood tests, specialists and gps - but you do not provide space to detail each one" | 
                                       autoimmune_id_other == "raynaulds", 1, 0)) %>%
 # mutate(autoimmune_id___67 = ifelse(autoimmune_id_other == "frontal fibrosing alopecia also have high ra factor but no arthritis", 1, 0)) %>%
  mutate(autoimmune_id___68 = ifelse(autoimmune_id_other == "functional neurological disorder" | 
                                       autoimmune_id_other == "functional neurological disorder fnd", 1, 0)) %>%
  mutate(autoimmune_id___69 = ifelse(autoimmune_id_other == "gastritis", 1, 0)) %>%
  mutate(autoimmune_id___70 = ifelse(autoimmune_id_other == "itp" | 
                                       autoimmune_id_other == "idiopathic thrombocytopenia purpura" |
                                       autoimmune_id_other == "immune thrombocytopenia purpura itp" |
                                       autoimmune_id_other == "autoimmune platelet disorder,", 1, 0)) %>%
  mutate(autoimmune_id___71 = ifelse(autoimmune_id_other == "inflammatory arthritis" | 
                                       autoimmune_id_other == "seronegative inflammatory arthritis; active rose river virus", 1, 0)) %>%
  mutate(autoimmune_id___72 = ifelse(autoimmune_id_other == "inflammatory bowel syndrome" |
                                       autoimmune_id_other == "ibs", 1, 0)) %>%
  mutate(autoimmune_id___73 = ifelse(autoimmune_id_other == "frontal fibrosing alopecia also have high ra factor but no arthritis", 1, 0))
  
df.ad <- df.ad %>% 
  mutate(autoimmune_id___74 = ifelse(autoimmune_id_other == "lichen sclerosis"| 
                                       autoimmune_id_other == "lichen sclerosus", 1, 0)) %>%
  mutate(autoimmune_id___75 = ifelse(autoimmune_id_other == "lymphocytic vasculitis", 1, 0)) %>%
  mutate(autoimmune_id___76 = ifelse(autoimmune_id_other == "mastocytosis", 1, 0)) %>%
  mutate(autoimmune_id___77 = ifelse(autoimmune_id_other == "mctd (mixed connective tissue disease)" | 
                                       autoimmune_id_other == "mixed connective tissue" | 
                                       autoimmune_id_other == "mixed connective tissue disease" | 
                                       autoimmune_id_other == "mixed connective tissue disease - formally diagnosed coeliac gene and antibody positive but biopsy negative so its a grey area but cannot have gluten at all, like a coeliac possible pernicious anaemia also but not properly tested due to doctors ignorance", 1, 0)) %>%
  mutate(autoimmune_id___78 = ifelse(autoimmune_id_other == "narcolepsy with cataplexy (assumed to be autoimmune, cause unknown)", 1, 0)) %>%
  mutate(autoimmune_id___79 = ifelse(autoimmune_id_other == "cerebral vasculitis", 1, 0)) %>%
  mutate(autoimmune_id___80 = ifelse(autoimmune_id_other == "chronic recurrent multifocal osteomyelitis", 1, 0)) %>%
  mutate(autoimmune_id___81 =  0) %>%
  mutate(autoimmune_id___82 = ifelse(autoimmune_id_other == "pandas and autoimmune issues based in the digestive tract", 1, 0)) %>%
  mutate(autoimmune_id___83 = ifelse(autoimmune_id_other == "pfapa (periodic fever, aphthous stomatitis, pharyngitis, adenitis)", 1, 0)) %>%
  mutate(autoimmune_id___84 = ifelse(autoimmune_id_other == "polycystic ovary syndrome", 1, 0)) %>%
  mutate(autoimmune_id___85 = ifelse(autoimmune_id_other == "poly myalgia rumatica" |
                                       autoimmune_id_other == "polymyalgia rheumatica", 1, 0)) %>%
  mutate(autoimmune_id___86 = 0) %>%
  mutate(autoimmune_id___87 = 0) %>%
  mutate(autoimmune_id___88 = ifelse(autoimmune_id_other == "prurigo nodularis", 1, 0)) %>%
  mutate(autoimmune_id___89 = ifelse(autoimmune_id_other == "psoriasis" | 
                                       autoimmune_id_other == "psoriasis (list only had psoriatic arthritis)" | 
                                       autoimmune_id_other == "psoriasis ive been told i have ra and psa"| 
                                       autoimmune_id_other == "inverse psoriasis ", 1, 0)) %>%
  mutate(autoimmune_id___90 = ifelse(autoimmune_id_other == "clinically isolated syndrome", 1,  0)) %>%
  mutate(autoimmune_id___91 =  0) %>%
  mutate(autoimmune_id___92 =  0) %>%
  mutate(autoimmune_id___93 =  0) %>%
  mutate(autoimmune_id___94 =  0) %>%
  mutate(autoimmune_id___95 = ifelse(autoimmune_id_other == "pyoderma gangrenosis", 1, 0)) %>%
  mutate(autoimmune_id___96 = ifelse(autoimmune_id_other == "rheumatic fever", 1, 0)) %>%
  #mutate(autoimmune_id___97 =  0) %>%
  mutate(autoimmune_id___98 =  0) %>%
  mutate(autoimmune_id___99 =  0) %>%
  mutate(autoimmune_id___100 = 0) %>%
  mutate(autoimmune_id___101 = 0) %>%
  mutate(autoimmune_id___102 = 0) %>%
  mutate(autoimmune_id___103 = ifelse(autoimmune_id_other == "systemic small vessel vasculitis", 1, 0)) %>%
  mutate(autoimmune_id___104 = ifelse(autoimmune_id_other == "takayasus arteritis", 1, 0)) %>%
  mutate(autoimmune_id___105 =  ifelse(autoimmune_id_other == "chronic fatigue (if that counts)" | 
           autoimmune_id_other == "chronic fatigue symptoms", 1, 0)) %>%
  mutate(autoimmune_id___106 = 0) %>%
  mutate(autoimmune_id___107 = ifelse(autoimmune_id_other == "undifferentiated connective tissue disease" , 1, 0)) %>%
  mutate(autoimmune_id___108 = ifelse(autoimmune_id_other == "high positive ana and joint pain, fatigue symptoms and inflammation and rashes" | 
                                        autoimmune_id_other == "still attempting to determine if antiphospholipid syndrome or lupus" | 
                                        autoimmune_id_other == "undergoing testing for sjogrens, lupas, addisons and coeliacs because many doctors have said i have an autoimmune disorders but have not investigated which" |
                                        autoimmune_id_other == "unknown autoimmune that presents similarly to osteoarthritis and sjogrens syndrome" | 
                                        autoimmune_id_other == "general immune dysfunction (ie multiple symptoms, including angioedema)", 1, 0)) %>%
  mutate(autoimmune_id___109 = ifelse(autoimmune_id_other == "urticarial vasculitis" |
                                        autoimmune_id_other == "vasculitis" |
                                        autoimmune_id_other == "p anca vasculitis (renal limited)" |
                                        autoimmune_id_other == "unspecified vasculitis", 1, 0)) %>%
  mutate(autoimmune_id___110 = ifelse(autoimmune_id_other == "iritis" |
                                        autoimmune_id_other == "uveitis (rare)", 1, 0)) %>%
  mutate(autoimmune_id___111 = ifelse(autoimmune_id_other == "vcfs, otherwise known as 22q11 deletion syndrome or digeorge syndrome", 1, 0)) %>%
  mutate(autoimmune_id___112 = ifelse(autoimmune_id_other == "hypothyroidism, but unsure if it is caused by hashimotos disease", 1, 0)) %>%
  mutate(autoimmune_id___113 = 0) %>%
  mutate(autoimmune_id___114 = 0) %>%
  mutate(autoimmune_id___115 = 0) %>%
  mutate(autoimmune_id___116 = 0) %>% 
  mutate(autoimmune_id___117 = 0) %>%
  mutate(autoimmune_id___118 = ifelse(autoimmune_id_other == "behcets disease" | 
                                        autoimmune_id_other == "behçet's syndrome ", 1, 0)) %>%
  mutate(autoimmune_id___119 = ifelse(autoimmune_id_other == "hidradenitis supparativa", 1, 0)) %>%
  mutate(autoimmune_id___120 = ifelse(autoimmune_id_other == "eds" | 
                                        autoimmune_id_other == "ehlers danlos syndrome and its numerous comorbities", 1, 0)) %>%
  mutate(autoimmune_id___121 = 0) %>%
  mutate(autoimmune_id___122 = ifelse(autoimmune_id_other == "eosinophilic esophagitis", 1, 0)) %>%
  mutate(autoimmune_id___123 = 0) %>%
  mutate(autoimmune_id___124 = 0) %>%
  mutate(autoimmune_id___125 = 0) %>%
  mutate(autoimmune_id___126 = ifelse(autoimmune_id_other == "cvid", 1, 0)) %>%
  mutate(autoimmune_id___127 = 0) %>%
  mutate(autoimmune_id___128 = 0) %>%
  mutate(autoimmune_id___129 = 0) %>%
  mutate(autoimmune_id___130 = ifelse(autoimmune_id_other == "cycling neutropenia", 1, 0)) %>%
  mutate(autoimmune_id___131 = 0) %>%
  mutate(autoimmune_id___132 = 0) %>%
  mutate(autoimmune_id___133 = ifelse(autoimmune_id_other == "mast cell activation syndrome (gp suspects this, but cant get a specialist for formal dx)" | 
                                        autoimmune_id_other == "mast cell disoder" | 
                                        autoimmune_id_other == "mcas", 1, 0)) %>%
  mutate(autoimmune_id___134 = 0) %>%
  mutate(autoimmune_id___135 = ifelse(autoimmune_id_other == "hypermobility spectrum disorder/ ehlers danlos syndrome", 1, 0)) %>%
  mutate(autoimmune_id___136 = 0) %>%
  mutate(autoimmune_id___137 = 0) %>%
  mutate(autoimmune_id___138 = 0) %>%
  mutate(autoimmune_id___139 = ifelse(autoimmune_id_other == "polychondritis", 1, 0)) %>%
  mutate(autoimmune_id___140 = ifelse(autoimmune_id_other == "menieres disease", 1, 0)) %>%
  mutate(autoimmune_id___141 =  0) %>%
  mutate(autoimmune_id___142 = ifelse(autoimmune_id_other == "microscopic colitis" |
                                        autoimmune_id_other == "lymphocytic colitis" |
                                        autoimmune_id_other == "collagenous colitis" |
                                        autoimmune_id_other == "colagenous colitis (microscopic colitis)"|
                                        autoimmune_id_other == "microscopic colitis diagnosed via colonoscopy feb 2019 currently awaiting another specialist appointment and undergoing gut testing as symptoms have worsened significantly this year" |
                                        autoimmune_id_other == "autoimmune colitis" , 1, 0)) %>%
  #mutate(autoimmune_id___143 = 0) %>%
  mutate(autoimmune_id___144 = 0) %>%
  mutate(autoimmune_id___145 = 0) %>%
  mutate(autoimmune_id___146 = 0) %>%
  mutate(autoimmune_id___147 = 0) %>%
  mutate(autoimmune_id___148 = 0) %>%
  mutate(autoimmune_id___149 = 0) %>%
  mutate(autoimmune_id___150 = 0) %>%
  mutate(autoimmune_id___151 = 0) %>%
  mutate(autoimmune_id___152 = 0) %>%
  mutate(autoimmune_id___153 = 0) %>%
  mutate(autoimmune_id___154 = 0) %>%
  mutate(autoimmune_id___155 = 0) %>%
  mutate(autoimmune_id___156 = ifelse(autoimmune_id_other == "and haemochromatosis", 1, 0)) %>%
  mutate(autoimmune_id___157 = ifelse(autoimmune_id_other == "hereditary alpha tryptasemia", 1, 0)) %>%
  mutate(autoimmune_id___158 = 0) %>%
  mutate(autoimmune_id___159 = 0) %>%
  mutate(autoimmune_id___160 = 0) %>%
  mutate(autoimmune_id___161 = ifelse(autoimmune_id_other == "periferal neuropathy" |
                                        autoimmune_id_other == "permanent peripheral neuropathy in both feet and left hand since sudden onset in early 2017, cause/disease unknown despite two years of subsequent testing by neurological specialist" |
                                        autoimmune_id_other == "idiopathic peripheral neuropathy", 1, 0)) %>%
  mutate(autoimmune_id___162 = ifelse(autoimmune_id_other == "juvenile idiopathic arthritis persisting into adulthood", 1, 0)) %>%
  mutate(autoimmune_id___163 = 0) %>%
  mutate(autoimmune_id___164 = ifelse(autoimmune_id_other == "long covid from april 2020", 1, 0)) %>%
  mutate(autoimmune_id___165 = 0) %>%
  mutate(autoimmune_id___166 = 0) %>%
  mutate(autoimmune_id___167 = 0) %>%
  mutate(autoimmune_id___168 = ifelse(autoimmune_id_other == "neuralgic amyotrophy", 1, 0)) %>%
  mutate(autoimmune_id___169 = 0) %>%
  mutate(autoimmune_id___170 = 0) %>%
  mutate(autoimmune_id___171 = 0) %>%
  mutate(autoimmune_id___173 = 0) %>%
  mutate(autoimmune_id___174 = 0) %>%
  mutate(autoimmune_id___175 = 0) %>%
  mutate(autoimmune_id___176 = 0) %>%
  mutate(autoimmune_id___177 = 0) %>%
  mutate(autoimmune_id___178 = 0) %>%
  mutate(autoimmune_id___179 = ifelse(autoimmune_id_other == "was also diagnosed at one point with autoimmune hypophysitis - 2005 by a specialist in endocrinology will mostly refer to coeliac disease in answers to questions", 1, 0)) %>%
  mutate(autoimmune_id___180 = 0) %>%
  mutate(autoimmune_id___181 = 0) %>%
  mutate(autoimmune_id___182 = 0) %>%
  mutate(autoimmune_id___183 = 0) %>%
  mutate(autoimmune_id___184 = 0) %>%
  mutate(autoimmune_id___185 = 0) %>%
  mutate(autoimmune_id___186 = 0) %>%
  mutate(autoimmune_id___187 = 0) %>%
  mutate(chronic___9 = 0) %>%
  mutate(chronic___10 = 0) %>%
  mutate(chronic___11 = 0) %>%
  mutate(chronic___12 = 0) %>%
  mutate(chronic___13 = 0) %>%
  mutate(chronic___14 = 0) %>%
  mutate(chronic___15 = 0) %>%
  mutate(chronic___16 = 0)  %>%
  mutate(chronic___17 = 0)  %>%
  mutate(chronic___18 = 0) %>%
  mutate(chronic___19 = 0) %>%
  mutate(chronic___20 = 0) %>%
  mutate(chronic___21 = 0) %>%
  mutate(chronic___22 = 0) %>%
  mutate(chronic___23 = 0)%>%
  mutate(chronic___24 = 0) %>%
  mutate(chronic___25 = 0) %>%
  mutate(chronic___26 = 0) 

### now include the multi column shenannignas
df.ad <- df.ad %>%
  mutate_at(vars(c(autoimmune_id___34, autoimmune_id___35)), ~ ifelse(autoimmune_id_other == "psoriasis ive been told i have ra and psa", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___31, autoimmune_id___37)), ~ ifelse(autoimmune_id_other == "polymyositis scleroderma overlap - this one was only recently diagnosed in may 2021", 1, .)) %>%
  #mutate_at(vars(c(autoimmune_id___44)), ~ ifelse(diseasestat == "0", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___46 , autoimmune_id___112)), ~ ifelse(autoimmune_id_other == "addisons disease hypothyroidism", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___46 , autoimmune_id___81)), ~ ifelse(autoimmune_id_other == "addisons disease & osteoporosis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___14 , autoimmune_id___46)), ~ ifelse(autoimmune_id_other == "addisons disease diagnosed 1996 (gp diagnosis) graves disease diagnosed 2016 (endocrinologist diagnosis)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___17 , autoimmune_id___46)), ~ ifelse(autoimmune_id_other == "addisons disease hashimotos thyroiditis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___46 , autoimmune_id___115)), ~ ifelse(autoimmune_id_other == "addisons disease neuromyotonia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___46 , autoimmune_id___47)), ~ ifelse(autoimmune_id_other == "addissons disease (primary) and alopecia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___47 , autoimmune_id___64 , autoimmune_id___89)), ~ ifelse(autoimmune_id_other == "alopecia areata, fibromyalgia, psoriasis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___47 , autoimmune_id___108 , autoimmune_id___55 , autoimmune_id___116)), ~ ifelse(autoimmune_id_other == "alopecia areata, unknown cause of autoimmune inflammation, me/cfs, pots", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___8 , autoimmune_id___110 , autoimmune_id___35)), ~ ifelse(autoimmune_id_other == "anterior uveitis 2000, diagnosed quickly crohns disease started 2002, diagnosed 2006 sero-negative arthritis started jan 2009, diagnosed april 2010", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___108 , autoimmune_id___113)), ~ ifelse(autoimmune_id_other == "as yet unknown autoinflammatory/autoimmune disorder linked with connective tissue disorder (rare enough to not be able to be genetically typed), often reactive to infection, exacerbated by stress, and has caused multiple bouts of sepsis had been also previously dx with graves, but later told this was a misdiagnosis and was instead recurrent periods of reactive thyroiditis causing the abnormal nuclear imaging and deranged thyroid hormone levels", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___117 , autoimmune_id___112 , autoimmune_id___46 , autoimmune_id___52)), ~ ifelse(autoimmune_id_other == "autoimmune hepatitis addisons disease under active thyroid lymphodeama", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___119 , autoimmune_id___70)), ~ ifelse(autoimmune_id_other == "idiopathic thrombocytopenia purpura (now post-splenectomy) i also have hidradenitis suppurativa, which is a skin condition that some believe to be autoimmune", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "cfs/fibromyalgia" |
                                                                         autoimmune_id_other == "cfs/me fibromyalgia" |
                                                                         autoimmune_id_other == "me 1982 fibromyalgia 2021" |
                                                                         autoimmune_id_other == "cfs/me, more specifically diagnosed by specialist as chronic fatigue immune dysfunction syndrome fibromyalgia" |
                                                                         autoimmune_id_other == "chronic fatigue fibromyalgia" |
                                                                         autoimmune_id_other == "fibromyalgia, cfs" |
                                                                         autoimmune_id_other == "chronic fatigue syndrome/me and or fibromyalgia" |
                                                                         autoimmune_id_other == "me/cfs - primary condition fibromyalgia - secondary condition" |
                                                                         autoimmune_id_other == "me/cfs and fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___116)), ~ ifelse(autoimmune_id_other == "pots and me/cfs and possibly addisons disease" |
                                                                          autoimmune_id_other == "myalgic encephalomyelitis postural orthostatic tachycardia syndrome" |
                                                                          autoimmune_id_other == "me/cfs and pots" |
                                                                          autoimmune_id_other == "cfs pots" |
                                                                          autoimmune_id_other == "chronic fatigue syndrome, postural orthostatic tachycardia (pots) pre-diabetes", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___65 , autoimmune_id___72)), ~ ifelse(autoimmune_id_other == "fibromyalgia, irritable bowel syndrome, asthma, endometriosis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___123)), ~ ifelse(autoimmune_id_other == "fibromyalgia and hypermobile eds (heds)" |
                                                                          autoimmune_id_other == "formally diagnosed with fibromyalgia self-diagnoses of hypermobile ehlers-danlos syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___125)), ~ ifelse(autoimmune_id_other == "conns disease , fibromyalgia,", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___126 , autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "common variable immune disorder, fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___127 , autoimmune_id___85)), ~ ifelse(autoimmune_id_other == "cidp - polymialgia rheumatica" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___120 , autoimmune_id___72 , autoimmune_id___128 , autoimmune_id___116)), ~ ifelse(autoimmune_id_other == "myalgic encephalomyelitis (fatigue chronic syndrome), fibromyalgia ehlers danlos syndrome ibs pots mals", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___118)), ~ ifelse(autoimmune_id_other == "myalgic encephalomyelitis, behcets disease", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___124 , autoimmune_id___116)), ~ ifelse(autoimmune_id_other == "cveds pots", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___131 , autoimmune_id___89)), ~ ifelse(autoimmune_id_other == "eczema and psoriasis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___52 , autoimmune_id___38 , autoimmune_id___59)), ~ ifelse(autoimmune_id_other == "cutaneous lupus and sjogrens syndrome overlap also autoimmune hepatitis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___132 , autoimmune_id___133 , autoimmune_id___55, autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "fibromyalgia me/cfs mcs mcas", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___135 , autoimmune_id___120 , autoimmune_id___116 , autoimmune_id___134)), ~ ifelse(autoimmune_id_other == "fibromyalgia (recently reclassified as an immune disorder) rheumatologist 24/08/21 unsure if autoimmune hypermobility (rheumatologist) 24/08/21 pots, eds diagnosed integrative gp (01/09/21) fibromuscular dysplasia (diagnosed cardiologist 08/05/20)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___131)), ~ ifelse(autoimmune_id_other == "fibromyalgia and eczema", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___135)), ~ ifelse(autoimmune_id_other == "fibromyalgia, unspecified joint hypermobility" |
                                                                          autoimmune_id_other == "fibromyalgia hyper mobility spectrum disorder", 1, .))
#done
df.ad <- df.ad %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___65 , autoimmune_id___116 , autoimmune_id___133 , autoimmune_id___123)), ~ ifelse(autoimmune_id_other == "formally diagnosed: me/cfs, endometriosis, pots self diagnosed: strongly suspect mcas and heds", 1, .)) %>%
  mutate_at(vars(c(chronic___15 , autoimmune_id___55 , autoimmune_id___72)), ~ ifelse(autoimmune_id_other == "self-diagnosed me-csf based on online available criteria the specialists i have visited do not believe in the disease, so i do not have a proper diagnosis for the condition the broad number of symptoms i have match completely with a me-csf diagnosis and only partially with psa i do not have blood markers for psa, neither most of the symptoms used for its diagnosis, yet, having a first-line relative with the disease and markers of systemic chronic inflammation, i got diagnosed with psa my neurologist defined my complex condition as post-infectious neurologically driven pain and fatigue additionally, diagnosed with ibs", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___73 , autoimmune_id___89)), ~ ifelse(autoimmune_id_other == "psoriasis frontal fibrosing alopecia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___68 , autoimmune_id___123 , autoimmune_id___116 , autoimmune_id___55 , autoimmune_id___72)), ~ ifelse(autoimmune_id_other == "functional neurological disorder hypermobile possibly eds pots chronic fatigue syndrome ibs", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___69 , autoimmune_id___72)), ~ ifelse(autoimmune_id_other == "formally diagnosed: chronic gastritis currently investigating: inflammatory bowel disease", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___38 , autoimmune_id___59 , autoimmune_id___66)), ~ ifelse(autoimmune_id_other == "sjogrens syndrome sub acute cutaneous lupus raynauds phenomenom", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___38 , autoimmune_id___120 , autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "sjogrens (also eds and fibro although not autoimmune technically)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___109 , autoimmune_id___66 , autoimmune_id___136)), ~ ifelse(autoimmune_id_other == "vasculitis raynauds dysphagia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 , autoimmune_id___72 , autoimmune_id___105)), ~ ifelse(autoimmune_id_other == "very bad ibs , endometriosis, chronic fatigue, thyroid issues", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___66)), ~ ifelse(autoimmune_id_other == "reynauds fibromyalgia not sure connective tissue disorder", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___66, autoimmune_id___55)), ~ ifelse(autoimmune_id_other == "fibromyalgia me cfs raynauds neuropathy", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___38 , autoimmune_id___98 , autoimmune_id___99, autoimmune_id___77 , autoimmune_id___100 , autoimmune_id___35 , chronic___15)), ~ ifelse(autoimmune_id_other == "sicca (dry eyes/dry mouth) lactose intolerant pancreatic malabsorption disease/pancreatic insufficiency gord gastric oesophagul reflux lphs loin pain he materia disease (kidney) rheumatoid arthritis mix connective tissue disease tachycardia", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___66 , autoimmune_id___137 , autoimmune_id___138)), ~ ifelse(autoimmune_id_other == "reynauds, lupus nephritis, discoid lupus", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___72 , autoimmune_id___64 , autoimmune_id___65 , autoimmune_id___84)), ~ ifelse(autoimmune_id_other == "ibs, fibromyalgia , endometriosis, pcos", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___66 , autoimmune_id___74)), ~ ifelse(autoimmune_id_other == "lichen sclerosis reynauds", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___81 , autoimmune_id___92)), ~ ifelse(autoimmune_id_other == "osteoporosis, osteoarthritis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___89 , autoimmune_id___140 , autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "psoriasis, menieres disease, fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___89 , autoimmune_id___34 , autoimmune_id___46)), ~ ifelse(autoimmune_id_other == "psoriasis, psoriatic arthritis, adrenal insufficiency and steroid induced diabetes", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___59 , autoimmune_id___36)), ~ ifelse(autoimmune_id_other == "pulmonary sarcoidosis, cutaneous lupus", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___66 , autoimmune_id___92)), ~ ifelse(autoimmune_id_other == "raunauds, osteoarthritis", 1, .)) %>%
  mutate_at(vars(c( autoimmune_id___142 , autoimmune_id___92 , autoimmune_id___93 , autoimmune_id___66, chronic___24, chronic___5, chronic___1, chronic___2)), ~ ifelse(autoimmune_id_other == "pulmonary arterial hypertension type 2 diabetes - diet controlled - no medications lymphocytic colitis osteo arthritis severe knee joint irregularities watermelon stomach (gave) causing iron-deficiency anemia right heart failure severe heartburn atrial fibrillation requiring a heart pacemaker (july 2019) raynauds syndrome asthma", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___106 , autoimmune_id___66)), ~ ifelse(autoimmune_id_other == "t-cell lymphopenia, raynauds, lupus anti-coagulant syndrome, possible sjogrens (not formally tested)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___135)), ~ ifelse(autoimmune_id_other == "me/cfs, hypermobility and looking at other conditions", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___112)), ~ ifelse(autoimmune_id_other == "me/cfs hypothyroidism of in known cause (not serum positive hashimotos)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___142 , autoimmune_id___133 , autoimmune_id___120 , autoimmune_id___116)), ~ ifelse(autoimmune_id_other == "lymphocytic microscopic colitis mcas eds pots being investigated for other conditions", 1, .))
  #TODO done
df.ad <- df.ad %>%
  mutate_at(vars(c(autoimmune_id___89 , autoimmune_id___142)), ~ ifelse(autoimmune_id_other == "lymphocitic colitis, psoriasis,", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___61 , autoimmune_id___50)), ~ ifelse(autoimmune_id_other == "lupus anticoagulant anti phospholipid diverticulitis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___74 , autoimmune_id___144 , autoimmune_id___131 , autoimmune_id___145 , autoimmune_id___146 , autoimmune_id___147 , chronic___10 , autoimmune_id___149)), ~ ifelse(autoimmune_id_other == "lichen sclerosis, notalgia parasthetica, excema, rosacea, cf, bletharitis, visual migraine, gastroparesis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___130 , autoimmune_id___112 , autoimmune_id___72)), ~ ifelse(autoimmune_id_other == "immune neutropaenia ibs low thyroid", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___56 , autoimmune_id___57 , autoimmune_id___86)), ~ ifelse(autoimmune_id_other == "idiopathic urticaria & angioedema, mast cell issues (not formally diagnosed)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___156 , autoimmune_id___155, autoimmune_id___154 , autoimmune_id___64 , chronic___18 , autoimmune_id___151 , autoimmune_id___152)), ~ ifelse(autoimmune_id_other == "hereditary hemochromatosis trigeminal neuralgia 1 & 2 with empty sella syndrome fibromyalgia ddd of spine whole spine facet athroparhy axial arthritis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___158 , autoimmune_id___66, chronic___10)), ~ ifelse(autoimmune_id_other == "fibro myalgia 1999-- trachea malasia 2019-- raynauds syndrome 2010-- disequilibrium exacerbated by painless migraine 2013", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___159 , autoimmune_id___77)), ~ ifelse(autoimmune_id_other == "cryptogenic organising pneumonitis / mixed connective tissue disease", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___57 , autoimmune_id___56)), ~ ifelse(autoimmune_id_other == "chronic spontaneous urticaria and angiodema", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___107)), ~ ifelse(autoimmune_id_other == "chronic fatigue syndrome possibly connective tissue disease", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___66 , autoimmune_id___161 , autoimmune_id___37 , autoimmune_id___162)), ~ ifelse(autoimmune_id_other == "both feet - raynauds phenomena, foot ulcers, peripheral neuropathy, scleroderma and arthritis", 1, .))  %>%
  mutate_at(vars(c(autoimmune_id___131 , autoimmune_id___116 )), ~ ifelse(autoimmune_id_other == "atopic dermatitis, pots,", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___63 , autoimmune_id___135 , autoimmune_id___116 )), ~ ifelse(autoimmune_id_other == "chronic fatigue syndrome, hyper mobility, dysautonomia, pots", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___63 , autoimmune_id___116 )), ~ ifelse(autoimmune_id_other == "autoimmune dysautonomia/pots", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120 , autoimmune_id___116 )), ~ ifelse(autoimmune_id_other == "eds & pots possible sub-clinical cushings", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120 , autoimmune_id___133 , autoimmune_id___84, chronic___22, chronic___1 )), ~ ifelse(autoimmune_id_other == "ehlers-danlos syndrome, mcas, neurocadiogenic syncope, pcos, asthma", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___65 )), ~ ifelse(autoimmune_id_other == "fibromyalgia, endometriosis (considered to be autoimmune in some areas of study)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___17 , autoimmune_id___73  , autoimmune_id___38 )), ~ ifelse(autoimmune_id_other == "hashimotos, lichen planopilaris, and possible sjohrens", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___63 ,autoimmune_id___116 , autoimmune_id___53 ,autoimmune_id___133 , autoimmune_id___64, chronic___22, chronic___16, chronic___5 )), ~ ifelse(autoimmune_id_other == "history of multiple tick borne disease infections (chronic symptoms / positive labs 2017), me / cfs, dysautonomia (pots, neurocardiogenic syncope disorder, orthostatic intolerance), mcas mast cell activation disorder, fibromyalgia and a seizure disorder (most likely neuroborreliosis / bartonellosis linked or mast cell - non epileptic but managed with anti epilepsy meds) - also have large scale hole in heart", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___133 , autoimmune_id___165, autoimmune_id___141 )), ~ ifelse(autoimmune_id_other == "mast cell activation disorder intermittent porphyria solar urticaria", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___133 , autoimmune_id___132 , autoimmune_id___116, chronic___14)), ~ ifelse(autoimmune_id_other == "mast cell activation syndrome multiple chemical sensitivities pots stephen johnson syndrome chronic inflammatory response syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64, autoimmune_id___132, autoimmune_id___108 )), ~ ifelse(autoimmune_id_other == "me/cfs fibromyalgia multiple chemcal sensivities, asthma, hyperactive immmune system", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___42 )), ~ ifelse(autoimmune_id_other == "myalgic encephalomyelitis (me/cfs) note that the diagnosis date only allows for one illness! i me/cfs because it came first my uc diagnosis date is january 2019", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___38 , autoimmune_id___66 , autoimmune_id___112 )), ~ ifelse(autoimmune_id_other == "sjögrens raynauds hypothyroid", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___89 , autoimmune_id___142 )), ~ ifelse(autoimmune_id_other == "psoriasis microscopic colitia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___78 , autoimmune_id___116 )), ~ ifelse(autoimmune_id_other == "pots and narcolepsy (type 1)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___132 , autoimmune_id___133 , autoimmune_id___161  )), ~ ifelse(autoimmune_id_other == "me/cfs, mcs, neuropathy, probable mcas 13 years before any diagnosis (me/cfs) was made, and the dr didnt actually initially communicate with me that he had done so or maybe my brain fog got in the way before this i didnt feel validated by any medical professionals i had seen all these conditions continue to worsen over time, mostly gradually but sometimes quite suddenly occasionally theres significant improvement for a few months or years, but it doesnt seem to last pretty sure the pfizer has resulted in a significant uptick in the neuropathy, and a minor uptick in chemical and food sensitivities other than that, im happy that i chose to get vaccinated", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___133, autoimmune_id___40 )), ~ ifelse(autoimmune_id_other == "myalgic encephalomyelitis, interstitial myositis, mast cell activation syndrome,", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___6 , autoimmune_id___17 )), ~ ifelse(autoimmune_id_other == "prostate cancer 2009 hashimotos disease (autoimmune hypothyroidism) 1996 coeliac disease 2000", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___89 , autoimmune_id___165 , autoimmune_id___72, chronic___1 )), ~ ifelse(autoimmune_id_other == "scalp psoriasis, fibromyalgia, ibs, asthma, cold urticara", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___35 , autoimmune_id___166 )), ~ ifelse(autoimmune_id_other == "sero negative arthritis lichen planus", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___169 )), ~ ifelse(autoimmune_id_other == "mesenteric peniculitis fibromyalgia", 1, .)) 


### look at what illnesses me/cfs co-morbs have added, run before the co-morb section so it doesn't add to those counts yet
#run labels before code below

# df.ad.cf <- df.ad %>%
#   mutate(cfs_co = 0) %>%
#   #todo work out if secondary hypoadr. is an illness or comorbidity
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "ehlers-danlos syndrome (hypermobile with vascular features), postural orthostatic tachycardia syndrome with arrhythmia, aortic root aneurysm, raynaud's syndrome, narcolepsy and cataplexy, widespread neuropathic pain, multiple chronic herniated disks, osteoarthritis of the spine, chronic fatigue, trans ischemic attacks, migraine diathesis, chronic respiratory infections, pneumothorax, endometriosis, irritable bowel syndrome, bowel perforation.     inflammatory bowel disease (?), mast cell activation syndrome (?), chiari malformation (?), benedikt syndrome (?).    anxiety, depression, complex post traumatic stress disorder, schizoid personality disorder, dissociative identity disorder", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "sjogrens, hyponatreamea, bursitis, chronic fatigue, borderline personality disorder, major depression, cptsd, chronic pain,", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "deafness for a year as a child after contracting german measles  but resolved itself  episodes of malaria as a child growing up in papua new guinea  follicle bacterial infections causing abscesses and hospitalisations( father and 1 sister also have this as well)  severe tosillitis and infection at 17 yrs-removed  diverticulitis-2021  salycilate intolerances with mood behaviours  multiple fibroids and anemia- full hysterectomy 2017    chronic fatigue 2021  fibromalagia 2021", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me-csf, psa and ibs. anxiety", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "chronic migraine, thoracic outlet syndrome, fibromyalgia, chronic fatigue", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "chronic fatigue syndrome, osteoarthritis, osteomyelitis, depression, anxiety, ptsd.", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "post viral chronic fatigue from ross river virus  venous sinus thrombosis", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me/cfs  fnd  fibromyalgia", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me/cfs", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "cfs me", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "myalgic encephalomyelitis  orthostatic hypotension", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me/cfs, heds, pots, fibromyalgia", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "endometriosis   myalgic encephalomyelitis   functional neurological disorder", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "chronic fatigue syndrome  super tachycardia   bell's palsy/ramsey hunt syndrome   hyperthyroidism", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "cfs/me as above", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "pituitary tumour (removed oct 2019)  me/cfs  fibromyalgia  pots  vascular eagle's syndrome", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "chronic fatigue syndrome", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me/cfs  severe lifelong eczema", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "myalgic encephalomyelitis and behcets disease to me constitute a severe illness as they have left me bedbound and unable to work requiring daily care for basic living activities without any treatment options at this time.", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "fibromyalgia, me/cfs, metabolic syndrome, postural orthostatic tachycardic syndrome", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "cfsme fibromyalgia", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "non-alcoholic fatty liver disease, thoracic outlet syndrome (n-v-a), pelvic congestion syndrome, normochromic anemia, neutropenia, elevated chromogranin a, borderline tryptase, mast cell activation, fibromyalgia, raynaud's phenomenon, myalgic encephalomyelitis, trigeminal neuralgia, osteopenia, gastroparesis, ibs, cervical disc disease, recurring facial oedema, leg oedema, low competent c3, low crp, retrograde flow left ovarian vein, adhd, severe major depressive disorder, generalized anxiety disorder.", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "cfs/me", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me/cfs, fibromyalgia", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "adhd (diagnosed years after viral illness that triggered post-viral fatigue syndrome)", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "myalgic encephalomyelitis, fibromyalgia, irritable bowel syndrome, orthostatic intolerance,", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "scoliosis  ibs  fibromyalgia  cfs  diverticulosis", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "graves, hashimotos, high bp, fibromyagia, myalgic encephalomyelitis", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me/cfs  ibs", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me/ cfs  ibs", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "loeys dietz syndrome type 2, pots (postural orthostatic tachycardia) chronic fatigue syndrome", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "pots , me/cfs,  haemochromatosis, possible coeliac", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "chronic fatigue syndrome & pcos", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "cfs  fibromyalgia", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "mecfs", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "chronic fatigue triggered by glandular fever", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me & fibromyalgia", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "myalgic encephalomyelitis (fatigue chronic syndrome),   fibromyalgia   ehlers danlos syndrome  ibs  pots  mals", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "orthostatic intolerance - since childhood but substantially worsened by me/cfs each time", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "heds, pots, mcas, me/cfs, ibs, gerd, fibromyalgia, hormonal migraines, cervicogenic headaches, asd, adhd, anxiety, depression, ptsd, hyperacusis, scoliosis, plantar fasciitis, pgad, rosacea, gallstone, muscle spasms", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "hypothyroidism that can't be controlled with medication due reactions to medications.    chronic fatigue syndrome", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "eds, chronic pain and chronic fatigue syndrome", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "chronic fatigue , raynaus", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "cfs, endometriosis, migraines, anxiety", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me/cfs formally diagnosed 2017  chronic fatigue diagnosed 1998", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "cfs", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "fnd disorder   hypermobile possibly eds syndrome   cfs   ibs  graves disease   possible pots", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me/cfs  mast cell activation disorder  dysautonomia (pots, gastroparesis, nmh)  chronic migraine", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "eds (subtype yet to be determined suspects heds and veds) - tos with subclavical artery impingement, pots, vcd associated conditions all confirmed  seronegative spondyloarthritis (still determining all subtypes - psoriatic arthritis confirmed) hla-dq8 & hla-b27 positive - post infection onset  cfs/me - post infection onset  macular ped - slow progression to macular degeneration  fibromyalgia (thought to be misdiagnosis of eds and autoimmune diagnoses being subtype comfirmed)  past severe gerd with esophageal ulceration  childhood asthma  past chronic infections (borreliosis - europe, babesia duncani, rickettsia spotted fever - nsw)", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "endometriosis, chronic fatigue syndrome, postural orthostatic tachycardia syndrome, possible ehlers-danlos but on the hypermobile spectrum at least", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "me/cfs, fibromyalgia, new daily persistent headache, svt, gerd, possible endometriosis", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "fibromyalgia / chronic fatigue", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "fibromyalgia  chronic fatigue syndrome", 1, .)) %>%
#   mutate_at(vars(c(cfs_co )), ~ ifelse(otherchronic == "myalgic encephalomyelitis (me/cfs)  fibromyalgia  ulcerative colitis  enthesitis  pots (postural orthostatic intolerance syndrome)  multiple chemical sensitivity  skin cancers  dissociative ptsd", 1, .)) %>%
#   mutate_at(vars(c(cfs_co)), ~ ifelse(otherchronic == "chronic fatigue syndrome, fibromyalgia", 1, .))
# cfs.co <- df.ad.cf %>%
#   select(autoimmune_id___1:autoimmune_id___162, cfs_co, -autoimmune_id___45) %>%
#   filter(cfs_co == 1) %>%
#   select(-cfs_co)
# cfs.labels <- cfs.co %>%
#   get_label
# colnames(cfs.co) <- cfs.labels
# cfs.count <- cfs.co %>%
#   summarise_if(is.numeric, sum) %>%
#   transpose()
# cfs.count.df <- as.data.frame(do.call(cbind, cfs.count)) %>%
#   rownames_to_column(.) %>%
#   rename(., word = rowname, freq = v1)
# cfs.count.df$freq <- as.numeric(as.character(cfs.count.df$freq))
# cfs.count.df <- arrange(cfs.count.df, desc(freq)) %>%
#   filter(freq != 0)


### here section from co-morbs that should be ads starts
#TODO up to here
df.ad <- df.ad %>%
  mutate_at(vars(c(autoimmune_id___84, chronic___10)), ~ ifelse(otherchronic == "polycystic ovarian syndrome, insulin resistance migraine hyperemesis gravidarum when pregnant currently pregnant with a toddler and infant, the eldest has major sleep and behavioral issues", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64, autoimmune_id___46, chronic___9, autoimmune_id___87)), ~ ifelse(otherchronic == "secondary hypoadrenalism, acromegaly, fibromyalgia, radiculopathy i am also autistic, which is obviously not an illness as such but there is a strong and complex relationship between my autism and my other diseases", 1, .)) %>%
  mutate_at(vars(c(chronic___15 , autoimmune_id___123 , autoimmune_id___116 , autoimmune_id___66 , autoimmune_id___78 , autoimmune_id___92 , autoimmune_id___105 , autoimmune_id___65 , autoimmune_id___72 , autoimmune_id___133, chronic___19, chronic___10)), ~ ifelse(otherchronic == "ehlers-danlos syndrome (hypermobile with vascular features), postural orthostatic tachycardia syndrome with arrhythmia, aortic root aneurysm, raynauds syndrome, narcolepsy and cataplexy, widespread neuropathic pain, multiple chronic herniated disks, osteoarthritis of the spine, chronic fatigue, trans ischemic attacks, migraine diathesis, chronic respiratory infections, pneumothorax, endometriosis, irritable bowel syndrome, bowel perforation inflammatory bowel disease (), mast cell activation syndrome (), chiari malformation (), benedikt syndrome () anxiety, depression, complex post traumatic stress disorder, schizoid personality disorder, dissociative identity disorder", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___74 , autoimmune_id___81 , chronic___10)), ~ ifelse(otherchronic == "lichen sclerosus, osteoporosis, steroid induced diabetes, anxiety, migraines", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___116 )), ~ ifelse(otherchronic == "postural orthostatic tachycardia syndrome (pots)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___35 , autoimmune_id___85 , chronic___15)), ~ ifelse(otherchronic == "i was diagnosed with polymyalgia rheumatica in 2014 after treatment with prednisone for approximately 15 months, i believed i was over polymyalgia rheumatica but started having different symptoms, pain and stiffness in my hands, fatigue etc and was diagnosed with rheumatoid arthritis i had had a carpal tunnel operation on each wrist in early 2014", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___19 )), ~ ifelse(otherchronic == "bergers disease", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___50 )), ~ ifelse(otherchronic == "antiphospholipid syndrome", 1, .)) %>%
  mutate_at(vars(c(chronic___15 , autoimmune_id___38 , autoimmune_id___105, autoimmune_id___102 )), ~ ifelse(otherchronic == "sjogrens, hyponatreamea, bursitis, chronic fatigue, borderline personality disorder, major depression, cptsd, chronic pain,", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___126 , autoimmune_id___81 , chronic___16)), ~ ifelse(otherchronic == "epilepsy osteoporosis probable cvid", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___96 , chronic___10)), ~ ifelse(otherchronic == "rheumatic fever (aged 4) aplastic anaemia (aged 13) migraines throughout my life", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___84 )), ~ ifelse(otherchronic == "polycystic ovarian syndrome", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___61 )), ~ ifelse(otherchronic == "deafness for a year as a child after contracting german measles but resolved itself episodes of malaria as a child growing up in papua new guinea follicle bacterial infections causing abscesses and hospitalisations( father and 1 sister also have this as well) severe tosillitis and infection at 17 yrs-removed diverticulitis-2021 salycilate intolerances with mood behaviours multiple fibroids and anemia- full hysterectomy 2017 chronic fatigue 2021 fibromalagia 2021", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___96 , autoimmune_id___92 )), ~ ifelse(otherchronic == "rheumatic fever at age 4, osteoarthritis since age 25", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 )), ~ ifelse(otherchronic == "fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___92 , autoimmune_id___72 , autoimmune_id___61 , autoimmune_id___74 , autoimmune_id___161, chronic___24 )), ~ ifelse(otherchronic == "high blood pressure, osteoarthritis, was diagnosed and treated for asthma but ended up being paradoxical vocal chord movement disorder, irritable bowel, diverticulitis , probable lichen sclerosis, herpetic neuralgia, minor peripheral neuropathy, movement disorder ( chorea, restless legs, essential tremor)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___72 , autoimmune_id___34, chronic___4 )), ~ ifelse(otherchronic == "me-csf, psa and ibs anxiety", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___72 , chronic___4)), ~ ifelse(otherchronic == "ibs, ptsd leading to long term cortisol elevation", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___85 )), ~ ifelse(otherchronic == "polymyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___89 )), ~ ifelse(otherchronic == "psoriasis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64, chronic___16, autoimmune_id___162, chronic___16 )), ~ ifelse(otherchronic == "epilepsy arthritis fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___66, autoimmune_id___185 )), ~ ifelse(otherchronic == "fibromyalgia familial hypercholesterolaemia raynauds", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___105 , autoimmune_id___64 , chronic___10, autoimmune_id___187)), ~ ifelse(otherchronic == "chronic migraine, thoracic outlet syndrome, fibromyalgia, chronic fatigue", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 )), ~ ifelse(otherchronic == "endometriosis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___92 )), ~ ifelse(otherchronic == "osteoarthritis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___123 , autoimmune_id___116 , autoimmune_id___72 , autoimmune_id___66, chronic___9, autoimmune_id___51 )), ~ ifelse(otherchronic == "severe presentation of hypermobile type ehlers-danlos syndrome; small fibre neuropathy (preliminary diagnosis);postural orthostatic tachycardia syndrome; ibs; and others that i regard as minor like raynauds and asthma im also autistic (self diagnosed, backed up by multiple professionals but no formal diagnosis), but not sure that that fits this question", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___98, autoimmune_id___182, chronic___5 )), ~ ifelse(otherchronic == "endocarditis, gord, hepatitis c (spontaneously resolved)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___135 , chronic___9)), ~ ifelse(otherchronic == "possible lynch syndrome, autism, hyper mobility", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___92, chronic___4 )), ~ ifelse(otherchronic == "chronic fatigue syndrome, osteoarthritis, osteomyelitis, depression, anxiety, ptsd", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 )), ~ ifelse(otherchronic == "post viral chronic fatigue from ross river virus venous sinus thrombosis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___68)), ~ ifelse(otherchronic == "me/cfs fnd fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 )), ~ ifelse(otherchronic == "me/cfs", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 )), ~ ifelse(otherchronic == "cfs me", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___92, chronic___17 )), ~ ifelse(otherchronic == "osteoarthritis recurring iron deficiency excessive cholesterol sleep difficulties parathyroidectomy (inactive)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___53 )), ~ ifelse(otherchronic == "myalgic encephalomyelitis orthostatic hypotension", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___116 , autoimmune_id___123 , autoimmune_id___64 )), ~ ifelse(otherchronic == "me/cfs, heds, pots, fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___68 , autoimmune_id___65 )), ~ ifelse(otherchronic == "endometriosis myalgic encephalomyelitis functional neurological disorder", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___100 , autoimmune_id___113 )), ~ ifelse(otherchronic == "chronic fatigue syndrome super tachycardia bells palsy/ramsey hunt syndrome hyperthyroidism", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___55 )), ~ ifelse(otherchronic == "cfs/me as above", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___116, autoimmune_id___186, chronic___3 )), ~ ifelse(otherchronic == "pituitary tumour (removed oct 2019) me/cfs fibromyalgia pots vascular eagles syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 )), ~ ifelse(otherchronic == "chronic fatigue syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___131 )), ~ ifelse(otherchronic == "me/cfs severe lifelong eczema", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___118 )), ~ ifelse(otherchronic == "myalgic encephalomyelitis and behcets disease to me constitute a severe illness as they have left me bedbound and unable to work requiring daily care for basic living activities without any treatment options at this time", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___116 )), ~ ifelse(otherchronic == "fibromyalgia, me/cfs, metabolic syndrome, postural orthostatic tachycardic syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65, chronic___19)), ~ ifelse(otherchronic == "endometriosis chiari 1", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64)), ~ ifelse(otherchronic == "cfsme fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___55 , autoimmune_id___133 , autoimmune_id___66 , autoimmune_id___155 , autoimmune_id___58 , autoimmune_id___149 , autoimmune_id___72 , chronic___11, chronic___4, autoimmune_id___187, autoimmune_id___183)), ~ ifelse(otherchronic == "non-alcoholic fatty liver disease, thoracic outlet syndrome (n-v-a), pelvic congestion syndrome, normochromic anemia, neutropenia, elevated chromogranin a, borderline tryptase, mast cell activation, fibromyalgia, raynauds phenomenon, myalgic encephalomyelitis, trigeminal neuralgia, osteopenia, gastroparesis, ibs, cervical disc disease, recurring facial oedema, leg oedema, low competent c3, low crp, retrograde flow left ovarian vein, adhd, severe major depressive disorder, generalized anxiety disorder", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___92 )), ~ ifelse(otherchronic == "spinal osteoarthritis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 )), ~ ifelse(otherchronic == "cfs/me", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55, autoimmune_id___64 )), ~ ifelse(otherchronic == "me/cfs, fibromyalgia", 1, .))
#TODO done
df.ad <- df.ad %>%
  mutate_at(vars(c(autoimmune_id___55 , chronic___11)), ~ ifelse(otherchronic == "adhd (diagnosed years after viral illness that triggered post-viral fatigue syndrome)", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___53 , autoimmune_id___72 )), ~ ifelse(otherchronic == "myalgic encephalomyelitis, fibromyalgia, irritable bowel syndrome, orthostatic intolerance,", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___72 , autoimmune_id___64 , autoimmune_id___55 , autoimmune_id___61 , chronic___18)), ~ ifelse(otherchronic == "scoliosis ibs fibromyalgia cfs diverticulosis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___14 , autoimmune_id___17 , autoimmune_id___64 , autoimmune_id___55, chronic___24 )), ~ ifelse(otherchronic == "graves, hashimotos, high bp, fibromyagia, myalgic encephalomyelitis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120 , autoimmune_id___81, autoimmune_id___181, chronic___13 )), ~ ifelse(otherchronic == "copd osteoporosis ehlers danlos glaucoma", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___116 )), ~ ifelse(otherchronic == "pots/oi", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___92 , autoimmune_id___58, chronic___24, autoimmune_id___181 , chronic___13)), ~ ifelse(otherchronic == "glaucoma, bronchiectasis, osteoarthritis, bladder incontinence, high blood pressure, osteopenia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___66 )), ~ ifelse(otherchronic == "raynauds syndrome glandular fever chronic tonsillitis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___35 )), ~ ifelse(otherchronic == "ra", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120 )), ~ ifelse(otherchronic == "ehlers dhanlos syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___132 )), ~ ifelse(otherchronic == "multiple chemical sensitivities", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___72 )), ~ ifelse(otherchronic == "me/cfs ibs", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___84 , chronic___18)), ~ ifelse(otherchronic == "pcos, scoliosis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___119 , autoimmune_id___84, chronic___4, autoimmune_id___117 )), ~ ifelse(otherchronic == "hidradenitis suppurativa, depression, anxiety, pcos, lymphedema, epiglottitis (severe, not chronic obviously!)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___72 )), ~ ifelse(otherchronic == "me/ cfs ibs", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___116 , autoimmune_id___55, autoimmune_id___153 )), ~ ifelse(otherchronic == "loeys dietz syndrome type 2, pots (postural orthostatic tachycardia) chronic fatigue syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___135 , autoimmune_id___116 )), ~ ifelse(otherchronic == "hypermobility spectrum disorder postural orthostatic tachycardia syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 , autoimmune_id___135 , autoimmune_id___64 )), ~ ifelse(otherchronic == "endometriosis, hypermobility, fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___116 , autoimmune_id___55 , autoimmune_id___156 , autoimmune_id___6 )), ~ ifelse(otherchronic == "pots , me/cfs, haemochromatosis, possible coeliac", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___84 )), ~ ifelse(otherchronic == "polystic ovarian syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___133 )), ~ ifelse(otherchronic == "mast cell activation syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___123 , autoimmune_id___116, chronic___4 )), ~ ifelse(otherchronic == "hypermobile eds pots ocd depression", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___72)), ~ ifelse(otherchronic == "ibs, food intolerances", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___84 , autoimmune_id___55 )), ~ ifelse(otherchronic == "chronic fatigue syndrome & pcos", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 )), ~ ifelse(otherchronic == "cfs fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___92 )), ~ ifelse(otherchronic == "fibromyalgia osteoarthritis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120, chronic___4 )), ~ ifelse(otherchronic == "ehlers danlos syndrome i also have c-ptsd", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___135 , chronic___18 , autoimmune_id___60 , chronic___21)), ~ ifelse(otherchronic == "i have an undiagnosed connective tissue disorder - somewhere on the hsd-heds continuum i have early onset polyarthralgia, degenerative disc disease, extreme allergies, easy and extreme bleeding/bruising, menorrhagia (requiring quarterly iron infusions), precipitous labour etc ive been hospitalised with mumps ive also had whooping cough", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 , autoimmune_id___101 , autoimmune_id___116 , autoimmune_id___135 , autoimmune_id___133 , autoimmune_id___162 , autoimmune_id___64 )), ~ ifelse(otherchronic == "endometriosis, adenomyosis, pots, hypermobility, suspected mcas, arthritis, fibromyalgia / central sensitisation", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 )), ~ ifelse(otherchronic == "mecfs", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 )), ~ ifelse(otherchronic == "chronic fatigue triggered by glandular fever", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 )), ~ ifelse(otherchronic == "me & fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___22 , autoimmune_id___70 , autoimmune_id___156 , chronic___20, autoimmune_id___96)), ~ ifelse(otherchronic == "told i had lupus/sle at 19 diagnosed with idiopathic thrombocytopenia at aged 19 by doctors after attending hospital for strep throat infection results came up in blood work hereditary hemochromatosis only diagnosed by pressuring my gp but i was 42 yrs old by then the damages to my liver was already quite severe mthfr i found out myself by doing dna and genome testing i then developed the other auto immunes by age 21 (i had severe dose of rheumatic fever as a toddler)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120 )), ~ ifelse(otherchronic == "ehlers danlos syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___116 )), ~ ifelse(otherchronic == "pots (postural orthostatic tachycardia syndrome)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___116 )), ~ ifelse(otherchronic == "pots", 1, .))
#TODO done
df.ad <- df.ad %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___116)), ~ ifelse(otherchronic == "fibromyalgia, pots", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___78 , autoimmune_id___114)), ~ ifelse(otherchronic == "narcolepsy type 2 / idiopathic hypersomnia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___120 , autoimmune_id___72 , autoimmune_id___128 , autoimmune_id___116 )), ~ ifelse(otherchronic == "myalgic encephalomyelitis (fatigue chronic syndrome), fibromyalgia ehlers danlos syndrome ibs pots mals", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___133 , autoimmune_id___123 )), ~ ifelse(otherchronic == "heds, mcas", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___72 , autoimmune_id___98, chronic___3)), ~ ifelse(otherchronic == "genetic neuroendocrine cancer - sdhd genetic fault, with four primary paragangliomas also ibs and medication resistant gerd", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___53 , autoimmune_id___55 )), ~ ifelse(otherchronic == "orthostatic intolerance - since childhood but substantially worsened by me/cfs each time", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___120 , autoimmune_id___64, chronic___4)), ~ ifelse(otherchronic == "ehlers danlos syndrome, fibromyalgia, complex post traumatic stress disorder", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___135 , autoimmune_id___116 , chronic___11, autoimmune_id___100)), ~ ifelse(otherchronic == "hypermobile spectrum disorder (connective tissue suspected eds pending dx), pots, adhd, inappropriate tachycardia", 1, .)) %>%
  mutate_at(vars(c(chronic___15 , autoimmune_id___135 , autoimmune_id___64)), ~ ifelse(otherchronic == "hsd/eds - chronic pain fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___135 )), ~ ifelse(otherchronic == "hypermobility spectrum disorder/heds", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___123 , autoimmune_id___116 , autoimmune_id___133 , autoimmune_id___55 , autoimmune_id___98 , autoimmune_id___64 , autoimmune_id___129 , autoimmune_id___145 , autoimmune_id___178, chronic___18 , chronic___11, chronic___9, chronic___10)), ~ ifelse(otherchronic == "heds, pots, mcas, me/cfs, ibs, gerd, fibromyalgia, hormonal migraines, cervicogenic headaches, asd, adhd, anxiety, depression, ptsd, hyperacusis, scoliosis, plantar fasciitis, pgad, rosacea, gallstone, muscle spasms", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___123 , autoimmune_id___116 , chronic___10)), ~ ifelse(otherchronic == "hypermobile ehlers danlos syndrome postural orthostatic tachycardia syndrome (since 2017) chronic migraines (2000-2016) hemiplegic migraine (since 2019)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___116 , chronic___9)), ~ ifelse(otherchronic == "autism, pots", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___112 , autoimmune_id___55)), ~ ifelse(otherchronic == "hypothyroidism that cant be controlled with medication due reactions to medications chronic fatigue syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___149)), ~ ifelse(otherchronic == "gastroparesis", 1, .)) %>%
  mutate_at(vars(c(chronic___15 , autoimmune_id___120 , autoimmune_id___55 )), ~ ifelse(otherchronic == "eds, chronic pain and chronic fatigue syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___105 , autoimmune_id___120 )), ~ ifelse(otherchronic == "chronic fatigue , raynaus", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120 )), ~ ifelse(otherchronic == "ehlers danlos", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___65 , chronic___10, chronic___4)), ~ ifelse(otherchronic == "cfs, endometriosis, migraines, anxiety", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___63 , autoimmune_id___64 )), ~ ifelse(otherchronic == "traumatic brain injury, autonomic dysfunction, fibromyalgia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120 , chronic___10)), ~ ifelse(otherchronic == "eds, chronic migraines, idiopathic intracranial hypertension", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120 , autoimmune_id___72 )), ~ ifelse(otherchronic == "ibs, eds, li fraumeni syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 , autoimmune_id___116 , autoimmune_id___101 , autoimmune_id___69 , autoimmune_id___98 , chronic___10, chronic___1)), ~ ifelse(otherchronic == "endometriosis postural orthostatic tachycardia syndrome adenomyosis asthma impaired glucose tolerance migraine prophylaxis gastritis gastro-oesophageal reflux disease palpitations allergic conjunctivitis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 )), ~ ifelse(otherchronic == "me/cfs formally diagnosed 2017 chronic fatigue diagnosed 1998", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___116 , autoimmune_id___135 )), ~ ifelse(otherchronic == "hsd & pots", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 )), ~ ifelse(otherchronic == "cfs", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___61 , autoimmune_id___72 , autoimmune_id___92 , autoimmune_id___120, chronic___4 )), ~ ifelse(otherchronic == "fibromyalgia diverticulitulosis osteoarthritis ibs remnant ovary ptsd elhers danlos", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___135 , autoimmune_id___68 , autoimmune_id___55 , autoimmune_id___72 , autoimmune_id___116 , autoimmune_id___14 )), ~ ifelse(otherchronic == "fnd disorder hypermobile possibly eds syndrome cfs ibs graves disease possible pots", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___84 )), ~ ifelse(otherchronic == "pcos", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___160 )), ~ ifelse(otherchronic == "svt", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___133 , autoimmune_id___116 , autoimmune_id___149 , autoimmune_id___63 , chronic___10, chronic___22)), ~ ifelse(otherchronic == "me/cfs mast cell activation disorder dysautonomia (pots, gastroparesis, nmh) chronic migraine", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120 , autoimmune_id___187, autoimmune_id___116 , autoimmune_id___34 , autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___98 , chronic___1, chronic___18)), ~ ifelse(otherchronic == "eds (subtype yet to be determined suspects heds and veds) - tos with subclavical artery impingement, pots, vcd associated conditions all confirmed seronegative spondyloarthritis (still determining all subtypes - psoriatic arthritis confirmed) hla-dq8 & hla-b27 positive - post infection onset cfs/me - post infection onset macular ped - slow progression to macular degeneration fibromyalgia (thought to be misdiagnosis of eds and autoimmune diagnoses being subtype comfirmed) past severe gerd with esophageal ulceration childhood asthma past chronic infections (borreliosis - europe, babesia duncani, rickettsia spotted fever - nsw)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___123 )), ~ ifelse(otherchronic == "heds", 1, .)) %>%
  mutate_at(vars(c(chronic___15 , autoimmune_id___72 , autoimmune_id___61 , autoimmune_id___101 , autoimmune_id___65 )), ~ ifelse(otherchronic == "irritable bowel syndrome, lactose intolerance, diverticulitis, endometriosis and adenomyosis chronic joint pain", 1, .))
#TODO done
df.ad <- df.ad %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___131 )), ~ ifelse(otherchronic == "fibromyalgia, eczema", 1, .)) %>%  
  mutate_at(vars(c(autoimmune_id___65 , autoimmune_id___55 , autoimmune_id___116 , autoimmune_id___135 )), ~ ifelse(otherchronic == "endometriosis, chronic fatigue syndrome, postural orthostatic tachycardia syndrome, possible ehlers-danlos but on the hypermobile spectrum at least", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___17 , autoimmune_id___46 )), ~ ifelse(otherchronic == "hashimotos addisons disease", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___149 )), ~ ifelse(otherchronic == "hyper plastic polyposis syndrome gastro paresis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___116 , autoimmune_id___120 , autoimmune_id___163, chronic___5 )), ~ ifelse(otherchronic == "pots, eds, generalized dystonia, heart disease", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___84 , autoimmune_id___98 , chronic___11, chronic___10)), ~ ifelse(otherchronic == "migraine with aura, pcos, gerd, adhd", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___65 , autoimmune_id___98, autoimmune_id___160  )), ~ ifelse(otherchronic == "me/cfs, fibromyalgia, new daily persistent headache, svt, gerd, possible endometriosis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___105 , autoimmune_id___64)), ~ ifelse(otherchronic == "fibromyalgia / chronic fatigue", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120, chronic___22, autoimmune_id___177)), ~ ifelse(otherchronic == "ehlers danlos syndrome neurocardiogenic syncope thalassemia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___72 )), ~ ifelse(otherchronic == "irritable bowel syndrome; aka we dont know whats wrong with your guts syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___84 , autoimmune_id___89 , autoimmune_id___92 , autoimmune_id___98 , autoimmune_id___65  )), ~ ifelse(otherchronic == "endometriosis, polycystic ovarian syndrome, psoriasis, osteoarthritis as a result of untreated friebergs infarction, gastrointestinal reflux disease", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___123 )), ~ ifelse(otherchronic == "hypermobile ehlers danlos syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___96 )), ~ ifelse(otherchronic == "chronic sinusitis rheumatic fever aged 3 1959 found to have tnfrsf13b a181e/ taci when son then aged 16 being investigated for cvid after years of illnesses mother had rheumatoid arthritis from about age 30 - found to also have the taci mutation she also had glomerulonephritis from about age 60 father was diagnosed with glomerulonephritis shortly before his death aged 88", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 )), ~ ifelse(otherchronic == "endometriosis, and now have pre stage cancerous cells in stomach, metaplasia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 , autoimmune_id___64, chronic___4 )), ~ ifelse(otherchronic == "stage 4 endometriosis, fibromyalgia, adjustment disorder with anxiety", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 , autoimmune_id___84 , chronic___11)), ~ ifelse(otherchronic == "endometriosis, pcos, adhd", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 , autoimmune_id___116 )), ~ ifelse(otherchronic == "endometriosis and pots", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___123 , autoimmune_id___64 , autoimmune_id___68 , autoimmune_id___65 , autoimmune_id___98 , autoimmune_id___72 , autoimmune_id___116  , autoimmune_id___84 , chronic___10 , chronic___21)), ~ ifelse(otherchronic == "hypermobility syndrome ( hyper mobile ehlers danlos syndrome) fibromyalgia chronic migraines functional neurological disorder pcos endometriosis reflux/gord hayfever/ severe allergies ibs possible pots seasonal asthma", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 )), ~ ifelse(otherchronic == "fibromyalgia chronic fatigue syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 , autoimmune_id___101 , autoimmune_id___63, autoimmune_id___51 )), ~ ifelse(otherchronic == "small fibre neuropathy endometriosis adenomysosis dysautonomia", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___89 , autoimmune_id___131 )), ~ ifelse(autoimmune_id_other == "eczema and psoriasis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 )), ~ ifelse(otherchronic == "chronic fatigue syndrome, fibromyalgia", 1, .))



#### test that the multicolumn mutation only affects single row, make 2 multocoltest dfs one with and one without mutate_at(). eg.  "treated & now in remission"
#multicoltest1 <- data.clean %>% select(record_id, autoimmune_id_other, autoimmune_id___15 , autoimmune_id___22)
#multicoltest2 <- df.ad %>% select(record_id, autoimmune_id_other, autoimmune_id___15 , autoimmune_id___22)
#then search for "in remission" and "sle" and compare before & after
#checking for number of cf entries
#df.ad %>% select(record_id, autoimmune_id_other, autoimmune_id___38, autoimmune_id___97) %>% filter(autoimmune_id___97 == 1)
#label all the categories by illness

######
#
# co-morbidity addition
#
######

#### need to check what was logged as co-morbidity and whether it needs to be in df.ad
# other.comorbs.all <- data.clean %>%
#   select(otherchronic) %>%
#   #  filter_all(any_vars(str_detect(., pattern = ""))) %>%
#   mutate_all(tolower) %>%
#   mutate_all(str_trim) %>%
#   filter(otherchronic != "") %>%
#   distinct(otherchronic) 
# 
# other.comorbs.old <- read_excel("/home/anna/postdoc/spoonie/autoimmunity_community/bin/comorbs-other-all_unique.xlsx") %>%
#   select(otherchronic) %>%
#   #  filter_all(any_vars(str_detect(., pattern = ""))) %>%
#   mutate_all(tolower) %>%
#   mutate_all(str_trim) %>%
#   filter(otherchronic != "") %>%
#   distinct(otherchronic)
# 
# other.comorbs.new <- other.comorbs.all %>%
#   filter(!other.comorbs.all$otherchronic %in% other.comorbs.old$otherchronic)
# ### todo this returns 6 more entries than it should... load other old data.clean and skip excel file gave same result. perhaps those extra email duplication that were removed
# write_tsv(other.comorbs.new, "1-22_comorbs-new.tsv")


##todo work out why there is a difference of 1 entry between how otherchronic is processed in df.ad or other.comorbs.new, but shows no difference when comparing the two
# test.trim <- df.ad %>%
#   select(otherchronic) %>%
#   filter(df.ad$otherchronic %in% other.comorbs.new$otherchronic)
# 
# test.trim$otherchronic[!(test.trim$otherchronic %in% other.comorbs.new$otherchronic)]
# 
# test.trim %>%  filter(!other.comorbs.new$otherchronic %in% test.trim$otherchronic)

#TODO done
df.ad <- df.ad %>%
#fix co-morbidities logged in 'other ad'
  mutate_at(vars(chronic___15, chronic___21 ), ~ ifelse(autoimmune_id_other == "fibromyalgia symptoms that led to the diagnosis: 1 hair loss (not attributed to alopecia) 2 unexplained pain throughout body 3 emotional trauma through marriage breakup and divorce 4 stress from demanding job as a high school teacher 5 fatigue 6 multiple medical conditions that led to several surgical procedures (gall bladder removal, ankle reconstruction, uterine ablation, carpel tunnel surgery, removal of several recurring ganglions) 7 severe allergic reactions resulting in anaphylactic reactions and increased food sensitivity", 1, .)) %>% 
  mutate_at(vars(chronic___3 ), ~ ifelse(autoimmune_id_other == "neurofibromatosis", 1, .)) %>%
  mutate_at(vars(chronic___3 ), ~ ifelse(autoimmune_id_other == "prostate cancer 2009 hashimotos disease (autoimmune hypothyroidism) 1996 coeliac disease 2000", 1, .)) %>%
  mutate_at(vars(chronic___3 ), ~ ifelse(autoimmune_id_other == "non hodgkins follicular lymphoma", 1, .)) %>%
  mutate_at(vars(chronic___2 ), ~ ifelse(autoimmune_id_other == "psoriasis, psoriatic arthritis, adrenal insufficiency and steroid induced diabetes" |
                                           otherchronic == "lichen sclerosus, osteoporosis, steroid induced diabetes, anxiety, migraines", 1, .)) %>%
  mutate_at(vars(chronic___3 ), ~ ifelse(autoimmune_id_other == "t-cell large granular lymphocytic leukemia", 1, .)) %>%
  mutate_at(vars(chronic___1 ), ~ ifelse(autoimmune_id_other == "fibromyalgia, irritable bowel syndrome, asthma, endometriosis", 1, .)) %>%
  mutate_at(vars(chronic___1, autoimmune_id___55 ), ~ ifelse(autoimmune_id_other == "chronic asthma since baby, chronic fatigue syndrome since 2015", 1, .)) %>%
  mutate_at(vars(chronic___1 ), ~ ifelse(autoimmune_id_other == "me/cfs fibromyalgia multiple chemcal sensivities, asthma, hyperactive immmune system", 1, .)) %>%
  mutate_at(vars(chronic___2 ), ~ ifelse(otherchronic == "perhaps undiagnosed type 2 diabetes shown as gestational diabetes", 1, .)) %>%
  mutate_at(vars(chronic___10 ), ~  ifelse(otherchronic == "migraines", 1, .)) %>% 
  mutate_at(vars(autoimmune_id___36, autoimmune_id___112, autoimmune_id___6, chronic___13), ~  ifelse(autoimmune_id_other == "perthes disease 1964 sarcoidosis l/lung 1990 l/lung collapse phrenic nerve palsy - sarcoidosis 2002 hypothyroid 2003 ongoing coeliacs disease 2010 ongoing central sleep apnoea 2012 ongoing nerve damage - l/leg, l/shoulder interstitial cystitis 2018 ongoing bursitis l/shoulder, l/elbow 2020", 1, .)) %>%
  mutate_at(vars(chronic___11, chronic___4 ), ~  ifelse(otherchronic == "adhd, anxiety disorder", 1, .)) %>%  
  mutate_at(vars(chronic___13 ), ~  ifelse(autoimmune_id_other == "myalgic encephalomyelitis, interstitial myositis, mast cell activation syndrome," |
                                             autoimmune_id_other == "interstitial lung disease" |
                                             autoimmune_id_other == "copd (never smoked) diagnosed in august 2019 at broome wa whilst on holidays" , 1, .)) %>%
  mutate_at(vars(chronic___11 ), ~ ifelse(otherchronic == "recently diagnosed with adhd, unclear whether it counts as an illness/impairment etc", 1, .)) %>%
  mutate_at(vars(chronic___15 , chronic___11 , chronic___9 ), ~ ifelse(otherchronic == "do pervasive developmental disorders count adhd, autistic, nerve pain", 1, .)) %>%
  mutate_at(vars(chronic___10), ~ ifelse(otherchronic == "family history of chronic headaches - father and paternal cousin had trigeminal neuralgia, my paternal aunty and female cousins have chronic migraines with bells palsy symptoms i have also had chronic headaches since i was a teenager - one psychiatrist said possible cluster migranes,", 1, .)) %>%
  mutate_at(vars(chronic___11), ~ ifelse(otherchronic == "adhd", 1, .)) %>%
  mutate_at(vars(chronic___11), ~ ifelse(otherchronic == "probably adhd", 1, .)) %>%
  mutate_at(vars(chronic___10), ~ ifelse(otherchronic == "migraine with aura", 1, .)) %>%
  mutate_at(vars(chronic___10 , chronic___15), ~ ifelse(data.clean$drug_other_other == "weed for migraine pain", 1, .)) %>%
  mutate_at(vars(chronic___10), ~ ifelse(data.clean$symptoms_other == "magnesium, n-acetyl cysteine, inositol for pcos  magnesium, riboflavin for migraines", 1, .)) %>%
  mutate_at(vars(chronic___10), ~ ifelse(data.clean$symptoms_other == "beta blocker prescribed for ectopic heart beats fixed my migraines (mostly)", 1, .)) %>%
  mutate_at(vars(chronic___10), ~ ifelse(data.clean$symptoms_other == "rest, electrolytes, medication to prevent and relieve migraines (low dose endep and maxalt wafers, respectively), aspirin and ibuprofen when needed, magnesium and iron supplements", 1, .)) 

#TODO done
#1-22 additions

df.ad <- df.ad %>%
  mutate_at(vars(c(chronic___11, chronic___9)), ~ ifelse(otherchronic == "adhd autism", 1, .)) %>%
  mutate_at(vars(c(chronic___15 , autoimmune_id___135)), ~ ifelse(otherchronic == "chronic pain, joint hypermobility, central sensitisation", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___43)), ~ ifelse(otherchronic == "wegeners gpa", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___112)), ~ ifelse(otherchronic == "underactive thyroid diagnosed after pregnancy at 40 years of age", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___162, chronic___5)), ~ ifelse(otherchronic == "other: arthritis (from prolonged steriod use - prednisolone, which was likely to be a factor in having a coronary arterial atherosclerosis event)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___43)), ~ ifelse(otherchronic == "granulomatosis and polyangiitis", 1, .)) %>%
  mutate_at(vars(c(chronic___3, chronic___10 , chronic___15)), ~ ifelse(otherchronic == "- squamous cell carcinoma removed near lt eye - experienced debilitating pain when 25yrs disscovered i had one kidney, uterus didelphys, bicollis and rt and lt cervices all specialist saying nothing wrong - kidney test fine no one could give a reason for the pain after approx 8months saw an acupuncturist in desperation he said kidney had very little pulse and treated it for 6weeks no relapses - severe dyslexia but i have learned how to compensate - severe migraine attacks (lasting two days) from 12yrs to 19yrs treated with homeopathy and chiropractic rare attacks until approx 30yrs then none 3yrs ago diagnosed with wet macular degeneration in both eyes (now dry)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55)), ~ ifelse(otherchronic == "me/cfs, alpha 1 antitrypsin deficiency ph z", 1, .)) %>%
  mutate_at(vars(c(chronic___3)), ~ ifelse(otherchronic == "prolactinoma", 1, .)) %>%
  mutate_at(vars(c(chronic___13, autoimmune_id___64, autoimmune_id___66, autoimmune_id___162)), ~ ifelse(otherchronic == "fibromyalgia, raynauds, ild, right knee needs replacement and severe arthritis in c4,5 & 6 with nerve compression", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64)), ~ ifelse(otherchronic == "fybromyalgia myalgic encephalomyelitis/chronic fatigue syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___72)), ~ ifelse(otherchronic == "irritable bowel syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___101 , autoimmune_id___65, chronic___16, chronic___4, chronic___11, chronic___9)), ~ ifelse(otherchronic == "endometriosis, adenomyosis, cystic lesion in left temporal lobe, epilepsy, viral meningitis, adhd, cptsd, ocd, asd,", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___123 , autoimmune_id___116 )), ~ ifelse(otherchronic == "formally diagnosed with heds, me/cfs, pots", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___70)), ~ ifelse(otherchronic == "idiopathic thrombocytopenia took my spleen out to cure it", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___98, chronic___17, autoimmune_id___114)), ~ ifelse(otherchronic == "gord idiopathic hypersomnia sleep apnea corneal errosion", 1, .)) %>%
  mutate_at(vars(c(chronic___13, chronic___23)), ~ ifelse(otherchronic == "clotting disorder lung scaring bronchieactisis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___131 , autoimmune_id___65 , autoimmune_id___64 , autoimmune_id___135 , autoimmune_id___149 , autoimmune_id___133 , autoimmune_id___55 , autoimmune_id___116 , autoimmune_id___66 , chronic___10 , chronic___17)), ~ ifelse(otherchronic == "anemia eczema endometriosis fibromyalgia gastroporesis hypermobility spectrum disorder insomnia mast cell activation syndrome migraines myalgic encephalomyelitis pots (postural orthostatic tachycardia syndrome) renyards", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 , autoimmune_id___92 , autoimmune_id___66 , autoimmune_id___140 , autoimmune_id___72 , autoimmune_id___64 , autoimmune_id___98 , chronic___10 , chronic___17, chronic___5)), ~ ifelse(otherchronic == "endometriosis, osteoarthritis, raynauds, cochlea hydrops/meniers, irritable bowel syndrome, neurogenic bladder, migraine, fibromyalgia, hiartus hernia/gord/barretts oesophagus, sleep apnoea, atrial fibrillation", 1, .)) %>%
  mutate_at(vars(c(chronic___15 , chronic___4 , autoimmune_id___170)), ~ ifelse(otherchronic == "back injury in 1988 - rsi resulting in prolapsed disc and chronic pain, with occasional flare ups of sciatica approx every few years 3 months bed rest and hospital stay due to enlarged discs in l4/l5 in 1998 ross river fever in 2000 de quervains thyroiditis - 2005 + mild depression pregnancy in 2006/7 - preeclampsia resulting in preterm birth at 30 weeks played hockey at representative level from 2010 but started to experience tendinitis in many different joints following pregnancy including both wrists, elbow, hips, shoulder", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 ,  autoimmune_id___131)), ~ ifelse(otherchronic == "endometriosis eczema", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___34)), ~ ifelse(otherchronic == "psoriatic arthritis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___92 , autoimmune_id___116)), ~ ifelse(otherchronic == "pots osteoarthritis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___81)), ~ ifelse(otherchronic == "osteoporosis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___131, chronic___11)), ~ ifelse(otherchronic == "adhd, eczema", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___81, chronic___21)), ~ ifelse(otherchronic == "osteoporosis hayfever constipation", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___68)), ~ ifelse(otherchronic == "have recently been diagnosed with functional neurological disorder by a neurologist", 1, .)) %>%
  mutate_at(vars(c(chronic___18)), ~ ifelse(otherchronic == "scoliosis limited lung capacity", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120)), ~ ifelse(otherchronic == "ehers danloss", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___92 , autoimmune_id___112)), ~ ifelse(otherchronic == "currently diagnosed with hypothyroidism but causenot yet investigated also have osteoarthritis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___6 , autoimmune_id___156)), ~ ifelse(otherchronic == "haemochromatosis- which has just been diagnosed iron overload after coeliac disease (gf diet ) under control", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___131)), ~ ifelse(otherchronic == "atopic eczema", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___6, chronic___3)), ~ ifelse(otherchronic == "coeliac disease---diagnosed and diet since 1950,,apparent cure 1966 ----- rediagnosed in 1995--still on diet--- prostate cancer 2018 then prostate removed 2019", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___123 , autoimmune_id___116 , autoimmune_id___133)), ~ ifelse(otherchronic == "ehlers danlos syndrome (hypermobile type), postural orthostatic tachycardia syndrome, likely mast cell activation syndrome", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___123 , autoimmune_id___116 , autoimmune_id___114 , chronic___17)), ~ ifelse(otherchronic == "i have hypermobile ehlers danlos (geneticist confirmed), pots (diagnosed by prof wilcox & dr spies, including ttt) & hypersomnia (by multiple sleep latency test) i have autoimmune infertility of unclear type, i am fairly sure i have undiagnosed antiphospholipid syndrome", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___116 , autoimmune_id___65)), ~ ifelse(otherchronic == "endometriosis pots", 1, .))
#TODO done
df.ad <- df.ad %>%
  mutate_at(vars(c(chronic___2, chronic___15, chronic___4, autoimmune_id___65, autoimmune_id___64, chronic___18)), ~ ifelse(otherchronic == "my type 2 diabetes was caused by prolonged use of a moderate dose of prednisolone over many years fibromyalgia general anxiety mild depression post-herpetic neuralgia dry eyes i have also had endometriosis i also have spondylolisthesis and spinal stenosis and am on a waiting list for a lumbar spine fusion these are not diseases but they do impact my pain, fatigue and activity levels so they influence some of my answers in this survey", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___101, chronic___18)), ~ ifelse(otherchronic == "adenomyosis keratosis obturans foraminal stenosis, degenerative disc disease with herniated lumbar discs, annular tear", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___55 ,  autoimmune_id___42)), ~ ifelse(otherchronic == "chronic fatigue syndrome, ulcerative colitis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___26)), ~ ifelse(otherchronic == "ms", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___135 , chronic___9)), ~ ifelse(otherchronic == "chronic fatigue syndrome, hypermobility syndrome, autism spectrum disorder", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___35)), ~ ifelse(otherchronic == "severe deforming rheumatoid arthritis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___123)), ~ ifelse(otherchronic == "ehlers danlos syndrome hyper mobility type", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___63, chronic___19, autoimmune_id___184)), ~ ifelse(otherchronic == "chiari malformation syringomyelia dysautonomia (orthostatic hypotension) premenstrual dysphoric disorder", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___116)), ~ ifelse(otherchronic == "chronic fatigue syndrome fibromyalgia pots i cant proceed as it is asking me to specify which i have", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65 , autoimmune_id___165 , autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___132 , chronic___10)), ~ ifelse(otherchronic == "endometriosis chronic atypical migraine cold urticaria myalgic encephelomyalitis fribromyalgia multiple chemical sensitivity", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , chronic___9)), ~ ifelse(otherchronic == "fibromyalgia autism", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___84 , autoimmune_id___120)), ~ ifelse(otherchronic == "ehlers-danlos syndrome, polycystic ovary syndrome", 1, .)) %>%
  mutate_at(vars(c(chronic___3)), ~ ifelse(otherchronic == "pituitary tumour", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___81)), ~ ifelse(otherchronic == "oesteoporosis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___161 , autoimmune_id___89 , chronic___5)), ~ ifelse(otherchronic == "peripheral neuropathy, atrial fibrillation, psoriasis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___63 , autoimmune_id___55)), ~ ifelse(otherchronic == "me/cfs dysautonomia, being investigated for pots", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___65, autoimmune_id___187)), ~ ifelse(otherchronic == "insulin resistance thoracic outlet syndrome stage 3 endometriosis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 , autoimmune_id___112 , autoimmune_id___72 , autoimmune_id___171 )), ~ ifelse(otherchronic == "chronic fatigue syndrome fibromyalgia hypothyroidism lyme disease irritable bowel syndrome ive seen public and private specialists the survey wouldnt let me enter this", 1, .)) %>%
  mutate_at(vars(c(chronic___20)), ~ ifelse(otherchronic == "i have rh negative blood also mthfr gene", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___131 , autoimmune_id___120 , autoimmune_id___133 , autoimmune_id___64 , autoimmune_id___63 , autoimmune_id___116 , autoimmune_id___98 , autoimmune_id___65 , autoimmune_id___125 , chronic___5 , chronic___4 , chronic___21)), ~ ifelse(otherchronic == "eczema, hayfever, eds, mcas, primary hyperaldosteronism, svts, fibromyalgia, depression, anxiety, eosinophilic & allergic asthma, dysautonomia, pots, gerd, endometriosis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64 , autoimmune_id___55 , chronic___4 , chronic___9 , chronic___11)), ~ ifelse(otherchronic == "fibromyalgia, chronic mental health concerns, me/cfs, autism, adhd", 1, .)) %>%
  mutate_at(vars(c(chronic___10)), ~ ifelse(otherchronic == "i did suffer tonsillitis severely from 8yrs to 19yrs & had surgery to remove at 19yrs i suffered severe migraine from 8yrs to age 60ys this was dark room and vomiting for up to 2 days then a week to finally recover", 1, .)) %>%
  mutate_at(vars(c(chronic___11 , chronic___9)), ~ ifelse(otherchronic == "autism add", 1, .)) %>%
  mutate_at(vars(c(chronic___11 , chronic___20 , chronic___21)), ~ ifelse(otherchronic == "adhd, mthfr under methylated gene mutation, allergies", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___116)), ~ ifelse(otherchronic == "me/cfs, pots, idiopathic pancreatic exocrine insufficiency", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55 , autoimmune_id___64 , chronic___9)), ~ ifelse(otherchronic == "fibromyalgia me/cfs, autism", 1, .)) %>%
  mutate_at(vars(c(chronic___4 , autoimmune_id___53)), ~ ifelse(otherchronic == "in 2020 as a nurse caught a virus swabbing for covid whilst being bullied at work-> anxiety there was a long time until healthcare was accessible and the diagnosis reached turned away from community and ed department due to covid pandemic diagnosed with epstein-barr virus, glandular fever with positive antibodies over a year later, and then autonomic nervous system disorder - orthostatic intolerance", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___112)), ~ ifelse(otherchronic == "chronic, recurring (every few weeks) tonsillitis from infancy until 19-yrs when tonsils removed in 1989 i developed hsv-2 in 2019 i was found to have low thyroid levels and have been on a low-dose thyroid treatment since then", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___105 , autoimmune_id___107)), ~ ifelse(otherchronic == "chronic fatigue and connective tissue disorder", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55)), ~ ifelse(otherchronic == "chronic fatigue syndrome/me", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___112)), ~ ifelse(otherchronic == "hypothyriod", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___92)), ~ ifelse(otherchronic == "hip dysplasia diagnosed 2021 with severe osteoarthritis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___63, chronic___5)), ~ ifelse(otherchronic == "please see above all the medical diagnoses labels (i also have a history of q fever, ebv, dengue fever early 2001 and febrile convulsions which my children also had and the hole in heart / dysautonomia heart issues)", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___81, chronic___13)), ~ ifelse(otherchronic == "bronchiectasis, osteoporosis", 1, .)) 

###final datapull additions
#other chronic
df.ad <- df.ad %>% 
  mutate_at(vars(c(autoimmune_id___173, autoimmune_id___120)), ~ ifelse(autoimmune_id_other == "- autoimmune progesterone anaphylaxis - ehlers-danlos syndrome", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___46, autoimmune_id___92)), ~ ifelse(autoimmune_id_other == "addisons disease osteoarthritis", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___46)), ~ ifelse(autoimmune_id_other == "anti parietal cell antibody addisons", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___50)), ~ ifelse(autoimmune_id_other == "anti phospholipid antibody syndrome" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___50, autoimmune_id___174)), ~ ifelse(autoimmune_id_other == "anti-phospholipid syndrome gi dysmotility" , 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___50, autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "antiphospholipid syndrome fibromyalga" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___52)), ~ ifelse(autoimmune_id_other == "auto immune hepatitis" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___52)), ~ ifelse(autoimmune_id_other == "auto immune hepatitis barretts disease" , 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___71)), ~ ifelse(autoimmune_id_other == "autoimmune inflammatory arthritis subtype as yet undetermined but suspected seronegative rheumatoid arthritis" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55)), ~ ifelse(autoimmune_id_other == "ccfs" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55, autoimmune_id___116)), ~ ifelse(autoimmune_id_other == "cfs, pots" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___90)), ~ ifelse(autoimmune_id_other == "clinically isolated syndrome under investigation for multiple sclerosis" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___142)), ~ ifelse(autoimmune_id_other == "collagenous colitis, lymphocytic colitis" , 1, .)) 
#TODO done
df.ad <- df.ad %>%
  mutate_at(vars(c(autoimmune_id___108)), ~ ifelse(autoimmune_id_other == "doctor says i probably have an autoimmune disease without specifying which 28 years of chronic fatigue" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___120, autoimmune_id___101, autoimmune_id___17, autoimmune_id___113, autoimmune_id___132, autoimmune_id___98, autoimmune_id___6, autoimmune_id___133, chronic___13, chronic___1, chronic___21)), ~ ifelse(autoimmune_id_other == "ehlers - danlos syndrome, costens syndrome, bronchiectasis, adenomyosis, masticatory muscle myositis, tmjd, hashimotos thyroiditis, hyperthyroidism, asthma, multiple chemical sensitivity, food allergies, coeliac disease, gord, barretts oesophagus, mast cell activation syndrome left ventricular hyopertrophy, left branch bundle block and more", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___65, autoimmune_id___84, autoimmune_id___55, autoimmune_id___64, autoimmune_id___72)), ~ ifelse(autoimmune_id_other == "endometriosis, polycystic ovaries, chronic fatigue, fibromyalgia, ibs" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___121)), ~ ifelse(autoimmune_id_other == "enthesitis" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55, autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "fibromyalgia and mecfs previously diagnosed" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55, autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "fibromyalgia myalgic encephalomyelitis" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64, autoimmune_id___89)), ~ ifelse(autoimmune_id_other == "fibromyalgia psoriasis" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "fibromyalgia though i didnt think of it as autoimmune" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "fibromyalgia which i thought was an autoimmune disorder" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___64, chronic___18)), ~ ifelse(autoimmune_id_other == "fibromyalgia, degenerative disc disease" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___35)), ~ ifelse(autoimmune_id_other == "formerly diagnosed with seronegative arthritis i identify with as problems discussed in an american and australian as support groups on fb", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___73)), ~ ifelse(autoimmune_id_other == "frontal fibrosing alopecia" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___17)), ~ ifelse(autoimmune_id_other == "hashimotos" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___135, autoimmune_id___64, autoimmune_id___160, chronic___4, autoimmune_id___66, chronic___18, chronic___15)), ~ ifelse(autoimmune_id_other == "hyper mobility spectrum disorder, fibromyalgia, herniated discs, sciatica, meralgia parathesia, bile acid malabsorption, raynauds, anaemia, superventricular tachycardia, depression and cptsd", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___71)), ~ ifelse(autoimmune_id_other == "inflammatory arthritis - not defined" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___89)), ~ ifelse(autoimmune_id_other == "inverse psoriasis" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55, autoimmune_id___116)), ~ ifelse(autoimmune_id_other == "ive been formally diagnosed with postural orthostatic tachycardia syndrome (pots) and chronic fatigue syndrome (myalgic encephalomyelitis; me/cfs) both of these are thought to have an autoimmune component, so i wasnt sure whether to count them or not i was first taken to the doctor for my pots symptoms when i was 11 (in 2001) but i was repeatedly misdiagnosed and dismissed it took 19 years to get my formal diagnosis of pots the only way i was able to do this was to teach myself about medicine and find the conditions that were affecting my health myself, then prove it to the specialists by then id had to leave my phd due to my health and was almost completely bedridden", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___176)), ~ ifelse(autoimmune_id_other == "leukocytoclastic vasculitis" , 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___73)), ~ ifelse(autoimmune_id_other == "lichen planopilaris" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___74, autoimmune_id___64, autoimmune_id___89)), ~ ifelse(autoimmune_id_other == "lichen sclerosis, psoriasis, fibromyalgia" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___164)), ~ ifelse(autoimmune_id_other == "long covid" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___171)), ~ ifelse(autoimmune_id_other == "lyme disease" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___116, autoimmune_id___41, autoimmune_id___179)), ~ ifelse(autoimmune_id_other == "lymphocytic hypophysitis dx 2020 postural orthostatic tachycardia syndrome dx 2022 following answer is dated for t1 diabetes", 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___133)), ~ ifelse(autoimmune_id_other == "mast cell activation disorder" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___133, chronic___3)), ~ ifelse(autoimmune_id_other == "mast cell disorder complicated by metastatic neuroendocrine cancer" , 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___55)), ~ ifelse(autoimmune_id_other == "me chronic fatigue syndrome" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55, autoimmune_id___53)), ~ ifelse(autoimmune_id_other == "me/cfs (incl oi)" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55, autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "me/cfs fibromyalgia" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55, autoimmune_id___68)), ~ ifelse(autoimmune_id_other == "me/cfs fnd still seeking dx (post pfizer vaccine injury multiple symptoms)" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55, autoimmune_id___64, autoimmune_id___72)), ~ ifelse(autoimmune_id_other == "me/cfs, ibs, fibromyalgia" , 1, .)) %>%
  mutate_at(vars(c(chronic___3)), ~ ifelse(autoimmune_id_other == "metastatic breast cancer" , 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___55)), ~ ifelse(autoimmune_id_other == "myalgic encephalomyelitis - national centre for neuroimmune and emerging diseases (ncned) at griffith uni lead au researchers", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55, autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "myalgic encephalomyelitis (me/cfs) and fibromyalgia" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___55)), ~ ifelse(autoimmune_id_other == "myalgic encephalomyelitis/chronic fatigue syndrome," , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___116, autoimmune_id___78)), ~ ifelse(autoimmune_id_other == "narcolepsy, postural orthostatic tachycardia syndrome" , 1, .))

#TODO done
df.ad <- df.ad %>%
  mutate_at(vars(c(autoimmune_id___55)), ~ ifelse(autoimmune_id_other == "no disease but extremely high ana marker (2560) since acquiring me/ cfs 5 years ago" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___50)), ~ ifelse(autoimmune_id_other == "obstetric antiphospholipid syndrome" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___35)), ~ ifelse(autoimmune_id_other == "pending ra diagnosis" , 1, .)) %>%
  #mutate_at(vars(c(autoimmune_id___)), ~ ifelse(autoimmune_id_other == "polio surviver 1950 life long imbalance right side of the body" , 1, .)) %>% #TODO good question
  mutate_at(vars(c(autoimmune_id___116, chronic___15)), ~ ifelse(autoimmune_id_other == "pots crps" , 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___46)), ~ ifelse(autoimmune_id_other == "primary adrenal insufficiency" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___46)), ~ ifelse(autoimmune_id_other == "primary adrenal insufficiency (addisons disease)" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___89, autoimmune_id___34)), ~ ifelse(autoimmune_id_other == "psoriasis - diagnosed in the early 1980s psa - believed to be suffering from, since 2019, not formally diagnosed" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___89, autoimmune_id___131)), ~ ifelse(autoimmune_id_other == "psoriasis / eczema" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___89)), ~ ifelse(autoimmune_id_other == "psoriasis of the skin also and maybe other" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___89)), ~ ifelse(autoimmune_id_other == "psoriasis-like skin condition but did not have sufficient radiographic changes or inflammatory markers (in rheumatologists opinion) for psa", 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___101, autoimmune_id___66, autoimmune_id___59)), ~ ifelse(autoimmune_id_other == "raynauds, adenmyosis subacute cutaneous lupus" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___66, autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "raynauds, fibromyalgia" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___66, autoimmune_id___145)), ~ ifelse(autoimmune_id_other == "rosacea, raynauds" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___66)), ~ ifelse(autoimmune_id_other == "secondary reynauds disease" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___35)), ~ ifelse(autoimmune_id_other == "sero-negative arthritis" , 1, .)) %>%
  mutate_at(vars(c(chronic___18)), ~ ifelse(autoimmune_id_other == "seronegative spondyloarthrapthy" , 1, .)) %>% 
  mutate_at(vars(c(chronic___15)), ~ ifelse(autoimmune_id_other == "somatic pain" , 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___175)), ~ ifelse(autoimmune_id_other == "stiff person syndrome- perm varient" , 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___167)), ~ ifelse(autoimmune_id_other == "sweets syndrome" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___177)), ~ ifelse(autoimmune_id_other == "thalassemia trait" , 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___180)), ~ ifelse(autoimmune_id_other == "transverse myelitis," , 1, .)) %>% 
  mutate_at(vars(c(autoimmune_id___107, autoimmune_id___37)), ~ ifelse(autoimmune_id_other == "undifferentiated connective tissue disease - evolving to systemic sclerosis" , 1, .)) %>%
  mutate_at(vars(c(autoimmune_id___109, autoimmune_id___64)), ~ ifelse(autoimmune_id_other == "vasculitis and fibromyalgia" , 1, .))
 #TODO done
#co-morbs
df.ad <- df.ad %>% 
  mutate_at(vars(c(chronic___21)), ~ case_when(
    str_detect(otherchronic, "allergie") ~ 1,
    str_detect(otherchronic, "allergy") ~ 1,
    str_detect(otherchronic, "hayfever") ~ 1,
    str_detect(otherchronic, "hay fever") ~ 1,
    str_detect(otherchronic, "allergic") ~ 1,
    TRUE ~ .
    )) %>%
  mutate_at(vars(c(chronic___20)), ~ case_when(
    str_detect(otherchronic, "mthfr") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(chronic___10)), ~ case_when(
    str_detect(otherchronic, "migraine") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___65)), ~ case_when(
    str_detect(otherchronic, "endometriosis") ~ 1,
    str_detect(otherchronic, " endo ") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___116)), ~ case_when(
    str_detect(otherchronic, "pots") ~ 1,
    str_detect(otherchronic, "orthostatic tachycardia") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___55)), ~ case_when(
    str_detect(otherchronic, "myalgic encephalomyelitis") ~ 1,
    str_detect(otherchronic, "chronic fatigue syndrome") ~ 1,
    str_detect(otherchronic, "cfs") ~ 1,
    str_detect(otherchronic, "post viral fatigue") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___64)), ~ case_when(
    str_detect(otherchronic, "fibromyalgia") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___133)), ~ case_when(
    str_detect(otherchronic, "mast cell") ~ 1,
    str_detect(otherchronic, "mcas") ~ 1,
    str_detect(otherchronic, "chemical sensitivity") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___149)), ~ case_when(
    str_detect(otherchronic, "gastroparesis") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___72)), ~ case_when(
    str_detect(otherchronic, "irritable bowel") ~ 1,
    str_detect(otherchronic, "ibs") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___101)), ~ case_when(
    str_detect(otherchronic, "adenomyosis") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(chronic___15)), ~ case_when(
    str_detect(otherchronic, "crps") ~ 1,
    str_detect(otherchronic, "nerve pain") ~ 1,
    str_detect(otherchronic, "chronic atypical facial pain") ~ 1,
    str_detect(otherchronic, "chronic pain") ~ 1,
    str_detect(otherchronic, "musculoskeletal pain") ~ 1,
    str_detect(otherchronic, "lipoedema") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(chronic___4)), ~ case_when(
    str_detect(otherchronic, "ptsd") ~ 1,
    str_detect(otherchronic, "depression") ~ 1,
    str_detect(otherchronic, "anxiety") ~ 1,
    str_detect(otherchronic, "bipolar") ~ 1,
    str_detect(otherchronic, "affective disorder") ~ 1,
    str_detect(otherchronic, "borderline personality disorder") ~ 1,
    str_detect(otherchronic, "traumatic stress") ~ 1,
    TRUE ~ .
  )) 


df.ad <- df.ad %>%
  mutate_at(vars(c(chronic___9)), ~ case_when(
    str_detect(otherchronic, "autis") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___22)), ~ case_when(
    str_detect(otherchronic, "lupus") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___92)), ~ case_when(
    str_detect(otherchronic, "osteoarthritis") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___66)), ~ case_when(
    str_detect(otherchronic, "raynauds") ~ 1,
    str_detect(otherchronic, "reynauds") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(chronic___11)), ~ case_when(
    str_detect(otherchronic, "adhd") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(chronic___3)), ~ case_when(
    str_detect(otherchronic, "cancer") ~ 1,
    str_detect(otherchronic, "neurofibromatosis") ~ 1,    
    str_detect(otherchronic, "gestational trophoblastic disease") ~ 1,
    TRUE ~ .
  )) %>% #
  mutate_at(vars(c(chronic___16)), ~ case_when(
    str_detect(otherchronic, "epilepsy") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(chronic___25)), ~ case_when(
    str_detect(otherchronic, "intracranial hypertension") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(chronic___5)), ~ case_when(
    str_detect(otherchronic, "atrial fib") ~ 1,
    str_detect(otherchronic, "arrhythmia") ~ 1,
    str_detect(otherchronic, "cardiomyopathy") ~ 1,
    str_detect(otherchronic, "two holes in my heart") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(chronic___13)), ~ case_when(
    str_detect(otherchronic, "interstitial lung disease") ~ 1,
    str_detect(otherchronic, "copd") ~ 1,
    str_detect(otherchronic, "bronchiectasis") ~ 1,
    
    str_detect(otherchronic, "chronic obstructive pulmonary") ~ 1,
    str_detect(otherchronic, "nsip lung disease") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(chronic___17)), ~ case_when(
    str_detect(otherchronic, "insomnia") ~ 1,
    str_detect(otherchronic, "sleep apnoea") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___164)), ~ case_when(
    str_detect(otherchronic, "long covid") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___81)), ~ case_when(
    str_detect(otherchronic, "osteoporosis") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___84)), ~ case_when(
    str_detect(otherchronic, "pcos") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___112)), ~ case_when(
    str_detect(otherchronic, "hypothyr") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___14)), ~ case_when(
    str_detect(otherchronic, "grave") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___132)), ~ case_when(
    str_detect(otherchronic, "multiple chemical") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___131)), ~ case_when(
    str_detect(otherchronic, "eczema") ~ 1,
    str_detect(otherchronic, "ezcema") ~ 1,
    str_detect(otherchronic, "dermatitis") ~ 1,
    str_detect(otherchronic, "excema") ~ 1,
    TRUE ~ .
  ))%>%
  mutate_at(vars(c(autoimmune_id___171)), ~ case_when(
    str_detect(otherchronic, "lyme") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___98)), ~ case_when(
    str_detect(otherchronic, "gerd") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(chronic___23)), ~ case_when(
    str_detect(otherchronic, "leiden") ~ 1,
    str_detect(otherchronic, "mgus") ~ 1,
    TRUE ~ .
  ))


df.ad <- df.ad %>%
  mutate_at(vars(c(chronic___24)), ~ case_when(
    str_detect(otherchronic, "pah") ~ 1,
    str_detect(otherchronic, "htn") ~ 1,
    str_detect(otherchronic, "high blood pressure") ~ 1,
    str_detect(otherchronic, "pulmonary hypertension") ~ 1,
    str_detect(otherchronic, "pulmonary arterial hypertension") ~ 1,
    str_detect(autoimmune_id_other, "pulmonary arterial hypertension") ~ 1,
    TRUE ~ .
  )) %>%
mutate_at(vars(c(chronic___25)), ~ ifelse(otherchronic == "intracranial hypertension", 1, .)) %>%
mutate_at(vars(c(chronic___24)), ~ ifelse(otherchronic == "hypertension, arrhythmia" |
   otherchronic == "anemia, xlh (x-linked hypophospatemia), hypertension, dermatitis, vcd, trigeminal nerve", 1, .)) %>%
   mutate_at(vars(c(autoimmune_id___1)), ~ case_when(
    str_detect(otherchronic, "ankylosing spondylitis") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___63)), ~ case_when(
    str_detect(otherchronic, "dysautonomia") ~ 1,
    TRUE ~ .
  ))%>%
  mutate_at(vars(c(chronic___26)), ~ case_when(
    str_detect(otherchronic, "cerebral palsy") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(chronic___12)), ~ case_when(
    str_detect(otherchronic, "kidney disease") ~ 1,
    str_detect(otherchronic, "chronic kidney") ~ 1,
    str_detect(otherchronic, "glomerulonephritis") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___182)), ~ case_when(
    str_detect(otherchronic, "hepatitis c") ~ 1,
    TRUE ~ .
  ))%>%
  mutate_at(vars(c(chronic___18)), ~ case_when(
    str_detect(otherchronic, "bertolotti") ~ 1,
    str_detect(otherchronic, "scoliosis") ~ 1,
    str_detect(otherchronic, "spine injury") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___85)), ~ case_when(
    str_detect(otherchronic, "polymyalgia") ~ 1,
    TRUE ~ .
  ))%>%
  mutate_at(vars(c(autoimmune_id___91)), ~ case_when(
    str_detect(otherchronic, "cushing") ~ 1,
    str_detect(autoimmune_id_other, "cushing") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___38)), ~ case_when(
    str_detect(otherchronic, "sjogrens") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___51)), ~ case_when(
    str_detect(otherchronic, "small fibre neuropathy") ~ 1,
    TRUE ~ .
  ))%>%
  mutate_at(vars(c(autoimmune_id___17)), ~ case_when(
    str_detect(otherchronic, "autoimmune thyroid") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___136)), ~ case_when(
    str_detect(otherchronic, "dysphagia") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___155)), ~ case_when(
    str_detect(otherchronic, "trigeminal nerve") ~ 1,
    TRUE ~ .
  ))%>%
  mutate_at(vars(c(autoimmune_id___46)), ~ case_when(
    str_detect(otherchronic, "adrenal insufficiency") ~ 1,
    str_detect(otherchronic, "adrenal and pituitary insufficiency") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___162)), ~ case_when(
    str_detect(otherchronic, "gout") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___74)), ~ case_when(
    str_detect(otherchronic, "lichen sclerosis") ~ 1,
    TRUE ~ .
  ))%>%
  mutate_at(vars(c(autoimmune_id___181)), ~ case_when(
    str_detect(otherchronic, "glaucoma") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___117)), ~ case_when(
    str_detect(otherchronic, "lymphoedema") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___183)), ~ case_when(
    str_detect(otherchronic, "nash") ~ 1,
    str_detect(otherchronic, "fatty liver") ~ 1,
    TRUE ~ .
  ))%>%
  mutate_at(vars(c(autoimmune_id___184)), ~ case_when(
    str_detect(otherchronic, "dysphoric") ~ 1,
    str_detect(otherchronic, "pmdd") ~ 1,
    TRUE ~ .
  )) %>%
  mutate_at(vars(c(autoimmune_id___42)), ~ case_when(
    str_detect(otherchronic, "ulcerative colitis") ~ 1,
    TRUE ~ .
  ))
### HSD, EDS and hEDS are too similar for str_detect so have to allocate them individually
#data.clean %>% filter(str_detect(otherchronic, "heds|eds|ehler|hypermob|hsd")) %>% select(otherchronic) %>% unique()
  #EDS

 df.ad <- df.ad %>%
  mutate_at(vars(c(autoimmune_id___120)), ~ ifelse(otherchronic == "eds, chronic migraines, idiopathic intracranial hypertension" |
                                                     otherchronic ==   "eds, pots, cfs/me" |
                                                     otherchronic =="eds, chronic pain and chronic fatigue syndrome"|
                                                     otherchronic == "ibs, eds, li fraumeni syndrome" |
                                                     otherchronic == "eds (subtype yet to be determined suspects heds and veds) - tos with subclavical artery impingement, pots, vcd associated conditions all confirmed seronegative spondyloarthritis (still determining all subtypes - psoriatic arthritis confirmed) hla-dq8 & hla-b27 positive - post infection onset cfs/me - post infection onset macular ped - slow progression to macular degeneration fibromyalgia (thought to be misdiagnosis of eds and autoimmune diagnoses being subtype comfirmed) past severe gerd with esophageal ulceration childhood asthma past chronic infections (borreliosis - europe, babesia duncani, rickettsia spotted fever - nsw)" |
                                                     otherchronic == "eczema, hayfever, eds, mcas, primary hyperaldosteronism, svts, fibromyalgia, depression, anxiety, eosinophilic & allergic asthma, dysautonomia, pots, gerd, endometriosis" |
                                                     otherchronic == "pots eds mcas crps cfs hypothyroidism endometriosis adenomyosis" |
                                                     otherchronic == "eds, pots, mcas" |
                                                     otherchronic == "pots, eds, generalized dystonia, heart disease" |
                                                     otherchronic == "ehlers danlos syndrome, postural orthostatic tachycardia syndrome, gastroparesis/delayed gastric emptying, irritable bowel syndrome, undiagnosed neurological disorder which is functionally similar to a tbi (including delayed processing speed, elements of aphasia, memory loss, and difficulty with fine motor coordination), undiagnosed muscular issues which are symptomatically similar to myaesthenia gravis except the one conclusive test for it i got was negative so my doctors gave up on that" |
                                                     otherchronic == "ehlers danlos syndrome, pots, adenomyosis, lipoedema (painful fat disease, not lymphoedema however it is related)" |
                                                     otherchronic == "ehlers-danlos syndrome, pituitary-driven adrenal insufficiency, suspected mast cell activation syndrome (not diagnosed but taking medication for it), lichen sclerosis, associative prosopagnosia, ulnar nerve compression at elboow", 1, .)) %>%
  #hEDS
  mutate_at(vars(c(autoimmune_id___123)), ~ ifelse(otherchronic == "hypermobile ehlers-danlos syndrome; autism" |
                                                     otherchronic == "me/cfs, heds, pots, fibromyalgia" |
                                                     otherchronic =="suspected heds, chronic migraine, endometriosis, pcos" |
                                                     otherchronic =="hypermobile spectrum disorder (connective tissue suspected eds pending dx), pots, adhd, inappropriate tachycardia" |
                                                     otherchronic =="ehlers danlos syndrome (hypermobile type), postural orthostatic tachycardia syndrome, likely mast cell activation syndrome" |
                                                     otherchronic =="heds, pots, mcas, me/cfs, ibs, gerd, fibromyalgia, hormonal migraines, cervicogenic headaches, asd, adhd, anxiety, depression, ptsd, hyperacusis, scoliosis, plantar fasciitis, pgad, rosacea, gallstone, muscle spasms" |
                                                     otherchronic == "hypermobile ehlers danlos syndrome, pots currently investigating/treating mast cell disorder"|
                                                     otherchronic == "hypermobile ehlers danlos syndrome, adrenal and pituitary insufficiency, fibromyalgia, bipolar affective disorder ii, complex ptsd, autism" |
                                                     otherchronic == "hypermobile ehlers-danlos syndrome" |
                                                     otherchronic == "endometriosis, hypermobility, fibromyalgia" |
                                                     otherchronic == "heds"|
                                                     otherchronic == "hypermobile ehlers danlos syndrome" |
                                                     otherchronic == "i have hypermobile ehlers danlos (geneticist confirmed), pots (diagnosed by prof wilcox & dr spies, including ttt) & hypersomnia (by multiple sleep latency test) i have autoimmune infertility of unclear type, i am fairly sure i have undiagnosed antiphospholipid syndrome" |
                                                     otherchronic == "formally diagnosed with heds, me/cfs, pots"|
                                                     otherchronic == "hypermobile ehlers danlos syndrome postural orthostatic tachycardia syndrome (since 2017) chronic migraines (2000-2016) hemiplegic migraine (since 2019)" |
                                                     otherchronic == "hypermobile eds pots ocd depression" |
                                                     otherchronic == "heds, mcas" |
                                                     otherchronic == "ehlers-danlos syndrome (hypermobile with vascular features), postural orthostatic tachycardia syndrome with arrhythmia, aortic root aneurysm, raynauds syndrome, narcolepsy and cataplexy, widespread neuropathic pain, multiple chronic herniated disks, osteoarthritis of the spine, chronic fatigue, trans ischemic attacks, migraine diathesis, chronic respiratory infections, pneumothorax, endometriosis, irritable bowel syndrome, bowel perforation inflammatory bowel disease (), mast cell activation syndrome (), chiari malformation (), benedikt syndrome () anxiety, depression, complex post traumatic stress disorder, schizoid personality disorder, dissociative identity disorder" |
                                                     otherchronic == "pots heds pmdd ibs fibromyalgia" |
                                                     otherchronic == "severe presentation of hypermobile type ehlers-danlos syndrome; small fibre neuropathy (preliminary diagnosis);postural orthostatic tachycardia syndrome; ibs; and others that i regard as minor like raynauds and asthma im also autistic (self diagnosed, backed up by multiple professionals but no formal diagnosis), but not sure that that fits this question" , 1, .)) %>%
  #HSD
  mutate_at(vars(c(autoimmune_id___136, autoimmune_id___94)), ~ ifelse(otherchronic == "endometriosis, chronic fatigue syndrome, postural orthostatic tachycardia syndrome, possible ehlers-danlos but on the hypermobile spectrum at least" |
                                                     otherchronic =="pots, hypermobility syndrome, chronic migraine, bipolar, gastritis" |
                                                     otherchronic =="hypermobility syndrome ( hyper mobile ehlers danlos syndrome) fibromyalgia chronic migraines functional neurological disorder pcos endometriosis reflux/gord hayfever/ severe allergies ibs possible pots seasonal asthma" |
                                                     otherchronic =="anemia eczema endometriosis fibromyalgia gastroporesis hypermobility spectrum disorder insomnia mast cell activation syndrome migraines myalgic encephalomyelitis pots (postural orthostatic tachycardia syndrome) renyards" |
                                                     otherchronic == "chronic fatigue syndrome, hypermobility syndrome, autism spectrum disorder" |
                                                     otherchronic == "chronic pain, joint hypermobility, central sensitisation" |
                                                     otherchronic == "i have an undiagnosed connective tissue disorder - somewhere on the hsd-heds continuum i have early onset polyarthralgia, degenerative disc disease, extreme allergies, easy and extreme bleeding/bruising, menorrhagia (requiring quarterly iron infusions), precipitous labour etc ive been hospitalised with mumps ive also had whooping cough" |
                                                     otherchronic == "hypermobility spectrum disorder/heds" |
                                                     otherchronic == "fnd disorder hypermobile possibly eds syndrome cfs ibs graves disease possible pots" |
                                                     otherchronic == "hypermobility spectrum disorder postural orthostatic tachycardia syndrome" |
                                                     otherchronic == "hsd/eds - chronic pain fibromyalgia" |
                                                     otherchronic == "endometriosis, adenomyosis, pots, hypermobility, suspected mcas, arthritis, fibromyalgia / central sensitisation" |
                                                     otherchronic == "ehlers danlos syndrome - hsd type", 1, .)) 
 ### include relevant sections from other parts of the survey
 df.ad <- df.ad %>%
   mutate_at(vars(c(chronic___11)), ~ case_when(
     str_detect(data.clean$mental_diagn_other, "adhd") ~ 1,
     str_detect(data.clean$mental_diagn_other, "attention deficit") ~ 1,
     TRUE ~ .
   )) %>%
   mutate_at(vars(c(chronic___9)), ~ case_when(
     str_detect(data.clean$mental_diagn_other, "autism") ~ 1,
     TRUE ~ .
   ))  %>%
   mutate_at(vars(c(autoimmune_id___184)), ~ case_when(
     str_detect(data.clean$mental_diagn_other, "pre-menstrual dysphor") ~ 1,
     TRUE ~ .
   )) 
 #TODO continue running from here aftert other sections are complete
  ### merging some illnesses like EDS
df.ad <- df.ad %>%
  mutate(autoimmune_id___172 = ifelse(autoimmune_id___120 == "1" |
                                          autoimmune_id___123 == "1" |
                                          autoimmune_id___124 == "1" |
                                          autoimmune_id___135 == "1", 1, 0)) %>%

  mutate(autoimmune_id___148 = ifelse( autoimmune_id___47 == "1" |
                                         autoimmune_id___48 == "1" |
                                         autoimmune_id___49 == "1", 1, 0))
                                        
label(df.ad$autoimmune_id___172)="Hypermobility disorders (EDS, HSD)"
label(df.ad$autoimmune_id___148)="Alopecia (grouped)"
  
   
label(df.ad$autoimmune_id___1)="Ankylosing spondylitis"
label(df.ad$autoimmune_id___2)="Autoimmune haemolytic anaemia"
label(df.ad$autoimmune_id___3)="Autoimmune vitiligo"
label(df.ad$autoimmune_id___4)="Bullous pemphigoid"
label(df.ad$autoimmune_id___5)="Chronic inflammatory demyelinating neuropathy"
label(df.ad$autoimmune_id___6)="Coeliac disease"
label(df.ad$autoimmune_id___7)="CREST syndrome (limited scleroderma)"
label(df.ad$autoimmune_id___8)="Crohn's disease"
label(df.ad$autoimmune_id___9)="Dermatomyositis"
label(df.ad$autoimmune_id___10)="Essential mixed cryoglobulinemia"
label(df.ad$autoimmune_id___11)="Evans syndrome"
label(df.ad$autoimmune_id___12)="GBM disease (Goodpasture's syndrome)"
label(df.ad$autoimmune_id___13)="Giant cell arteritis"
label(df.ad$autoimmune_id___14)="Graves' disease"
label(df.ad$autoimmune_id___15)="Guillain Barré syndrome"
label(df.ad$autoimmune_id___16)="Infective endocarditis associated vasculitis"
label(df.ad$autoimmune_id___17)="Hashimoto's disease (autoimmune hypothyroidism)"
label(df.ad$autoimmune_id___18)="Henoch Schonlein purpura"
label(df.ad$autoimmune_id___19)="IgA nephropathy"
label(df.ad$autoimmune_id___20)="IgG4 related disease"
label(df.ad$autoimmune_id___21)="Immune thrombocytopenia"
label(df.ad$autoimmune_id___22)="Lupus (systemic lupus erythematosus)"
label(df.ad$autoimmune_id___23)="Membranous nephropathy"
label(df.ad$autoimmune_id___24)="Microscopic polyangiitis"
label(df.ad$autoimmune_id___25)="Motor neurone disease"
label(df.ad$autoimmune_id___26)="Multiple sclerosis"
label(df.ad$autoimmune_id___27)="Myasthenia gravis"
label(df.ad$autoimmune_id___28)="Neuromyelitis optica (Devic's disease)"
label(df.ad$autoimmune_id___29)="Pemphigus vulgaris"
label(df.ad$autoimmune_id___30)="Pernicious anaemia"
label(df.ad$autoimmune_id___31)="Polymyositis"
label(df.ad$autoimmune_id___32)="Primary biliary cirrhosis"
label(df.ad$autoimmune_id___33)="Primary antiphospholipid syndrome"
label(df.ad$autoimmune_id___34)="Psoriatic arthritis"
label(df.ad$autoimmune_id___35)="Rheumatoid arthritis"
label(df.ad$autoimmune_id___36)="Sarcoidosis"
label(df.ad$autoimmune_id___37)="Scleroderma (systemic sclerosis)"
label(df.ad$autoimmune_id___38)="Sjögren's syndrome"
label(df.ad$autoimmune_id___39)="Susac's syndrome"
label(df.ad$autoimmune_id___40)="Sporadic Inclusion Body Myositis"
label(df.ad$autoimmune_id___41)="Type 1 diabetes"
label(df.ad$autoimmune_id___42)="Ulcerative colitis"
label(df.ad$autoimmune_id___43)="Wegener's granulomatosis"
label(df.ad$autoimmune_id___44)="Healthy control"
label(df.ad$autoimmune_id___45)="Other"
label(df.ad$autoimmune_id___46)="Addison’s disease"
label(df.ad$autoimmune_id___47)="Alopeica areata"
label(df.ad$autoimmune_id___48)="Alopecia universalis"
label(df.ad$autoimmune_id___49)="Alopecia totalis"
label(df.ad$autoimmune_id___50)="Antiphospholipid Syndrome"
label(df.ad$autoimmune_id___51)="Small fibre neuropathy"
label(df.ad$autoimmune_id___52)="Autoimmune Hepatitis"
label(df.ad$autoimmune_id___53)="Orthostatic intolerance"
label(df.ad$autoimmune_id___54)="Central Nervous System Vasculitis"
label(df.ad$autoimmune_id___55)="ME/CFS"
label(df.ad$autoimmune_id___56)="Angiodema"
label(df.ad$autoimmune_id___57)="Chronic Spontaneous Urticaria"
label(df.ad$autoimmune_id___58)="Osteopenia"
label(df.ad$autoimmune_id___59)="Cutaneous lupus erythematosus"
label(df.ad$autoimmune_id___60)="Polyarthralgia"
label(df.ad$autoimmune_id___61)="Diverticulitis"
label(df.ad$autoimmune_id___62)="Eosinophilic granulomatosis with polyangiitis (EGPA)"
label(df.ad$autoimmune_id___63)="Dysautonomia"
label(df.ad$autoimmune_id___64)="Fibromyalgia"
label(df.ad$autoimmune_id___65)="Endometriosis"
label(df.ad$autoimmune_id___66)="Raynaud syndrome"
#label(df.ad$autoimmune_id___67)=""
label(df.ad$autoimmune_id___68)="Functional neurological disorder"
label(df.ad$autoimmune_id___69)="Gastritis"
label(df.ad$autoimmune_id___70)="Immune thrombocytopenic purpura (ITP)"
label(df.ad$autoimmune_id___71)="Inflammatory arthritis"
label(df.ad$autoimmune_id___72)="Inflammatory bowel disease (IBD)"
label(df.ad$autoimmune_id___73)="Lichen planopilaris"
label(df.ad$autoimmune_id___74)="Lichen sclerosus"
label(df.ad$autoimmune_id___75)="Lymphocytic vasculitis"
label(df.ad$autoimmune_id___76)="Mastocytosis"
label(df.ad$autoimmune_id___77)="Mixed connective tissue disease"
label(df.ad$autoimmune_id___78)="Narcolepsy type 1"
label(df.ad$autoimmune_id___79)="Cerebral vasculitis"
label(df.ad$autoimmune_id___80)="Chronic recurrent multifocal osteomyelitis (CRMO)"
label(df.ad$autoimmune_id___81)="Osteoporosis"
label(df.ad$autoimmune_id___82)="Pediatric autoimmune neuropsychiatric disorder (PANDAS)"
label(df.ad$autoimmune_id___83)="Periodic Fever, Aphthous Stomatitis, Pharyngitis, Adenitis Syndrome (PFAPA)"
label(df.ad$autoimmune_id___84)="Polycystic ovary syndrome (PCOS)"
label(df.ad$autoimmune_id___85)="Polymyalgia rheumatica"
label(df.ad$autoimmune_id___86)="Unknown Mast cell disorder"
label(df.ad$autoimmune_id___87)="Acromegaly"
label(df.ad$autoimmune_id___88)="Prurigo nodularis"
label(df.ad$autoimmune_id___89)="Psoriasis"
label(df.ad$autoimmune_id___90)="Clinically isolated syndrome"
label(df.ad$autoimmune_id___91)="Cushings syndrome"
label(df.ad$autoimmune_id___92)="Osteoarthritis"
label(df.ad$autoimmune_id___93)="Gastric antral vascular ectasia (GAVE)"
label(df.ad$autoimmune_id___94)="Autoimmune gastritis"
label(df.ad$autoimmune_id___95)="Pyoderma Gangrenosis"
label(df.ad$autoimmune_id___96)="Rheumatic Fever"
#label(df.ad$autoimmune_id___97)="Sicca"
label(df.ad$autoimmune_id___98)="Gastro oesophageal reflux (GORD)"
label(df.ad$autoimmune_id___99)="Loin pain hematuria syndrome (LPHS)"
label(df.ad$autoimmune_id___100)="Tachycardia"
label(df.ad$autoimmune_id___101)="Adenomyosis"
label(df.ad$autoimmune_id___102)="Hynonatremia"
label(df.ad$autoimmune_id___103)="Cutaneous small vessel vasculitis"
label(df.ad$autoimmune_id___104)="Takayasu's Arteritis"
label(df.ad$autoimmune_id___105)="Chronic fatigue"
label(df.ad$autoimmune_id___106)="Lymphocytopenia"
label(df.ad$autoimmune_id___107)="Undifferentiated connective tissue disease"
label(df.ad$autoimmune_id___108)="Unknown autoimmune"
label(df.ad$autoimmune_id___109)="Vasculitis"
label(df.ad$autoimmune_id___110)="Uveitis"
label(df.ad$autoimmune_id___111)="Velocardiofacial syndrome (VCFS)"
label(df.ad$autoimmune_id___112)="Hypothyroidism"
label(df.ad$autoimmune_id___113)="Hyperthyroidism"
label(df.ad$autoimmune_id___114)="Idiopathic hypersomnia"
label(df.ad$autoimmune_id___115)="Neuromyotonia"
label(df.ad$autoimmune_id___116)="Postural orthostatic tachycardia syndrome (POTS)"
label(df.ad$autoimmune_id___117)="Lymphedeama"
label(df.ad$autoimmune_id___118)="Behçet's disease"
label(df.ad$autoimmune_id___119)="Hidradenitis Suppurativa"
label(df.ad$autoimmune_id___120)="Ehlers danlos syndrome (EDS)"
label(df.ad$autoimmune_id___121)="Enthesitis"
label(df.ad$autoimmune_id___122)="Eosinophilic esophagitis"
label(df.ad$autoimmune_id___123)="Hypermobile Ehlers danlos syndrome (hEDS)"
label(df.ad$autoimmune_id___124)="Cardiac-valvular Ehlers danlos syndrome (cvEDS)"
label(df.ad$autoimmune_id___125)="Primary aldosteronism (Conn’s syndrome)"
label(df.ad$autoimmune_id___126)="Common variable immunodeficiency (CVID)"
label(df.ad$autoimmune_id___127)="Chronic inflammatory demyelinating polyneuropathy (CIDP)"
label(df.ad$autoimmune_id___128)="Median arcuate ligament syndrome (MALS)"
label(df.ad$autoimmune_id___129)="Hyperacusis"
label(df.ad$autoimmune_id___130)="Cyclic neutropenia"
label(df.ad$autoimmune_id___131)="Eczema"
label(df.ad$autoimmune_id___132)="Multiple chemical sensitivity (MCS)"
label(df.ad$autoimmune_id___133)="Mast cell activation syndrome (MCAS)"
label(df.ad$autoimmune_id___134)="Fibromuscular dysplasia"
label(df.ad$autoimmune_id___135)="Hypermobility spectrum disorder"
label(df.ad$autoimmune_id___136)="Dysphagia"
label(df.ad$autoimmune_id___137)="Lupus nephritis"
label(df.ad$autoimmune_id___138)="Discoid lupus erythematosus"
label(df.ad$autoimmune_id___139)="Polychondritis"
label(df.ad$autoimmune_id___140)="Meniere's disease"
label(df.ad$autoimmune_id___141)="Acute intermittent porphyria"
label(df.ad$autoimmune_id___142)="Microscopic colitis"
#label(df.ad$autoimmune_id___143)="Lupus Anticoagulant"
label(df.ad$autoimmune_id___144)="Notalgia paraesthetica"
label(df.ad$autoimmune_id___145)="Rosacea"
label(df.ad$autoimmune_id___146)="Cystic fibrosis"
label(df.ad$autoimmune_id___147)="Blepharitis"
label(df.ad$autoimmune_id___149)="Gastroparesis"
label(df.ad$autoimmune_id___150)="Autoimmune neutropenia"
label(df.ad$autoimmune_id___151)="Axial spondyloarthritis"
label(df.ad$autoimmune_id___152)="Facet athroparhy"
label(df.ad$autoimmune_id___153)="Loeys dietz syndrome"
label(df.ad$autoimmune_id___154)="Empty sella syndrome"
label(df.ad$autoimmune_id___155)="Trigeminal neuralgia"
label(df.ad$autoimmune_id___156)="Hemochromatosis"
label(df.ad$autoimmune_id___157)="Hereditary alpha tryptasemia"
label(df.ad$autoimmune_id___158)="Tracheomalacia"
label(df.ad$autoimmune_id___159)="Cryptogenic organizing pneumonia "
label(df.ad$autoimmune_id___160)="Supraventricular tachycardia"
label(df.ad$autoimmune_id___161)="Peripheral neuropathy"
label(df.ad$autoimmune_id___162)="Arthritis"
label(df.ad$autoimmune_id___163)="Dystonia"
label(df.ad$autoimmune_id___164)="Long COVID"
label(df.ad$autoimmune_id___165)="Urticaria"
label(df.ad$autoimmune_id___166)="Lichen planus"
label(df.ad$autoimmune_id___167)="Acute febrile neutrophilic dermatosis"
label(df.ad$autoimmune_id___168)="Neuralgic Amyotrophy"
label(df.ad$autoimmune_id___169)="Mesenteric panniculitis"
label(df.ad$autoimmune_id___170)="de Quervain thyroiditis"
label(df.ad$autoimmune_id___171)="Lyme disease"
label(df.ad$autoimmune_id___173)="Autoimmune progesterone anaphylaxis"
label(df.ad$autoimmune_id___174)="Dysmotility"
label(df.ad$autoimmune_id___175)="Stiff-person syndrome"
label(df.ad$autoimmune_id___176)="Leukocytoclastic vasculitis"
label(df.ad$autoimmune_id___177)="Thallasemia (incl. trait)"
label(df.ad$autoimmune_id___178)="Plantar fasciitis"
label(df.ad$autoimmune_id___179)="Lymphocytic hypophysitis"
label(df.ad$autoimmune_id___180)="Transverse myelitis"
label(df.ad$autoimmune_id___181)="Glaucoma"
label(df.ad$autoimmune_id___182)="Hepatitits C"
label(df.ad$autoimmune_id___183)="Non alcoholic fatty liver"
label(df.ad$autoimmune_id___184)="Premenstrual dysphoric disorder"
label(df.ad$autoimmune_id___185)="Familial hypercholesterolaemia"
label(df.ad$autoimmune_id___186)="Eagles syndrome"
label(df.ad$autoimmune_id___187)="Thoracic outlet syndrome"

label(df.ad$chronic___1)="Asthma"
label(df.ad$chronic___2)="Diabetes (type 2, steroid induced)"
label(df.ad$chronic___3)="Cancer"
label(df.ad$chronic___4)="Mental illness"
label(df.ad$chronic___5)="Heart disease"
label(df.ad$chronic___7)="Other"
label(df.ad$chronic___8)="None"
label(df.ad$chronic___9)="Autism"
label(df.ad$chronic___10)="Migranes"
label(df.ad$chronic___11)="ADHD"
label(df.ad$chronic___13)="Inflammatory lung disease"
label(df.ad$chronic___14)="Chemically induced illness" ##Stephen-Johnson syndrome & Chronic inflammatory response Syndrome
label(df.ad$chronic___15)="Chronic pain"
label(df.ad$chronic___16)="Epilepsy"
label(df.ad$chronic___17)="Sleep disorder"
label(df.ad$chronic___18)="Spinal issues"
label(df.ad$chronic___19)="Chiari malformation"
label(df.ad$chronic___20)="MTHFR gene variant"
label(df.ad$chronic___21)="Allergies (incl. anaphylaxis)"
label(df.ad$chronic___22)="Syncope"
label(df.ad$chronic___23)="Blood disorder"
label(df.ad$chronic___24)="Hypertension"
label(df.ad$chronic___25)="Idiopathic intracranial hypertension"
label(df.ad$chronic___26)="Cerebral palsy"
label(df.ad$chronic___12)="Chronic kidney disease"


df.ad <- df.ad %>%
  select(-c(autoimmune_id_other,
            otherchronic,
            autoimmune_id___45,
            chronic___7,
            autoimmune_id___120,
            autoimmune_id___123,
            autoimmune_id___124,
            autoimmune_id___135,
            autoimmune_id___47,
            autoimmune_id___48,
            autoimmune_id___49
            )) %>%
  mutate(ad.sum = rowSums(select(., contains("autoimmune_id"))))

#check why some chronic cohort were coming up with zero ADs & some controls with ADs logged
query.noAD <- df.ad %>%
  filter(ad.sum == 0) %>%
  left_join((data.clean %>% select(record_id, 
                                   autoimmune_id_other, 
                                   otherchronic, 
                                   self, 
                                   formal,         
                                   intro_complete,
                                   consent_form_complete,
                                   demographics_complete,
                                   disease_and_diagnosis_complete,
                                   symptom_management_complete,
                                   mental_health_complete,
                                   neuroqol_bank_v10_fatigue_complete,
                                   alcohol_dependence_scale_complete,
                                   rand_36_item_sf_health_survey_instrument_version_1_complete)), by = "record_id")
saveRDS(query.noAD, file = "query-no-ADs.rds")
noAD.check <- query.noAD %>%
  select(record_id) %>%
  left_join((data.clean %>% select(record_id,
                                   contains("autoimmune_id___"),
                                   -contains(".factor"),
                                   -c(autoimmune_id___44, autoimmune_id___45))), by = "record_id") %>%
  mutate(across(autoimmune_id___1:autoimmune_id___43, .fns = as.numeric)) %>%
  mutate(ad.sum = rowSums(select(., contains("autoimmune_id"))))
query.contAD  <- df.ad %>%
  filter(diseasestat == 0 & ad.sum > 0) %>%
  left_join((data.clean %>% select(record_id, 
                                   autoimmune_id_other, 
                                   otherchronic, 
                                   self, 
                                   formal,         
                                   intro_complete,
                                   consent_form_complete,
                                   demographics_complete,
                                   disease_and_diagnosis_complete,
                                   symptom_management_complete,
                                   mental_health_complete,
                                   neuroqol_bank_v10_fatigue_complete,
                                   alcohol_dependence_scale_complete,
                                   rand_36_item_sf_health_survey_instrument_version_1_complete)), by = "record_id")
saveRDS(query.contAD, file = "query-controls-ADs.rds")
contAD.check <- query.contAD %>%
  select(record_id) %>%
  left_join((data.clean %>% select(record_id,
                                   contains("autoimmune_id___"),
                                   autoimmune_id_other,
                                   otherchronic,
                                   -contains(".factor"),
                                   -c(autoimmune_id___44, autoimmune_id___45))), by = "record_id") %>%
  mutate(across(autoimmune_id___1:autoimmune_id___43, .fns = as.numeric)) %>%
  mutate(ad.sum = rowSums(select(., contains("autoimmune_id___"))))

#check how many self reported controls have a formal AD dx
controls.ADdx <- df.ad %>%
  filter(ad.sum > 0) %>%
  left_join((data.clean %>% select(record_id,
                                   autoimmune_id_other, 
                                   otherchronic, 
                                   self, 
                                   formal)), by = "record_id") %>%
  filter(diseasestat == 0 & formal == 1) %>%
  select(where( ~ is.numeric(.x) && sum(.x) != 0)) #bad fix: removes non-numeric columns
 
#add disease status column 
df.ad <- df.ad %>%
  mutate(cohort.id = case_when(
    ad.sum == 0 ~ "control",
    ad.sum > 0 ~ "chronically ill",
    is.na(ad.sum) ~ NA_character_
  )) %>%
  mutate(autoimmune_id___44 = case_when(
    ad.sum == 0 ~ 1,
    ad.sum > 0 ~ 0
  )) %>%
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
    ad.sum > 1 ~ "multiple illnesses",
    TRUE ~ NA_character_
  ))
label(df.ad$illness.grouped)="Illness grouped"
label(df.ad$ad.sum)="Illness count"

saveRDS(df.ad, file = "df-ad.rds")


###############################
### Summary statistics df
###############################
df.sumstats <- data.clean %>%
  select(record_id,
         year_of_birth,
         gender.factor,
         ethnicity.factor,
         education.factor,
         employment_status.factor,
         rel_status.factor,
         home.factor,
         state.factor,
         diagnosis_length.factor,
         misdiag.factor)

df.sumstats$age <- (2022 - as.numeric(df.sumstats$year_of_birth))
df.sumstats <- df.sumstats %>% #prev hist.test
  rename("gender.id" = "gender.factor") %>%
  # mutate(gender.id = case_when(
  #   data.clean$gender == "1" ~ "Female",
  #   data.clean$gender == "2" ~ "Male",
  #   data.clean$gender == "3" ~ "Gender fluid",
  #   data.clean$gender == "4" ~ "Non-binary",
  #   data.clean$gender == "5" ~ "Other",
  #   data.clean$gender == "6" ~ "Prefer not to say",
  #   data.clean$gender == "NA" ~ "Non response"
  # )) %>%
  mutate(age.cat = case_when(
    age %in% 0:4 ~ "0-4",
    age %in% 5:9 ~ "5-9",
    age %in% 10:14 ~ "10-14",
    age %in% 15:19 ~ "15-19",
    age %in% 20:24 ~ "20-24",
    age %in% 25:29 ~ "25-29",
    age %in% 30:34 ~ "30-34",
    age %in% 35:39 ~ "35-39",
    age %in% 40:44 ~ "40-44",
    age %in% 45:49 ~ "45-49",
    age %in% 50:54 ~ "50-54",
    age %in% 55:59 ~ "55-59",
    age %in% 60:64 ~ "60-64",
    age %in% 65:69 ~ "65-69",
    age %in% 70:74 ~ "70-74",
    age %in% 75:79 ~ "75-79",
    age %in% 80:84 ~ "80-84",
    age %in% 85:89 ~ "85-89",
    age %in% 90:94 ~ "90-94",
    age %in% 95:99 ~ "95-99"
  ))%>%
  rename("dia.length" = "diagnosis_length.factor") %>%
  mutate(dx.group = case_when(
    dia.length == "Less than six months" ~"< 6 months",
    dia.length == "6 months - 1 year" ~">6 months - 1 year",
    dia.length == "1 - 2 years" ~ ">1 - 5 years",
    dia.length == "3 - 4 years" ~">1 - 5 years",
    dia.length == "4 - 5 years" ~">1 - 5 years",
    dia.length == "6 - 7  years" ~ ">5 - 9 years",
    dia.length == "7 - 8  years" ~">5 - 9 years",
    dia.length == "9 - 10 years" ~ "9+ years",
    dia.length == "10+  years (please specify length in the other section)" ~"9+ years",
    is.na(dia.length)  ~ NA_character_
))  %>%
rename("misdiag.id" = "misdiag.factor") %>%
  # mutate(misdiag.id = case_when(
  #   data.clean$misdiag == "0" ~ "Not misdiagnosed",
  #   data.clean$misdiag == "1" ~ "Misdiagnosed",
  #   data.clean$misdiag == "NA" ~ "Non response"
  # ))
  rename("ethnicity.id" = "ethnicity.factor") %>%
  rename("education.id" = "education.factor") %>%
  rename("employment.id" = "employment_status.factor") %>%
  rename("relationship.id" = "rel_status.factor") %>%
  rename("home.id" = "home.factor") %>%
  rename("state" = "state.factor") %>%
  mutate(gender.group = case_when(
    gender.id == "Female" ~ "Female",
    gender.id == "Male" ~ "Male", 
    is.na(gender.id) ~ NA_character_,
    TRUE ~ "Other"
  )) %>%
  left_join((df.ad %>% select(record_id, cohort.id, ad.sum)), by= "record_id")
df.sumstats$gender.group = factor(df.sumstats$gender.group,levels=c("Female","Male","Other"))
levels(df.sumstats$gender.group)=c("Female","Male","Other")
df.sumstats$dx.group = factor(df.sumstats$dx.group,levels=c("< 6 months",">6 months - 1 year",">1 - 5 years", ">5 - 9 years", "9+ years"))
levels(df.sumstats$dx.group)=c("< 6 months",">6 months - 1 year",">1 - 5 years", ">5 - 9 years", "9+ years")
#Check allocations worked out correctly
# test %>% filter(!(as.character(gender.id) == as.character(gender.group))) %>% select(record_id, gender.id, gender.group)

saveRDS(df.sumstats, file = "AD-sumstats.rds")

# ### because ME/CFS is so over represented, check if it's a valid categoriztion
# cf.check <- data.clean %>%
#   mutate(otherchronic = tolower(otherchronic))%>%
#   mutate(otherchronic = str_trim(otherchronic)) %>%
#   subset(., autoimmune_id_other == "Chronic Fatigue  Fibromyalgia " |
#                      autoimmune_id_other == "Very bad ibs , endometriosis, chronic fatigue, thyroid issues " |
#                      otherchronic == "ehlers-danlos syndrome (hypermobile with vascular features), postural orthostatic tachycardia syndrome with arrhythmia, aortic root aneurysm, raynaud's syndrome, narcolepsy and cataplexy, widespread neuropathic pain, multiple chronic herniated disks, osteoarthritis of the spine, chronic fatigue, trans ischemic attacks, migraine diathesis, chronic respiratory infections, pneumothorax, endometriosis, irritable bowel syndrome, bowel perforation.     inflammatory bowel disease (?), mast cell activation syndrome (?), chiari malformation (?), benedikt syndrome (?).    anxiety, depression, complex post traumatic stress disorder, schizoid personality disorder, dissociative identity disorder" |
#                      otherchronic == "sjogrens, hyponatreamea, bursitis, chronic fatigue, borderline personality disorder, major depression, cptsd, chronic pain," |
#                      otherchronic == "deafness for a year as a child after contracting german measles  but resolved itself  episodes of malaria as a child growing up in papua new guinea  follicle bacterial infections causing abscesses and hospitalisations( father and 1 sister also have this as well)  severe tosillitis and infection at 17 yrs-removed  diverticulitis-2021  salycilate intolerances with mood behaviours  multiple fibroids and anemia- full hysterectomy 2017    chronic fatigue 2021  fibromalagia 2021" |
#                      otherchronic == "chronic migraine, thoracic outlet syndrome, fibromyalgia, chronic fatigue" |
#                      otherchronic == "chronic fatigue , raynaus" |
#                      otherchronic == "fibromyalgia / chronic fatigue") %>%
#   select(record_id, diseasestat, self, formal, contains("rand36"), contains("autoimmune_id"), -contains("factor")) %>%
#   subset(., record_id =="90" |
#            record_id =="364" |
#            record_id =="914")
# cf.check2 <- df.ad %>%
#   subset(., record_id =="90" |
#            record_id =="364" |
#            record_id =="914")# %>%
#   select(record_id, contains("autoimmune_id"), contains("chronic__") -contains("factor"), otherchronic)
# 
# ### um.... 2 records ticked yes for chronic illness, then no for both formal and self diagnosis. check rest: turns out 94 entries 
# df.weird <- data.clean %>%
#   subset(., diseasestat == "1" &
#            self == "2" &
#            formal == "2") %>%
#   select(record_id,
#          contains("chronic"),
#          contains("autoimmune_id"))







# 
# #stats for representation
# df.ad %>% 
#   select(autoimmune_id___55) %>%
#   filter(autoimmune_id___55 == 1) %>%
#   count(.) #MECFS 155, 12.6%
# df.ad %>% 
#   select(autoimmune_id___6) %>%
#   filter(autoimmune_id___6 == 1) %>%
#   count(.) # coeliac 198, 16.1%
# df.ad %>% 
#   select(autoimmune_id___22) %>%
#   filter(autoimmune_id___22 == 1) %>%
#   count(.) #lupus 63, 5.1%
# df.ad %>% 
#   select(autoimmune_id___65) %>%
#   filter(autoimmune_id___65 == 1) %>%
#   count(.) #endo 41, 3.3%
# df.ad %>% 
#   select(autoimmune_id___35) %>%
#   filter(autoimmune_id___35 == 1) %>%
#   count(.) #RA 110, 8.9%
# df.ad %>% 
#   select(autoimmune_id___17) %>%
#   filter(autoimmune_id___17 == 1) %>%
#   count(.) #hashi 155, 12.6%
# df.ad %>% 
#   select(autoimmune_id___92) %>%
#   filter(autoimmune_id___92 == 1) %>%
#   count(.) #osteoporosis 18, 1.46%
# 
# 
# #classifier cloud
# clad.cloud <- df.ad %>%
#   select(contains("autoimmune_id__"), -c(autoimmune_id___44, autoimmune_id___45, autoimmune_id___120, autoimmune_id___123, autoimmune_id___124, autoimmune_id___135)) 
# ad.labels <- ad.cloud %>%
#   get_label #%>%
# #  ad.labels <-  str_split(ad.labels, split = "\(" , simplify = FALSE) [[1]][1]#, `[`, 1)
# #  strsplit(ad.labels, split = "\(")
# colnames(ad.cloud) <- ad.labels
# ad.count <- ad.cloud %>% 
#   summarise_if(is.numeric, sum) %>% 
#   transpose()
# ad.count.df <- as.data.frame(do.call(cbind, ad.count)) %>% 
#   rownames_to_column(.) %>% 
#   rename(., word = rowname, freq = V1)
# ad.count.df$freq <- as.numeric(as.character(ad.count.df$freq))

#wordcloud2(ad.count.df, shape = 'diamond', color = "random-dark", backgroundColor = "white")