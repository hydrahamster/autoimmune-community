#load helper file
source("0_pachages-function.R")

#load data
df.ad <- readRDS("df-ad.rds")
#TODO recoded 51, 91, 172 to 185
#TODO rm 94, and grouped terms
ad.type <- df.ad %>%
  mutate(ad_type_healthy = ifelse(autoimmune_id___44 == 1, 1, 0)) %>%
  mutate(ad_type_loc.blood = ifelse(autoimmune_id___2 == 1 |
                                      autoimmune_id___11 == 1 |
                                      autoimmune_id___21 == 1 |
                                      autoimmune_id___70 == 1 |
                                      autoimmune_id___106 == 1 |
                                      autoimmune_id___126 == 1 |
                                      autoimmune_id___130 == 1 |
                                      autoimmune_id___134 == 1 |
                                      autoimmune_id___143 == 1 |
                                      autoimmune_id___150 == 1 |
                                      autoimmune_id___156 == 1 |
                                      autoimmune_id___157 == 1, 1, 0)) %>%
  mutate(ad_type_loc.card = ifelse(autoimmune_id___16 == 1 |
                                     autoimmune_id___53 == 1 |
                                     autoimmune_id___116 == 1 |
                                     autoimmune_id___100 == 1 |
                                     autoimmune_id___160 == 1, 1, 0)) %>%
  mutate(ad_type_loc.ear = ifelse(autoimmune_id___140 == 1, 1 |
                                    autoimmune_id___129 == 1, 0)) %>%
  mutate(ad_type_loc.endocr = ifelse(autoimmune_id___125 == 1 |
                                       autoimmune_id___14 == 1 |
                                       autoimmune_id___17 == 1 |
                                       autoimmune_id___112 == 1 |
                                       autoimmune_id___113 == 1 |
                                       autoimmune_id___170 == 1, 1, 0)) %>%
  mutate(ad_type_loc.gastro = ifelse(autoimmune_id___30 == 1 |
   #TODO reecoded                                    autoimmune_id___51 == 1 |
                                       autoimmune_id___61 == 1 |
                                       autoimmune_id___69 == 1 |
                                       autoimmune_id___93 == 1 |
                                       autoimmune_id___98 == 1 |
                                       autoimmune_id___136 == 1 |
                                       autoimmune_id___141 == 1 |
                                       autoimmune_id___149 == 1 |
                                       autoimmune_id___105 == 1 |
                                       autoimmune_id___122 == 1 |
                                       autoimmune_id___6 == 1 |
                                       autoimmune_id___8 == 1 |
                                       autoimmune_id___42 == 1 |
                                       autoimmune_id___72 == 1 |
                                       autoimmune_id___142 == 1, 1, 0)) %>%
  mutate(ad_type_loc.gento = ifelse(autoimmune_id___65 == 1 |
                                      autoimmune_id___84 == 1 |
                                      autoimmune_id___99 == 1 |
                                      autoimmune_id___101 == 1, 1, 0)) %>%
  mutate(ad_type_loc.skel = ifelse(autoimmune_id___1 == 1 |
                                     autoimmune_id___58 ==1 |
                                     autoimmune_id___71 == 1 | 
                                     autoimmune_id___81 == 1 |
                                     autoimmune_id___92 == 1 |
                                     autoimmune_id___111 == 1 |
                                     autoimmune_id___121 == 1 |
                                     autoimmune_id___151 == 1 |
                                     autoimmune_id___152 == 1 |
                                     autoimmune_id___154 == 1 |
                                     autoimmune_id___158 == 1 |
                                     autoimmune_id___162 == 1 |
                                     autoimmune_id___153 == 1, 1, 0)) %>%
  mutate(ad_type_local.kid = ifelse(autoimmune_id___19 == 1 |
                                      autoimmune_id___23 == 1 |
                                      autoimmune_id___137 == 1, 1, 0)) %>%
  mutate(ad_type_loc.liv = ifelse(autoimmune_id___32 == 1 |
                                    autoimmune_id___52 == 1, 1, 0)) %>%
  mutate(ad_type_loc.lymph = ifelse(autoimmune_id___83 == 1 |
                                      autoimmune_id___117 == 1, 1, 0)) %>%
  mutate(ad_type_loc.musc = ifelse(autoimmune_id___27 == 1 |
                                     autoimmune_id___40 == 1 |
                                     autoimmune_id___102 == 1 |
                                     autoimmune_id___128 == 1, 1, 0)) %>%
  mutate(ad_type_loc.nerv = ifelse(autoimmune_id___5 == 1 |
                                     autoimmune_id___15 == 1 |
                                     autoimmune_id___25 == 1 |
                                     autoimmune_id___26 == 1 |
                                     autoimmune_id___63 == 1 |
                                     autoimmune_id___64 == 1 |
                                     autoimmune_id___66 == 1 |
                                     autoimmune_id___68 == 1 |
                                     autoimmune_id___78 == 1 |
                                     autoimmune_id___90 == 1 |
                                     autoimmune_id___115 == 1 |
                                     autoimmune_id___127 == 1 |
                                     autoimmune_id___82 == 1 |
                                     autoimmune_id___114 == 1|
                                     autoimmune_id___144 == 1 |
                                     autoimmune_id___155 == 1 |
                                     autoimmune_id___161 == 1|
                                     autoimmune_id___163 == 1 |
                                     autoimmune_id___168 == 1, 1, 0)) %>%
  mutate(ad_type_loc.ocul = ifelse(autoimmune_id___28 == 1 |
                                     autoimmune_id___110 == 1 |
                                     autoimmune_id___147 == 1 |
                                     autoimmune_id___148 == 1, 1, 0)) %>%
  mutate(ad_type_loc.panc = ifelse(autoimmune_id___41 == 1, 1, 0)) %>%
  mutate(ad_type_loc.resp = ifelse(autoimmune_id___94 == 1 |
                                     autoimmune_id___159 == 1, 1, 0)) %>%
  mutate(ad_type_loc.skin = ifelse(autoimmune_id___3 == 1 |
                                     autoimmune_id___4 == 1 |
                                     autoimmune_id___29 == 1 |
                                     autoimmune_id___47 == 1 |
                                     autoimmune_id___48 == 1 |
                                     autoimmune_id___49 == 1 |
                                     autoimmune_id___56 == 1 |
                                     autoimmune_id___57 == 1 |
                                     autoimmune_id___59 == 1 |
                                     autoimmune_id___67 == 1 |
                                     autoimmune_id___73 == 1 |
                                     autoimmune_id___74 == 1 |
                                     autoimmune_id___75 == 1 |
                                     autoimmune_id___87 == 1 |
                                     autoimmune_id___88 == 1 |
                                     autoimmune_id___89 == 1 |
                                     autoimmune_id___95 == 1 |
                                     autoimmune_id___103 == 1 |
                                     autoimmune_id___119 == 1 |
                                     autoimmune_id___131 == 1 |
                                     autoimmune_id___138 == 1 |
                                     autoimmune_id___145 == 1 |
                                     autoimmune_id___165 == 1 |
                                     autoimmune_id___166 == 1 |
                                     autoimmune_id___167 == 1 |
                                     autoimmune_id___169 == 1 , 1, 0)) %>%
  mutate(ad_type_sys.gen = ifelse(autoimmune_id___97 == 1 |
                                    autoimmune_id___108 == 1 |
                                    autoimmune_id___146 == 1 |
                                    autoimmune_id___132 == 1, 1, 0)) %>%
  mutate(ad_type_sys.rheum = ifelse(autoimmune_id___7 == 1 |
                                      autoimmune_id___9 == 1 |
                                      autoimmune_id___22 == 1 |
                                      autoimmune_id___31 == 1 |
                                      autoimmune_id___33 == 1 |
                                      autoimmune_id___34 == 1 |
                                      autoimmune_id___35 == 1 |
                                      autoimmune_id___37 == 1 |
                                      autoimmune_id___38 == 1 |
                                      autoimmune_id___46 == 1 |
                                      autoimmune_id___50 == 1 |
                                      autoimmune_id___55 == 1 |
                                      autoimmune_id___60 == 1 |
                                      autoimmune_id___77 == 1 |
                                      autoimmune_id___80 == 1 |
                                      autoimmune_id___96 == 1 |
                                      autoimmune_id___107 == 1 |
                                      autoimmune_id___120 == 1 |
                                      autoimmune_id___123 == 1 |
                                      autoimmune_id___124 == 1 |
                                      autoimmune_id___135 == 1 |
                                      autoimmune_id___139 == 1 |
                                      autoimmune_id___164 == 1 |
                                      autoimmune_id___171 == 1 |
                                      autoimmune_id___85 == 1, 1, 0)) %>%
  mutate(ad_type_sys.vasc = ifelse(autoimmune_id___10 == 1 |
                                     autoimmune_id___12 == 1 |
                                     autoimmune_id___13 == 1 |
                                     autoimmune_id___18 == 1 |
                                     autoimmune_id___20 == 1 |
                                     autoimmune_id___24 == 1 |
                                     autoimmune_id___36 == 1 |
                                     autoimmune_id___39 == 1 |
                                     autoimmune_id___43 == 1 |
                                     autoimmune_id___54 == 1 |
                                     autoimmune_id___62 == 1 |
                                     autoimmune_id___76 == 1 |
                                     autoimmune_id___79 == 1 |
                                     autoimmune_id___86 == 1 |
                                     autoimmune_id___104 == 1 |
                                     autoimmune_id___109 == 1 |
                                     autoimmune_id___118 == 1 |
                                     autoimmune_id___133 == 1, 1, 0))

#TODO include co-morbs and then label them as such
#TODO include new illnesses

#label
label(ad.type$ad_type_healthy)="Healthy"
label(ad.type$ad_type_loc.blood)="Local blood"
label(ad.type$ad_type_loc.card)="Local cardiac"
label(ad.type$ad_type_loc.ear)="Local ear"
label(ad.type$ad_type_loc.endocr)="Local endocrine"
label(ad.type$ad_type_loc.gastro)="Local gastrointestinal system"
label(ad.type$ad_type_loc.gento)="Local genitourinary system"
label(ad.type$ad_type_loc.skel)="Local joint/skeletal"
label(ad.type$ad_type_local.kid)="Local kidney"
label(ad.type$ad_type_loc.liv)="Local liver"
label(ad.type$ad_type_loc.lymph)="Local lymphatic"
label(ad.type$ad_type_loc.musc)="Local muscular"
label(ad.type$ad_type_loc.nerv)="Local nervous system"
label(ad.type$ad_type_loc.ocul)="Local occular"
label(ad.type$ad_type_loc.panc)="Local pancreas"
label(ad.type$ad_type_loc.resp)="Local respiratory"
label(ad.type$ad_type_loc.skin)="Local skin"
label(ad.type$ad_type_sys.gen)="Systemic general"
label(ad.type$ad_type_sys.rheum)="Systemic rheumatological"
label(ad.type$ad_type_sys.vasc)="Systemic vasculitis"

adty.cloud <- ad.type %>%
  select(contains("ad_type")) %>% #other was reclassified so doesn't need to be in
  replace_na(list(ad_type_healthy = 0)) #NA entries from diseasestat break downstream process
adty.labels <- adty.cloud %>%
  get_label 
colnames(adty.cloud) <- adty.labels
adty.count <- adty.cloud %>% 
  summarise_if(is.numeric, sum) %>% 
  transpose()
adty.count.df <- as.data.frame(do.call(cbind, adty.count)) %>% 
  rownames_to_column(.) %>% 
  rename(., word = rowname, freq = V1)
adty.count.df$freq <- as.numeric(as.character(adty.count.df$freq))

wordcloud2(adty.count.df, shape = 'cardioid', color = "random-dark", backgroundColor = "white")

#quick check on how different types are distributed
#df.ad %>%
#  select(ad_type_healthy:ad_type_sys.vasc) %>%
#  colSums(.)