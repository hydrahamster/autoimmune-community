#load helper file
source("0_pachages-function.R")

#load data
df.ad <- readRDS("df-ad.rds")

ad.type <- df.ad %>%
  mutate(ad_type_healthy = ifelse(cohort.id == "control", 1, 0)) %>%
  mutate(ad_type_loc.blood = ifelse(autoimmune_id___2 == 1 |
                                      autoimmune_id___11 == 1 |
                                      autoimmune_id___21 == 1 |
                                      autoimmune_id___70 == 1 |
                                      autoimmune_id___102 == 1 |
                                      autoimmune_id___106 == 1 |
                                      autoimmune_id___126 == 1 |
                                      autoimmune_id___130 == 1 |
                                      autoimmune_id___134 == 1 |
                                      autoimmune_id___150 == 1 |
                                      autoimmune_id___156 == 1 |
                                      autoimmune_id___157 == 1 |
                                      autoimmune_id___177 == 1, 1, 0)) %>%
  mutate(ad_type_loc.card = ifelse(autoimmune_id___16 == 1 |
                                     autoimmune_id___53 == 1 |
                                     autoimmune_id___116 == 1 |
                                     autoimmune_id___100 == 1 |
                                     autoimmune_id___160 == 1, 1, 0)) %>%
  mutate(ad_type_loc.ear = ifelse(autoimmune_id___140 == 1, 1 |
                                    autoimmune_id___129 == 1, 0)) %>%
  mutate(ad_type_loc.endocr = ifelse(autoimmune_id___14 == 1 |
                                       autoimmune_id___17 == 1 |
                                       autoimmune_id___87 == 1 |
                                       autoimmune_id___91 == 1 |
                                       autoimmune_id___112 == 1 |
                                       autoimmune_id___113 == 1 |
                                       autoimmune_id___125 == 1 |
                                       autoimmune_id___170 == 1|
                                       autoimmune_id___173 == 1|
                                       autoimmune_id___179 == 1|
                                       autoimmune_id___184 == 1, 1, 0)) %>%
  mutate(ad_type_loc.gastro = ifelse(autoimmune_id___6 == 1 |
                                       autoimmune_id___8 == 1 |
                                       autoimmune_id___30 == 1 |
                                       autoimmune_id___42 == 1 |
                                       autoimmune_id___61 == 1 |
                                       autoimmune_id___69 == 1 |
                                       autoimmune_id___72 == 1 |
                                       autoimmune_id___93 == 1 |
                                       autoimmune_id___94 == 1 |
                                       autoimmune_id___98 == 1 |
                                       autoimmune_id___122 == 1 |
                                       autoimmune_id___136 == 1 |
                                       autoimmune_id___141 == 1 |
                                       autoimmune_id___142 == 1 |
                                       autoimmune_id___149 == 1 |
                                       autoimmune_id___174 == 1, 1, 0)) %>%
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
                                     autoimmune_id___153 == 1 |
                                     autoimmune_id___154 == 1 |
                                     autoimmune_id___158 == 1 |
                                     autoimmune_id___162 == 1 |
                                     autoimmune_id___180 |
                                     autoimmune_id___187 == 1, 1, 0)) %>%
  mutate(ad_type_loc.kid = ifelse(autoimmune_id___19 == 1 |
                                      autoimmune_id___23 == 1 |
                                      autoimmune_id___137 == 1, 1, 0)) %>%
  mutate(ad_type_loc.liv = ifelse(autoimmune_id___32 == 1 |
                                    autoimmune_id___52 |
                                    autoimmune_id___182 |
                                    autoimmune_id___183 |
                                    autoimmune_id___185 == 1, 1, 0)) %>%
  mutate(ad_type_loc.lymph = ifelse(autoimmune_id___83 == 1 |
                                      autoimmune_id___117 == 1, 1, 0)) %>%
  mutate(ad_type_loc.musc = ifelse(autoimmune_id___27 == 1 |
                                     autoimmune_id___40 == 1 |
                                     autoimmune_id___178 == 1 | 
                                     autoimmune_id___186 == 1 |
                                     autoimmune_id___128 == 1, 1, 0)) %>%
  mutate(ad_type_loc.nerv = ifelse(autoimmune_id___5 == 1 |
                                     autoimmune_id___15 == 1 |
                                     autoimmune_id___25 == 1 |
                                     autoimmune_id___26 == 1 |
                                     autoimmune_id___51 == 1 |
                                     autoimmune_id___63 == 1 |
                                     autoimmune_id___64 == 1 |
                                     autoimmune_id___66 == 1 |
                                     autoimmune_id___68 == 1 |
                                     autoimmune_id___78 == 1 |
                                     autoimmune_id___82 == 1 |
                                     autoimmune_id___90 == 1 |
                                     autoimmune_id___114 == 1|
                                     autoimmune_id___115 == 1 |
                                     autoimmune_id___127 == 1 |
                                     autoimmune_id___144 == 1 |
                                     autoimmune_id___155 == 1 |
                                     autoimmune_id___161 == 1|
                                     autoimmune_id___163 == 1 |
                                     autoimmune_id___168 == 1 |
                                     autoimmune_id___175 == 1, 1, 0)) %>%
  mutate(ad_type_loc.ocul = ifelse(autoimmune_id___28 == 1 |
                                     autoimmune_id___110 == 1 |
                                     autoimmune_id___147 == 1 |
                                     autoimmune_id___181 == 1, 1, 0)) %>%
  mutate(ad_type_loc.panc = ifelse(autoimmune_id___41 == 1, 1, 0)) %>%
  mutate(ad_type_loc.resp = ifelse(autoimmune_id___159 == 1, 1, 0)) %>%
  mutate(ad_type_loc.skin = ifelse(autoimmune_id___3 == 1 |
                                     autoimmune_id___4 == 1 |
                                     autoimmune_id___29 == 1 |
                                     autoimmune_id___56 == 1 |
                                     autoimmune_id___57 == 1 |
                                     autoimmune_id___59 == 1 |
                                     autoimmune_id___73 == 1 |
                                     autoimmune_id___74 == 1 |
                                     autoimmune_id___75 == 1 |
                                     autoimmune_id___88 == 1 |
                                     autoimmune_id___89 == 1 |
                                     autoimmune_id___95 == 1 |
                                     autoimmune_id___103 == 1 |
                                     autoimmune_id___119 == 1 |
                                     autoimmune_id___131 == 1 |
                                     autoimmune_id___138 == 1 |
                                     autoimmune_id___145 == 1 |
                                     autoimmune_id___148 == 1 |
                                     autoimmune_id___165 == 1 |
                                     autoimmune_id___166 == 1 |
                                     autoimmune_id___167 == 1 |
                                     autoimmune_id___169 == 1 , 1, 0)) %>%
  mutate(ad_type_sys.gen = ifelse(  autoimmune_id___105 == 1 |
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
                                      autoimmune_id___139 == 1 |
                                      autoimmune_id___164 == 1 |
                                      autoimmune_id___171 == 1 |
                                      autoimmune_id___172 == 1 |
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
                                     autoimmune_id___133 == 1 |
                                     autoimmune_id___176 == 1, 1, 0))
 #TODO work out whether to drop ADs
#TODO chron.sum?
#TODO ad.type.sum ?
#TODO variables with type ADs might be needed to?

#label
label(ad.type$ad_type_healthy)="Healthy"
label(ad.type$ad_type_loc.blood)="Local blood"
label(ad.type$ad_type_loc.card)="Local cardiac"
label(ad.type$ad_type_loc.ear)="Local ear"
label(ad.type$ad_type_loc.endocr)="Local endocrine"
label(ad.type$ad_type_loc.gastro)="Local gastrointestinal system"
label(ad.type$ad_type_loc.gento)="Local genitourinary system"
label(ad.type$ad_type_loc.skel)="Local joint/skeletal"
label(ad.type$ad_type_loc.kid)="Local kidney"
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

saveRDS(ad.type, file = "AD-type-classification.rds")

############
# Variables of AD classification
############

# Subsetting aim:
# data %>%
#   select(any_of(variable))

autoimmune.yes <- c("autoimmune_id___1",
                    "autoimmune_id___2",
                    "autoimmune_id___3",
                    "autoimmune_id___4",
                    "autoimmune_id___5",
                    "autoimmune_id___6",
                    "autoimmune_id___7",
                    "autoimmune_id___8",
                    "autoimmune_id___9",
                    "autoimmune_id___10",
                    "autoimmune_id___11",
                    "autoimmune_id___12",
                    "autoimmune_id___13",
                    "autoimmune_id___14",
                    "autoimmune_id___15",
                    "autoimmune_id___16",
                    "autoimmune_id___17",
                    "autoimmune_id___18",
                    "autoimmune_id___19",
                    "autoimmune_id___20",
                    "autoimmune_id___21",
                    "autoimmune_id___22",
                    "autoimmune_id___23",
                    "autoimmune_id___24",
                    "autoimmune_id___25",
                    "autoimmune_id___26",
                    "autoimmune_id___27",
                    "autoimmune_id___28",
                    "autoimmune_id___29",
                    "autoimmune_id___30",
                    "autoimmune_id___31",
                    "autoimmune_id___32",
                    "autoimmune_id___33",
                    "autoimmune_id___34",
                    "autoimmune_id___35",
                    "autoimmune_id___36",
                    "autoimmune_id___37",
                    "autoimmune_id___38",
                    "autoimmune_id___39",
                    "autoimmune_id___40",
                    "autoimmune_id___41",
                    "autoimmune_id___42",
                    "autoimmune_id___43",
                    "autoimmune_id___46",
                    "autoimmune_id___50",
                    "autoimmune_id___52",
                    "autoimmune_id___54",
                    "autoimmune_id___59",
                    "autoimmune_id___62",
                    "autoimmune_id___70",
                    "autoimmune_id___71",
                    "autoimmune_id___73",
                    "autoimmune_id___75",
                    "autoimmune_id___77",
                    "autoimmune_id___78",
                    "autoimmune_id___79",
                    "autoimmune_id___80",
                    "autoimmune_id___82",
                    "autoimmune_id___88",
                    "autoimmune_id___89",
                    "autoimmune_id___90",
                    "autoimmune_id___94",
                    "autoimmune_id___96",
                    "autoimmune_id___103",
                    "autoimmune_id___104",
                    "autoimmune_id___107",
                    "autoimmune_id___108",
                    "autoimmune_id___109",
                    "autoimmune_id___110",
                    "autoimmune_id___115",
                    "autoimmune_id___122",
                    "autoimmune_id___127",
                    "autoimmune_id___131",
                    "autoimmune_id___137",
                    "autoimmune_id___138",
                    "autoimmune_id___139",
                    "autoimmune_id___148",
                    "autoimmune_id___150",
                    "autoimmune_id___151",
                    "autoimmune_id___162",
                    "autoimmune_id___165",
                    "autoimmune_id___166",
                    "autoimmune_id___168",
                    "autoimmune_id___171",
                    "autoimmune_id___173",
                    "autoimmune_id___174",
                    "autoimmune_id___175",
                    "autoimmune_id___176",
                    "autoimmune_id___179",
                    "autoimmune_id___181",
                    "autoimmune_id___183")
saveRDS(autoimmune.yes, file = "classification-autoimmune-yes.rds")
autoimmune.no <- c("autoimmune_id___51",
                   "autoimmune_id___53",
                   "autoimmune_id___56",
                   "autoimmune_id___58",
                   "autoimmune_id___60",
                   "autoimmune_id___61",
                   "autoimmune_id___63",
                   "autoimmune_id___64",
                   "autoimmune_id___65",
                   "autoimmune_id___66",
                   "autoimmune_id___68",
                   "autoimmune_id___69",
                   "autoimmune_id___72",
                   "autoimmune_id___76",
                   "autoimmune_id___81",
                   "autoimmune_id___83",
                   "autoimmune_id___86",
                   "autoimmune_id___87",
                   "autoimmune_id___91",
                   "autoimmune_id___92",
                   "autoimmune_id___93",
                   "autoimmune_id___98",
                   "autoimmune_id___101",
                   "autoimmune_id___102",
                   "autoimmune_id___105",
                   "autoimmune_id___106",
                   "autoimmune_id___111",
                   "autoimmune_id___112",
                   "autoimmune_id___113",
                   "autoimmune_id___114",
                   "autoimmune_id___117",
                   "autoimmune_id___119",
                   "autoimmune_id___121",
                   "autoimmune_id___126",
                   "autoimmune_id___128",
                   "autoimmune_id___129",
                   "autoimmune_id___130",
                   "autoimmune_id___132",
                   "autoimmune_id___133",
                   "autoimmune_id___134",
                   "autoimmune_id___136",
                   "autoimmune_id___140",
                   "autoimmune_id___141",
                   "autoimmune_id___142",
                   "autoimmune_id___144",
                   "autoimmune_id___145",
                   "autoimmune_id___146",
                   "autoimmune_id___147",
                   "autoimmune_id___152",
                   "autoimmune_id___153",
                   "autoimmune_id___154",
                   "autoimmune_id___155",
                   "autoimmune_id___156",
                   "autoimmune_id___157",
                   "autoimmune_id___158",
                   "autoimmune_id___160",
                   "autoimmune_id___161",
                   "autoimmune_id___163",
                   "autoimmune_id___167",
                   "autoimmune_id___170",
                   "autoimmune_id___172",
                   "autoimmune_id___177",
                   "autoimmune_id___178",
                   "autoimmune_id___180",
                   "autoimmune_id___182",
                   "autoimmune_id___184",
                   "autoimmune_id___185",
                   "autoimmune_id___186",
                   "autoimmune_id___187")
saveRDS(autoimmune.no, file = "classification-autoimmune-no.rds")
autoimmune.unknown <- c("autoimmune_id___55",
                        "autoimmune_id___57",
                        "autoimmune_id___74",
                        "autoimmune_id___84",
                        "autoimmune_id___85",
                        "autoimmune_id___95",
                        "autoimmune_id___99",
                        "autoimmune_id___100",
                        "autoimmune_id___116",
                        "autoimmune_id___118",
                        "autoimmune_id___125",
                        "autoimmune_id___149",
                        "autoimmune_id___159",
                        "autoimmune_id___164",
                        "autoimmune_id___169")
saveRDS(autoimmune.unknown, file = "classification-autoimmune-unknown.rds")

type.healthy <- c("autoimmune_id___44")
type.local.blood <- c("autoimmune_id___2", 
                        "autoimmune_id___11", 
                        "autoimmune_id___21", 
                        "autoimmune_id___70", 
                        "autoimmune_id___102", 
                        "autoimmune_id___106", 
                        "autoimmune_id___126", 
                        "autoimmune_id___130", 
                        "autoimmune_id___134", 
                        "autoimmune_id___150", 
                        "autoimmune_id___156", 
                        "autoimmune_id___157", 
                        "autoimmune_id___177")
saveRDS(type.local.blood, file = "type-local-blood.rds")
type.local.cardiac <- c("autoimmune_id___16", 
                                     "autoimmune_id___53", 
                                     "autoimmune_id___116", 
                                     "autoimmune_id___100", 
                                     "autoimmune_id___160")
saveRDS(type.local.cardiac, file = "type-local-cardiac.rds")
type.local.ear <- c("autoimmune_id___140",
                                    "autoimmune_id___129")
saveRDS(type.local.ear, file = "type-local-ear.rds")
type.local.endocrine <- c("autoimmune_id___14", 
                                       "autoimmune_id___17", 
                                       "autoimmune_id___87", 
                                       "autoimmune_id___91", 
                                       "autoimmune_id___112", 
                                       "autoimmune_id___113", 
                                       "autoimmune_id___125", 
                                       "autoimmune_id___170",
                                       "autoimmune_id___173",
                                       "autoimmune_id___179",
                                       "autoimmune_id___184")
saveRDS(type.local.endocrine, file = "type-local-endocrine.rds")
type.local.gastrointestinal <- c("autoimmune_id___6", 
                                       "autoimmune_id___8", 
                                       "autoimmune_id___30", 
                                       "autoimmune_id___42", 
                                       "autoimmune_id___61", 
                                       "autoimmune_id___69", 
                                       "autoimmune_id___72", 
                                       "autoimmune_id___93", 
                                       "autoimmune_id___94", 
                                       "autoimmune_id___98", 
                                       "autoimmune_id___122", 
                                       "autoimmune_id___136", 
                                       "autoimmune_id___141", 
                                       "autoimmune_id___142", 
                                       "autoimmune_id___149", 
                                       "autoimmune_id___174")
saveRDS(type.local.gastrointestinal, file = "type-local-gastrointestinal.rds")
type.local.genitourinary <- c("autoimmune_id___65", 
                                      "autoimmune_id___84", 
                                      "autoimmune_id___99", 
                                      "autoimmune_id___101")
saveRDS(type.local.genitourinary, file = "type-local-genitourinary.rds")
type.local.skeletal <- c("autoimmune_id___1", 
                                     "autoimmune_id___58",
                                     "autoimmune_id___71",  
                                     "autoimmune_id___81", 
                                     "autoimmune_id___92", 
                                     "autoimmune_id___111", 
                                     "autoimmune_id___121", 
                                     "autoimmune_id___151", 
                                     "autoimmune_id___152", 
                                     "autoimmune_id___153", 
                                     "autoimmune_id___154", 
                                     "autoimmune_id___158", 
                                     "autoimmune_id___162", 
                                     "autoimmune_id___180",
                                     "autoimmune_id___187")
saveRDS(type.local.skeletal, file = "type-local-skeletal.rds")
type.local.kidney <- c("autoimmune_id___19", 
                                      "autoimmune_id___23", 
                                      "autoimmune_id___137")
saveRDS(type.local.kidney, file = "type-local-kidney.rds")
type.local.liver <- c("autoimmune_id___32", 
                                    "autoimmune_id___52",
                                    "autoimmune_id___182",
                                    "autoimmune_id___183",
                                    "autoimmune_id___185")
saveRDS(type.local.liver, file = "type-local-liver.rds")
type.local.lymphatic <- c("autoimmune_id___83", 
                            "autoimmune_id___117")
saveRDS(type.local.lymphatic, file = "type-local-lymphatic.rds")
type.local.muscular <- c("autoimmune_id___27", 
                           "autoimmune_id___40", 
                           "autoimmune_id___178",  
                           "autoimmune_id___186", 
                           "autoimmune_id___128")
saveRDS(type.local.muscular, file = "type-local-muscular.rds")
type.local.nervous <- c("autoimmune_id___5", 
                          "autoimmune_id___15", 
                          "autoimmune_id___25", 
                          "autoimmune_id___26", 
                          "autoimmune_id___51", 
                          "autoimmune_id___63", 
                          "autoimmune_id___64", 
                          "autoimmune_id___66", 
                          "autoimmune_id___68", 
                          "autoimmune_id___78", 
                          "autoimmune_id___82", 
                          "autoimmune_id___90", 
                          "autoimmune_id___114",
                          "autoimmune_id___115", 
                          "autoimmune_id___127", 
                          "autoimmune_id___144", 
                          "autoimmune_id___155", 
                          "autoimmune_id___161",
                          "autoimmune_id___163", 
                          "autoimmune_id___168", 
                          "autoimmune_id___175")
saveRDS(type.local.nervous, file = "type-local-nervous.rds")
type.local.ocular <- c("autoimmune_id___28", 
                         "autoimmune_id___110", 
                         "autoimmune_id___147", 
                         "autoimmune_id___181")
saveRDS(type.local.ocular, file = "type-local-ocular.rds")
type.local.pancreas <- c("autoimmune_id___41")
saveRDS(type.local.pancreas, file = "type-local-pancreas.rds")
type.local.respiratory <- c("autoimmune_id___159")
saveRDS(type.local.respiratory, file = "type-local-respiratory.rds")
type.local.skin <- c("autoimmune_id___3", 
                       "autoimmune_id___4", 
                       "autoimmune_id___29", 
                       "autoimmune_id___56", 
                       "autoimmune_id___57", 
                       "autoimmune_id___59", 
                       "autoimmune_id___73", 
                       "autoimmune_id___74", 
                       "autoimmune_id___75", 
                       "autoimmune_id___88", 
                       "autoimmune_id___89", 
                       "autoimmune_id___95", 
                       "autoimmune_id___103", 
                       "autoimmune_id___119", 
                       "autoimmune_id___131", 
                       "autoimmune_id___138", 
                       "autoimmune_id___145", 
                       "autoimmune_id___148", 
                       "autoimmune_id___165", 
                       "autoimmune_id___166", 
                       "autoimmune_id___167", 
                       "autoimmune_id___169")
saveRDS(type.local.skin, file = "type-local-skin.rds")
type.systemic.general <- c("autoimmune_id___105", 
                             "autoimmune_id___108", 
                             "autoimmune_id___146", 
                             "autoimmune_id___132")
saveRDS(type.systemic.general, file = "type-systemic-general.rds")
type.systemic.rheum <- c("autoimmune_id___7", 
                           "autoimmune_id___9", 
                           "autoimmune_id___22", 
                           "autoimmune_id___31", 
                           "autoimmune_id___33", 
                           "autoimmune_id___34", 
                           "autoimmune_id___35", 
                           "autoimmune_id___37", 
                           "autoimmune_id___38", 
                           "autoimmune_id___46", 
                           "autoimmune_id___50", 
                           "autoimmune_id___55", 
                           "autoimmune_id___60", 
                           "autoimmune_id___77", 
                           "autoimmune_id___80", 
                           "autoimmune_id___96", 
                           "autoimmune_id___107", 
                           "autoimmune_id___139", 
                           "autoimmune_id___164", 
                           "autoimmune_id___171", 
                           "autoimmune_id___172", 
                           "autoimmune_id___85")
saveRDS(type.systemic.rheum, file = "type-systemic-rheum.rds")
type.systemic.vasc <- c("autoimmune_id___10", 
                          "autoimmune_id___12", 
                          "autoimmune_id___13", 
                          "autoimmune_id___18", 
                          "autoimmune_id___20", 
                          "autoimmune_id___24", 
                          "autoimmune_id___36", 
                          "autoimmune_id___39", 
                          "autoimmune_id___43", 
                          "autoimmune_id___54", 
                          "autoimmune_id___62", 
                          "autoimmune_id___76", 
                          "autoimmune_id___79", 
                          "autoimmune_id___86", 
                          "autoimmune_id___104", 
                          "autoimmune_id___109", 
                          "autoimmune_id___118", 
                          "autoimmune_id___133", 
                          "autoimmune_id___176")
saveRDS(type.systemic.vasc, file = "type-systemic-vasc.rds")

adty.cloud <- ad.type %>%
  select(contains("ad_type")) 
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

############
#
# ICD-11 parent classification
#
###########

#Certain infectious or parasitic diseases
icd.parasitic <- c(
  "autoimmune_id___171",
  "autoimmune_id___182",
  "autoimmune_id___96"
)
saveRDS(icd.parasitic, file = "icd-parasitic.rds")

#Codes for special purposes
icd.special <- c(
  "autoimmune_id___164"
)
saveRDS(icd.special, file = "icd-special.rds")

#Developmental anomalies
icd.developmental <- c(
  "autoimmune_id___111",
  "autoimmune_id___172",
  "chronic___19"
)
saveRDS(icd.developmental, file = "icd-developmental.rds")

#Diseases of the blood or blood-forming organs
icd.blood <- c(
  "autoimmune_id___133",
  "autoimmune_id___157",
  "autoimmune_id___86",
  "chronic___23",
  "autoimmune_id___11",
  "autoimmune_id___177",
  "autoimmune_id___2",
  "autoimmune_id___21",
  "autoimmune_id___30",
  "autoimmune_id___70"
)
saveRDS(icd.blood, file = "icd-blood.rds")

# Diseases of the circulatory system
icd.circulatory <- c(
  "autoimmune_id___117",
  "autoimmune_id___128",
  "autoimmune_id___134",
  "autoimmune_id___153",
  "autoimmune_id___16",
  "autoimmune_id___160",
  "autoimmune_id___66",
  "chronic___24",
  "chronic___5"
)
saveRDS(icd.circulatory, file = "icd-circulatory.rds")

# Diseases of the digestive system
icd.digestive <- c(
  "autoimmune_id___122",
  "autoimmune_id___142",
  "autoimmune_id___149",
  "autoimmune_id___183",
  "autoimmune_id___32",
  "autoimmune_id___42",
  "autoimmune_id___52",
  "autoimmune_id___6",
  "autoimmune_id___61",
  "autoimmune_id___69",
  "autoimmune_id___72",
  "autoimmune_id___8",
  "autoimmune_id___93",
  "autoimmune_id___94",
  "autoimmune_id___98",
  "autoimmune_id___83"
)
saveRDS(icd.digestive, file = "icd-digestive.rds")

# Diseases of the ear or mastoid process
icd.ear <- c(
  "autoimmune_id___129",
  "autoimmune_id___140"
)
saveRDS(icd.ear, file = "icd-ear.rds")

#Diseases of the genitourinary system
icd.genitourinary <- c(
  "autoimmune_id___101",
  "autoimmune_id___184",
  "autoimmune_id___23",
  "autoimmune_id___65",
  "chronic___12"
)
saveRDS(icd.genitourinary, file = "icd-genitourinary.rds")

#Diseases of the immune system
icd.immune <- c(
  "autoimmune_id___10",
  "autoimmune_id___104",
  "autoimmune_id___106",
  "autoimmune_id___107",
  "autoimmune_id___108",
  "autoimmune_id___109",
  "autoimmune_id___118",
  "autoimmune_id___126",
  "autoimmune_id___13",
  "autoimmune_id___130",
  "autoimmune_id___137",
  "autoimmune_id___150",
  "autoimmune_id___173",
  "autoimmune_id___176",
  "autoimmune_id___18",
  "autoimmune_id___20",
  "autoimmune_id___22",
  "autoimmune_id___24",
  "autoimmune_id___31",
  "autoimmune_id___33",
  "autoimmune_id___36",
  "autoimmune_id___37",
  "autoimmune_id___38",
  "autoimmune_id___40",
  "autoimmune_id___43",
  "autoimmune_id___50",
  "autoimmune_id___54",
  "autoimmune_id___62",
  "autoimmune_id___7",
  "autoimmune_id___77",
  "autoimmune_id___80",
  "autoimmune_id___9",
  "autoimmune_id___132",
  "chronic___21"
)
saveRDS(icd.immune, file = "icd-immune.rds")

# Diseases of the musculoskeletal system or connective tissue
icd.musculoskeletal <- c(
  "autoimmune_id___1",
  "autoimmune_id___121",
  "autoimmune_id___139",
  "autoimmune_id___151",
  "autoimmune_id___152",
  "autoimmune_id___162",
  "autoimmune_id___178",
  "autoimmune_id___186",
  "autoimmune_id___34",
  "autoimmune_id___35",
  "autoimmune_id___58",
  "autoimmune_id___71",
  "autoimmune_id___81",
  "autoimmune_id___85",
  "autoimmune_id___92",
  "chronic___18"
)
saveRDS(icd.musculoskeletal, file = "icd-musculoskeletal.rds")

# Diseases of the nervous system
icd.nervous <- c(
  "autoimmune_id___115",
  "autoimmune_id___116",
  "autoimmune_id___127",
  "autoimmune_id___15",
  "autoimmune_id___155",
  "autoimmune_id___161",
  "autoimmune_id___168",
  "autoimmune_id___175",
  "autoimmune_id___180",
  "autoimmune_id___187",
  "autoimmune_id___25",
  "autoimmune_id___26",
  "autoimmune_id___27",
  "autoimmune_id___28",
  "autoimmune_id___39",
  "autoimmune_id___5",
  "autoimmune_id___51",
  "autoimmune_id___53",
  "autoimmune_id___55",
  "autoimmune_id___79",
  "autoimmune_id___82",
  "autoimmune_id___90",
  "chronic___10",
  "chronic___16",
  "chronic___25",
  "chronic___26"
)
saveRDS(icd.nervous, file = "icd-nervous.rds")

#Diseases of the respiratory system
icd.respiratory <- c(
  "autoimmune_id___146",
  "autoimmune_id___158",
  "autoimmune_id___159",
  "chronic___1",
  "chronic___13"
)
saveRDS(icd.respiratory, file = "icd-respiratory.rds")

#Diseases of the skin
icd.skin <- c(
  "autoimmune_id___103",
  "autoimmune_id___119",
  "autoimmune_id___131",
  "autoimmune_id___138",
  "autoimmune_id___144",
  "autoimmune_id___145",
  "autoimmune_id___148",
  "autoimmune_id___165",
  "autoimmune_id___166",
  "autoimmune_id___167",
  "autoimmune_id___169",
  "autoimmune_id___29",
  "autoimmune_id___57",
  "autoimmune_id___59",
  "autoimmune_id___73",
  "autoimmune_id___74",
  "autoimmune_id___75",
  "autoimmune_id___88",
  "autoimmune_id___89",
  "autoimmune_id___95",
  "autoimmune_id___3",
  "autoimmune_id___4",
  "chronic___14"
)
saveRDS(icd.skin, file = "icd-skin.rds")

#Diseases of the visual system
icd.visual <- c(
  "autoimmune_id___110",
  "autoimmune_id___147",
  "autoimmune_id___181"
)
saveRDS(icd.visual, file = "icd-visual.rds")

#Endocrine, nutritional or metabolic diseases
icd.endocrine <- c(
  "autoimmune_id___102",
  "autoimmune_id___112",
  "autoimmune_id___113",
  "autoimmune_id___125",
  "autoimmune_id___14",
  "autoimmune_id___141",
  "autoimmune_id___154",
  "autoimmune_id___156",
  "autoimmune_id___17",
  "autoimmune_id___170",
  "autoimmune_id___179",
  "autoimmune_id___185",
  "autoimmune_id___41",
  "autoimmune_id___46",
  "autoimmune_id___84",
  "autoimmune_id___87",
  "autoimmune_id___91",
  "chronic___2",
  "chronic___20"
)
saveRDS(icd.endocrine, file = "icd-endocrine.rds")

#Mental, behavioural or neurodevelopmental disorders
icd.mental <- c(
  "autoimmune_id___68",
  "chronic___11",
  "chronic___4",
  "chronic___9"
)
saveRDS(icd.mental, file = "icd-mental.rds")

#Neoplasms
icd.neoplasm <- c(
  "autoimmune_id___76",
  "chronic___3"
)
saveRDS(icd.neoplasm, file = "icd-neoplasm.rds")

#Sleep-wake disorders
icd.sleep <- c(
  "autoimmune_id___114",
  "autoimmune_id___78",
  "chronic___17"
)
saveRDS(icd.sleep, file = "icd-sleep.rds")

#Symptoms, signs or clinical findings, not elsewhere classified
icd.symptom <- c(
  "autoimmune_id___100",
  "autoimmune_id___105",
  "autoimmune_id___12",
  "autoimmune_id___136",
  "autoimmune_id___163",
  "autoimmune_id___174",
  "autoimmune_id___19",
  "autoimmune_id___56",
  "autoimmune_id___60",
  "autoimmune_id___63",
  "autoimmune_id___64",
  "autoimmune_id___99",
  "chronic___15",
  "chronic___22"
)
saveRDS(icd.symptom, file = "icd-symptom.rds")
