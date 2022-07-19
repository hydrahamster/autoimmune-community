library(plyr)
library(tidyverse)
library(magrittr)
library(RColorBrewer)
library(wesanderson)
library(ggrepel)
library(arsenal) # for comparing dfs, to check if mutations on columns worked
library(ggbeeswarm)
library(wordcloud2)
library(sjlabelled)
library(viridis)


source("1_1-22_data-clean.R")

########
#
# Other treatments used by participants
#
######
# other.treatments <- data.clean %>%  # for future stuff include: filter_all(any_vars(str_detect(., pattern = ""))) %>%
#    select(symptoms_other) %>%
#    mutate_all(tolower) %>%
#    mutate_all(str_trim) %>%
#    unique(.)
#  write.table(x = other.treatments,
#              file = "treatments-other-all_1-22.txt",
#              row.names = FALSE,
#              sep = "\t")
## then run new-treatment-search.sh

 ########
 #
 # Other drugs used by participants
 #
 # ######
 # other.drugs <- data.clean %>%
 #   select(drug_other_other) %>%
 #   mutate_all(tolower) %>%
 #   mutate_all(str_trim) %>%
 #   unique(.)
 # 
 # write.table(x = other.drugs,
 #            file = "drugs-other-all_1-22.txt",
 #            row.names = FALSE,
 #            sep = "\t")
# then run new-drugs-search.sh
 

#make other treatments df
df.other.treatments <- data.clean %>% 
  select(record_id,	symptoms_other, drug_other_other) %>%
  mutate_all(tolower) %>%
  mutate_all(str_trim)


df.other.treatments <- df.other.treatments %>%
  mutate(cl.imsup = 0) %>%
  mutate(cl.suppl = 0) %>%
  mutate(cl.lifest = 0) %>%
  mutate(cl.diet = 0) %>%
  mutate(cl.nomed = 0) %>%
  mutate(cl.opia = 0) %>%
  mutate(cl.anac = 0) %>%
  mutate(cl.endoc = 0) %>%
  mutate(cl.antinf = 0) %>%
  mutate(cl.psych = 0) %>%
  mutate(cl.imod = 0) %>%
  mutate(cl.dmard = 0) %>%
  mutate(cl.ahist = 0) %>%
  mutate(cl.recdr = 0) %>%
  mutate(cl.stool = 0) %>%
  mutate(cl.antiem = 0) %>%
  mutate(cl.afung = 0) %>%
  mutate(cl.dopag = 0) %>%
  mutate(cl.avir = 0) %>%
  mutate(cl.diaz = 0) %>%
  mutate(cl.surg = 0) %>%
  mutate(cl.neurp = 0) %>%
  mutate(cl.diur = 0) %>%
  mutate(cl.cholai = 0) %>%
  mutate(cl.enzy = 0) %>%
  mutate(cl.abiot = 0) %>%
  mutate(cl.mpara = 0) %>%
  mutate(cl.mstm = 0) %>%
  mutate(cl.bldpr = 0) %>%
  mutate(cl.altpr = 0) %>%
  mutate(cl.stim = 0) %>%
  mutate(cl.heart = 0) %>%
  mutate(cl.tyki = 0) %>%
  mutate(cl.thrag = 0) %>%
  mutate(cl.albl = 0) %>%
  mutate(cl.vascon = 0) %>%
  mutate(tr.esom = 0) %>%
  mutate(tr.mesal = 0) %>%
  mutate(tr.cortst = 0) %>%
  mutate(tr.strav = 0) %>%
  mutate(tr.diet = 0) %>%
  mutate(tr.rest = 0) %>%
  mutate(tr.nsai = 0) %>%
  mutate(tr.neri = 0) %>%
  mutate(tr.bath = 0) %>%
  mutate(tr.heat = 0) %>%
  mutate(tr.vit = 0) %>%
  mutate(tr.mins = 0) %>%
  mutate(tr.iod = 0) %>%
  mutate(tr.ashw = 0) %>%
  mutate(tr.vitd = 0) %>%
  mutate(tr.vitb12 = 0) %>%
  mutate(tr.melat = 0) %>%
  mutate(tr.ndte = 0) %>%
  mutate(tr.exer = 0) %>%
  mutate(tr.suppl = 0) %>%
  mutate(tr.sleep = 0) %>%
  mutate(tr.ldn = 0) %>%
  mutate(tr.anac = 0) %>%
  mutate(tr.cool = 0) %>%
  mutate(tr.bdyw = 0) %>%
  mutate(tr.massa = 0) %>%
  mutate(tr.dexi = 0) %>%
  mutate(tr.levo = 0) %>%
  mutate(tr.phys = 0) %>%
  mutate(tr.magn = 0) %>%
  mutate(tr.acc = 0) %>%
  mutate(tr.inos = 0) %>%
  mutate(tr.ribo = 0) %>%
  mutate(tr.imsup = 0) %>%
  mutate(tr.metr = 0) %>%
  mutate(tr.bact = 0) %>%
  mutate(tr.hydrx = 0) %>%
  mutate(tr.rita = 0) %>%
  mutate(tr.phen = 0) %>%
  mutate(tr.hydrco = 0) %>%
  mutate(tr.flrco = 0) %>%
  mutate(tr.cortinj = 0) %>%
  mutate(tr.propr = 0) %>%
  mutate(tr.effex = 0) %>%
  mutate(tr.micrgy = 0) %>%
  mutate(tr.modaf = 0) %>%
  mutate(tr.zyrt = 0) %>%
  mutate(tr.parc = 0) %>%
  mutate(tr.tapa = 0) %>%
  mutate(tr.colo = 0) %>%
  mutate(tr.celeb = 0) %>%
  mutate(tr.osteo = 0) %>%
  mutate(tr.psych = 0) %>%
  mutate(tr.allexp = 0) %>%
  mutate(tr.alco = 0) %>%
  mutate(tr.mdma = 0) %>%
  mutate(tr.lsd = 0) %>%
  mutate(tr.keta = 0) %>%
  mutate(tr.melox = 0) %>%
  mutate(tr.apath = 0) %>%
  mutate(tr.mido = 0) %>%
  mutate(tr.ivig = 0) %>%
  mutate(tr.preb = 0) %>%
  mutate(tr.prob = 0) %>%
  mutate(tr.apci = 0) %>%
  mutate(tr.cthsu = 0) %>%
  mutate(tr.thyrx = 0) %>%
  mutate(tr.collgn = 0) %>%
  mutate(tr.curc = 0) %>%
  mutate(tr.coq10 = 0) %>%
  mutate(tr.domp = 0) %>%
  mutate(tr.cital = 0) %>%
  mutate(tr.bldth = 0) %>%
  mutate(tr.calbl = 0) %>%
  mutate(tr.nitrg = 0) %>%
  mutate(tr.lifest = 0) %>%
  mutate(tr.cyclo = 0) %>%
  mutate(tr.fish = 0) %>%
  mutate(tr.cana = 0) %>%
  mutate(tr.eyero = 0) %>%
  mutate(tr.bala = 0) %>%
  mutate(tr.stret = 0) %>%
  mutate(tr.ritx = 0) %>%
  mutate(tr.cycph = 0) %>%
  mutate(tr.pila = 0) %>%
  mutate(tr.altmed = 0) %>%
  mutate(tr.aqua = 0) %>%
  mutate(tr.berzm = 0) %>%
  mutate(tr.warf = 0) %>%
  mutate(tr.orca = 0) %>%
  mutate(tr.skca = 0) %>%
  mutate(tr.lerca = 0) %>%
  mutate(tr.medit = 0) %>%
  mutate(tr.taich = 0) %>%
  mutate(tr.ppi = 0) %>%
  mutate(tr.mndfl = 0) %>%
  mutate(tr.chiro = 0) %>%
  mutate(tr.chmed = 0) %>%
  mutate(tr.accp = 0) %>%
  mutate(tr.adep = 0) %>%
  mutate(tr.bihor = 0) %>%
  mutate(tr.myla = 0) %>%
  mutate(tr.dmard = 0) %>%
  mutate(tr.ahist = 0) %>%
  mutate(tr.rivxb = 0) %>%
  mutate(tr.valac = 0) %>%
  mutate(tr.prox = 0) %>%
  mutate(tr.flucz = 0) %>%
  mutate(tr.prmx = 0) %>%
  mutate(tr.gaba = 0) %>%
  mutate(tr.clonz = 0) %>%
  mutate(tr.mpred = 0) %>%
  mutate(tr.pace = 0) %>%
  mutate(tr.gard = 0) %>%
  mutate(tr.natr = 0) %>%
  mutate(tr.cmzl = 0) %>%
  mutate(tr.iron = 0) %>%
  mutate(tr.minox = 0) %>%
  mutate(tr.stcetr = 0) %>%
  mutate(tr.essoi = 0) %>%
  mutate(tr.zostx = 0) %>%
  mutate(tr.zinc = 0) %>%
  mutate(tr.aspr = 0) %>%
  mutate(tr.pntoz = 0) %>%
  mutate(tr.ovest = 0) %>%
  mutate(tr.talad = 0) %>%
  mutate(tr.opsum = 0) %>%
  mutate(tr.pregab = 0) %>%
  mutate(tr.acei = 0) %>%
  mutate(tr.sprlc = 0) %>%
  mutate(tr.fusem = 0) %>%
  mutate(tr.eztim = 0) %>%
  mutate(tr.creon = 0) %>%
  mutate(tr.preds = 0) %>%
  mutate(tr.alpur = 0) %>%
  mutate(tr.dymi = 0) %>%
  mutate(tr.fexo = 0) %>%
  mutate(tr.iginj = 0) %>%
  mutate(tr.bose = 0) %>%
  mutate(tr.wrkp = 0) %>%
  mutate(tr.wrkna = 0) %>%
  mutate(tr.petco = 0) %>%
  mutate(tr.walk = 0) %>%
  mutate(tr.omep = 0) %>%
  mutate(tr.nifd = 0) %>%
  mutate(tr.sild = 0) %>%
  mutate(tr.itrcz = 0) %>%
  mutate(tr.pill = 0) %>%
  mutate(tr.hmeo = 0) %>%
  mutate(tr.botx = 0) %>%
  mutate(tr.galcz = 0) %>%
  mutate(tr.thc = 0) %>%
  mutate(tr.cbd = 0) %>%
  mutate(tr.surg = 0) %>%
  mutate(tr.nasl = 0) %>%
  mutate(tr.ntzl = 0) %>%
  mutate(tr.herb = 0) %>%
  mutate(tr.yoga = 0) %>%
  mutate(tr.mntlk = 0) %>%
  mutate(tr.cetrz = 0) %>%
  mutate(tr.amtrp = 0) %>%
  mutate(tr.betbl = 0) %>%
  mutate(tr.whlch = 0) %>%
  mutate(tr.elctr = 0) %>%
  mutate(tr.bed = 0) %>%
  mutate(tr.mclo = 0) %>%
  mutate(tr.smmp = 0) %>%
  mutate(tr.dulox = 0) %>%
  mutate(tr.endp = 0) %>%
  mutate(tr.peas = 0) %>%
  mutate(tr.splpl = 0) %>%
  mutate(tr.mxbst = 0) %>%
  mutate(tr.abiot = 0) %>%
  mutate(tr.afung = 0) %>%
  mutate(tr.myth = 0) %>%
  mutate(tr.tocrt = 0) %>%
  mutate(tr.clchn = 0) %>%
  mutate(tr.arprz = 0) %>%
  mutate(tr.vanst = 0) %>%
  mutate(tr.trcrms = 0) %>%
  mutate(tr.mtfr = 0) %>%
  mutate(tr.qtpn = 0) %>%
  mutate(tr.lmtrg = 0) %>%
  mutate(tr.cobt = 0) %>%
  mutate(tr.pyrdo = 0) %>%
  mutate(tr.p5p = 0) %>%
  mutate(tr.mits = 0) %>%
  mutate(tr.cmpst = 0) %>%
  mutate(tr.pscph = 0) %>%
  mutate(tr.drndl = 0) %>%
  mutate(tr.detx = 0) %>%
  mutate(tr.msccr = 0) %>%
  mutate(tr.splnec = 0) %>%
  mutate(tr.gcsf = 0) %>%
  mutate(tr.rmplo = 0) %>%
  mutate(tr.bldtr = 0) %>%
  mutate(tr.trax = 0) %>%
  mutate(tr.trav = 0) %>%
  mutate(tr.drkhm = 0) %>%
  mutate(tr.c1in = 0) %>%
  mutate(tr.smth = 0) %>%
  mutate(tr.vasc = 0) %>%
  mutate(tr.aubuf = 0) %>%
  mutate(tr.resr = 0) %>%
  mutate(tr.relax = 0) %>%
  mutate(tr.shyp = 0) %>%
  mutate(tr.bflvt = 0) %>%
  mutate(tr.kins = 0) %>%
  mutate(tr.mthf = 0) %>%
  mutate(tr.vascon = 0) %>%
  mutate(tr.stim = 0) %>%
  mutate(tr.water = 0) %>%
  mutate(tr.escip = 0) %>%
  mutate(tr.brthe = 0) %>%
  mutate(tr.swm = 0) %>%
  mutate(tr.progn = 0) %>%
  mutate(tr.drib = 0) %>%
  mutate(tr.nutr = 0) %>%
  mutate(tr.sulfz = 0) %>%
  mutate(tr.salt = 0) %>%
  mutate(tr.brbon = 0) %>%
  mutate(tr.licr = 0) %>%
  mutate(tr.om3 = 0) %>%
  mutate(tr.gibi = 0) %>%
  mutate(tr.lima = 0) %>%
  mutate(tr.glcsm = 0) %>%
  mutate(tr.chndr = 0) %>%
  mutate(tr.msm = 0) %>%
  mutate(tr.pqq = 0) %>%
  mutate(tr.clcm = 0) %>%
  mutate(tr.vitc = 0) %>%
  mutate(tr.at1 = 0) %>%
  mutate(tr.post = 0) %>%
  mutate(tr.cupr = 0) %>%
  mutate(tr.splnt = 0) %>%
  mutate(tr.tens = 0) %>%
  mutate(tr.ems = 0) %>%
  mutate(tr.hydt = 0) %>%
  mutate(tr.antinf = 0) %>%
  mutate(tr.dxpn = 0) %>%
  mutate(tr.dsvx = 0) %>%
  mutate(tr.nrdvt = 0) %>%
  mutate(tr.mbai = 0) %>%
  mutate(tr.ephd = 0) %>%
  mutate(tr.sumt = 0) %>%
  mutate(tr.vite = 0) %>%
  mutate(tr.vitb7 = 0) %>%
  mutate(tr.ntre = 0) %>%
  mutate(tr.emdr = 0) %>%
  mutate(tr.ivab = 0) %>%
  mutate(tr.spmba = 0) %>%
  mutate(tr.reiki = 0) %>%
  mutate(tr.hcrtp = 0) %>%
  mutate(tr.gltm = 0) %>%
  mutate(tr.glyc = 0) %>%
  mutate(tr.chrc = 0) %>%
  mutate(tr.mlbd = 0) %>%
  mutate(tr.slnem = 0) %>%
  mutate(tr.bthy = 0) %>%
  mutate(tr.cpap = 0) %>%
  mutate(tr.blcfn = 0) %>%
  mutate(cl.msrlx = 0) %>%
  mutate(tr.tpest = 0) %>%
  mutate(tr.tnfi = 0) %>%
  mutate(tr.bwth = 0) %>%
  mutate(tr.rhro = 0) %>%
  mutate(tr.ciqu = 0) %>%
  mutate(tr.insln = 0) %>%
  mutate(tr.sugr = 0) %>%
  mutate(tr.vitb9 = 0) %>%
  mutate(tr.ahist1 = 0) %>%
  mutate(tr.ahist2 = 0) %>%
  mutate(tr.mcst = 0) %>%
  mutate(tr.imth = 0) %>%
  mutate(tr.neurp = 0) %>%
  mutate(tr.blreg = 0) %>%
  mutate(tr.rztrp = 0) %>%
  mutate(tr.ibup = 0) %>%
  mutate(tr.diaz = 0) %>%
  mutate(tr.testo = 0) %>%
  mutate(tr.psylo = 0) %>%
  mutate(tr.coca = 0) %>%
  mutate(tr.medca = 0) %>%
  mutate(tr.acyc = 0) %>%
  mutate(tr.colos = 0) %>%
  mutate(tr.sert = 0) %>%
  mutate(tr.nint = 0) %>%
  mutate(tr.swri = 0) %>%
  mutate(tr.lstm = 0) %>%
  mutate(tr.detc = 0) %>%
  mutate(tr.xylm = 0) %>%
  mutate(tr.vbin = 0) %>%
  mutate(tr.irin = 0) %>%
  mutate(tr.hypb = 0) %>%
  mutate(tr.etrm = 0) %>%
  mutate(tr.mthl = 0) %>%
  mutate(tr.amac = 0) %>%
  mutate(tr.resn = 0) %>%
  mutate(tr.fibr = 0) %>%
  mutate(tr.laxa = 0) %>%
  mutate(tr.antiem = 0) %>%
  mutate(tr.prchz = 0) %>%
  mutate(tr.vens = 0) %>%
  mutate(tr.art = 0) %>%
  mutate(tr.bilg = 0) %>%
  mutate(tr.aarh = 0) %>%
  mutate(tr.dmt = 0) %>%
  mutate(tr.ocrz = 0) %>%
  mutate(tr.meds = 0) %>%
  mutate(tr.prkn = 0) %>%
  mutate(tr.thia = 0) %>%
  mutate(tr.inex = 0) %>%
  mutate(tr.tthr = 0) %>%
  mutate(tr.niac = 0) %>%
  mutate(tr.orth = 0) %>%
  mutate(tr.trinj = 0) %>%
  mutate(tr.msrlx = 0) %>%
  mutate(tr.mrbg = 0) %>%
  mutate(tr.flmx = 0) %>%
  mutate(tr.mrtz = 0) %>%
  mutate(tr.vpap = 0) %>%
  mutate(tr.plph = 0) %>%
    mutate(de.smok = 0) %>%
  mutate(de.edib = 0) %>%
  mutate(de.oil = 0) 

df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(tr.esom, 
                   tr.mesal, 
                   tr.cortst, 
                   tr.strav, 
                   tr.diet,
                   tr.rest,
                   cl.anac,
                   cl.antinf,
                   cl.lifest,
                   cl.diet)), ~ ifelse(symptoms_other == "esomeprazole, mesalazine, corticosteroids, avoiding stressors, diet management, and controlled rest", 1, .)) %>%
  mutate_at(vars(c(tr.nsai,
                   cl.antinf)), ~ ifelse(symptoms_other == "non-steroidal anti-inflammatories", 1, .)) %>%
  mutate_at(vars(c(tr.neri,
                   cl.psych)), ~ ifelse(symptoms_other == "norepinephrine reuptake inhibitor", 1, .)) %>%
  mutate_at(vars(c(tr.bath,
                   tr.heat,
                   tr.rest,
                   cl.lifest,
                   cl.nomed)), ~ ifelse(symptoms_other == "rest, having a bath for pain, heat rub,", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   tr.mins,
                   tr.iod,
                   tr.ashw,
                   cl.suppl)), ~ ifelse(symptoms_other == "vitamin and mineral supplements, some iodine, ashwaganda at present as a trial, - not medicated as bloodwork not optimsal but in range md will not treat so my onw 1 rat experiment", 1, .)) %>%
  mutate_at(vars(c(tr.vitd,
                   tr.vitb12,
                   tr.melat,
                   tr.ndte,
                   cl.suppl,
                   cl.endoc)), ~ ifelse(symptoms_other == "vit d.  b12.  melatonin.  ndt.", 1, .)) %>%
  mutate_at(vars(c(tr.exer,
                   tr.diet,
                   cl.lifest,
                   cl.diet)), ~ ifelse(symptoms_other == "clean diet mild excercise", 1, .)) %>%
  mutate_at(vars(c(tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "supplements", 1, .)) %>%
  mutate_at(vars(c(tr.sleep,
                   tr.strav,
                   tr.melat,
                   cl.endoc,
                   cl.lifest)), ~ ifelse(symptoms_other == "good sleep hygiene, address stress through mindfulness, melatonin", 1, .)) %>%
  mutate_at(vars(c(tr.ldn,
                   cl.imod)), ~ ifelse(symptoms_other == "ldn", 1, .)) %>%
  mutate_at(vars(c(tr.anac,
                   tr.diet,
                   cl.diet,
                   cl.anac)), ~ ifelse(symptoms_other == "gluten free diet and antacids/stomach settling medication", 1, .)) %>%
  mutate_at(vars(c(cl.antinf,
                   tr.preds)), ~ ifelse(symptoms_other == "prednisone", 1, .)) %>%
  mutate_at(vars(c(tr.cool,
                   tr.heat,
                   tr.sleep,
                   tr.diet,
                   cl.nomed,
                   cl.diet,
                   cl.lifest)), ~ ifelse(symptoms_other == "heating and cooling pads, a lot of naps, modified diet during periods of digestion issues", 1, .)) %>%
  mutate_at(vars(c(tr.anac,
                   tr.heat,
                   tr.bdyw,
                   tr.massa,
                   cl.anac,
                   cl.nomed)), ~ ifelse(symptoms_other == "tablets to supress gut acids    massage/heat packs    special body washes", 1, .)) %>%
  mutate_at(vars(c(tr.dexi,
                   tr.nsai,
                   cl.stim,
                   cl.antinf)), ~ ifelse(symptoms_other == "i am medicated with dexamphetamines throughout the day and take anti-inflammatories p.r.n.", 1, .)) %>%
  mutate_at(vars(c(cl.antinf,
                   cl.endoc,
                   tr.levo,
                   tr.hydrco,
                   tr.flrco)), ~ ifelse(symptoms_other == "prescribed medications: levothyroxine, hydrcortisone, fludrocortisone", 1, .)) %>%
  mutate_at(vars(c(tr.phys,
                   cl.nomed)), ~ ifelse(symptoms_other == "physio therapy", 1, .)) %>%
  mutate_at(vars(c(tr.magn,
                   tr.acc,
                   tr.inos,
                   tr.ribo,
                   cl.suppl)), ~ ifelse(symptoms_other == "magnesium, n-acetyl cysteine, inositol for pcos  magnesium, riboflavin for migraines.", 1, .)) %>%
  mutate_at(vars(c(tr.mesal,
                   cl.anac)), ~ ifelse(symptoms_other == "mesalazine", 1, .)) %>%
  mutate_at(vars(c(tr.imsup,
                   tr.cortinj,
                   cl.antinf,
                   cl.imsup)), ~ ifelse(symptoms_other == "have tried different things. cortisone injections and immunosuppressive medicwtion", 1, .)) %>%
  mutate_at(vars(c(tr.metr,
                   tr.hydrx,
                   cl.imsup,
                   cl.imod)), ~ ifelse(symptoms_other == "methotrexate, hydroxychloriquin", 1, .)) %>% 
  mutate_at(vars(c(tr.rita,
                   cl.stim,
                   tr.phen,
                   cl.ahist,
                   tr.propr,
                   cl.heart,
                   tr.effex,
                   cl.psych,
                   tr.micrgy,
                   cl.endoc,
                   tr.modaf,
                   cl.stim,
                   tr.zyrt,
                   cl.ahist,
                   tr.parc,
                   cl.antinf,
                   tr.tapa,
                   cl.opia,
                   tr.colo,
                   cl.stool,
                   tr.celeb,
                   tr.osteo,
                   tr.psych,
                   tr.allexp,
                   cl.nomed,
                   tr.alco,
                   tr.mdma,
                   tr.lsd,
                   tr.keta,
                   tr.petco,
                   cl.recdr)), ~ ifelse(symptoms_other == "do you want the full run down? might as well give you the full run down. i have a rap sheet on hand for new doctors :^p    o	propranalol 40mg twice daily (morn/eve), + 30mg once daily (mid)  o	effexor 37.5mg once daily (morn)  o	microgynon once daily (morn)  o	modafinil 100mg once daily (morn), 50mg once daily (mid)  o	zyrtec three times daily (morn/mid/eve)  o	paracetamol 1330mg twice daily (morn/eve)  o	tapentadol 150mg twice daily (morn/eve)  o	coloxyl once daily (eve)  o	celebrex 200mg once daily (eve)  o	phenergan 25mg once daily (eve)    there are also other modes of treatment: physiotherapy, osteopathy, psychological therapy, controlled dietary intake and exposure to environmental allergens, using heat packs and electric blankets, and memory foam. i also find cuddling my cat helps - thankfully she's hypoallergenic.    and, of course self medication, mostly with alcohol to take the edge off the pain, though that's just deferment with interest. i used to take recreational drugs (mdma, acid, ketamine) as a kind of circuit breaker, but they usually induce trans ischemic attacks these days so i had to stop.     i often use sleep aids to overcome 'painsomnia' (inability to sleep due to, well, pain), usually phenergan and/or restavit.     occasionally i take short acting opiates if my pain is too severe, obtained through both legal and less than legal means, but this is hard to balance with gastrointestinal issues.    ritalin is also very effective at improving executive dysfunction and allowing me to work through pain or other symptoms.", 1, .)) %>%
  mutate_at(vars(c(tr.melox,
                   cl.antinf,
                   tr.phys,
                   cl.nomed,
                   tr.apath,
                   cl.lifest)), ~ ifelse(symptoms_other == "meloxicam, lots of physio, and some of it i just put up with", 1, .)) %>%
  mutate_at(vars(c(tr.mido,
                   cl.vascon,
                   tr.dexi,
                   cl.stim)), ~ ifelse(symptoms_other == "vasoconstrictors (midodrine) and dexamphetamines", 1, .)) %>%
  mutate_at(vars(c(tr.ivig,
                   cl.imod)), ~ ifelse(symptoms_other == "ivig", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.vitb12,
                   cl.suppl,
                   tr.vit,
                   cl.suppl,
                   cl.endoc,
                   tr.preb,
                   tr.prob,
                   tr.apci,
                   tr.cthsu,
                   tr.thyrx,
                   cl.endoc)), ~ ifelse(symptoms_other == "thyroxine and vitamin supplements every day  caruso's thyroid supplement  500mg b1  activated b12  apple cider vinegar  prebiotic & probiotic   gluten free diet  i have used this routine for 3 years now and will not change it. i have had no pain and fatigue since using this combination", 1, .))

df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(cl.suppl,
                   tr.collgn,
                   tr.curc,
                   tr.coq10)), ~ ifelse(symptoms_other == "collagen, curcumin, coq10,", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.sleep,
                   cl.lifest,
                   tr.meds)), ~ ifelse(symptoms_other == "during flare-ups i try to get adequate rest/sleep, do not drive and cancel any social engagements.  ibs/gastroentoritis - ongoing dietary restrictions and medications", 1, .)) %>%
  mutate_at(vars(c(tr.nsai,
                   cl.antinf,
                   tr.esom,
                   cl.anac,
                   tr.domp,
                   cl.antiem,
                   tr.cital,
                   cl.psych)), ~ ifelse(symptoms_other == "anti-inflammatory medication, nexium, motilium, citalopram,", 1, .)) %>%
  mutate_at(vars(c(tr.sleep,
                   cl.lifest,
                   tr.diet,
                   cl.diet,
                   tr.exer,
                   cl.lifest)), ~ ifelse(symptoms_other == "i manage my sleep, diet and exercise as well as i can", 1, .)) %>%
  mutate_at(vars(c(tr.cyclo,
                   cl.imsup)), ~ ifelse(symptoms_other == "cyclosporin eye drops", 1, .)) %>%
  mutate_at(vars(c(tr.hydrx,
                   cl.imod)), ~ ifelse(symptoms_other == "hydroxychloroquine", 1, .)) %>%
  mutate_at(vars(c(tr.fish,
                   cl.suppl,
                   tr.celeb,cl.antinf,
                   tr.hydrx,
                   cl.imod)), ~ ifelse(symptoms_other == "fish oil tablets, celebrex, plaquinal", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(symptoms_other == "have tried cannibis", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   cl.suppl)), ~ ifelse(symptoms_other == "vitamins", 1, .)) %>%
  mutate_at(vars(c(tr.bldth,
                   cl.heart,
                   tr.nitrg,
                   tr.calbl)), ~ ifelse(symptoms_other == "blood thinners, calcium blockers, nitroglycerin patches", 1, .)) %>%
  mutate_at(vars(c(tr.lifest,
                   cl.lifest)), ~ ifelse(symptoms_other == "management strategies.", 1, .)) %>%
  mutate_at(vars(c(tr.exer,
                   cl.lifest,
                   tr.massa,
                   cl.nomed,
                   tr.diet,
                   cl.diet,
                   tr.sleep,
                   tr.eyero,
                   tr.bala,
                   tr.stret)), ~ ifelse(symptoms_other == "exercise, stretching, massage, strict eyecare routine, good diet, good sleep, good work balance.", 1, .)) %>%
  mutate_at(vars(c(tr.ritx,
                   cl.imsup,
                   tr.cycph,
                   cl.imsup)), ~ ifelse(symptoms_other == "failed rituximab and cyclophosphamide.  no other treaty options available for anca+ vasculitis", 1, .)) %>%
  mutate_at(vars(c(tr.magn,
                   cl.suppl,
                   tr.altmed,
                   cl.altpr,
                   tr.pila,
                   cl.lifest)), ~ ifelse(symptoms_other == "alternative treatments, magnesium, pilates", 1, .)) %>%
  mutate_at(vars(c(tr.exer,
                   tr.aqua,
                   cl.lifest)), ~ ifelse(symptoms_other == "exercise walking & aquarobics", 1, .)) %>%
  mutate_at(vars(c(tr.exer,
                   cl.lifest,
                   tr.diet,
                   cl.diet,
                   tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "i pay close attention to my diet and gut health. some foods agravate symptoms so i avoid them. i try to eat a healthy diet. ensure to include food that supports immune system and thyroid. i also take some supplements.  i also try to stay fit and do regular exercise.", 1, .)) %>%
  mutate_at(vars(c(tr.berzm,
                   cl.imod)), ~ ifelse(symptoms_other == "benralizumab: biologic treating hyper-eosinophilia", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.exer,
                   cl.lifest,
                   tr.sleep,
                   tr.alco,
                   cl.recdr,
                   tr.magn,
                   cl.suppl,
                   tr.hydrx,
                   cl.imod,
                   tr.warf,
                   cl.heart,
                   tr.blreg)), ~ ifelse(symptoms_other == "cleaner diet (less aggravating foods), gentle exercise, monitor sleep/rest quality, (alcohol or magnesium), i also now take plaquenil since 2014, and warfarin (i had a stroke in 2011 following pregnancies 3 & 4 ending early (27-28 weeks) with pre/eclampsia & hellp syndrome. the hellp was the worst!!!! worse than my second labour where i selected to go pain med free. hellp was passed off as gas and other digestive issues. it was the worst! in the earlier stages of the illness, i did require immune suppressants, i also take a blood pressure regulating tablet since i was 24, so since 2001.", 1, .)) %>%
  mutate_at(vars(c(tr.warf,
                   cl.heart)), ~ ifelse(symptoms_other == "warfarin", 1, .)) %>%
  mutate_at(vars(c(tr.keta,
                   cl.recdr,
                   tr.ldn,
                   cl.imod)), ~ ifelse(symptoms_other == "ketamine infusions.  ldn (low dose naltrexone)", 1, .)) %>%
  mutate_at(vars(c(tr.hydrx,
                   cl.imod,
                   tr.eyero,
                   cl.lifest,
                   tr.orca,
                   tr.skca,
                   tr.lerca,
                   cl.heart)), ~ ifelse(symptoms_other == "plaquenil   eye lubricants   oral lubricants    lercanidipine hydrochloride   creams and clothing to protect skin from sun", 1, .)) %>%
  mutate_at(vars(c(tr.nsai,
                   cl.antinf)), ~ ifelse(symptoms_other == "anti-inflamatories,", 1, .)) %>%
  mutate_at(vars(c(tr.medit,
                   tr.taich,
                   tr.sleep,
                   tr.exer,
                   cl.lifest,
                   tr.ppi,
                   cl.anac)), ~ ifelse(symptoms_other == "ppi, rest, exercise, meditation, tai chi", 1, .)) %>%
  mutate_at(vars(c( tr.diet,
                    cl.diet)), ~ ifelse(symptoms_other == "very strict gluten free diet", 1, .)) %>%
  mutate_at(vars(c(tr.exer,
                   cl.lifest,
                   tr.diet,
                   cl.diet,
                   tr.massa,
                   cl.nomed,
                   tr.vit,
                   cl.suppl,
                   tr.mndfl,
                   tr.chiro,
                   tr.chmed,
                   cl.altpr,
                   tr.accp)), ~ ifelse(symptoms_other == "diet, exercise, mindfulness , massage , chiropractor, chineese medicine , acupuncture , vitamins", 1, .)) %>%
  mutate_at(vars(c(tr.strav,
                   tr.diet,
                   cl.diet,
                   tr.exer,
                   cl.lifest)), ~ ifelse(symptoms_other == "exercise and diet and stress management", 1, .)) %>%
  mutate_at(vars(c(tr.cortinj,
                   cl.antinf,
                   tr.bldth,
                   cl.heart,
                   tr.cortst,
                   tr.adep,
                   cl.psych)), ~ ifelse(symptoms_other == "cortisone injections, antidepressants,  blood thinners, steroids", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   cl.suppl)), ~ ifelse(symptoms_other == "vitamin supplements", 1, .)) %>%
  mutate_at(vars(c(tr.bihor,
                   cl.endoc,
                   tr.vit,
                   cl.suppl,
                   tr.mins,
                   tr.ldn,
                   cl.imod)), ~ ifelse(symptoms_other == "vitamins and trace minerals, low dose naltrexone-ldn, bio-identical hormones", 1, .)) %>%
  mutate_at(vars(c(tr.myla,
                   cl.anac,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "mylanta. avoid foods i'm intolerant to.", 1, .)) %>%
  mutate_at(vars(c(tr.dmard,
                   cl.dmard,
                   tr.hydrx,
                   cl.imod)), ~ ifelse(symptoms_other == "dmard- plaquenil", 1, .)) %>%
  mutate_at(vars(c(tr.parc,
                   cl.antinf,
                   tr.exer,
                   cl.lifest)), ~ ifelse(symptoms_other == "exercise and changing position to relieve pain panadol osteo x3 per day", 1, .))

df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(tr.eyero,
                   cl.lifest,
                   tr.orca)), ~ ifelse(symptoms_other == "for eyes refresh plus eye drops, dry mouth toothpaste.", 1, .)) %>%
  mutate_at(vars(c(tr.strav,
                   tr.wrkna,
                   cl.lifest,
                   tr.lifest,
                   tr.phys,
                   tr.massa,
                   cl.nomed)), ~ ifelse(symptoms_other == "physiotherapy and regular massage.  managing lifestyle - moved house, stopped working professionally, limit stress and manage triggers (heat, exercise etc).", 1, .)) %>%
  mutate_at(vars(c(tr.ahist,
                   cl.ahist,
                   tr.hydrx,
                   cl.imod,
                   tr.propr,
                   cl.heart,
                   tr.esom,
                   cl.anac,
                   tr.vit,
                   tr.fish,
                   tr.magn,
                   cl.suppl,
                   tr.levo,
                   cl.endoc,
                   tr.rivxb,
                   tr.valac,
                   tr.prox,
                   cl.psych,
                   tr.flucz,
                   cl.afung,
                   tr.prmx,
                   cl.dopag,
                   tr.gaba,
                   cl.neurp,
                   cl.avir,
                   tr.clonz,
                   cl.diaz,
                   tr.mpred,
                   cl.antinf)), ~ ifelse(symptoms_other == "other medications eg xarelto, plaquenil, valaciclovir sodium valproate, paroxetine, fluconazole, apo-propranolol, sifrol, neurontin, eltroxin, clonazepam, methylprednisolone, nexium, multi vitamins, calcium, pro biopics, fish oil, magnesium, anti- histamine (for urticaria)", 1, .)) %>%
  mutate_at(vars(c(tr.massa,
                   cl.nomed)), ~ ifelse(symptoms_other == "deep tissue massages help settle the pain.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "autoimmune protocol diet", 1, .)) %>%
  mutate_at(vars(c(tr.pace,
                   tr.gard,
                   tr.sleep,
                   cl.lifest)), ~ ifelse(symptoms_other == "pacing activities when necessary. getting adequate rest. gardening in the shade.", 1, .)) %>%
  mutate_at(vars(c(tr.natr,
                   cl.altpr,
                   tr.cmzl,
                   cl.endoc,
                   tr.iron,
                   tr.vitb12,
                   tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "a naturopath prescribed supplement regime which is amended and tailored every quarter. also have recently being prescribed carbimazole for thyroid. i have also required b12 shots and intensive iron supplements.", 1, .)) %>%
  mutate_at(vars(c(tr.minox,
                   cl.heart,
                   tr.hydrx,
                   cl.imod)), ~ ifelse(symptoms_other == "plaeuqunil. minixiidol caps", 1, .)) %>%
  mutate_at(vars(c(tr.thyrx,
                   cl.endoc)), ~ ifelse(symptoms_other == "thyroxine", 1, .)) %>%
  mutate_at(vars(c(tr.ritx,
                   cl.imsup)), ~ ifelse(symptoms_other == "biologic infusion (rituximab)", 1, .)) %>%
  mutate_at(vars(c(tr.ahist,
                   cl.ahist)), ~ ifelse(symptoms_other == "antihistamines", 1, .)) %>%
  mutate_at(vars(c(tr.stcetr,
                   cl.surg,
                   tr.essoi,
                   cl.lifest,
                   tr.massa,
                   cl.nomed)), ~ ifelse(symptoms_other == "stem cell transplant  complementary therapies eg. massage, essential oils", 1, .)) %>%
  mutate_at(vars(c(tr.vitd,
                   tr.iron,
                   tr.hydrx,
                   cl.imod,
                   tr.parc,
                   tr.zostx,
                   tr.zinc,
                   cl.suppl,
                   tr.aspr,
                   cl.antinf,
                   tr.pntoz,
                   cl.anac,
                   tr.ovest,
                   cl.nomed,
                   tr.talad,
                   tr.opsum,
                   tr.pregab,
                   cl.neurp,
                   tr.acei,
                   cl.heart,
                   tr.sprlc,
                   tr.fusem,
                   cl.diur,
                   tr.eztim,
                   cl.cholai,
                   tr.creon,
                   tr.preds,
                   cl.antinf,
                   tr.alpur,
                   cl.enzy)), ~ ifelse(symptoms_other == "allopurinol - gout, prednisone - acute gout, creon - pancreas, ezetrol - cholesterol, frusemide and spiractin - fluid retention with pulmonary arterial hypertension, lisinopril - hypertension, lyrica - nerve pain, opsumit and tadalafil - pulmonary arterial hypertension, osteomol - osteoarthritis, ovestin vaginal cream - utis and dry vagina, pantoprazole - indigestion/reflux, plaquenil - inflammation, pain, aspirin - cholesterol, iron - anaemia, vitamin d - vitamin d deficiency, zinc - zinc deficiency, zostrix (capsacain cream) lower back and knee pain,", 1, .)) %>%
  mutate_at(vars(c(tr.dymi,
                   cl.antinf,
                   tr.fexo,
                   cl.ahist)), ~ ifelse(symptoms_other == "dymista 125/50 nasal spray twice per day; fexofenadine hydrochloride 180mg hayfever tablets 2-3 per day.", 1, .)) %>%
  mutate_at(vars(c(tr.levo,
                   cl.endoc,
                   tr.hydrx,
                   cl.imod,
                   tr.cortst,
                   cl.antinf)), ~ ifelse(symptoms_other == "oroxine  hydroxychloroquine  corticosteroid", 1, .)) %>%
  mutate_at(vars(c(tr.heat,
                   cl.nomed)), ~ ifelse(symptoms_other == "heat packs on joints. but with most of my joints affected sometimes a very hot shower several times a day.", 1, .)) %>%
  mutate_at(vars(c(tr.iginj,
                   cl.imod,
                   tr.bose,
                   cl.heart,
                   tr.hydrx,
                   tr.domp,
                   cl.antiem,
                   tr.esom,
                   tr.pntoz,
                   cl.anac)), ~ ifelse(symptoms_other == "weekly immunoglobulin injections due to recurrent infections.  bosentan for pulmonary hypertension   plaquenil   motilium, nexium and somac for digestive issues.", 1, .)) %>%
  mutate_at(vars(c(tr.exer,
                   cl.lifest,
                   tr.stret)), ~ ifelse(symptoms_other == "mild exercises & stretching", 1, .)) %>%
  mutate_at(vars(c(tr.rest,
                   cl.lifest,
                   tr.wrkp)), ~ ifelse(symptoms_other == "rest where possible, only work 3 days per week", 1, .)) %>%
  mutate_at(vars(c(tr.strav,
                   cl.lifest,
                   tr.phys,
                   cl.nomed,
                   tr.walk,
                   tr.heat,
                   tr.rest)), ~ ifelse(symptoms_other == "rest, thermal pool,physiotherapy, walking , physiotherapy exercises, reducing stress. listen to what my body is telling me.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.cmzl,
                   cl.endoc)), ~ ifelse(symptoms_other == "coeliac's disease is managed by diet.  grave's disease is managed by carbimazole.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "diet has stopped seizures and significantly improved cognitive function", 1, .)) %>%
  mutate_at(vars(c(tr.omep,
                   cl.anac)), ~ ifelse(symptoms_other == "loser for reflux", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "no grain diet has helped with some pain joint issues", 1, .)) %>%
  mutate_at(vars(c(tr.nifd,
                   tr.sild,
                   cl.heart)), ~ ifelse(symptoms_other == "drugs that open my blood vessels such as niphetopine and viagra", 1, .)) %>%
  mutate_at(vars(c(tr.bact,
                   cl.abiot,
                   tr.itrcz,
                   cl.afung)), ~ ifelse(symptoms_other == "bactrim and itraconozol", 1, .)) %>%
  mutate_at(vars(c(tr.fish,
                   cl.suppl)), ~ ifelse(symptoms_other == "fish oil for clicking joints", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(symptoms_other == "i help to control my pain and fatigue and general quality of life with cannabis oil taken orally.", 1, .)) %>%
  mutate_at(vars(c(tr.adep,
                   cl.psych,
                   tr.pill,
                   cl.endoc)), ~ ifelse(symptoms_other == "anti-depressants and the pill", 1, .)) %>%
  mutate_at(vars(c(tr.hmeo,
                   tr.natr,
                   cl.altpr)), ~ ifelse(symptoms_other == "homeopathic, naturopathic, integrated approach", 1, .)) %>%
  mutate_at(vars(c(tr.propr,
                   cl.heart,
                   tr.adep,
                   cl.psych,
                   tr.osteo,
                   cl.nomed,
                   tr.phys,
                   tr.accp,
                   tr.massa,
                   tr.heat,
                   tr.cool,
                   tr.botx,
                   cl.mpara,
                   tr.galcz,
                   cl.imod,
                   tr.thc,
                   cl.recdr,
                   tr.cbd)), ~ ifelse(symptoms_other == "botox, emgality, propanolol, ssri, osteo, thc/cbd oil, physio, acupuncture, massage, heat and ice", 1, .))

df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(tr.hydrco,
                   cl.antinf,
                   tr.flrco)), ~ ifelse(symptoms_other == "corticosteroids specifically hydrocortisone (hysone) & fludrocortisone", 1, .)) %>%
  mutate_at(vars(c(tr.hydrco,
                   cl.antinf)), ~ ifelse(symptoms_other == "hydrocortisone", 1, .)) %>%
  mutate_at(vars(c(tr.hydrco,
                   cl.antinf)), ~ ifelse(symptoms_other == "hydrocortisone morning and mid afternoon daily.", 1, .)) %>%
  mutate_at(vars(c(tr.cortst,
                   cl.antinf)), ~ ifelse(symptoms_other == "daily doses of cortisons", 1, .)) %>%
  mutate_at(vars(c(tr.hydrco,
                   cl.antinf,
                   tr.flrco)), ~ ifelse(symptoms_other == "steroids, hydrocortisone and fludrocortisone", 1, .)) %>%
  mutate_at(vars(c(tr.surg,
                   cl.surg)), ~ ifelse(symptoms_other == "surgery", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "diet", 1, .)) %>%
  mutate_at(vars(c(tr.nasl,
                   cl.lifest,
                   tr.orca,
                   tr.skca,
                   tr.skca,
                   tr.lifest,
                   tr.rest,
                   tr.heat,
                   cl.nomed)), ~ ifelse(symptoms_other == "rest, heat packs, management of personal time and activities. moisture relievers for mouth, nose and skin.", 1, .)) %>%
  mutate_at(vars(c(tr.ntzl,
                   cl.imod)), ~ ifelse(symptoms_other == "tysabri", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "i've tried a number of dietary options and natural supplements.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "dietary - no gluten for coealiac and avoid/minimise nightshade foods for hashimoto's.", 1, .)) %>%
  mutate_at(vars(c(tr.herb,
                   cl.suppl)), ~ ifelse(symptoms_other == "herbal remedies", 1, .)) %>%
  mutate_at(vars(c(tr.parc,
                   cl.antinf)), ~ ifelse(symptoms_other == "i haven't been offered any treatment nor been advised how to manage pain and fatigue so i take the occasional dose of paracetamol and grit my teeth.", 1, .)) %>%
  mutate_at(vars(c(tr.phys,
                   cl.nomed)), ~ ifelse(symptoms_other == "physio", 1, .)) %>%
  mutate_at(vars(c(tr.heat,
                   cl.nomed,
                   tr.tocrt,
                   cl.antinf,
                   tr.diet,
                   cl.diet,
                   tr.lifest,
                   cl.lifest)), ~ ifelse(symptoms_other == "heat-based treatments, steroidal ointment for the skin problems, diet-based changes to avoid severe gut issues, modifying how i live my life to avoid pushing my body(/allowing the world to push my body) into a flare.", 1, .)) %>%
  mutate_at(vars(c(tr.cortst,
                   cl.antinf)), ~ ifelse(symptoms_other == "corticosteroids", 1, .)) %>%
  mutate_at(vars(c(tr.yoga,
                   cl.lifest)), ~ ifelse(symptoms_other == "yoga and exercise", 1, .)) %>%
  mutate_at(vars(c(tr.mntlk,
                   tr.fexo,
                   cl.ahist,
                   tr.cetrz)), ~ ifelse(symptoms_other == "allergy medication- montrelukast and antihistamines (fexofenadine and cetirizine)    also tried omalizumab injections but they weren't effective in managing symptoms.", 1, .)) %>%
  # ^ has omazilumab on there as a past, ineffective medication. Add in if needed
  mutate_at(vars(c(tr.surg,
                   cl.surg)), ~ ifelse(symptoms_other == "breast cancer- mastectomy   skin cancer - excision, surgery", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "gluten free diet", 1, .)) %>%
  mutate_at(vars(c(tr.lifest,
                   tr.pace,
                   cl.lifest)), ~ ifelse(symptoms_other == "doing less", 1, .)) %>%
  mutate_at(vars(c(tr.pace,
                   cl.lifest)), ~ ifelse(symptoms_other == "pacing", 1, .)) %>%
  mutate_at(vars(c(tr.melat,
                   cl.endoc)), ~ ifelse(symptoms_other == "melatonin tablets.", 1, .)) %>%
  mutate_at(vars(c(tr.rest,
                   cl.lifest,
                   tr.thc,
                   cl.recdr)), ~ ifelse(symptoms_other == "rest and self medicating with thc", 1, .)) %>%
  mutate_at(vars(c(tr.yoga,
                   cl.lifest,
                   tr.walk,
                   tr.pila,
                   tr.stret)), ~ ifelse(symptoms_other == "walking, stretching, yoga and pilates", 1, .)) %>%
  mutate_at(vars(c(tr.mndfl,
                   cl.lifest,
                   tr.lifest,
                   tr.pace)), ~ ifelse(symptoms_other == "i participated in a chronic conditions course through macquarie university which taught me coping skills, mindfulness, underdoing, overdoing, pacing etc", 1, .)) %>%
  mutate_at(vars(c(tr.massa,
                   cl.nomed,
                   tr.heat,
                   tr.magn,
                   cl.suppl,
                   tr.pace,
                   cl.lifest,
                   tr.ldn,
                   cl.imod,
                   tr.amtrp,
                   cl.psych)), ~ ifelse(symptoms_other == "massage, electric heat pad, magnesium, pacing (trying to live within energy envelope and not doing more than i can manage without payback), low dose naltrexone, low dose amitriptyline.", 1, .)) %>%
  mutate_at(vars(c(tr.betbl,
                   cl.heart)), ~ ifelse(symptoms_other == "beta blocker. prescribed for ectopic heart beats. fixed my migraines (mostly).", 1, .)) %>%
  mutate_at(vars(c(tr.whlch,
                   cl.nomed,
                   tr.lifest,
                   cl.lifest,
                   tr.elctr,
                   tr.vit,
                   cl.suppl,
                   tr.ahist,
                   cl.ahist,
                   tr.bed,
                   cl.nomed)), ~ ifelse(symptoms_other == "various medications to manage my mast cell activation syndrome (eg anti-histamines, etc)  various medications to supplement vitamins that are regularly depleted by mcas    to manage the me/cfs, learning how to stay below our anaerobic threshold, to avoid further harm.    avoid or reduce being upright, so that there is less stress on our bodies.  drinking lots of electrolytes fluids to compensate for the reduced blood flow.  use equipment (eg wheelchairs) to preserve the very limited supply of mitochondrial energy.", 1, .)) %>%
  mutate_at(vars(c(tr.suppl,
                   tr.vit,
                   cl.suppl,
                   tr.ldn,
                   cl.imod,
                   tr.pace,
                   cl.lifest)), ~ ifelse(symptoms_other == "vitamins and supplements, ldn and pacing", 1, .)) %>%
  mutate_at(vars(c(tr.cbd,
                   cl.recdr,
                   tr.thc,
                   tr.vitb12,
                   cl.suppl,
                   tr.vitd,
                   cl.suppl,
                   tr.ldn,
                   cl.imod,
                   tr.clonz,
                   cl.diaz,
                   tr.mclo)), ~ ifelse(symptoms_other == "clonezapam, moclobemide, lamotrigine, low dose naltrexone, b12, vit d,  spectrum yellow (cbd oil) spectrum blue (a small amount of thc included with cbd)", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.mins,
                   tr.vit,
                   cl.suppl,
                   tr.smmp,
                   cl.nomed)), ~ ifelse(symptoms_other == "self managed somatic movement program. low salycilate diet. no commercially prepared foods  vitamin and mineral supplements.", 1, .)) %>%
  mutate_at(vars(c(tr.dulox,
                   cl.neurp,
                   tr.endp)), ~ ifelse(symptoms_other == "cymbalta and endep.", 1, .)) %>%
  mutate_at(vars(c(tr.cortst,
                   cl.antinf)), ~ ifelse(symptoms_other == "replacement doses of corticosteroids", 1, .)) %>%
  mutate_at(vars(c(tr.pace,
                   cl.lifest,
                   tr.ldn,
                   cl.imod,
                   tr.peas,
                   cl.suppl)), ~ ifelse(symptoms_other == "low dose naltrexone  pacing  palmitoylethanolamide", 1, .)) %>%
  mutate_at(vars(c(tr.bath,
                   cl.nomed,
                   tr.meds)), ~ ifelse(symptoms_other == "prescription medications for management of dysautonomia symptoms, epsom salt baths", 1, .)) %>%
  mutate_at(vars(c(tr.meds)), ~ ifelse(symptoms_other == "prescription medication", 1, .)) %>%
  mutate_at(vars(c(tr.meds)), ~ ifelse(symptoms_other == "medication for thyroid" |
                                         symptoms_other ==  "medication taken regularly." |
                                         symptoms_other ==  "medications for pbc and digestive problems and raynauds" |
                                         symptoms_other == "medicated" |
                                         symptoms_other == "medication" |
                                         symptoms_other == "other medications" |
                                         symptoms_other == "symptomatic medication (not immunosuppressants)" |
                                         symptoms_other == "several medications" |
                                         symptoms_other == "have trialled numerous medications to help manage conditions, most have been unable to be tolerated.  still in trial process under guidance of specialists and gp", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "low histamine diet, avoid artificial preservatives, additives  colours and flavours. now avoiding sprays on fresh food. probably sulphite based.", 1, .)) %>%
  mutate_at(vars(c(tr.splpl,
                   cl.diaz,
                   tr.suppl,
                   cl.suppl,
                   tr.diet,
                   cl.diet,
                   tr.medit,
                   cl.lifest,
                   tr.rest,
                   tr.chmed,
                   cl.altpr,
                   cl.nomed,
                   tr.accp,
                   tr.mxbst)), ~ ifelse(symptoms_other == "supplements, good nutrition, meditation, rest, chinese herbal medicine, moxa, accupuncture, sleeping tablets", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "avoiding wheat", 1, .)) %>%
  mutate_at(vars(c(tr.medit,
                   cl.lifest,
                   tr.alpur,
                   cl.enzy,
                   tr.dmard,
                   cl.dmard,
                   tr.ldn,
                   cl.imod,
                   tr.hydrx,
                   cl.imod)), ~ ifelse(symptoms_other == "allopurinol and dhea for impact of immune inflammation, ldn for general muscle pain/myalgia, hydroxychloroquine for joint pain,     have used ondansetron for concentration loss when it was milder which helped me return to work at the time.     was on immunosuppressants for a year when igan was very bad    vedic meditation", 1, .)) %>%
  #TODO previous medication listed above
  mutate_at(vars(c(tr.abiot,
                   cl.abiot,
                   tr.afung,
                   cl.afung,
                   tr.vit,
                   cl.suppl)), ~ ifelse(symptoms_other == "vitamin injection, antifungals, antibiotics", 1, .))

df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(tr.pace,
                   cl.lifest,
                   tr.exer,
                   tr.myth,
                   cl.nomed,
                   tr.osteo,
                   tr.vit,
                   cl.suppl)), ~ ifelse(symptoms_other == "vitamin supplements, regular appointments with osteopath and myotherapist, at-home gentle exercise, pacing myself", 1, .)) %>%
  mutate_at(vars(c(tr.bath,
                   cl.nomed,
                   tr.diet,
                   cl.diet,
                   tr.exer,
                   cl.lifest)), ~ ifelse(symptoms_other == "exercise, diet, hot baths", 1, .)) %>%
  mutate_at(vars(c(tr.cbd,
                   cl.recdr)), ~ ifelse(symptoms_other == "cbd oil; no thc", 1, .)) %>%
  mutate_at(vars(c(tr.tocrt,
                   cl.antinf)), ~ ifelse(symptoms_other == "daivobet topical ointment", 1, .)) %>%
  mutate_at(vars(c(tr.ldn,
                   cl.imod,
                   tr.cortst,
                   cl.antinf,
                   tr.adep,
                   cl.psych)), ~ ifelse(symptoms_other == "corticosteroids   low dose naltrexone  antidepressants", 1, .)) %>%
  mutate_at(vars(c(tr.ldn,
                   cl.imod,
                   tr.clchn,
                   tr.preds,
                   cl.antinf,
                   tr.arprz,
                   cl.psych)), ~ ifelse(symptoms_other == "low dose naltrexone. colchicine (anti-inflammatory) steroids (prednisone). low dose abilify (experimental treatment).", 1, .)) %>%
  mutate_at(vars(c(tr.effex,
                   tr.vanst,
                   cl.nomed,
                   tr.suppl,
                   cl.suppl,
                   tr.flrco,
                   cl.antinf,
                   tr.qtpn,
                   cl.psych,
                   tr.lmtrg,
                   tr.mtfr,
                   cl.enzy,
                   tr.trcrms)), ~ ifelse(symptoms_other == "effexor, lamictal, seroquel, florinef, metformin,  rtranscranial magnetic stimulation, supplements, vagal nerve stimulation", 1, .)) %>%
  mutate_at(vars(c(tr.pace,
                   cl.lifest,
                   tr.cobt,
                   cl.psych)), ~ ifelse(symptoms_other == "pacing /cbt", 1, .)) %>%
  mutate_at(vars(c(tr.adep,
                   cl.psych)), ~ ifelse(symptoms_other == "antidepressants which help with pain.", 1, .)) %>%
  mutate_at(vars(c(tr.ivig,
                   cl.imod,
                   tr.fish,
                   cl.suppl,
                   tr.ldn,
                   tr.curc)), ~ ifelse(symptoms_other == "ivig infusions  ldn  turmeric and fish oil", 1, .)) %>%
  mutate_at(vars(c(tr.mido,
                   cl.vascon,
                   tr.pyrdo,
                   cl.mstm)), ~ ifelse(symptoms_other == "mestinon and midodrine", 1, .)) %>%
  #TODO below is about past treatments
  #mutate_at(vars(c(  )), ~ ifelse(symptoms_other == "have tried a variety of treatments (prescription) but no effect. have tried stimulants (prescribed) even though they're not the best for me/cfs, side effects were no good.", 1, .)) %>%
  mutate_at(vars(c(tr.magn,
                   cl.suppl,
                   tr.coq10,
                   tr.zinc,
                   tr.p5p)), ~ ifelse(symptoms_other == "magnesium. co q10, zinc & p5p", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   cl.suppl,
                   tr.fish)), ~ ifelse(symptoms_other == "vitamins, fish oils", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   cl.suppl,
                   tr.magn,
                   tr.mits)), ~ ifelse(symptoms_other == "multivitamin/mitochondrial supplement (kpax pharmaceuticals), magnesium", 1, .)) %>%
  mutate_at(vars(c(tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "supplements, iv infusions", 1, .)) %>%
  #TODO don't know what to classify IV infusions as. nutirents? POTS? IgG?
  mutate_at(vars(c(tr.heat,
                   cl.nomed,
                   tr.cbd,
                   cl.recdr,
                   tr.thc,
                   tr.alco,
                   tr.stret,
                   cl.lifest)), ~ ifelse(symptoms_other == "heat packs, cbd oil, alcohol, edibles, stretching", 1, .)) %>%
  mutate_at(vars(c(tr.cmpst,
                   cl.lifest,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "dietary restrictions, compression stockings", 1, .)) %>%
  mutate_at(vars(c(tr.pscph,
                   cl.psych)), ~ ifelse(symptoms_other == "psycho active medication prescribed to me", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "try to eat very light and easy to digest foods during ibs flare ups (like steamed veges, frutis and soups).", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.cool,
                   cl.nomed,
                   tr.heat)), ~ ifelse(symptoms_other == "heat and ice, anti-inflammatory diet.", 1, .)) %>%
  mutate_at(vars(c(tr.mndfl,
                   cl.lifest)), ~ ifelse(symptoms_other == "mind-body techniques", 1, .)) %>%
  mutate_at(vars(c(tr.rest,
                   cl.lifest,
                   tr.pace,
                   tr.sleep,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "rest and not overdo things, good nights sleep and eat healthily", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.yoga,
                   cl.lifest)), ~ ifelse(symptoms_other == "yoga four times a week and switching to a healthier diet with more plant based proteins and fish and vegetables also helps.", 1, .)) %>%
  mutate_at(vars(c(tr.drndl,
                   cl.altpr,
                   tr.cbd,
                   cl.recdr)), ~ ifelse(symptoms_other == "dry needling and cbd oil", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.exer,
                   cl.lifest,
                   tr.sleep)), ~ ifelse(symptoms_other == "diet and exercise and sleep", 1, .)) %>%
  mutate_at(vars(c(tr.strav,
                   cl.lifest,
                   tr.diet,
                   cl.diet,
                   tr.detx,
                   cl.altpr)), ~ ifelse(symptoms_other == "i have found by reducing my stress & toxic load and improving nutrition i feel a lot a better than i used to", 1, .)) %>%
  mutate_at(vars(c(tr.massa,
                   cl.nomed,
                   tr.bath,
                   tr.preds,
                   cl.antinf,
                   tr.heat,
                   tr.msccr)), ~ ifelse(symptoms_other == "wheat bags, massages per month, hot baths/showers, muscle relief creams (physiocream, doterra ice blue), occasional prednisolone", 1, .)) %>%
  mutate_at(vars(c(tr.ivig,
                   cl.imod,
                   tr.rivxb,
                   cl.heart,
                   tr.cortst,
                   cl.antinf,
                   cl.bldpr,
                   cl.surg,
                   tr.splnec,
                   tr.gcsf,
                   tr.rmplo,
                   tr.bldtr,
                   tr.trax)), ~ ifelse(symptoms_other == "ivig, splenectomy, rituximab, gcsf, romiplostim, blood and platelet transfusions, steroids   (none effective as far as we can tell)    also transexemic acid.", 1, .)) %>%
  mutate_at(vars(c(cl.lifest,
                   tr.trav,
  )), ~ ifelse(symptoms_other == "avoidance of triggers", 1, .))

df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(tr.cbd,
                   cl.recdr)), ~ ifelse(symptoms_other == "cbd oil", 1, .)) %>%
  mutate_at(vars(c(tr.massa,
                   cl.nomed,
                   tr.exer,
                   cl.lifest,
                   tr.medit,
                   tr.heat,
                   tr.drkhm)), ~ ifelse(symptoms_other == "massage, light exercise, meditation, heat packs, dark humour :)", 1, .)) %>%
  mutate_at(vars(c(tr.ivig,
                   cl.imod)), ~ ifelse(symptoms_other == "immunoglobulin / henzendra  was on predizone", 1, .)) %>%
  #TODO prev med above 
  mutate_at(vars(c(tr.trax,
                   cl.bldpr,
                   tr.c1in,
                   cl.heart)), ~ ifelse(symptoms_other == "txa, c1 inhibitors", 1, .)) %>%
  mutate_at(vars(c(tr.surg,
                   cl.surg,
                   tr.meds)), ~ ifelse(symptoms_other == "when all other treatments failed (some immediately, some after a period of time) surgery put me into remission with the itp. the hidradenitis suppurativa is sometimes controlled by medications to a degree. iv antibiotics seemed to help a bit when i was in hospital for a surgery.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.strav,
                   cl.lifest,
                   tr.sleep,
                   cl.lifest,
                   tr.heat,
                   cl.nomed)), ~ ifelse(symptoms_other == "food and stress monitoring   sleep and naps  heat pack", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.smth,
                   cl.stool)), ~ ifelse(symptoms_other == "diet restrictions, de-gas and other over the counter stomach relief medication when that might help.", 1, .)) %>%
  mutate_at(vars(c(tr.ivig,
                   cl.imod,
                   cl.vascon,
                   tr.vasc)), ~ ifelse(symptoms_other == "ivig, vasoconstrictor,", 1, .)) %>%
  mutate_at(vars(c(tr.medit,
                   cl.lifest,
                   tr.stret,
                   tr.vit,
                   cl.suppl,
                   tr.suppl,
                   tr.aubuf,
                   tr.resr,
                   cl.altpr)), ~ ifelse(symptoms_other == "mediation, gentle stretches, australian bush flower essences, vitamins and supplements, rescue remedy.", 1, .)) %>%
  mutate_at(vars(c(tr.phys,
                   cl.nomed,
                   tr.massa)), ~ ifelse(symptoms_other == "physiotherapy  remedial massage", 1, .)) %>%
  mutate_at(vars(c(tr.magn,
                   cl.suppl,
                   tr.yoga,
                   cl.lifest,
                   tr.medit,
                   tr.relax)), ~ ifelse(symptoms_other == "magnesium cream/spray+ oral supplement daily  yoga/ meditation / relaxation", 1, .)) %>%
  mutate_at(vars(c(tr.shyp,
                   cl.altpr,
                   tr.medit,
                   cl.lifest,
                   tr.strav)), ~ ifelse(symptoms_other == "pain management by self-hypnosis (the gut centre, melbourne), meditation and anti stress such as cognitive training.", 1, .)) %>%
  mutate_at(vars(c(cl.suppl,
                   tr.bflvt,
                   cl.altpr,
                   tr.kins,
                   tr.mthf)), ~ ifelse(symptoms_other == "beef liver tablets have been  the most affective as well as mthf supplement taken after i started seeing my kinesioligist a few years ago", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "strict gluten free diet.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "diet- coeliac disease  supplements to help energy levels etc", 1, .)) %>%
  mutate_at(vars(c(tr.ahist,
                   cl.ahist,
                   tr.vitb12,
                   cl.suppl,
                   tr.p5p)), ~ ifelse(symptoms_other == "b12 & b6 and antihistamines", 1, .)) %>%
  mutate_at(vars(c(tr.ldn,
                   cl.imod,
                   tr.vascon,
                   cl.vascon,
                   tr.stim,
                   cl.stim,
                   tr.betbl,
                   cl.heart,
                   tr.pace,
                   cl.lifest)), ~ ifelse(symptoms_other == "immune modulator - low dose naltrexone, vasopressors and beta blockers for pots, stimulants for concentration, pacing", 1, .)) %>%
  mutate_at(vars(c(tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "supplements, i.e. for digestion support and energy", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.anac,
                   cl.anac,
                   tr.water,
                   cl.lifest,
                   tr.levo,
                   cl.endoc)), ~ ifelse(symptoms_other == "for coeliac i have kept to the diet strictly. if i accidently ingest gluten i immediately have chest pains (gentle not too bad) i take chewable ant acid tablets and go to bed for about a half hour.  i drink alot of water to help too.  and for under active thyroid i take one tablet everyday and have been for at least 10 years and i have no problem.", 1, .)) %>%
  mutate_at(vars(c(tr.cool,
                   cl.nomed,
                   tr.rest,
                   cl.lifest)), ~ ifelse(symptoms_other == "icing and rest.", 1, .)) %>%
  mutate_at(vars(c(tr.phys,
                   cl.nomed,
                   tr.cmpst,
                   cl.lifest,
                   tr.pace)), ~ ifelse(symptoms_other == "physio  exercise  compression   pacing", 1, .)) %>%
  #mutate_at(vars(c(  )), ~ ifelse(symptoms_other == "medication administered via intravenous once a month", 1, .)) %>%
  #not sure what meds to log there
  mutate_at(vars(c(tr.valac,
                   cl.avir,
                   tr.escip,
                   cl.psych,
                   tr.melat,
                   cl.endoc)), ~ ifelse(symptoms_other == "valtrex, lexapro and melatonin", 1, .)) %>%
  mutate_at(vars(c(tr.keta,
                   cl.recdr,
                   tr.medit,
                   cl.lifest,
                   tr.meds)), ~ ifelse(symptoms_other == "ketamine infusions   meditation  pain medication", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.meds)), ~ ifelse(symptoms_other == "gluten free diet for coeluac disease.  medication for graves disease", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   cl.suppl,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "vitamins and healthy diet", 1, .)) %>%
  mutate_at(vars(c(tr.phys,
                   cl.nomed,
                   tr.ldn,
                   cl.imod,
                   tr.vit,
                   cl.suppl,
                   cl.suppl,
                   tr.rest,
                   cl.lifest,
                   tr.meds)), ~ ifelse(symptoms_other == "physiotherapy, low dose naltrexone, a variety of compounded medications, many different vitamins and supplements, lots of rest", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   cl.suppl,
                   tr.mins,
                   tr.medit,
                   cl.lifest,
                   cl.nomed,
                   tr.brthe,
                   tr.swm,
                   tr.cool,
                   tr.psych)), ~ ifelse(symptoms_other == "supplements of vitamins, minerals  meditation   deep breathing   swimming  cold showers  psychological therapy", 1, .)) %>%
  mutate_at(vars(c(tr.rest,
                   cl.lifest,
                   tr.phys,
                   cl.nomed)), ~ ifelse(symptoms_other == "doctors don't give a crap, even after formal diagnosis. physio and lots of bed rest because no gp on this planet takes me seriously.", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   cl.suppl,
                   tr.pace,
                   cl.lifest,
                   tr.progn,
                   cl.endoc)), ~ ifelse(symptoms_other == "pacing, progesterone, vitamins", 1, .)) %>%
  mutate_at(vars(c(tr.dexi,
                   cl.stim,
                   tr.meds)), ~ ifelse(symptoms_other == "i have a diagnosis of adhd for which i am on medication. my dosage is extremely high (30mg/day dexamphetamine) and at least part of that is because my adhd symptoms are compounded by other issues which i mentally assign to an autoimmune origin", 1, .)) %>%
  mutate_at(vars(c(tr.drib,
                   cl.suppl,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "d-ribose supplements diet", 1, .)) %>%
  mutate_at(vars(c(tr.ivig,
                   cl.imod,
                   tr.pyrdo,
                   cl.mstm)), ~ ifelse(symptoms_other == "ivig and mestinon", 1, .)) %>%
  mutate_at(vars(c(tr.ldn,
                   cl.imod)), ~ ifelse(symptoms_other == "low dose naltrexone", 1, .)) %>%
  mutate_at(vars(c(tr.ldn,
                   cl.imod)), ~ ifelse(symptoms_other == "low dose naltrexone for pain and immune modulation  effects", 1, .)) %>%
  mutate_at(vars(c(tr.exer,
                   cl.lifest,
                   tr.osteo,
                   cl.nomed,
                   tr.accp,
                   tr.trav,
                   tr.nutr,
                   tr.suppl,
                   cl.suppl,
                   tr.thyrx,
                   cl.endoc)), ~ ifelse(symptoms_other == "* pain management - exercise physiology,  osteo   * malaise & other issues - accupuncture & osteo  * allergies, gut issues, inflammation management - avoidance of allergens, nutritionist   * nutritional supplements - as recommended by specialist  * endocrinologist - thyroxine   * specialist - currently trialing different drugs for me/cfs symptom management", 1, .)) %>%
  mutate_at(vars(c(tr.suppl,
                   cl.suppl,
                   tr.cbd,
                   cl.recdr,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "cbd oil and natural holistic oils and supplements to help me sleep and support my liver health etc along with strict dietary habits such as plant based health foods no alcohol or high sugary foods etc", 1, .)) %>%
  mutate_at(vars(c(tr.stim,
                   cl.stim)), ~ ifelse(symptoms_other == "i self medicated with illegal stimulants on and off", 1, .))

df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(tr.massa,
                   cl.nomed)), ~ ifelse(symptoms_other == "remedial massage", 1, .)) %>%
  mutate_at(vars(c(tr.sulfz,
                   cl.dmard)), ~ ifelse(symptoms_other == "sulfasalazine - allergic to everything else", 1, .)) %>%
  mutate_at(vars(c(tr.salt,
                   cl.diet,
                   tr.water,
                   cl.lifest,
                   tr.exer,
                   tr.diet)), ~ ifelse(symptoms_other == "salt, water, exercise (running to strengthen calf muscles to improve blood flow during postural changes), stopped drinking alcohol, eating small low carb meals,have used midodrine in past", 1, .)) %>%
  #TODO previous treatment included above
  mutate_at(vars(c(tr.suppl,
                   cl.suppl,
                   tr.yoga,
                   cl.lifest,
                   tr.phys,
                   cl.nomed,
                   tr.medit,
                   tr.cana,
                   cl.recdr,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "iv supplements and long list of things like physio, yoga, meditation, diet and medicinal cannabis", 1, .)) %>%
  mutate_at(vars(c(tr.cortst,
                   cl.antinf,
                   tr.meds)), ~ ifelse(symptoms_other == "other medications - can include steroids", 1, .)) %>%
  mutate_at(vars(c(tr.flrco,
                   cl.antinf)), ~ ifelse(symptoms_other == "fludrocortisone", 1, .)) %>%
  mutate_at(vars(c(tr.adep,
                   cl.psych,
                   tr.dexi,
                   cl.stim)), ~ ifelse(symptoms_other == "anti depressants. dexamphetamine", 1, .)) %>%
  mutate_at(vars(c(tr.brbon,
                   cl.diet,
                   tr.zinc,
                   tr.magn,
                   tr.vitd,
                   tr.prob,
                   cl.suppl,
                   tr.curc,
                   tr.vit,
                   tr.salt,
                   tr.licr,
                   tr.om3,
                   tr.gibi,
                   tr.lima,
                   tr.glcsm,
                   tr.chndr,
                   tr.msm,
                   tr.pqq,
                   tr.clcm,
                   tr.vitc)), ~ ifelse(symptoms_other == "bone broth  supplements; licorice root, curcumin, omega 3, ginkgo biloba, multivitamins, himalayan pink salt, lion's mane,  glucosamine, chondroitin, msm, pqq, calcium, magnesium, zinc, d3, c vitamin, probiotics.", 1, .)) %>%
  mutate_at(vars(c(tr.at1,
                   cl.heart,
                   tr.salt,
                   cl.diet,
                   tr.water,
                   cl.lifest,
                   tr.exer,
                   tr.flrco,
                   cl.antinf,
                   tr.ldn,
                   cl.imod,
                   tr.diet,
                   tr.cmpst,
                   tr.dexi,
                   cl.stim,
                   tr.post)), ~ ifelse(symptoms_other == "sartan at night, raised head of bed, salt consumption, fluids, postural, movements, fludrocortisone, low dose naltrexone, low histamine diet, compression clothing, postural 'cheats' when standing, sitting with legs raised or on floor, leaning body onto legs to squash, dexamphetamine for brain fog", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "plant based diet: symptoms have reduced dramatically along with reductions in crp and calproctectin levels", 1, .)) %>%
  mutate_at(vars(c(tr.vitd,
                   cl.suppl,
                   tr.magn,
                   tr.cupr)), ~ ifelse(symptoms_other == "vit d, magnesium, starting cusack protocol to see if it helps", 1, .)) %>%
  mutate_at(vars(c(tr.massa,
                   cl.nomed,
                   tr.splnt,
                   cl.lifest,
                   tr.tens,
                   tr.ems)), ~ ifelse(symptoms_other == "massage,tens, ems, bracing/splinting", 1, .)) %>%
  mutate_at(vars(c(tr.exer,
                   cl.lifest,
                   tr.psych,
                   cl.nomed,
                   tr.suppl,
                   cl.suppl,
                   tr.medit,
                   tr.rest,
                   tr.hydt,
                   tr.herb,
                   cl.altpr)), ~ ifelse(symptoms_other == "exercise, hydrotherapy, psychotherapy (focused on reducing tms), herbs and supplements, meditation, rest", 1, .)) %>%
  mutate_at(vars(c(tr.antinf,
                   cl.antinf)), ~ ifelse(symptoms_other == "antiinflammatories", 1, .)) %>%
  mutate_at(vars(c(tr.herb,
                   cl.suppl,
                   tr.mins,
                   tr.vit,
                   tr.ldn,
                   cl.imod,
                   cl.psych,
                   tr.dxpn,
                   tr.dsvx)), ~ ifelse(symptoms_other == "herbs, vitamins, minerals, ldn, sinnequan, pristiq", 1, .)) %>%
  mutate_at(vars(c(tr.ldn,
                   cl.imod,
                   tr.diet,
                   cl.diet,
                   cl.nomed,
                   tr.nrdvt)), ~ ifelse(symptoms_other == "diet, ndt and low dose naltrexone", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "diet (carnivore) has worked wonders for the pain and fatigue..... gone from a 'i want to kill myself as soon as i wake - to a yes this hurts but we can get through it pain.", 1, .)) %>%
  mutate_at(vars(c(tr.dmard,
                   cl.dmard,
                   tr.adep,
                   tr.adep)), ~ ifelse(symptoms_other == "dmards, tricyclic antidepressant", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.rest,
                   cl.lifest)), ~ ifelse(symptoms_other == "rest , coeliac diet", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.suppl,
                   cl.suppl,
                   tr.pace,
                   cl.lifest,
                   tr.meds)), ~ ifelse(symptoms_other == "medications, supplements, pacing, diet", 1, .)) %>%
  mutate_at(vars(c(cl.nomed,
                   tr.mbai,
                   tr.phys,
                   tr.exer,
                   cl.lifest)), ~ ifelse(symptoms_other == "use of mobility aids/slings etc, physio-approved exercise regime.", 1, .)) %>%
  mutate_at(vars(c(tr.amtrp,
                   cl.psych,
                   tr.propr,
                   cl.heart,
                   tr.qtpn,
                   cl.stim,
                   tr.ephd,
                   tr.sumt,
                   cl.vascon)), ~ ifelse(symptoms_other == "medicines- amitriptyline, propranolol, seroquel, ephedrine, imigran", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.om3,
                   cl.suppl,
                   tr.vitb12,
                   tr.vitd,
                   tr.vitb7,
                   tr.ntre,
                   cl.lifest,
                   tr.swm,
                   cl.nomed)), ~ ifelse(symptoms_other == "'grounding' (sleeping in a tent on the ground/ocean swimming)  outdoor sleeping not in a tent  extremely careful diet, includes many veg and also meat  supplemented zinc, vit d, vit e, b vitamins, biotin, atp fuel, omega 3, occasional other trace minerals  fresh air outside the city  trees", 1, .)) %>%
  mutate_at(vars(c(tr.medca,
                   cl.recdr,
                   tr.meds)), ~ ifelse(symptoms_other == "medical cannabis with occasional prescription pain relief", 1, .)) %>%
   mutate_at(vars(c(tr.tens,
                   cl.nomed,
                   tr.massa,
                   tr.heat,
                   tr.magn,
                   cl.suppl,
                   tr.vitd,
                   tr.vitb12)), ~ ifelse(symptoms_other == "tens machine   remedial massage   heat packs   vitd d b12 magnesium", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.eyero,
                   cl.lifest)), ~ ifelse(symptoms_other == "gluten free diet. saline eye drops.", 1, .)) %>%
  mutate_at(vars(c(tr.sleep,
                   cl.lifest,
                   tr.rest)), ~ ifelse(symptoms_other == "sleep and rest alot", 1, .))

df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(cl.lifest,
                   tr.rest,
                   tr.elctr,
                   cl.suppl,
                   tr.aspr,
                   cl.antinf,
                   tr.endp,
                   cl.neurp,
                   tr.rztrp,
                   cl.vascon,
                   tr.ibup,
                   cl.antinf,
                   tr.magn,
                   tr.iron)), ~ ifelse(symptoms_other == "rest, electrolytes, medication to prevent and relieve migraines (low dose endep and maxalt wafers, respectively), aspirin and ibuprofen when needed, magnesium and iron supplements", 1, .)) %>%
  mutate_at(vars(c(tr.ahist2,
                   tr.ahist1,
                   cl.ahist)), ~ ifelse(symptoms_other == "h1's + h2's", 1, .)) %>%
  mutate_at(vars(c(tr.exer,
                   cl.lifest,
                   tr.mndfl)), ~ ifelse(symptoms_other == "exercise   mindfulness", 1, .)) %>%
  mutate_at(vars(c(tr.pace,
                   cl.lifest,
                   tr.massa,
                   cl.nomed,
                   tr.myth,
                   tr.psych)), ~ ifelse(symptoms_other == "pacing / not using too much energy  regular massage/myotherapy  all the psychological techniques science says are good to manage chronic pain", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   cl.suppl,
                   tr.fish,
                   tr.curc,
                   tr.glcsm,
                   tr.heat,
                   cl.nomed)), ~ ifelse(symptoms_other == "heat therapy and daily vitamins  fish oil 1000mg, turmeric 30,000mg and glucosamine 1500mg", 1, .)) %>%
  mutate_at(vars(c(tr.massa,
                   cl.nomed,
                   tr.osteo,
                   tr.myth,
                   tr.hmeo,
                   cl.altpr,
                   tr.diet,
                   cl.diet,
                   tr.cobt,
                   cl.psych,
                   tr.emdr)), ~ ifelse(symptoms_other == "osteopathy, remedial massage therapy (myotherapy), homeopathy, psychological counselling (cbt and emdr therapy), dietician.", 1, .)) %>%
  mutate_at(vars(c(tr.strav,
                   cl.lifest,
                   tr.pace,
                   cl.diaz,
                   tr.diaz)), ~ ifelse(symptoms_other == "stress management (as much as possible), reduced activity, valium", 1, .)) %>%
  mutate_at(vars(c(tr.mido,
                   cl.vascon,
                   tr.lifest,
                   cl.lifest)), ~ ifelse(symptoms_other == "midodrine, lifestyle changes", 1, .)) %>%
  mutate_at(vars(c(tr.ldn,
                   cl.imod,
                   tr.pace,
                   cl.lifest,
                   tr.prob,
                   cl.suppl,
                   tr.ivab,
                   cl.heart)), ~ ifelse(symptoms_other == "low dose naltrexone   ivabradine  strong probiotics  pacing", 1, .)) %>%
  mutate_at(vars(c(tr.gltm,
                   tr.glyc,
                   tr.chrc,
                   tr.mlbd,
                   tr.slnem,
                   tr.bthy,
                   tr.acc,
                   cl.suppl,
                   tr.zinc,
                   tr.magn)), ~ ifelse(symptoms_other == "an expensive and complicated cocktail of nutritional supplements keeps me functioning. without them i would be bedridden. including: glutamine, glycine, nac, charcoal, molybdenum, selenium, zinc, magnesium, betaine hcl.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "following a gluten free diet", 1, .)) %>%
  mutate_at(vars(c(tr.skca,
                   cl.lifest,
                   tr.vitd,
                   cl.suppl,
                   tr.iron)), ~ ifelse(symptoms_other == "take hormone replacement for the 2 conditions itself, but don't regularly take anything for symptoms other than use moisturizers on skin.  when required supplements such as iron and vit d.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.exer,
                   cl.lifest,
                   tr.meds)), ~ ifelse(symptoms_other == "medications (i'm told i'm not immunosuppressed.)  diet  exercise", 1, .)) %>%
  mutate_at(vars(c(tr.hcrtp,
                   cl.antinf)), ~ ifelse(symptoms_other == "cortisone pump", 1, .)) %>%
  mutate_at(vars(c(tr.reiki,
                   cl.altpr,
                   tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "reiki, supplements", 1, .)) %>%
  mutate_at(vars(c(tr.heat,
                   cl.nomed,
                   tr.medit,
                   cl.lifest,
                   tr.yoga,
                   tr.spmba)), ~ ifelse(symptoms_other == "pretty much anything i can think of - hot water bottles, spikey balls, yoga, meditation, anything.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.lifest,
                   cl.lifest,
                   tr.exer)), ~ ifelse(symptoms_other == "lifestyle modification - adaptable exercise (running and yoga), plant based diet with minimal processed foods", 1, .)) %>%
  mutate_at(vars(c(tr.strav,
                   cl.lifest)), ~ ifelse(symptoms_other == "breaking up scarred fascia through various means, stress reduction methods", 1, .)) %>%
  mutate_at(vars(c(tr.dmard,
                   cl.dmard,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "dmards, gluten free diet for coeliac", 1, .)) %>%
  mutate_at(vars(c(tr.mesal,
                   cl.anac)), ~ ifelse(symptoms_other == "mesalazine medication", 1, .)) %>%
  mutate_at(vars(c(tr.rest,
                   cl.lifest)), ~ ifelse(symptoms_other == "rest", 1, .)) %>%
  mutate_at(vars(c(tr.cpap,
                   cl.nomed)), ~ ifelse(symptoms_other == "cpap therapy", 1, .)) %>%
  mutate_at(vars(c(tr.botx,
                   cl.mpara,
                   tr.blcfn,
                   cl.msrlx,
                   tr.myth,
                   cl.nomed,
                   tr.propr,
                   cl.heart,
                   tr.surg,
                   cl.surg,
                   tr.meds)), ~ ifelse(symptoms_other == "botox, baclofen, myotherapy, propranolol, surgery for svt, inhalor, other medications", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.tocrt,
                   cl.antinf,
                   cl.endoc,
                   tr.tpest)), ~ ifelse(symptoms_other == "celiac disease: strict diet to avoid gluten.   lichen sclerosis: steroid ointment and topical estrogen", 1, .)) %>%
  mutate_at(vars(c(cl.antinf,
                   tr.tnfi)), ~ ifelse(symptoms_other == "tnf blockers", 1, .)) %>%
  mutate_at(vars(c(tr.phys,
                   cl.nomed,
                   tr.accp,
                   tr.bwth,
                   cl.altpr)), ~ ifelse(symptoms_other == "physio- bowen therapy /acupuncture", 1, .)) %>%
  mutate_at(vars(c(tr.ashw,
                   cl.suppl,
                   tr.vitd,
                   tr.magn,
                   tr.rhro,
                   tr.ciqu)), ~ ifelse(symptoms_other == "so many supplements 😄 currently on ashwaghanda, rhodiola rosea, cissus quarangularis, vitamin d and magnesium.", 1, .)) %>%
  mutate_at(vars(c(tr.orca,
                   cl.lifest,
                   tr.water,
                   tr.eyero)), ~ ifelse(symptoms_other == "eye drops, chewing gum, sip water", 1, .)) %>%
  mutate_at(vars(c(tr.celeb,
                   cl.antinf,
                   tr.peas,
                   cl.suppl,
                   tr.fish,
                   tr.vitd,
                   tr.phen,
                   cl.ahist,
                   tr.eyero,
                   cl.lifest)), ~ ifelse(symptoms_other == "celebrex, palmitoylethanolamide, fish oil, vitamin d, genteal gel eye drops, poly visc eye ointment, phenergan", 1, .)) %>%
  mutate_at(vars(c(tr.vitd,
                   cl.suppl)), ~ ifelse(symptoms_other == "vitamin d", 1, .)) %>%
  mutate_at(vars(c(tr.walk,
                   cl.lifest,
                   tr.yoga,
                   tr.pila,
                   tr.phys,
                   cl.nomed,
                   tr.exer,
                   tr.massa)), ~ ifelse(symptoms_other == "gentle exercise (walking, pilates, yoga), physio, massage", 1, .)) %>%
  mutate_at(vars(c(tr.exer,
                   cl.lifest,
                   tr.sleep,
                   tr.insln,
                   tr.sugr,
                   cl.endoc,
                   cl.diet)), ~ ifelse(symptoms_other == "insulin, sleep, sugar, exercise.", 1, .)) %>%
  mutate_at(vars(c(tr.sild,
                   cl.heart,
                   tr.metr,
                   cl.imsup,
                   tr.amtrp,
                   cl.psych,
                   tr.vitb9,
                   cl.suppl)), ~ ifelse(symptoms_other == "sildenafil, methotrexate, amitriptyline and folic acid.", 1, .)) %>%
  mutate_at(vars(c(tr.amtrp,
                   cl.psych,
                   tr.flrco,
                   cl.antinf,
                   tr.propr,
                   cl.heart,
                   tr.thc,
                   cl.recdr,
                   tr.melat,
                   cl.endoc,
                   tr.cbd)), ~ ifelse(symptoms_other == "head pain/pressure treated with amitriptyline (prescribed by gp),  dizziness and lightheadedness treated with florinef (prescribed by cardiologist)  fight or flight/panic attacks treated with propranolol (prescribed by gp)  cbd/thc and melatonin for sleep (prescribed by gp)", 1, .)) %>%
  mutate_at(vars(c(tr.alco,
                   cl.recdr,
                   tr.cana)), ~ ifelse(symptoms_other == "alcohol, marijuana", 1, .)) %>%
  mutate_at(vars(c(tr.imth,
                   cl.imod)), ~ ifelse(symptoms_other == "immunotherapy", 1, .)) %>%
  mutate_at(vars(c(tr.ahist2,
                   tr.ahist1,
                   cl.ahist,
                   cl.imod,
                   tr.mcst)), ~ ifelse(symptoms_other == "antihistamines h1 h2 and mast cell stabilisers", 1, .)) %>%
  mutate_at(vars(c(tr.dmard,
                   cl.dmard,
                   tr.preds,
                   cl.antinf,
                   tr.neurp,
                   cl.neurp)), ~ ifelse(symptoms_other == "neuropathic pain medicine  dmards  prednisone", 1, .)) %>%
  mutate_at(vars(c(tr.thyrx,
                   cl.endoc,
                   tr.flrco,
                   cl.antinf,
                   tr.hydrco)), ~ ifelse(symptoms_other == "hydrocortisone, fludrocortisone, thyroxine", 1, .)) %>%
  mutate_at(vars(c(tr.ldn,
                   cl.imod,
                   tr.dulox,
                   cl.neurp,
                   tr.coq10,
                   cl.suppl,
                   tr.magn,
                   tr.vitb12,
                   tr.essoi,
                   cl.lifest,
                   tr.bath,
                   cl.nomed,
                   tr.accp,
                   tr.medit,
                   tr.phys,
                   tr.pace,
                   tr.massa,
                   tr.sleep)), ~ ifelse(symptoms_other == "ldn, duluxotine, naltrexone, supplements (magnesium, coq10, b, immune and relax), bath salts, essential oils, remedial massage, acupuncture, physio (pre me/cfs), pacing, hrm, sleep tracking, meditation", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.pace,
                   cl.lifest,
                   tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "symptom contingent pacing.  strict diet.  nutritional supplements.  (note that my immunosuppressants are not \\\\\\\\\\\\\\for my me/cfs. i take one for ulcerative colitis and one for asthma; i have been offered humira for the enthesitis, but i have skin cancers, so i manage without.)", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   cl.suppl,
                   tr.bath,
                   cl.nomed)), ~ ifelse(symptoms_other == "natural remedies (magnesium baths, vitamins)", 1, .))

#Include drug_other_other
df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "occasional marijuana for pain relief and assistance sleeping.", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "marijuana", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr,
                   de.smok)), ~ ifelse(drug_other_other == "infrequently smoke cannabis", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "cannabis", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr,
                   de.edib)), ~ ifelse(drug_other_other == "marijuana cookies.", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr,
                   de.oil)), ~ ifelse(drug_other_other == "oil very rarely", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr,
                   de.oil)), ~ ifelse(drug_other_other == "sometime marajuana oil to help sleep", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr,
                   cl.endoc,
                   tr.testo)), ~ ifelse(drug_other_other == "i don't regularly but have used canabis and testosterone in the past", 1, .)) %>%
  mutate_at(vars(c(tr.cbd,
                   cl.recdr,
                   de.oil,
                   tr.thc)), ~ ifelse(drug_other_other == "cbd oil, cbd with tcd", 1, .)) %>%
  mutate_at(vars(c(tr.cbd,
                   cl.recdr,
                   de.oil,
                   tr.thc)), ~ ifelse(drug_other_other == "cbd/thc oil", 1, .)) %>%
  mutate_at(vars(c(tr.alco,
                   cl.recdr)), ~ ifelse(drug_other_other == "alcohol.", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "marijauna", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "marijuanas", 1, .)) %>%
  mutate_at(vars(c(tr.cbd,
                   cl.recdr,
                   de.oil)), ~ ifelse(drug_other_other == "cbd oil", 1, .)) %>%
  mutate_at(vars(c(tr.thc,
                   cl.recdr)), ~ ifelse(drug_other_other == "thc", 1, .)) %>%
  mutate_at(vars(c(tr.cbd,
                   cl.recdr)), ~ ifelse(drug_other_other == "cbd", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr,
                   de.oil,
                   tr.cbd)), ~ ifelse(drug_other_other == "weed, cbd oil", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "occasionally smoke weed to help sleep   it calms my mind, and takes the edge of the pain", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "i have tried marijuana. it helped, but i cannot take it legally and drive so haven't used in years.", 1, .)) %>%
  mutate_at(vars(c(tr.alco,
                   cl.recdr)), ~ ifelse(drug_other_other == "etoh", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   de.oil,
                   tr.medca)), ~ ifelse(drug_other_other == "no longer illicit- canaboid oil under the care of my gp", 1, .)) %>%
  mutate_at(vars(c(tr.medca,
                   cl.recdr)), ~ ifelse(drug_other_other == "i take medicinal cannabis, which is legal - maybe doesn't belong in this section?", 1, .)) %>%
  mutate_at(vars(c(tr.stim,
                   cl.stim)), ~ ifelse(drug_other_other == "the onset of my symptoms was after g.f it triggered me to sleep in a coma-like sleep for 23hrs a day for 8 looong yrs. i was planning my death as i couldn't function at all. so i tried illegal stimulants since my dr refused any other treatment and diagnosis apart from stress", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "cannabis sometimes", 1, .)) %>%
  mutate_at(vars(c(tr.cbd,
                   cl.recdr,
                   de.oil,
                   tr.thc)), ~ ifelse(drug_other_other == "cbd/thc oil - helps with difficulty sleeping and pain.", 1, .)) %>%
  mutate_at(vars(c(tr.thc,
                   cl.recdr,
                   de.edib)), ~ ifelse(drug_other_other == "occasional use of thc honey at night to help break through pain", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "occasional marijuana", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   tr.cbd,
                   cl.recdr)), ~ ifelse(drug_other_other == "cbd rich cannabis", 1, .)) %>%
  mutate_at(vars(c(tr.psylo,
                   cl.recdr)), ~ ifelse(drug_other_other == "psilocybin", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   tr.alco,
                   tr.keta,
                   tr.dexi,
                   cl.stim,
                   tr.coca)), ~ ifelse(drug_other_other == "alcohol, ketamine, cocaine, illicit dexamphetamine. stimulants help with fatigue and concentration issues. alcohol and ketamine for enjoyment and to support mental health", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "weed for migraine pain.", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   tr.alco,
                   tr.cbd,
                   de.edib)), ~ ifelse(drug_other_other == "alcohol, cbd edibles", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "very occasionally marijuana for pain", 1, .))

##################
#
#Addition 1-22  
# 
#
######################


df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(cl.avir,
                   tr.acyc,
                   tr.vitb12,
                   cl.suppl,
                   tr.vitd,
                   tr.om3)), ~ ifelse(symptoms_other == "antiviral (aciclovir), omega-3 fatty acids, b12, d3", 1, .)) %>%
  mutate_at(vars(c(tr.modaf,
                   cl.stim,
                   tr.ldn,
                   cl.imod)), ~ ifelse(symptoms_other == "low dose naltroxen, modafinil", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.suppl,
                   cl.suppl,
                   tr.vitd,
                   tr.vitc,
                   tr.lima,
                   tr.fish,
                   tr.colos,
                   cl.psych,
                   tr.sert)), ~ ifelse(symptoms_other == "keto, intermittent fast(20:4 -18:6), unique trace minerals, fish oil, vit d, vit c, lions maine, colostrum, zoloft", 1, .)) %>%
  mutate_at(vars(c(tr.pregab,
                   cl.neurp)), ~ ifelse(symptoms_other == "lireca", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "change of diet avoiding foods that trigger pain or inflammation", 1, .)) %>%
  mutate_at(vars(c(tr.preds,
                   cl.antinf,
                   tr.ritx,
                   cl.imsup)), ~ ifelse(symptoms_other == "rituximab infusions, prednisone ,", 1, .)) %>%
  mutate_at(vars(c(tr.preds,
                   cl.antinf,
                   tr.bact,
                   cl.abiot,
                   cl.tyki,
                   tr.nint)), ~ ifelse(symptoms_other == "prednisolone 5mg daily, bactrim m,w & f,  nintedanib(ofev) 150mg twice a day - for pulmonary fibrosis due to wegeners", 1, .)) %>%
  mutate_at(vars(c(tr.altmed,
                   cl.altpr)), ~ ifelse(symptoms_other == "alternative natural remedies.", 1, .)) %>%
  mutate_at(vars(c(tr.gaba,
                   cl.neurp,
                   tr.exer,
                   cl.lifest,
                   tr.cool,
                   cl.nomed,
                   tr.swri,
                   tr.pace,
                   tr.rest,
                   tr.lstm)), ~ ifelse(symptoms_other == "dont like to use pain relief medication if i can avoid it due to having one kidney. but i will if i have to.    use 300mg of gabapentin daily as when not using heat in soles worse.    walk in cold water to take heat out of hot soles. feels great and normal.    don't push myself past pain threshold. if i do i am completely knocked out the next day. its like you pay a price for trying to use brain over matter.    if vision is slightly blurry, i check bp (often high) and rest for the day -dont drive. or use the eyes - just listen to radio    usually do 20mins of physical work then rest for 5 to 10 mins.    prior to diagnosis could walk 15km no problem. for 1st year went from 400m to 800m. 2nd year able to do up to 3kms.    i accept when exhausted and give in and sleep 2 to 4 hrs    - suffer weeks of ulcers. tried every chemist drug - not much relief. found salt washes and gargling somewhat effective.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "i follow a keto diet to almost eliminate heartburn, inflammation and digestive issues", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "i do try different diets", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.medca,
                   cl.recdr,
                   tr.bath,
                   cl.nomed)), ~ ifelse(symptoms_other == "anti-inflammatory diet, acupuncture, magnesium salt baths, medicinal cannabis ( quest initiative )", 1, .)) %>%
  mutate_at(vars(c(tr.magn,
                   cl.suppl,
                   tr.eyero,
                   cl.lifest,
                   tr.orca,
                   tr.detc,
                   tr.heat,
                   cl.nomed,
                   tr.xylm)), ~ ifelse(symptoms_other == "eye drops, heat packs to the eyes.  frequent dental visits,implants,tooth mousse plus,dental trays,water pik. biotene mouth spray, dry mouth lozenges xylimelts etc etc  occasional magnesium.", 1, .)) %>%
  mutate_at(vars(c(tr.cortst,
                   cl.antinf)), ~ ifelse(symptoms_other == "steroids", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.exer,
                   cl.lifest,
                   tr.vit,
                   cl.suppl)), ~ ifelse(symptoms_other == "exercise, vitamins and diet", 1, .)) %>%
  mutate_at(vars(c(tr.vbin,
                   tr.irin,
                   cl.suppl,
                   tr.suppl,
                   tr.eyero,
                   cl.lifest,
                   tr.meds)), ~ ifelse(symptoms_other == "medication to stop blood vessel spasms due to raynaud's, medication for restless leg, medication for reflux, inflammation throughout the body, medication for motility through the digestive tract, medication for bowel issues, supplements for skin issues, medicated drops for the eyes, b12 injections, iron infusions.", 1, .)) %>%
  mutate_at(vars(c(tr.ivig,
                   cl.imod,
                   tr.afung,
                   cl.afung)), ~ ifelse(symptoms_other == "ivig  antifungals", 1, .)) %>%
  mutate_at(vars(c(tr.chmed,
                   cl.altpr,
                   tr.suppl,
                   cl.suppl,
                   tr.elctr,
                   cl.nomed,
                   tr.hypb)), ~ ifelse(symptoms_other == "chinese herbal medicine, supplements, electrolytes, hyperbaric oxygen therapy", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "avoid certain foods, preferentially eat foods known to be easy to digest.", 1, .)) %>% #TODO checked up to here, line 296
  mutate_at(vars(c(tr.phys,
                   cl.nomed,
                   tr.exer,
                   cl.lifest,
                   tr.ems,
                   tr.heat,
                   tr.brthe)), ~ ifelse(symptoms_other == "physcio, exercise, heat pack, electrostim, breathing exercises", 1, .)) %>%
  mutate_at(vars(c(cl.thrag,
                   tr.etrm)), ~ ifelse(symptoms_other == "eltromobopag", 1, .)) %>%
  mutate_at(vars(c(cl.antinf,
                   tr.cortinj)), ~ ifelse(symptoms_other == "steroid injections", 1, .)) %>%
  mutate_at(vars(c(tr.rest,
                   cl.lifest,
                   tr.etrm,
                   cl.thrag)), ~ ifelse(symptoms_other == "tpora - eltrombopag, rest", 1, .)) %>%
  mutate_at(vars(c(tr.medit,
                   cl.lifest)), ~ ifelse(symptoms_other == "after pain clinic i also use meditation", 1, .)) %>%
  mutate_at(vars(c(tr.ivig,
                   cl.imod,
                   tr.cortst,
                   cl.antinf)), ~ ifelse(symptoms_other == "cortisone   immunoglobulin infusions monthly", 1, .)) %>%
  mutate_at(vars(c(tr.hydrx,
                   cl.imod)), ~ ifelse(symptoms_other == "plaquenil", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.ems,
                   cl.nomed,
                   tr.massa,
                   tr.exer,
                   cl.lifest,
                   tr.rest)), ~ ifelse(symptoms_other == "diet, exercise, massage, emt machine, meditation, rest and naps.", 1, .)) %>%
  mutate_at(vars(c(tr.heat,
                   cl.nomed)), ~ ifelse(symptoms_other == "heat, hot water, heat packs.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.alco,
                   cl.recdr)), ~ ifelse(symptoms_other == "try to eat healthy.   alcohol.", 1, .)) %>%
  mutate_at(vars(c(tr.exer,
                   cl.lifest,
                   cl.nomed,
                   tr.mthl)), ~ ifelse(symptoms_other == "exercise,  mental health", 1, .)) %>%
  mutate_at(vars(c(tr.ems,
                   cl.nomed,
                   tr.heat,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "diet, ems device, heat packs", 1, .)) %>%
  mutate_at(vars(c(tr.massa,
                   cl.nomed,
                   tr.osteo)), ~ ifelse(symptoms_other == "massage  osteopathy", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr,
                   de.smok,
                   de.oil)), ~ ifelse(symptoms_other == "cannabis oil, balms & smoked/vaporised buds or oil", 1, .)) %>%
  mutate_at(vars(c(tr.cortst,
                   cl.antinf,
                   tr.adep,
                   cl.psych)), ~ ifelse(symptoms_other == "steroids  antidepressants used as pain modifiers", 1, .)) %>%
  mutate_at(vars(c(tr.ivig,
                   cl.imod)), ~ ifelse(symptoms_other == "privagen infusions", 1, .)) %>%
  mutate_at(vars(c(tr.amac,
                   cl.suppl,
                   tr.magn)), ~ ifelse(symptoms_other == "amino acids, mineral balancing e/g mag chloride injections", 1, .)) %>%
  mutate_at(vars(c(tr.ahist,
                   cl.ahist)), ~ ifelse(symptoms_other == "antihistamine for itchy skin", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.lifest,
                   cl.lifest,
                   tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "diet  supplements  lifestyle management", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "avoiding gluten containing foods", 1, .)) %>%
  mutate_at(vars(c(tr.levo,
                   cl.endoc,
                   cl.lifest,
                   tr.resn)), ~ ifelse(symptoms_other == "im on 50mg of ltburoxine   and ive learnt to have to live with the pain", 1, .)) %>%
  mutate_at(vars(c(tr.magn,
                   cl.suppl)), ~ ifelse(symptoms_other == "magnesium", 1, .)) %>%
  mutate_at(vars(c(tr.tocrt,
                   cl.antinf)), ~ ifelse(symptoms_other == "topical steroids", 1, .)) %>%
  mutate_at(vars(c(tr.dexi,
                   cl.stim)), ~ ifelse(symptoms_other == "dexamfetamine to combat fatigue so i can work.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.tocrt,
                   cl.antinf,
                   tr.eyero,
                   cl.lifest,
                   tr.thyrx,
                   cl.endoc)), ~ ifelse(symptoms_other == "coeliac disease - gluten free diet  psoriasis - steroid cream and lotion  sjogrens - eye drops  hashimotos - thyroxine", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.vitd,
                   tr.vitb12,
                   tr.iron,
                   cl.suppl)), ~ ifelse(symptoms_other == "gluten free diet and vitamin supplements when needed eg low iron, low b12 or vitamin d", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "gluten-free diet", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.exer,
                   cl.lifest)), ~ ifelse(symptoms_other == "coeliac disease modified by gf diet.  exercise everyday  mediterranean diet", 1, .)) %>%
  mutate_at(vars(c(tr.thyrx,
                   cl.endoc)), ~ ifelse(symptoms_other == "thyroxine replacement", 1, .))

df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.thyrx,
                   cl.endoc,
                   tr.insln)), ~ ifelse(symptoms_other == "insulin via pump  avoiding gluten in food  thyroxine", 1, .)) %>%
  mutate_at(vars(c(tr.ppi,
                   cl.anac,
                   tr.anac,
                   tr.fibr,
                   cl.suppl)), ~ ifelse(symptoms_other == "take daily a prescribed proton pump inhibitor, and metamucil.  gaviscon as needed", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "diet - gluten free", 1, .)) %>%
  mutate_at(vars(c(tr.natr,
                   cl.altpr,
                   tr.herb,
                   cl.suppl)), ~ ifelse(symptoms_other == "using herbal teas and naturopathic remedies", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.fibr,
                   cl.anac)), ~ ifelse(symptoms_other == "gluten free diet  mezalasine for colitis", 1, .)) %>%
  mutate_at(vars(c(tr.meds)), ~ ifelse(symptoms_other == "medications for bone loss and allergies", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "no symptoms except iron deficiency, manage with gluten free diet", 1, .)) %>%
  mutate_at(vars(c(tr.antinf,
                   cl.antinf,
                   tr.exer,
                   cl.lifest,
                   tr.rest,
                   tr.laxa,
                   cl.nomed)), ~ ifelse(symptoms_other == "anti inflammatory pills, specific exercises, rest, laxatives", 1, .)) %>%
  mutate_at(vars(c(tr.iron,
                   cl.suppl,
                   tr.anac,
                   cl.anac,
                   tr.antiem,
                   cl.antiem)), ~ ifelse(symptoms_other == "anti nausea tablets or antacids when needed. iron supplements.", 1, .)) %>%
  mutate_at(vars(c(tr.cortst,
                   cl.antinf,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "gluten free diet, however i also required medication (entocort) to finally achieve a near normal biopsy more than 4 years after diagnosis.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.chiro,
                   cl.nomed,
                   tr.pila,
                   cl.lifest)), ~ ifelse(symptoms_other == "diet  regular chiropractic visit  pilates", 1, .)) %>%
  mutate_at(vars(c(tr.vitd,
                   cl.suppl,
                   tr.magn,
                   tr.salt,
                   cl.diet,
                   tr.diet,
                   tr.water,
                   tr.diet)), ~ ifelse(symptoms_other == "limiting sugar intake and alcohol.  salt and rehydration mixes for orthostatic intolerance - really helps manage fatigue and muscle issues  magnesium - helps bowel regularity and reduces muscle ache and weakness  vitamin d as i'm deficient and have osteopenia", 1, .)) %>%
  mutate_at(vars(c(cl.psych,
                   tr.prchz,
                   tr.melox,
                   cl.antinf)), ~ ifelse(symptoms_other == "stemitil for vomitting, mobic for joint pain and inflam.", 1, .)) %>%
  mutate_at(vars(c(cl.diet,
                   tr.diet)), ~ ifelse(symptoms_other == "don't eat wheat", 1, .)) %>%
  mutate_at(vars(c(cl.diet,
                   tr.diet)), ~ ifelse(symptoms_other == "strict adherence to a gluten-free diet.", 1, .)) %>%
  mutate_at(vars(c(cl.diet,
                   tr.diet)), ~ ifelse(symptoms_other == "strictly gluten free diet.", 1, .)) %>%
  mutate_at(vars(c(cl.diet,
                   tr.diet)), ~ ifelse(symptoms_other == "avoid gluten", 1, .)) %>%
  mutate_at(vars(c(cl.diet,
                   tr.diet)), ~ ifelse(symptoms_other == "my coeliac disease is controlled with a very strict gluten free diet.  symptoms i experienced prior to diagnosis disappeared once i commenced this strict diet.", 1, .)) %>%
  mutate_at(vars(c(cl.diet,
                   tr.diet)), ~ ifelse(symptoms_other == "gluten free diet.", 1, .)) %>%
  mutate_at(vars(c(cl.diet,
                   tr.diet)), ~ ifelse(symptoms_other == "diet - coeliac disease", 1, .)) %>%
  mutate_at(vars(c(cl.diet,
                   tr.diet)), ~ ifelse(symptoms_other == "dietary management for coeliac disease", 1, .)) %>%
  mutate_at(vars(c(cl.diet,
                   tr.diet)), ~ ifelse(symptoms_other == "coeliac diet", 1, .)) %>%
  mutate_at(vars(c(cl.diet,
                   tr.diet)), ~ ifelse(symptoms_other == "coeliac disease - gluten free diet.  peripheral neuropathy - none.", 1, .)) %>%
  mutate_at(vars(c(tr.tocrt,
                   cl.antinf)), ~ ifelse(symptoms_other == "steroid creams", 1, .)) %>%
  mutate_at(vars(c(cl.diet,
                   tr.diet,
                   tr.irin,
                   cl.suppl,
                   tr.eyero,
                   cl.lifest)), ~ ifelse(symptoms_other == "eye drops, iron infusions, gluten free diet", 1, .)) %>%
  mutate_at(vars(c(cl.nomed,
                   tr.vens,
                   cl.diet,
                   tr.diet,)), ~ ifelse(symptoms_other == "therapeutic venesection for haemochromatosis   gf diet for cd", 1, .)) %>%
  mutate_at(vars(c(tr.cortst,
                   cl.antinf,
                   tr.iron,
                   cl.suppl)), ~ ifelse(symptoms_other == "thrombocytopenia resolved, only treatment was iron tablets.  autoimmune hepatitis resolved after 6 months on steroids", 1, .)) %>%
  mutate_at(vars(c(tr.heat,
                   cl.nomed)), ~ ifelse(symptoms_other == "heat packs", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.water,
                   cl.lifest,
                   tr.exer,
                   tr.magn,
                   cl.suppl,
                   tr.massa,
                   cl.nomed,
                   tr.mndfl,
                   tr.collgn)), ~ ifelse(symptoms_other == "strict gluten free/high fibre diet  drink 2 litres water per day  walking/exercises (if leg muscles not too sore)  self massage/hot pack/muscle rub cream to neck shoulder pain  magnesium supplement  collagen supplement  mindfulness meditation daily", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "gluten free diet for lifetime", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.lifest,
                   cl.lifest,
                   tr.exer,
                   tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "lifestyle, diet, exercise, supplementation", 1, .)) %>%
  mutate_at(vars(c(tr.preds,
                   cl.antinf,
                   tr.pyrdo,
                   cl.mstm)), ~ ifelse(symptoms_other == "mestinon  prednisolone", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "symptoms went away when i stopped eating gluten", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "strict adherence to a gluten free diet.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "diet- no gluten, barley, rye, oats", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "prevention - follow a strict gluten free diet", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "strict gluten free diet", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "strict gf diet", 1, .)) %>%
  mutate_at(vars(c(tr.magn,
                   cl.suppl,
                   tr.heat,
                   cl.nomed)), ~ ifelse(symptoms_other == "heat packs and magnesium spray", 1, .)) %>%
  mutate_at(vars(c(tr.anac,
                   cl.anac)), ~ ifelse(symptoms_other == "double strength gaviscon", 1, .)) 

df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(tr.cbd,
                   cl.recdr,
                   tr.medca,
                   tr.heat,
                   cl.nomed,
                   tr.cool,
                   tr.phys)), ~ ifelse(symptoms_other == "medically prescribed cbd, physiotherapy, heat and cold packs.", 1, .)) %>%
  mutate_at(vars(c(tr.phys,
                   cl.nomed)), ~ ifelse(symptoms_other == "physio, hydrotherapy", 1, .)) %>%
  mutate_at(vars(c(tr.insln,
                   cl.endoc)), ~ ifelse(symptoms_other == "insulin", 1, .)) %>%
  mutate_at(vars(c(tr.pill,
                   cl.endoc)), ~ ifelse(symptoms_other == "contraceptive pill since age 15", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   cl.suppl,
                   tr.suppl)), ~ ifelse(symptoms_other == "nutritional supplements, diet", 1, .)) %>%
  mutate_at(vars(c(tr.vbin,
                   cl.suppl,
                   tr.ldn,
                   cl.imod)), ~ ifelse(symptoms_other == "ldn  b12 injections", 1, .)) %>%
  mutate_at(vars(c(tr.arprz,
                   cl.psych,
                   tr.ldn,
                   cl.imod)), ~ ifelse(symptoms_other == "ldn plus   lda (low dose aripiprazole/abilify)", 1, .)) %>%
  mutate_at(vars(c(tr.phys,
                   cl.nomed)), ~ ifelse(symptoms_other == "physical therapy", 1, .)) %>%
  mutate_at(vars(c(cl.imod,
                   tr.ldn,
                   tr.suppl,
                   cl.suppl)), ~ ifelse(symptoms_other == "supplements and ldn", 1, .)) %>%
  mutate_at(vars(c(tr.magn,
                   cl.suppl,
                   tr.heat,
                   cl.nomed)), ~ ifelse(symptoms_other == "magnesium  warm baths", 1, .)) %>%
  mutate_at(vars(c(tr.heat,
                   cl.nomed,
                   tr.phys,
                   tr.splnt,
                   cl.lifest,
                   tr.tens)), ~ ifelse(symptoms_other == "physiotherapy, bracing/taping hypermobile joints, tens machine, heating pads", 1, .)) %>%
  mutate_at(vars(c(tr.rest,
                   cl.lifest,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "diet  rest", 1, .)) %>%
  mutate_at(vars(c(tr.art,
                   cl.lifest,
                   tr.suppl,
                   cl.suppl,
                   tr.rest)), ~ ifelse(symptoms_other == "rest, supplements and art", 1, .)) %>%
  mutate_at(vars(c(tr.salt,
                   cl.diet,
                   tr.pace,
                   cl.lifest,
                   tr.lifest,
                   tr.water)), ~ ifelse(symptoms_other == "increased fluid and salt. medication. re-organise lifestyle, eg. go out at times of the day less symptomatic and to activities that don't involve standing.", 1, .)) %>%
  mutate_at(vars(c(tr.stim,
                   cl.stim)), ~ ifelse(symptoms_other == "stimulants", 1, .)) %>%
  mutate_at(vars(c(tr.water,
                   tr.rest,
                   cl.lifest,
                   tr.heat,
                   cl.nomed,
                   tr.massa,
                   tr.bath,
                   tr.diet,
                   cl.diet,
                   tr.herb,
                   cl.suppl,
                   tr.mins,
                   tr.vit)), ~ ifelse(symptoms_other == "the most effective measure: prudent avoidance of food and chemical triggers. i'm sensitive to most pharmaceutical drugs, and find the unwanted side effects usually heavily outweigh any benefits.     lots of rest; heat packs, hydration; vitamin, mineral & herbal supplements in moderation. magnesium baths. massage can help, if i can afford it.", 1, .)) %>%
  mutate_at(vars(c(tr.cbd,
                   cl.recdr,
                   de.oil)), ~ ifelse(symptoms_other == "cbd oil   compounding pharmacy items prescribed by my gp", 1, .)) %>%
  mutate_at(vars(c(tr.rest,
                   cl.lifest)), ~ ifelse(symptoms_other == "rest. rest and rest is key to symptom abatement.  i use the research based protocol published by the workwell foundation to use heart rate monitoring to manage my me/cfs.  rest has resulted in me being able to do more and  not be 99% bedbound in severe pain, severe tiinnitis and 10 kg under weight.", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   cl.suppl,
                   tr.ldn,
                   cl.imod,
                   tr.mins,
                   tr.antiem,
                   cl.antiem,
                   tr.ppi,
                   cl.anac,
                   cl.diur,
                   tr.sprlc,
                   tr.bilg,
                   tr.preds,
                   cl.antinf,
                   tr.mcst,
                   cl.imod,
                   tr.ahist1,
                   tr.ahist2,
                   cl.ahist,
                   cl.heart,
                   tr.aarh,
                   tr.prkn,
                   cl.mstm)), ~ ifelse(symptoms_other == "mast cell stabilisers, h1 antagonists, h2 antagonists, anti-arrhythmics, cortisone/prednisone, biologics, aldosterone antagonists, ppi, vitamins & minerals (including infusions), ldn & an antiemetic and a prokinetic medicine.", 1, .)) %>%
  mutate_at(vars(c(tr.natr,
                   cl.altpr,
                   tr.herb,
                   cl.suppl,
                   tr.suppl)), ~ ifelse(symptoms_other == "herbal medicine and nutritional supplements as prescribed by naturopath", 1, .)) %>%
  mutate_at(vars(c(cl.suppl,
                   tr.suppl,
                   tr.diet,
                   cl.diet,
                   tr.thyrx,
                   cl.endoc)), ~ ifelse(symptoms_other == "thyroxine, diet, supplements", 1, .)) %>%
  mutate_at(vars(c(tr.phys,
                   cl.nomed)), ~ ifelse(symptoms_other == "physiotherapist", 1, .))

df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(tr.lifest,
                   cl.lifest,
                   tr.diet,
                   cl.diet,
                   tr.meds)), ~ ifelse(symptoms_other == "thyroid medication, diet and lifestyle changes", 1, .)) %>%
  mutate_at(vars(c(tr.vit,
                   cl.suppl,
                   tr.bath,
                   cl.nomed)), ~ ifelse(symptoms_other == "bath soak's   vitamin supplements", 1, .)) %>%
  mutate_at(vars(c(tr.suppl,
                   cl.suppl,
                   tr.phys,
                   cl.nomed,
                   tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "supplements, diet, physiotherapy", 1, .)) %>%
  mutate_at(vars(c(tr.cortst,
                   cl.antinf,
                   tr.ivig,
                   cl.imod)), ~ ifelse(symptoms_other == "ivig and steroids (both with terrible reactions)", 1, .)) %>%
  mutate_at(vars(c(tr.ntzl,
                   cl.imod)), ~ ifelse(symptoms_other == "natalizumab - immunomodulatory infusion", 1, .)) %>%
  mutate_at(vars(c(tr.suppl,
                   cl.suppl,
                   tr.rest,
                   cl.lifest,
                   tr.elctr,
                   tr.cmpst,
                   tr.meds)), ~ ifelse(symptoms_other == "medications, supplements, rest, electrolytes, compression socks", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   tr.dmt,
                   tr.ocrz,
                   cl.imsup)), ~ ifelse(symptoms_other == "trialled a dmt - ocrelizumab - in 2020, but due to side-effects did not continue.    pain relief is difficult as i cannot tolerate anti-inflammatory meds.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet)), ~ ifelse(symptoms_other == "just diet at the moment, but it does not help all of my symptoms.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.strav,
                   cl.lifest,
                   tr.exer,
                   tr.vit,
                   cl.suppl,
                   tr.meds)), ~ ifelse(symptoms_other == "diet. anti inflammatory diet.   alcohol only on special occasions  trying to maintain exercise and reduce stress.  about to start immunosuppressants for ms.  medication for hashimotos.  vitamin supplements", 1, .)) %>%
  mutate_at(vars(c(tr.meds,
                   tr.diet,
                   cl.diet,
                   cl.lifest,
                   tr.lifest,
                   tr.rest,
                   tr.mndfl)), ~ ifelse(symptoms_other == "rest. mindfullness. symptom management with diet lifestyle and medications.", 1, .)) %>%
  mutate_at(vars(c(tr.ldn,
                   cl.imod)), ~ ifelse(symptoms_other == "can't take pain killers   2021 i started ldn - some reprieve but early days   i am in pain 24/7 since 2017 and have had consistently high range crp (haven't re tested inflammation since starting ldn). a lumbar puncture in 2018 also showed mirrored banding in csf and serum and report said consistent with systemic inflammation. mri also showed inflamed nerve roots in cauda equina region. despite all this medical proof and ongoing pain i still have had some specialists and gps think it's somatic or not that bad", 1, .)) %>%
  mutate_at(vars(c(tr.meds)), ~ ifelse(symptoms_other == "i am on medication but it is not immunosuppresive.", 1, .)) %>%
  mutate_at(vars(c(tr.meds,
                   tr.vit,
                   cl.suppl,
                   tr.suppl,
                   tr.mins)), ~ ifelse(symptoms_other == "heart medications and vitamin /mineral additives and supplements", 1, .)) %>%
  mutate_at(vars(c(tr.pace,
                   cl.lifest,
                   tr.thia,
                   cl.suppl,
                   tr.ldn,
                   cl.imod,
                   tr.coq10)), ~ ifelse(symptoms_other == "pacing, b1, low-dose naltrexone, ubiquinol", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.pace,
                   cl.lifest,
                   tr.magn,
                   cl.suppl,
                   tr.vbin,
                   tr.trav)), ~ ifelse(symptoms_other == "pacing  b12 injections every 3 weeks  magnesium powder  dietary restrictions  avoiding petroleum products  avoiding strong scents", 1, .)) %>%
  mutate_at(vars(c(tr.rmplo,
                   cl.bldpr)), ~ ifelse(symptoms_other == "nplate injection weekly (tpo drug)", 1, .)) %>%
  mutate_at(vars(c(tr.pace,
                   cl.lifest)), ~ ifelse(symptoms_other == "i did not know about pacing to treat cfs until 5 years into my cfs journey.  i worsened my condition dramatically because i thought i should 'push through my fatigue' but it repeatedly made it worse and worse till i thought i was going to die.", 1, .)) %>%
  mutate_at(vars(c(tr.drkhm,
                   cl.lifest,
                   tr.lifest)), ~ ifelse(symptoms_other == "keeping myself busy & involved with others.  humour - very important.", 1, .)) %>%
  mutate_at(vars(c(tr.inex,
                   cl.lifest)), ~ ifelse(symptoms_other == "powerlifting, before the neurological issues started from tnf inhibitors. then they stopped when i stopped the drugs and started again later. still don't know why, i'm still lifting though because it's the best", 1, .)) %>%
  mutate_at(vars(c(tr.pscph,
                   cl.psych,
                   tr.antinf,
                   cl.antinf)), ~ ifelse(symptoms_other == "anti inflammatory medication, adhd medication", 1, .)) %>%
  mutate_at(vars(c(tr.antinf,
                   cl.antinf,
                   tr.diet,
                   cl.diet,
                   tr.rest,
                   cl.lifest,
                   tr.heat,
                   cl.nomed)), ~ ifelse(symptoms_other == "heat packs  anti-inflammatories  rest  diet", 1, .)) %>%
  mutate_at(vars(c(tr.heat,
                   cl.nomed,
                   tr.rest,
                   cl.lifest,
                   tr.pace,
                   tr.nsai,
                   cl.antinf,
                   tr.phys,
                   tr.rest,
                   tr.yoga,
                   tr.chiro,
                   tr.meds)), ~ ifelse(symptoms_other == "nsaids, heat packs, pacing/rest, physiotherapy, chiropractic, yoga, general stretching, muscle-relaxing medications on occasion.", 1, .)) %>%
  mutate_at(vars(c(cl.endoc,
                   tr.tthr,
                   tr.propr,
                   cl.heart,
                   tr.ivab,
                   tr.amtrp,
                   cl.psych,
                   tr.stret)), ~ ifelse(symptoms_other == "t3 for energy, amitriptyline for sleep, ivabradine & propanolol for heart rate & blood pressure", 1, .)) %>%
  mutate_at(vars(c(tr.eyero,
                   cl.lifest,
                   tr.niac,
                   cl.suppl,
                   tr.ovest,
                   cl.nomed,
                   tr.fibr,
                   tr.heat,
                   tr.splnt,
                   tr.orth)), ~ ifelse(symptoms_other == "vitamin b3 to reduce effects of sun on skin.  lubricant eye drops and ointment.  lubricant oral/nasal ointments.  oestrogen vaginal pesseries.  metamucil for ibs.  heat for back and joint pain.  strapping and elevation for arthritic ankes and feet.  orthopedic shoes and orthotic insoles for collapsed arches/painful feet.  cushioning for pressure at bursitis sites.", 1, .)) %>%
  mutate_at(vars(c(tr.diet,
                   cl.diet,
                   tr.pntoz,
                   cl.anac,
                   tr.amtrp,
                   cl.psych,
                   tr.tocrt,
                   cl.antinf,
                   tr.eyero,
                   cl.lifest,
                   tr.orca,
                   tr.swri)), ~ ifelse(symptoms_other == "pantoprazole, low fodmap diet. amitriptiline, elcon lotion, pos a ointment hi lo forte eye drops, nasal spray, biotene toothpaste.", 1, .)) %>%
  mutate_at(vars(c(tr.bed,
                   cl.nomed,
                   tr.hydt,
                   cl.altpr,
                   tr.salt,
                   cl.diet,
                   tr.diet,
                   tr.cool,
                   tr.cmpst,
                   cl.lifest)), ~ ifelse(symptoms_other == "staying in bed / horizontal   going in water..pool helps a lot to alleviate some symptoms    was on a high salt diet for 10 years but i don't really think that helped ultimately although i do agent of salt so that i don't crave salt.   would like to try iv saline infusions therapy as that is helpful for some people     i have tried probably every treatment that i have read about over the last 10 years and nothing else has really ultimately been very helpful maybe lot compression twice and all over the body when i frock when i remember and when it's cool enough and saying cool because heat  seems to exacerbate the condition due to the vasodilatation it causes", 1, .)) %>%
  mutate_at(vars(c(tr.msrlx,
                   cl.msrlx,
                   tr.trinj,
                   cl.nomed)), ~ ifelse(symptoms_other == "trigger point injections; muscle relaxants", 1, .)) %>%
  mutate_at(vars(c(tr.thyrx,
                   cl.endoc,
                   cl.msrlx,
                   tr.mrbg,
                   cl.albl,
                   tr.flmx,
                   cl.psych,
                   tr.mrtz,
                   tr.vpap,
                   cl.nomed)), ~ ifelse(symptoms_other == "thyroxin  betmiga  flowmaxtra  mirtazipine - 15mg for insomnia  vpap machine", 1, .)) %>%
  mutate_at(vars(c(tr.plph,
                   cl.nomed,
                   tr.pyrdo,
                   cl.mstm,
                   tr.modaf,
                   cl.stim,
                   tr.fexo,
                   cl.ahist,
                   tr.vitd,
                   tr.vitc,
                   cl.suppl,
                   tr.zinc,
                   tr.vbin,
                   tr.irin)), ~ ifelse(symptoms_other == "plasmaphraresis, mestinon, modafinil, telfast, vitamins d & c, zinc. recently iron infusion and b12 injections.     previously ivig (caused aseptic meningitis repeatedly) and rituximab.    i mostly only use pain killers after acute injury, surgery, or in a flare before treatment kicks in. i have a high tolerance for pain. and not a lot of pain when well managed.", 1, .)) %>%
#TODO previous treatment mentioned above
  mutate_at(vars(c(tr.pyrdo,
                   cl.mstm,
                   tr.propr,
                   cl.heart,
                   tr.ldn,
                   cl.imod,
                   tr.ahist,
                   cl.ahist,
                   tr.ahist2)), ~ ifelse(symptoms_other == "mestinon, propanolol, low dose naltrexone, famotidine, anti-histamines", 1, .)) %>%
  mutate_at(vars(c(tr.ibup,
                   cl.antinf,
                   tr.parc,
                   tr.curc,
                   cl.suppl,
                   tr.bath,
                   cl.nomed,
                   tr.vit)), ~ ifelse(symptoms_other == "panadol/ nurofen, turmeric and vitamins with elson salt baths and topical creams. i am extremely sensitive to medications so everything i have tried has given me baf side effects . i try treating pain and symptoms naturally."  , 1, .)) %>%
  mutate_at(vars(c(tr.amtrp,
                   cl.psych)), ~ ifelse(symptoms_other == "amitriptyline daily"  , 1, .))

  
  
#1-22 drug_other_other
df.other.treatments <- df.other.treatments %>% 
  mutate_at(vars(c(tr.medca,
                   cl.recdr,
                   tr.cbd)), ~ ifelse(drug_other_other == "cannabis and cbd which has been discussed with my healthcare providers", 1, .)) %>%
  mutate_at(vars(c(tr.cana,
                   cl.recdr)), ~ ifelse(drug_other_other == "when able cannabis but it only helps a little and if i have too much it increases pain.", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   tr.cana)), ~ ifelse(drug_other_other == "ocassional joint for severe pain", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   tr.alco)), ~ ifelse(drug_other_other == "alcohol", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   tr.cana)), ~ ifelse(drug_other_other == "cannabis for joint pain and muscle soreness", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   de.oil,
                   tr.thc)), ~ ifelse(drug_other_other == "tsh oil", 1, .)) %>%
 # mutate_at(vars(c()), ~ ifelse(drug_other_other == "i have to buy pain medication that isn't from a doctor because despite seeing a pain specialist and doing everything they've told me, i'm still in pain and doctors are worried about addiction. i'm worried more about being in pain every day. i couldn't give a hoot if i become drug dependent. it's less stress on my body. pain meds should be part of a holistic pain management process that also includes exercise, diet, herbs whatever", 1, .)) %>%
  mutate_at(vars(c(tr.diaz,
                   cl.diaz)), ~ ifelse(drug_other_other == "source valium often when having a particularly bad flare", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   tr.cana)), ~ ifelse(drug_other_other == "weed", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   tr.cana)), ~ ifelse(drug_other_other == "i have used marijuana for pain relief and to address the psychological distress of having ra since diagnosis 40 years ago. my first rheumatologist believed the addiction risk too high to prescribe adequate pain relief so i was forced to seek help elsewhere.", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   tr.cana,
                   de.smok)), ~ ifelse(drug_other_other == "cannabis flower (smoked) for pain, gastrointestinal symptoms, stress relief", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   tr.medca)), ~ ifelse(drug_other_other == "i have a prescription for medicinal cannabis, but do not take this continually as it is very cost-prohibitive.", 1, .)) %>%
  mutate_at(vars(c(cl.recdr,
                   tr.coca)), ~ ifelse(drug_other_other == "cocaine", 1, .)) 

#label classifiers
label(df.other.treatments$cl.imsup)="immunosuppresants"
label(df.other.treatments$cl.suppl)="supplements"
label(df.other.treatments$cl.lifest)="lifestyle"
label(df.other.treatments$cl.diet)="diet"
label(df.other.treatments$cl.nomed)="Non-medication intervention"
label(df.other.treatments$cl.opia)="opiates"
label(df.other.treatments$cl.anac)="antacid"
label(df.other.treatments$cl.endoc)="endocrine intervention"
label(df.other.treatments$cl.antinf)="Anti-inflammatories"
label(df.other.treatments$cl.psych)="psychiatric medication"
label(df.other.treatments$cl.imod)="immunomodulator"
label(df.other.treatments$cl.dmard)="disease-modifying antirheumatic drug"
label(df.other.treatments$cl.ahist)="antihistamine"
label(df.other.treatments$cl.recdr)="recreational drugs"
label(df.other.treatments$cl.stool)="bowel modulator"
label(df.other.treatments$cl.vascon)="vasopressor"
label(df.other.treatments$cl.antiem)="antiemetic"
label(df.other.treatments$cl.heart)="heart medication"
label(df.other.treatments$cl.afung)="anti-fungal"
label(df.other.treatments$cl.dopag)="dopamine agonist"
label(df.other.treatments$cl.diaz)="benzodiazepines"
label(df.other.treatments$cl.surg)="surgical intervention"
label(df.other.treatments$cl.neurp)="neuropathic painkiller"
label(df.other.treatments$cl.avir)="anti-virals"
label(df.other.treatments$cl.diur)="diuretic"
label(df.other.treatments$cl.cholai)="cholesterol absorption inhibitors"
label(df.other.treatments$cl.enzy)="enzyme supplement or modulators"
label(df.other.treatments$cl.abiot)="antibiotic"
label(df.other.treatments$cl.mpara)="muscular paralysis agent"
label(df.other.treatments$cl.mstm)="muscle stimulant"
label(df.other.treatments$cl.bldpr)="blood production"
label(df.other.treatments$cl.altpr)="alternative practice"
label(df.other.treatments$cl.stim)="stimulant"
label(df.other.treatments$cl.msrlx)="muscle relaxant"
label(df.other.treatments$cl.tyki)="tyrosine kinase inhibitor"
label(df.other.treatments$cl.thrag)="thrombopoietin receptor agonists"
label(df.other.treatments$cl.albl)="alpha-blocker"

#label treatments
label(df.other.treatments$tr.esom)="esomeprazole"
label(df.other.treatments$tr.mesal)="mesalazine"
label(df.other.treatments$tr.cortst)="corticosteroids"
label(df.other.treatments$tr.strav)="stress avoidance"
label(df.other.treatments$tr.diet)="diet"
label(df.other.treatments$tr.rest)="rest"
label(df.other.treatments$tr.nsai)="non-steroidal anti-inflammatories"
label(df.other.treatments$tr.neri)="norepinephrine reuptake inhibitor"
label(df.other.treatments$tr.bath)="bath"
label(df.other.treatments$tr.heat)="heat rub"
label(df.other.treatments$tr.vit)="vitamins"
label(df.other.treatments$tr.mins)="minerals"
label(df.other.treatments$tr.iod)="iodine"
label(df.other.treatments$tr.ashw)="ashwagandha"
label(df.other.treatments$tr.vitd)="vitamin d"
label(df.other.treatments$tr.vitb12)="vitamin b12"
label(df.other.treatments$tr.melat)="melatonin"
label(df.other.treatments$tr.ndte)="natural desiccated thyroid extract"
label(df.other.treatments$tr.exer)="exercise"
label(df.other.treatments$tr.suppl)="supplements"
label(df.other.treatments$tr.sleep)="sleep hygiene"
label(df.other.treatments$tr.blreg)="blood pressure regulators"
label(df.other.treatments$tr.ldn)="low dose naltrexone"
label(df.other.treatments$tr.anac)="antacids"
label(df.other.treatments$tr.cool)="cooling"
label(df.other.treatments$tr.bdyw)="body wash"
label(df.other.treatments$tr.massa)="massage"
label(df.other.treatments$tr.dexi)="dexamphetamines"
label(df.other.treatments$tr.levo)="levothyroxine"
label(df.other.treatments$tr.phys)="physio therapy"
label(df.other.treatments$tr.magn)="magnesium"
label(df.other.treatments$tr.acc)="N-acetyl cysteine"
label(df.other.treatments$tr.inos)="inositol"
label(df.other.treatments$tr.ribo)="riboflavin"
label(df.other.treatments$tr.imsup)="immunosuppresants"
label(df.other.treatments$tr.metr)="methotrexate"
label(df.other.treatments$tr.hydrx)="hydroxychloroquine"
label(df.other.treatments$tr.rita)="ritalin"
label(df.other.treatments$tr.phen)="phenergan"
label(df.other.treatments$tr.hydrco)="hydrcortisone"
label(df.other.treatments$tr.flrco)="fludrocortisone"
label(df.other.treatments$tr.cortinj)="cortisone injections"
label(df.other.treatments$tr.propr)="propranalol"
label(df.other.treatments$tr.effex)="effexor"
label(df.other.treatments$tr.micrgy)="microgynon"
label(df.other.treatments$tr.modaf)="modafinil"
label(df.other.treatments$tr.zyrt)="zyrtex"
label(df.other.treatments$tr.parc)="paracetamol"
label(df.other.treatments$tr.tapa)="tapentanol"
label(df.other.treatments$tr.colo)="coloxyl"
label(df.other.treatments$tr.celeb)="celebrex"
label(df.other.treatments$tr.osteo)="osteopathy"
label(df.other.treatments$tr.psych)="psychological therapy"
label(df.other.treatments$tr.allexp)="allergen exposure"
label(df.other.treatments$tr.alco)="alcohol"
label(df.other.treatments$tr.mdma)="mdma"
label(df.other.treatments$tr.lsd)="lsd"
label(df.other.treatments$tr.keta)="ketamine"
label(df.other.treatments$tr.melox)="meloxicam"
label(df.other.treatments$tr.apath)="apathy"
label(df.other.treatments$tr.mido)="midodrine"
label(df.other.treatments$tr.ivig)="iv Ig"
label(df.other.treatments$tr.preb)="prebiotic"
label(df.other.treatments$tr.prob)="probiotic"
label(df.other.treatments$tr.apci)="apple cider vinegar"
label(df.other.treatments$tr.cthsu)="caruso's thyroid supplement"
label(df.other.treatments$tr.thyrx)="thyroxine"
label(df.other.treatments$tr.collgn)="collagen"
label(df.other.treatments$tr.curc)="curcumin"
label(df.other.treatments$tr.coq10)="coq10"
label(df.other.treatments$tr.domp)="domperidone"
label(df.other.treatments$tr.cital)="citalopram"
label(df.other.treatments$tr.cyclo)="cyclosporin"
label(df.other.treatments$tr.fish)="fish oil"
label(df.other.treatments$tr.lifest)="lifestyle"
label(df.other.treatments$tr.nitrg)="nitroglycerin patches"
label(df.other.treatments$tr.calbl)="calcium blockers"
label(df.other.treatments$tr.bldth)="blood thinners"
label(df.other.treatments$tr.cana)="cannabis"
label(df.other.treatments$tr.zyrt)="zyrtec"
label(df.other.treatments$tr.ritx)="rituximab"
label(df.other.treatments$tr.eyero)="eyecare routine"
label(df.other.treatments$tr.bala)="work life balance"
label(df.other.treatments$tr.stret)="stretching"
label(df.other.treatments$tr.cycph)="cyclophosphamide"
label(df.other.treatments$tr.altmed)="alternative medicine"
label(df.other.treatments$tr.pila)="pilates"
label(df.other.treatments$tr.aqua)="aquarobics"
label(df.other.treatments$tr.berzm)="benralizumab"
label(df.other.treatments$tr.warf)="warfarin"
label(df.other.treatments$tr.orca)="oral care"
label(df.other.treatments$tr.skca)="skin care"
label(df.other.treatments$tr.lerca)="lercanidipine"
label(df.other.treatments$tr.medit)="meditation"
label(df.other.treatments$tr.taich)="tai chi"
label(df.other.treatments$tr.ppi)="proton pump inhibitors"
label(df.other.treatments$tr.mndfl)="mindfulness"
label(df.other.treatments$tr.chiro)="chiropractor"
label(df.other.treatments$tr.chmed)="chinese medicine"
label(df.other.treatments$tr.accp)="acupuncture"
label(df.other.treatments$tr.adep)="antidepressants"
label(df.other.treatments$tr.bihor)="bioidentical hormones"
label(df.other.treatments$tr.myla)="mylanta"
label(df.other.treatments$tr.dmard)="disease-modifying antirheumatic drug"
label(df.other.treatments$tr.ahist)="anti-histamine"
label(df.other.treatments$tr.rivxb)="rivaroxaban"
label(df.other.treatments$tr.valac)="valporic acid"
label(df.other.treatments$tr.prox)="paroxetine"
label(df.other.treatments$tr.flucz)="fluconazole"
label(df.other.treatments$tr.prmx)="pramipexole"
label(df.other.treatments$tr.gaba)="gabapentin"
label(df.other.treatments$tr.clonz)="clonazepam"
label(df.other.treatments$tr.mpred)="methylprednisolone"
label(df.other.treatments$tr.pace)="pacing"
label(df.other.treatments$tr.gard)="gardening"
label(df.other.treatments$tr.natr)="naturopathy"
label(df.other.treatments$tr.cmzl)="carbimazole"
label(df.other.treatments$tr.iron)="iron"
label(df.other.treatments$tr.minox)="minoxidil"
label(df.other.treatments$tr.stcetr)="stem cell transplant"
label(df.other.treatments$tr.essoi)="essential oils"
label(df.other.treatments$tr.zostx)="zostrix"
label(df.other.treatments$tr.zinc)="zinc"
label(df.other.treatments$tr.aspr)="aspirin"
label(df.other.treatments$tr.pntoz)="pantoprazole"
label(df.other.treatments$tr.ovest)="ovestin"
label(df.other.treatments$tr.talad)="tadalafil"
label(df.other.treatments$tr.opsum)="opsumit"
label(df.other.treatments$tr.pregab)="pregabalin"
label(df.other.treatments$tr.acei)="ACE inhibitor"
label(df.other.treatments$tr.sprlc)="spironolactone"
label(df.other.treatments$tr.fusem)="frusemide"
label(df.other.treatments$tr.eztim)="ezetimibe"
label(df.other.treatments$tr.creon)="creon"
label(df.other.treatments$tr.preds)="prednisone"
label(df.other.treatments$tr.alpur)="allopurinol"
label(df.other.treatments$tr.dymi)="dymista"
label(df.other.treatments$tr.fexo)="fexofenadine"
label(df.other.treatments$tr.iginj)="immunogolublin injections"
label(df.other.treatments$tr.bose)="bosentan"
label(df.other.treatments$tr.wrkp)="work part-time"
label(df.other.treatments$tr.wrkna)="work none"
label(df.other.treatments$tr.petco)="pet companionship"
label(df.other.treatments$tr.walk)="walking"
label(df.other.treatments$tr.omep)="omeprazole"
label(df.other.treatments$tr.nifd)="nifedipine"
label(df.other.treatments$tr.sild)="sildenafil"
label(df.other.treatments$tr.bact)="bactrim"
label(df.other.treatments$tr.itrcz)="itraconozol"
label(df.other.treatments$tr.pill)="contraceptive pill"
label(df.other.treatments$tr.hmeo)="homeopathy"
label(df.other.treatments$tr.botx)="botox"
label(df.other.treatments$tr.galcz)="galcanezumab"
label(df.other.treatments$tr.thc)="thc"
label(df.other.treatments$tr.cbd)="cbd"
label(df.other.treatments$tr.surg)="surgery"
label(df.other.treatments$tr.nasl)="nasal care"
label(df.other.treatments$tr.ntzl)="natalizumab"
label(df.other.treatments$tr.herb)="herbal remedies"
label(df.other.treatments$tr.yoga)="yoga"
label(df.other.treatments$tr.mntlk)="montelukast"
label(df.other.treatments$tr.cetrz)="cetirizine"
label(df.other.treatments$tr.amtrp)="amitriptyline"
label(df.other.treatments$tr.betbl)="beta blocker"
label(df.other.treatments$tr.whlch)="wheel chair"
label(df.other.treatments$tr.elctr)="electrolytes"
label(df.other.treatments$tr.bed)="bed rest"
label(df.other.treatments$tr.mclo)="moclobemide"
label(df.other.treatments$tr.smmp)="somatic movement program"
label(df.other.treatments$tr.dulox)="duloxetine"
label(df.other.treatments$tr.endp)="endep"
label(df.other.treatments$tr.peas)="palmitoylethanolamide"
label(df.other.treatments$tr.splpl)="sleeping pills"
label(df.other.treatments$tr.mxbst)="moxibustion"
label(df.other.treatments$tr.afung)="antibiotcs"
label(df.other.treatments$tr.afung)="antifungals"
label(df.other.treatments$tr.myth)="myotherapy"
label(df.other.treatments$tr.tocrt)="topical corticosteroid"
label(df.other.treatments$tr.clchn)="colchine"
label(df.other.treatments$tr.arprz)="aripiprazole"
label(df.other.treatments$tr.vanst)="vagal nerve stimulation"
label(df.other.treatments$tr.trcrms)="transcranial magnetic stimulation"
label(df.other.treatments$tr.mtfr)="metformin"
label(df.other.treatments$tr.qtpn)="quetiapine"
label(df.other.treatments$tr.lmtrg)="lamotrigine"
label(df.other.treatments$tr.cobt)="cognitive behavioural therapy"
label(df.other.treatments$tr.pyrdo)="pyridostigmine"
label(df.other.treatments$tr.p5p)="pyridoxal 5-phosphate"
label(df.other.treatments$tr.mits)="mitochondrial nutrients"
label(df.other.treatments$tr.cmpst)="compression clothing"
label(df.other.treatments$tr.pscph)="psychopharmaceutical"
label(df.other.treatments$tr.drndl)="dry needling"
label(df.other.treatments$tr.detx)="de-toxing"
label(df.other.treatments$tr.msccr)="muscle relief creams"
label(df.other.treatments$tr.splnec)="splenectomy"
label(df.other.treatments$tr.gcsf)="granulocyte-colony stimulating factor"
label(df.other.treatments$tr.rmplo)="romiplostim"
label(df.other.treatments$tr.bldtr)="blood transfusions"
label(df.other.treatments$tr.trax)="tranexamic acid"
label(df.other.treatments$tr.trav)="trigger avoidance"
label(df.other.treatments$tr.drkhm)="dark humour"
label(df.other.treatments$tr.c1in)="c1 inhibitors"
label(df.other.treatments$tr.smth)="simethicone"
label(df.other.treatments$tr.vasc)="vasoconstrictor"
label(df.other.treatments$tr.aubuf)="australian bush flower essence"
label(df.other.treatments$tr.resr)="rescue remedy"
label(df.other.treatments$tr.relax)="relaxation"
label(df.other.treatments$tr.shyp)="self-hypnosis"
label(df.other.treatments$tr.bflvt)="beef liver tablets"
label(df.other.treatments$tr.kins)="kinesiologist"
label(df.other.treatments$tr.mthf)="methyltetrahydrofolate"
label(df.other.treatments$tr.vascon)="vassopressor"
label(df.other.treatments$tr.stim)="stimulants"
label(df.other.treatments$tr.water)="hydration"
label(df.other.treatments$tr.escip)="escitalopram"
label(df.other.treatments$tr.brthe)="breathing exercises"
label(df.other.treatments$tr.swm)="swimming"
label(df.other.treatments$tr.progn)="progesterone"
label(df.other.treatments$tr.drib)="d-ribose supplement"
label(df.other.treatments$tr.nutr)="nutritionist"
label(df.other.treatments$tr.sulfz)="sulfasalazine"
label(df.other.treatments$tr.salt)="salt"
label(df.other.treatments$tr.brbon)="bone broth"
label(df.other.treatments$tr.licr)="licorice root"
label(df.other.treatments$tr.om3)="omega 3"
label(df.other.treatments$tr.gibi)="ginkgo biloba"
label(df.other.treatments$tr.lima)="lion's mane"
label(df.other.treatments$tr.glcsm)="glucosamine"
label(df.other.treatments$tr.chndr)="chondroitin"
label(df.other.treatments$tr.msm)="methylsulfonylmethane"
label(df.other.treatments$tr.pqq)="pyrroloquinoline quinone"
label(df.other.treatments$tr.clcm)="calcium"
label(df.other.treatments$tr.vitc)="vitamin c"
label(df.other.treatments$tr.at1)="angiotensin II receptor blocker"
label(df.other.treatments$tr.post)="postural accomodations"
label(df.other.treatments$tr.cupr)="cusack protocol"
label(df.other.treatments$tr.splnt)="splinting joints"
label(df.other.treatments$tr.tens)="transcutaneous electrical nerve stimulation"
label(df.other.treatments$tr.ems)="electrical muscle stimulation"
label(df.other.treatments$tr.hydt)="hydrotherapy"
label(df.other.treatments$tr.antinf)="anti-inflammatories"
label(df.other.treatments$tr.dxpn)="doxepin"
label(df.other.treatments$tr.dsvx)="desvenlafaxine"
label(df.other.treatments$tr.nrdvt)="neurodevelopmental treatment"
label(df.other.treatments$tr.mbai)="mobility aids"
label(df.other.treatments$tr.ephd)="ephedrine"
label(df.other.treatments$tr.sumt)="sumatriptan"
label(df.other.treatments$tr.vite)="vitamin e"
label(df.other.treatments$tr.vitb7)="vitamin b7"
label(df.other.treatments$tr.ntre)="nature"
label(df.other.treatments$tr.emdr)="eye movement desensitization and reprocessing"
label(df.other.treatments$tr.ivab)="ivabradine"
label(df.other.treatments$tr.spmba)="spiky massage balls"
label(df.other.treatments$tr.reiki)="reiki"
label(df.other.treatments$tr.hcrtp)="hydrocortisone pump"
label(df.other.treatments$tr.gltm)="glutamine"
label(df.other.treatments$tr.glyc)="glycine"
label(df.other.treatments$tr.chrc)="charcoal"
label(df.other.treatments$tr.mlbd)="molybdenum"
label(df.other.treatments$tr.slnem)="selenium"
label(df.other.treatments$tr.bthy)="betaine hydrochloride"
label(df.other.treatments$tr.cpap)="continuous positive airway pressure"
label(df.other.treatments$tr.blcfn)="baclofen"
label(df.other.treatments$tr.tpest)="topical estrogen"
label(df.other.treatments$tr.tnfi)="tumor necrosis factor inhibitor"
label(df.other.treatments$tr.bwth)="bowen therapy"
label(df.other.treatments$tr.rhro)="rhodiola rosea"
label(df.other.treatments$tr.ciqu)="cissus quadrangularis"
label(df.other.treatments$tr.sugr)="sugar"
label(df.other.treatments$tr.insln)="insulin"
label(df.other.treatments$tr.vitb9)="vitamin b9"
label(df.other.treatments$tr.ahist1)="anti-histamine H1"
label(df.other.treatments$tr.ahist2)="anti-histamine H2"
label(df.other.treatments$tr.mcst)="mast cell stabilizer"
label(df.other.treatments$tr.imth)="immunotherapy"
label(df.other.treatments$tr.neurp)="neruopathic painkillers"
label(df.other.treatments$tr.rztrp)="rizatriptan"
label(df.other.treatments$tr.ibup)="ibuprofen"
label(df.other.treatments$tr.diaz)="diazepam"
label(df.other.treatments$tr.abiot)="antibiotics"
label(df.other.treatments$tr.testo)="testosterone"
label(df.other.treatments$tr.psylo)="psilocybin"
label(df.other.treatments$tr.coca)="cocaine"
label(df.other.treatments$tr.medca)="medicinal canabis"
label(df.other.treatments$tr.acyc)="acyclovir"
label(df.other.treatments$tr.colos)="colostrum"
label(df.other.treatments$tr.sert)="sertraline"
label(df.other.treatments$tr.nint)="nintedanib"
label(df.other.treatments$tr.swri)="sinus care"
label(df.other.treatments$tr.lstm)="stimulation avoidance"
label(df.other.treatments$tr.lstm)="stimulation avoidance"
label(df.other.treatments$tr.detc)="dental care routine"
label(df.other.treatments$tr.xylm)="xylimelts"
label(df.other.treatments$tr.vbin)="vit B12 injections"
label(df.other.treatments$tr.irin)="iron infusions"
label(df.other.treatments$tr.hypb)="hyperbaric oxygen therapy"
label(df.other.treatments$tr.etrm)="eltrombopag"
label(df.other.treatments$tr.mthl)="mental health care"
label(df.other.treatments$tr.amac)="amino acids"
label(df.other.treatments$tr.resn)="resignation"
label(df.other.treatments$tr.fibr)="fibre"
label(df.other.treatments$tr.laxa)="laxatives"
label(df.other.treatments$tr.antiem)="antiemetic"
label(df.other.treatments$tr.prchz)="prochlorperazine"
label(df.other.treatments$tr.vens)="venesection"
label(df.other.treatments$tr.art)="art"
label(df.other.treatments$tr.bilg)="biologics"
label(df.other.treatments$tr.aarh)="antiarrhythmic agents"
label(df.other.treatments$tr.dmt)="dmt"
label(df.other.treatments$tr.ocrz)="ocrelizumab"
label(df.other.treatments$tr.meds)="medication"
label(df.other.treatments$tr.prkn)="prokinetics"
label(df.other.treatments$tr.thia)="thiamine"
label(df.other.treatments$tr.inex)="intensive exercise"
label(df.other.treatments$tr.tthr)="triiodothyronine"
label(df.other.treatments$tr.niac)="niacin"
label(df.other.treatments$tr.orth)="orthopedics"
label(df.other.treatments$tr.trinj)="triggerpoint injections"
label(df.other.treatments$tr.msrlx)="muscle relaxant"
label(df.other.treatments$tr.mrbg)="mirabegron"
label(df.other.treatments$tr.flmx)=" flowmaxtra"
label(df.other.treatments$tr.mrtz)="mirtazapine"
label(df.other.treatments$tr.vpap)="vpap"
label(df.other.treatments$tr.plph)="plasmapheresis"

#label recreational delivery method
label(df.other.treatments$de.smok)="smoke"
label(df.other.treatments$de.edib)="edibles"
label(df.other.treatments$de.oil)="oil"

#df.other.treatments %>% select(record_id, tr.cana, tr.cbd, tr.thc) %>% filter(tr.cana == 1 | tr.cbd == 1 | tr.thc == 1)

##wordcloud individual treatment
tr.cloud <- df.other.treatments %>%
  select(contains("tr.")) %>%
  mutate(cannabis = 0) %>%
  mutate(corticosteroids = 0)
tr.cloud <- tr.cloud  %>%
  mutate_at(vars(c(cannabis)), ~ ifelse(tr.cana == "1" |
                                          tr.cbd == "1" |
                                          tr.medca == "1" |
                                          tr.thc == "1" , 1, .)) %>%
  mutate_at(vars(c(corticosteroids)), ~ ifelse(tr.tocrt == "1" |
                                                 tr.cortst == "1" |
                                                 tr.cortinj== "1" |
                                                 tr.flrco== "1" |
                                                 tr.hydrco == "1" |
                                                 tr.hcrtp== "1" |
                                                 tr.mpred== "1" |
                                                 tr.preds== "1", 1, .)) %>%
  subset(., select = -c(tr.cana,
            tr.cbd,
            tr.medca,
            tr.thc,
            tr.tocrt,
            tr.cortst,
            tr.cortinj,
            tr.flrco,
            tr.hydrco,
            tr.hcrtp,
            tr.mpred,
            tr.preds,
            tr.meds))
label(tr.cloud$cannabis)="cannabis"
label(tr.cloud$corticosteroids)="corticosteroids"
tr.labels <- tr.cloud %>%
  get_label 
colnames(tr.cloud) <- tr.labels

tr.count <- tr.cloud %>% 
  summarise_if(is.numeric, sum) %>% 
  transpose()
tr.count.df <- as.data.frame(do.call(cbind, tr.count)) %>% 
  rownames_to_column(.) %>% 
  rename(., word = rowname, freq = V1)
tr.count.df$freq <- as.numeric(as.character(tr.count.df$freq))

wordcloud2(tr.count.df, shape = 'diamond', color = "random-light", backgroundColor = "grey")

#check which column labels are duplicated
# if_else(duplicated(tr.labels) == TRUE, print(tr.labels), "NA")

##treatment barplot
#use ad.count.df from wordcloud 
tr.count.ord <- tr.count.df %>%
#  filter(word != "Healthy control") %>%
  filter(freq > 8)%>%
  rename(., treatment = word) %>%
  arrange(., desc(freq))

#ad.plot <- 
ggplot(tr.count.ord, aes(x = freq, y = treatment)) +
  geom_col(fill = viridis(24)) +
  scale_y_discrete(limits=rev(tr.count.ord$treatment)) +
  labs(title = NULL,
       x = NULL, #"Number of respondents",
       y = NULL) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0, 145),
                     breaks = c(0, 20, 40, 60, 80, 100, 120, 140)) +
  theme_bw() +
  theme(plot.title=element_text(size=20, face="bold"),
        axis.title=element_text(size=20),
        axis.text=element_text(size=20, face="bold"))

## wordcloud treatment classifiers
cltr.cloud <- df.other.treatments %>%
  select(contains("cl.")) 
cltr.labels <- cltr.cloud %>%
  get_label 
colnames(cltr.cloud) <- cltr.labels
cltr.count <- cltr.cloud %>% 
  summarise_if(is.numeric, sum) %>% 
  transpose()
cltr.count.df <- as.data.frame(do.call(cbind, cltr.count)) %>% 
  rownames_to_column(.) %>% 
  rename(., word = rowname, freq = V1)
cltr.count.df$freq <- as.numeric(as.character(cltr.count.df$freq))

wordcloud2(cltr.count.df, shape = 'diamond', color = "random-dark", backgroundColor = "white")
