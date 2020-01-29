required.packages <- c("reshape2", "ggplot2", "data.table")
lapply(required.packages, require, character.only = T)

wd <- "G:/My Drive/Work/GitHub/inclusion-works/"
setwd(wd)

load("project-data/crs 2012-2013.RData")
load("project-data/crs 2014-2015.RData")
load("project-data/crs 2016-2017.RData")

crs <- rbind(crs.2012.2013, crs.2014.2015, crs.2016.2017)
rm(list=c("crs.2012.2013", "crs.2014.2015", "crs.2016.2017"))

keep <- c(
  "crs_id"
  ,
  "project_number"
  ,
  "year"
  ,
  "aid_type"
  ,
  "flow_name"
  ,
  "donor_name"
  ,
  "recipient_name"
  ,
  "usd_commitment_deflated"
  ,
  "usd_disbursement_deflated"
  ,
  "purpose_name"
  ,
  "project_title"
  ,
  "short_description"
  ,
  "long_description"
  ,
  "gender"
)

crs <- crs[, ..keep]
crs <- crs[flow_name == "ODA Loans" | flow_name == "ODA Grants" | flow_name == "Equity Investment" | flow_name == "Private Development Finance"]

major.keywords <- c(
  "disab", "discapaci", "incapaci", "minusválido", "invalidit", "infirmité", "d-isab"
  #,
  #"disorder"
  ,
  "handicap"
  ,
  "impairment", "impaired"
  ,
  "pwd", "gwd", "cwd"
  ,
  "chronic health", "chronic ill", "maladie chronique", "enfermedad crónica"
  ,
  "deaf", "sordo", "sourd"
  ,
  "blind", "ciego", "aveugle", "eye health"
  ,
  "special needs", "necesidades especiales", "besoins spéciau"
  ,
  "autistic", "autism", "autist"
  ,
  "mental health", "santé mentale", "salud mental"
  ,
  "prosthes", "prosthès", "prótesis"
  ,
  "mobility device", "dispositivo de movilidad", "dispositif de mobilité"
  ,
  "wheelchair", "fauteuil roulant", "silla de ruedas"
  ,
  "hearing aid", "audífono", "dispositif d'écoute pour malentendant"
  ,
  "amputation", "amputee", "amputé", "amputa"
  ,
  "schizophreni", "esquizofrenia", "schizophrénie"
  ,
  "sign language", "langage des signes", "lenguaje de señas"
  ,
  "arthriti", "artritis", "arthrite"
  ,
  "rheumat", "rhumat", "reumat"
  ,
  "dementia", "démence", "demencia"
  ,
  "spina "
  ,
  "hydrocephalus", "hidrocefalia", "l'hydrocéphalie"
  ,
  "diabetes", "diabète"
  ,
  "atlas alliance", "atlas allinance", "abilis foundation", "zapdd"
  #,
  #"dpos ", "dpo ", "dpo's", "dpos[.]", "dpo[.]", "dpo's[.]"
)

minor.keywords <- c(
  "war victim", "victimas de guerra", "victimes de guerre"
  ,
  "landmine victim", "victime de mine", "víctima de minas terrestres"
  #,
  #"wounded"
  #,
  #"injured", "injuries"
  #,
  #"physiotherapy", "fisioterapia"
)

disqualifying.keywords <- c(
  "chronic malnutrition"
  ,
  "mole rat"
  ,
  "cgpwd"
  ,
  "cpwd"
  ,
  "rvcwda"
  ,
  "pwdtc"
  ,
  "road"
  ,
  "highway"
  ,
  "environmental health"
  ,
  "environmental condition"
  ,
  "rehydration therapy"
  #,
  #"-dpo", "cidpo", "hdpo", "dpo series", "financial sector dpo", "dpo (ri)", "management dpo", "poverty dpo", "growth dpo", "support dpo", "system dpo", "programmatic dpo"
  ,
  "fiscal"
  ,
  "growth and compet"
  ,
  "combination therap"
  ,
  "emergency dpo"
  ,
  "conventional weapons", "weapons destruction"
  ,
  "fairtradeafrica"
  ,
  "blindness prevention", "avoidable blindness"
  )

disqualifying.sectors <- c(
  "Public finance management (PFM)"
  ,
  "Domestic revenue mobilisation"
  #,
  #"Mineral/mining policy and administrative management"
)


inclusion.keywords <- c(
  "inclus"
  ,
  "empower", "habiliter", "autorizar"
  ,
  "rights", "droits", "derechos"
)

employment.keywords <- c(
  "employ", "emplear", "empleo", "emploi"
  ,
  "travail", "trabajo"
  ,
  "job"
  ,
  "labour", "labor[.]", "labor "
  ,
  "cash for work"
  ,
  "vocation"
)

intellectual.keywords <- c(
  "intellect", "intelect"
  ,
  "cognitive", "cognitiva"
  ,
  "autistic", "austism", "autist"
  ,
  "special needs", "necesidades especiales", "besoins spéciau"
  ,
  "special education", "educación especial", "éducation spéciale"
  ,
  "learning diff", "learning disa", "difficultés d'apprentissage", "dificultades de aprendizaje", "discapacidad de aprendizaje", "trouble d'apprentissage"
)

crs$relevance <- "None"
crs[grepl(paste(minor.keywords, collapse = "|"), tolower(paste(crs$project_title, crs$short_description, crs$long_description)))]$relevance <- "Minor"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$long_description))]$relevance <- "Minor"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(paste(crs$short_description, crs$project_title)))]$relevance <- "Major"

crs$check <- "No"
crs[relevance == "Minor"]$check <- "potential false positive"
crs[relevance != "None"][purpose_name %in% disqualifying.sectors]$check <- "potential false negative"
crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$project_title, crs[relevance != "None"]$short_description, crs[relevance != "None"]$long_description)))]$check <- "potential false negative"

crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$project_title, crs[relevance != "None"]$short_description, crs[relevance != "None"]$long_description)))]$relevance <- "None"
crs[relevance != "None"][purpose_name %in% disqualifying.sectors]$relevance <- "None"

crs$inclusion <- "Not inclusion"
crs[relevance != "None"][grepl(paste(inclusion.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$project_title, crs[relevance != "None"]$short_description, crs[relevance != "None"]$long_description)))]$inclusion <- "Inclusion"

crs$employment <- "Not employment"
crs[relevance != "None"][grepl(paste(employment.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$project_title, crs[relevance != "None"]$short_description, crs[relevance != "None"]$long_description)))]$employment <- "Employment"

crs$intellectual <- "Not intellectual"
crs[relevance != "None"][grepl(paste(intellectual.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$project_title, crs[relevance != "None"]$short_description, crs[relevance != "None"]$long_description)))]$intellectual <- "intellectual"

crs$gender <- as.character(crs$gender)
crs[is.na(gender)]$gender <- "0"
crs[gender != "1" & gender != "2"]$gender <- "No gender component"
crs[gender == "1"]$gender <- "Partial gender component"
crs[gender == "2"]$gender <- "Major gender component"

save(crs, file="output/crs.RData", compression_level = 9)

crs.years <- dcast.data.table(crs, year ~ relevance + inclusion + employment + gender, value.var = "usd_disbursement_deflated", fun.aggregate = function (x) sum(x, na.rm=T))
crs.donors <- dcast.data.table(crs, year + donor_name ~ relevance + inclusion + employment + gender, value.var = "usd_disbursement_deflated", fun.aggregate = function (x) sum(x, na.rm=T))
crs.recipients <- dcast.data.table(crs, year + recipient_name ~ relevance + inclusion + employment + gender, value.var = "usd_disbursement_deflated", fun.aggregate = function (x) sum(x, na.rm=T))
crs.sectors <- dcast.data.table(crs, year + purpose_name ~ relevance + inclusion + employment + gender, value.var = "usd_disbursement_deflated", fun.aggregate = function (x) sum(x, na.rm=T))
crs.flows <- dcast.data.table(crs, year + flow_name ~ relevance + inclusion + employment + gender, value.var = "usd_disbursement_deflated", fun.aggregate = function (x) sum(x, na.rm=T))

crs.iw.donors <- dcast(crs[recipient_name %in% c("Bangladesh", "Kenya", "Nigeria", "Uganda")], year + donor_name ~ relevance + employment + recipient_name, value.var="usd_disbursement_deflated", fun.aggregate = function (x) sum(x, na.rm=T))
crs.intellectual <- crs[, .(value.id=sum(.SD[intellectual == "intellectual"]$usd_disbursement_deflated,na.rm=T), count.id=nrow(.SD[intellectual == "intellectual"]), value.od=sum(.SD[intellectual != "intellectual" & relevance != "None"]$usd_disbursement_deflated, na.rm=T), count.id=nrow(.SD[intellectual != "intellectual" & relevance != "None"])), by=.(year)]

fwrite(crs.years, "output/crs years.csv")
fwrite(crs.sectors, "output/crs sectors.csv")
fwrite(crs.flows, "output/crs flows.csv")
fwrite(crs.donors, "output/crs donors.csv")
fwrite(crs.recipients, "output/crs recipients.csv")
fwrite(crs.iw.donors, "output/crs iw donors.csv")
fwrite(crs.intellectual, "output/crs intellectual.csv")

tocheck.positive <- crs[check == "potential false positive"]
tocheck.negative <- crs[check == "potential false negative"]
fwrite(tocheck.positive, "output/crs check positives.csv")
fwrite(tocheck.negative, "output/crs check negatives.csv")