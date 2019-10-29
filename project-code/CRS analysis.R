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
crs <- crs[flow_name == "ODA Loans" | flow_name == "ODA Grants"]

major.keywords <- c(
  "disab", "discapaci", "incapaci", "minusválido", "invalidit", "infirmité"
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
  "spina"
  ,
  "hydrocephalus", "hidrocefalia", "l'hydrocéphalie"
  ,
  "diabetes", "diabète"
  ,
  "atlas alliance", "atlas allinance"
  ,
  "dpos ", "dpo ", "dpo's", "dpos[.]", "dpo[.]", "dpo's[.]"
)

minor.keywords <- c(
  "war victim", "victimas de guerra", "victimes de guerre"
  ,
  "landmine victim", "victime de mine", "víctima de minas terrestres"
  ,
  #"wounded"
  #,
  #"injured", "injuries"
  #,
  "therapy", "terapia", "thérapie"
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
  "rehydration therapy"
  ,
  "-dpo", "cidpo", "hdpo", "dpo series", "financial sector dpo", "dpo (ri)"
  ,
  "growth and compet"
  ,
  "combination therap"
  )

disqualifying.sectors <- c(
  "Public finance management (PFM)"
  ,
  "Domestic revenue mobilisation"
  ,
  "Mineral/mining policy and administrative management"
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
)

crs$relevance <- "None"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$long_description))]$relevance <- "Minor"
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$short_description))]$relevance <- "Major"
crs[grepl(paste(minor.keywords, collapse = "|"), tolower(crs$project_title))]$relevance <- "Major"

crs[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$project_title, crs[relevance != "None"]$short_description, crs[relevance != "None"]$long_description)))]$relevance <- "None"
crs[relevance != "None"][purpose_name %in% disqualifying.sectors]$relevance <- "None"

crs$inclusion <- "Not inclusion"
crs[relevance != "None"][grepl(paste(inclusion.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$project_title, crs[relevance != "None"]$short_description, crs[relevance != "None"]$long_description)))]$inclusion <- "Inclusion"

crs$employment <- "Not employment"
crs[relevance != "None"][grepl(paste(employment.keywords, collapse = "|"), tolower(paste(crs[relevance != "None"]$project_title, crs[relevance != "None"]$short_description, crs[relevance != "None"]$long_description)))]$employment <- "Employment"

crs.years <- dcast.data.table(crs, year ~ relevance + inclusion + employment, value.var = "usd_disbursement_deflated", fun.aggregate = function (x) sum(x, na.rm=T))
crs.donors <- dcast.data.table(crs, donor_name ~ relevance + inclusion + employment, value.var = "usd_disbursement_deflated", fun.aggregate = function (x) sum(x, na.rm=T))
crs.recipients <- dcast.data.table(crs, recipient_name ~ relevance + inclusion + employment, value.var = "usd_disbursement_deflated", fun.aggregate = function (x) sum(x, na.rm=T))
crs.sectors <- dcast.data.table(crs, purpose_name ~ relevance + inclusion + employment, value.var = "usd_disbursement_deflated", fun.aggregate = function (x) sum(x, na.rm=T))

fwrite(crs.years, "output/crs years.csv")

fwrite(crs.sectors, "output/crs sectors.csv")

fwrite(crs.donors, "output/crs donors.csv")

fwrite(crs.recipients, "output/crs recipients.csv")