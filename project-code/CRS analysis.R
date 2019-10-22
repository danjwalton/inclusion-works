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

crs$major <- 0
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$long_description))]$major <- 1
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$short_description))]$major <- 2
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$project_title))]$major <- 2

crs$minor <- 0
crs[grepl(paste(minor.keywords, collapse = "|"), tolower(paste(crs$project_title, crs$short_description, crs$long_description)))]$minor <- 1

crs$inclusion <- 0
crs[major + minor > 0][grepl(paste(inclusion.keywords, collapse = "|"), tolower(paste(crs[major + minor > 0]$project_title, crs[minor+major>0]$short_description, crs[major + minor > 0]$long_description)))]$inclusion <- 1

crs$disqualified <- 0
crs[major + minor > 0][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[major + minor > 0]$project_title, crs[minor+major>0]$short_description, crs[major + minor > 0]$long_description)))]$disqualified <- 1
crs[major + minor > 0][purpose_name %in% disqualifying.sectors]$disqualified <- 1

years <- crs[,.(
  total=sum(usd_disbursement_deflated[major + minor > 0 & disqualified == 0], na.rm=T)
  , major=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(usd_disbursement_deflated[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
  , total.inclusive=sum(usd_disbursement_deflated[major + minor > 0 & disqualified == 0 & inclusion == 1], na.rm=T)
  , major.inclusive=sum(usd_disbursement_deflated[major == 2 & disqualified == 0 & inclusion == 1], na.rm=T)
  , minor.inclusive=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0 & inclusion == 1], na.rm=T)
  , total.share=sum(usd_disbursement_deflated[major + minor > 0 & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , major.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , total.inclusive.share=sum(usd_disbursement_deflated[major + minor > 0 & disqualified == 0 & inclusion == 1], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , major.inclusive.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0 & inclusion == 1], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.inclusive.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0 & inclusion == 1], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  )
  , by=.(year)]

fwrite(years, "output/crs years.csv")

sectors <- crs[,.(
  major=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(usd_disbursement_deflated[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
  , major.inclusive=sum(usd_disbursement_deflated[major == 2 & disqualified == 0 & inclusion == 1], na.rm=T)
  , minor.inclusive=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0 & inclusion == 1], na.rm=T)
  , major.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , major.inclusive.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0 & inclusion == 1], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.inclusive.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0 & inclusion == 1], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
)
  , by=.(purpose_name)]
sectors <- sectors[major+minor>0]

fwrite(sectors, "output/crs sectors.csv")

donors <- crs[,.(
  major=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(usd_disbursement_deflated[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
  , major.inclusive=sum(usd_disbursement_deflated[major == 2 & disqualified == 0 & inclusion == 1], na.rm=T)
  , minor.inclusive=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0 & inclusion == 1], na.rm=T)
  , major.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , major.inclusive.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0 & inclusion == 1], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.inclusive.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0 & inclusion == 1], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
)
, by=.(donor_name)]

fwrite(donors, "output/crs donors.csv")

recipients <- crs[,.(
  major=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(usd_disbursement_deflated[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
  , major.inclusive=sum(usd_disbursement_deflated[major == 2 & disqualified == 0 & inclusion == 1], na.rm=T)
  , minor.inclusive=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0 & inclusion == 1], na.rm=T)
  , major.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , major.inclusive.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0 & inclusion == 1], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.inclusive.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0 & inclusion == 1], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
)
, by=.(recipient_name)]

fwrite(recipients, "output/crs recipients.csv")

tocheck <- crs[minor==1 | disqualified == 1]
