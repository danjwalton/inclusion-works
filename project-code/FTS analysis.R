required.packages <- c("reshape2", "ggplot2", "data.table")
lapply(required.packages, require, character.only = T)

wd <- "G:/My Drive/Work/GitHub/inclusion-works/"
setwd(wd)

fts <- fread("project-data/FTS.csv", encoding = "UTF-8")

keep <- c(
  "id"
  ,
  "refCode"
  ,
  "budgetYear"
  ,
  "method"
  ,
  "flowType"
  ,
  "source_Location_name"
  ,
  "destination_Location_name"
  ,
  "originalAmount"
  ,
  "amountUSD"
  ,
  "destination_GlobalCluster_name"
  ,
  "destination_Project_name"
  ,
  "description"

)

fts <- fts[, ..keep]

fts$amountUSD <- as.double(fts$amountUSD)

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

fts$major <- 0
fts[grepl(paste(major.keywords, collapse = "|"), tolower(fts$description))]$major <- 1
fts[grepl(paste(major.keywords, collapse = "|"), tolower(fts$destination_Project_name))]$major <- 2

fts$minor <- 0
fts[grepl(paste(minor.keywords, collapse = "|"), tolower(fts$description))]$minor <- 1

fts$disqualified <- 0
fts[major + minor > 0][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(fts[major + minor > 0]$destination_Project_name, fts[major + minor > 0]$description)))]$disqualified <- 1

years <- fts[,.(
  major=sum(amountUSD[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(amountUSD[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(amountUSD[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
  , major.share=sum(amountUSD[major == 2 & disqualified == 0], na.rm=T)/sum(amountUSD, na.rm=T)
  , minor.share=sum(amountUSD[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)/sum(amountUSD, na.rm=T)
)
, by=.(budgetYear)]

sectors <- fts[,.(
  major=sum(amountUSD[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(amountUSD[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(amountUSD[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
)
, by=.(destination_GlobalCluster_name)]

sectors <- sectors[major+minor>0]

sectors <- sectors[,
                   strsplit(destination_GlobalCluster_name, "|", fixed = T)
                   , by = .(major, minor, none, destination_GlobalCluster_name)
                   ][,V2:=(nchar(destination_GlobalCluster_name) - nchar(gsub("[|]", "", destination_GlobalCluster_name)) + 1)
                     ][,.(destination_GlobalCluster_name = trimws(V1),count = V2,major,minor,none)
                       ][, .(major=sum(major/count), minor=sum(minor/count), none=sum(none/count)), by=destination_GlobalCluster_name
                         ][,.(destination_GlobalCluster_name, major, minor, none, major.share=major/sum(major,minor,none), minor.share=minor/sum(major,minor,none))]

donors <- fts[,.(
  major=sum(amountUSD[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(amountUSD[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(amountUSD[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
  , major.share=sum(amountUSD[major == 2 & disqualified == 0], na.rm=T)/sum(amountUSD, na.rm=T)
  , minor.share=sum(amountUSD[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)/sum(amountUSD, na.rm=T)
)
, by=.(source_Location_name)]

donors <- donors[,
                 strsplit(source_Location_name, "|", fixed = T)
                 , by = .(major, minor, none, source_Location_name)
                 ][,V2:=(nchar(source_Location_name) - nchar(gsub("[|]", "", source_Location_name)) + 1)
                   ][,.(source_Location_name = trimws(V1),count = V2,major,minor,none)
                     ][, .(major=sum(major/count), minor=sum(minor/count), none=sum(none/count)), by=source_Location_name
                       ][,.(source_Location_name, major, minor, none, major.share=major/sum(major,minor,none), minor.share=minor/sum(major,minor,none))]

recipients <- fts[,.(
  major=sum(amountUSD[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(amountUSD[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(amountUSD[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
  , major.share=sum(amountUSD[major == 2 & disqualified == 0], na.rm=T)/sum(amountUSD, na.rm=T)
  , minor.share=sum(amountUSD[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)/sum(amountUSD, na.rm=T)
)
, by=.(destination_Location_name)]

recipients <- recipients[,
                 strsplit(destination_Location_name, "|", fixed = T)
                 , by = .(major, minor, none, destination_Location_name)
                 ][,V2:=(nchar(destination_Location_name) - nchar(gsub("[|]", "", destination_Location_name)) + 1)
                   ][,.(destination_Location_name = trimws(V1),count = V2,major,minor,none)
                     ][, .(major=sum(major/count), minor=sum(minor/count), none=sum(none/count)), by=destination_Location_name
                       ][,.(destination_Location_name, major, minor, none, major.share=major/sum(major,minor,none), minor.share=minor/sum(major,minor,none))]

tocheck <- fts[minor==1 | disqualified == 1]
