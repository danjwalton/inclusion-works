required.packages <- c("reshape2", "ggplot2", "data.table")
lapply(required.packages, require, character.only = T)

wd <- "G:/My Drive/Work/GitHub/inclusion-works/"
setwd(wd)

load("project-data/crs.RData")

keep <- c(
  "crs_id"
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

major.keywords <- c(
  "disabl"
  #,
  #"disorder"
  ,
  "impairment", "impaired"
  ,
  "pwd", "gwd", "cwd"
  ,
  "chronic health", "chronic ill"
  ,
  "deaf"
  ,
  "blind"
  ,
  "special needs"
  ,
  "autistic", "autism"
  ,
  "mental health"
  ,
  "prosthes"
  ,
  "amputation", "amputee", "amputate"
  ,
  "schizophreni"
  ,
  "sign language"
  ,
  "arthriti"
  ,
  "rheumat"
  ,
  "dementia"
)

minor.keywords <- c(
  "war victim"
  ,
  "landmine"
  ,
  "wounded"
  ,
  "injured", "injuries"
  ,
  "therapy"
)

disqualifying.keywords <- c(
  "chronic malnutrition"
  ,
  "mole rat"
  ,
  "cgpwd"
  ,
  "road"
  ,
  "highway"
  ,
  "environmental health"
  )

crs$major <- 0
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$long_description))]$major <- 1
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$short_description))]$major <- 2
crs[grepl(paste(major.keywords, collapse = "|"), tolower(crs$project_title))]$major <- 2

crs$minor <- 0
crs[grepl(paste(minor.keywords, collapse = "|"), tolower(paste(crs$project_title, crs$short_description, crs$long_description)))]$minor <- 1

crs$disqualified <- 0
crs[major + minor > 0][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(crs[major + minor > 0]$project_title, crs[minor+major>0]$short_description, crs[major + minor > 0]$long_description)))]$disqualified <- 1

years <- crs[,.(
  major=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(usd_disbursement_deflated[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
  , major.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  )
  , by=.(year)]

sectors <- crs[,.(
  major=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(usd_disbursement_deflated[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
  , major.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  )
  , by=.(purpose_name)]
sectors <- sectors[major+minor>0]

donors <- crs[,.(
  major=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(usd_disbursement_deflated[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
  , major.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
)
, by=.(donor_name)]

recipients <- crs[,.(
  major=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)
  , minor=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)
  , none=sum(usd_disbursement_deflated[(major == 0 & minor == 0) | disqualified == 1], na.rm=T)
  , major.share=sum(usd_disbursement_deflated[major == 2 & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
  , minor.share=sum(usd_disbursement_deflated[(major == 1 | (minor ==1 & major < 2)) & disqualified == 0], na.rm=T)/sum(usd_disbursement_deflated, na.rm=T)
)
, by=.(recipient_name)]
