required.packages <- c("reshape2", "ggplot2", "data.table", "WDI")
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
  "source_iso3"
  ,
  "destination_Location_name"
  ,
  "destination_iso3"
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
fts <- unique(fts)
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

splitcols <- c("source_Location_name", "destination_Location_name", "destination_GlobalCluster_name")
fts.split <- fts
fts.split[fts.split==""] <- ".."

for(col in splitcols){
  newcol <- paste0(col, "_split")
  fts.split <- fts.split[, strsplit(get(col), " | ", fixed = T), by = names(fts.split)
                         ][,count:=(nchar(get(col)) - nchar(gsub("[|]", "", get(col))) + 1)
                           ][, amountUSD:=amountUSD/count , by = names(fts.split)
                             ]
  names(fts.split)[names(fts.split)=="V1"] <- newcol
  fts.split$count <- NULL
}

gdp <- as.data.table(WDI("all", c(gdp_constant_2010usd = "NY.GDP.MKTP.KD", gdp_current_usd = "NY.GDP.MKTP.CD"), start=2012, end=2018, extra=T))
gdp[, index2010:=gdp_current_usd/gdp_constant_2010usd, by=.(country, year)]
gdp[, .(index2016=index2010[year==2016]/index2010), by=.(country, year)]

fts.split <- merge(fts.split, gdp[, c("iso3c", "year", "index2016")], by.x=c("budgetYear", "source_iso3"), by.y=c("year", "iso3c"), all.x=T)
fts.split$amountUSD2016 <- fts.split$amountUSD / fts.split$index2016

fts.split$relevance <- "None"
fts.split[grepl(paste(minor.keywords, collapse = "|"), tolower(fts.split$description))]$relevance <- "Minor"
fts.split[grepl(paste(major.keywords, collapse = "|"), tolower(fts.split$description))]$relevance <- "Minor" #Maybe Major?
fts.split[grepl(paste(major.keywords, collapse = "|"), tolower(fts.split$destination_Project_name))]$relevance <- "Major"

fts.split[relevance != "None"][grepl(paste(disqualifying.keywords, collapse = "|"), tolower(paste(fts.split[relevance != "None"]$destination_Project_name, fts.split[relevance != "None"]$description)))]$relevance <- "None"

fts.split$inclusion <- "Not inclusion"
fts.split[relevance != "None"][grepl(paste(inclusion.keywords, collapse = "|"), tolower(paste(fts.split[relevance != "None"]$destination_Project_name, fts.split[relevance != "None"]$description)))]$inclusion <- "Inclusion"

fts.split$employment <- "Not employment"
fts.split[relevance != "None"][grepl(paste(employment.keywords, collapse = "|"), tolower(paste(fts.split[relevance != "None"]$destination_Project_name, fts.split[relevance != "None"]$description)))]$employment <- "Employment"

fts.years <- dcast.data.table(fts.split, budgetYear ~ relevance + inclusion + employment, value.var = "amountUSD2016", fun.aggregate = function (x) sum(x, na.rm=T))
fts.donors <- dcast.data.table(fts.split, source_Location_name_split ~ relevance + inclusion + employment, value.var = "amountUSD2016", fun.aggregate = function (x) sum(x, na.rm=T))
fts.recipients <- dcast.data.table(fts.split, destination_Location_name_split ~ relevance + inclusion + employment, value.var = "amountUSD2016", fun.aggregate = function (x) sum(x, na.rm=T))
fts.sectors <- dcast.data.table(fts.split, destination_GlobalCluster_name_split ~ relevance + inclusion + employment, value.var = "amountUSD2016", fun.aggregate = function (x) sum(x, na.rm=T))

fwrite(fts.years, "output/fts years.csv")
fwrite(fts.sectors, "output/fts sectors.csv")
fwrite(fts.donors, "output/fts donors.csv")
fwrite(fts.recipients, "output/fts recipients.csv")
