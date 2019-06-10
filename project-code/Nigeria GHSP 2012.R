#TODO - education disaggregation

required.packages <- c("reshape2","ggplot2","data.table","foreign")
lapply(required.packages, require, character.only=T)

setwd("G:/My DriVe/Work/GitHub/inclusion-works/")

#Pull post-harvest household roster, section 3a (employment) and section 4a (health)
ghsp.hhr <- read.spss("project-data/NGA_2012_GHSP-W2_v02_M_SPSS/Post Harvest Wave 2/Household/sect1_harvestw2.sav", to.data.frame=T)
names(ghsp.hhr) <- attributes(ghsp.hhr)[4]$variable.labels
ghsp.employ <- read.spss("project-data/NGA_2012_GHSP-W2_v02_M_SPSS/Post Harvest Wave 2/Household/sect3a_harvestw2.sav", to.data.frame=T)
names(ghsp.employ) <- attributes(ghsp.employ)[4]$variable.labels
names(ghsp.employ)[43:66] <- paste(names(ghsp.employ)[43:66],"JOB 2")
ghsp.health <- read.spss("project-data/NGA_2012_GHSP-W2_v02_M_SPSS/Post Harvest Wave 2/Household/sect4a_harvestw2.sav", to.data.frame=T)
names(ghsp.health) <- attributes(ghsp.health)[4]$variable.labels

#Merge and convert to data.table
ghsp.health <- merge(ghsp.hhr,ghsp.health, by.x=c("HOUSEHOLD IDENTIFICATION","LINE NO."),by.y=c("HOUSEHOLD IDENTIFICATION","INDIVIDUAL ID"))
ghsp.health <- as.data.table(ghsp.health)
ghsp.health.employ <- merge(ghsp.health,ghsp.employ,by.x=c("HOUSEHOLD IDENTIFICATION","LINE NO."),by.y=c("HOUSEHOLD IDENTIFICATION","INDIVIDUAL ID"))

#Create age groups of 5 year intervals
ghsp.health.employ$age.group <- floor(ghsp.health.employ$`AGE IN COMPLETED YEARS`/5)

#Create employment indicator - this hierachical.. may need to discuss what terms
ghsp.health.employ$employment <- "unemployed"
ghsp.health.employ$employment[which(ghsp.health.employ[,157]=="YES")] <- "employed household farm"
ghsp.health.employ$employment[which(ghsp.health.employ[,158]=="YES")] <- "self employed"
ghsp.health.employ$employment[which(ghsp.health.employ[,156]=="YES")] <- "employed 3rd party"
ghsp.health.employ$employment[which(is.na(ghsp.health.employ[,159]))] <- NA

#Create set of null entries for each age group and sex and append - this ensures that
#ghsp.groups <- unique(subset(ghsp.health,select=c("age.group","SEX")))
#ghsp.health <- rbind(ghsp.health,ghsp.groups,fill=T)

#Separate two sets of impairment questions
ghsp.wg.cut <- ghsp.health.employ[,c(8,111,113,115,117,119,121,228,229)]
ghsp.other.cut <- ghsp.health.employ[,c(8,106:110,228,229)]

#Replace "CANNOT SEE" and "CANNOT HEAR" responses with "CANNOT DO"
levels(ghsp.wg.cut$`DO YOU HAVE DIFFICULTY SEEING, EVEN IF YOU ARE WEARING GLASSES?`)[match("CANNOT SEE", levels(ghsp.wg.cut$`DO YOU HAVE DIFFICULTY SEEING, EVEN IF YOU ARE WEARING GLASSES?`))] <- "CANNOT DO"
levels(ghsp.wg.cut$`DO YOU HAVE DIFFICULTY HEARING, EVEN IF YOU ARE WEARING A HEARING AID?`)[match("CANNOT HEAR", levels(ghsp.wg.cut$`DO YOU HAVE DIFFICULTY HEARING, EVEN IF YOU ARE WEARING A HEARING AID?`))] <- "CANNOT DO"

#Create "Disabled/Not disabled/NA" indicator based on WG criteria
ghsp.wg.cut$disabled <- "Not disabled"
ghsp.wg.cut[which(rowSums(ghsp.wg.cut=="CANNOT DO"|ghsp.wg.cut=="YES, A LOT",na.rm=T)>0L)]$disabled <- "Disabled"
ghsp.wg.cut[which(rowSums(is.na(ghsp.wg.cut),na.rm=T)>0L)]$disabled <- NA

#Melt and cast
ghsp.wg.melt <- melt(ghsp.wg.cut, id.vars = c(1,8,9), na.rm=F)
ghsp.other.melt <- melt(ghsp.other.cut, id.vars = c(1,7,8), na.rm=F)
ghsp.wg <- dcast(ghsp.wg.melt, variable + age.group + SEX + employment ~ value, fun=length)
ghsp.other <- dcast(ghsp.other.melt, variable + age.group + SEX + employment ~ value, fun=length)
 
#Revert to data.frame as data.table doesn't like being coerced by sapply
ghsp.wg <- as.data.frame(ghsp.wg)
ghsp.wg[,5:11] <- sapply(ghsp.wg[,5:11], as.double)

#Tidy columns
ghsp.wg <- ghsp.wg[,c(1,2,3,4,6,8,5,9,10,7,11)]

#Calculate percentages across rows
ghsp.wg$count <- rowSums(ghsp.wg[,c(5:11)])
for(i in 1:length(ghsp.wg$age.group)){
  ghsp.wg[i,5:11] <- ghsp.wg[i,5:11]/sum(ghsp.wg[i,5:11])
}

#Assign working-age column for those between 15-64
ghsp.wg$working.age <- "No"
ghsp.wg$working.age[ghsp.wg$age.group>=4&ghsp.wg$age.group<=12] <- "Yes"

#Split dichotomous disability from domains
ghsp.wg.domains <- ghsp.wg[,c(1,2,3,4,13,5,9,11,6,7,12)]
ghsp.wg.domains <- subset(ghsp.wg.domains, variable != "disabled")
ghsp.wg.overall <- subset(ghsp.wg, variable == "disabled")[,c(1,2,3,4,13,10,8,7,12)]

ghsp.wg.domains$impaired <- ghsp.wg.domains$`CANNOT DO`+ghsp.wg.domains$`YES, A LOT`
ghsp.wg.domains$`not impaired` <- ghsp.wg.domains$`YES, SOME`+ghsp.wg.domains$`NO, NO DIFFICULTY`

ghsp.wg.overall.working <- as.data.table(ghsp.wg.overall)[working.age=="Yes", .(disabled=sum(count*Disabled)/sum(count),`not disabled`=sum(count*`Not disabled`)/sum(count),na=sum(count*`NA`)/sum(count)), by=.(employment)]
ghsp.wg.domains.working <- as.data.table(ghsp.wg.domains)[working.age=="Yes", .(impaired=sum(count*impaired)/sum(count),`not impaired`=sum(count*`not impaired`)/sum(count),na=sum(count*`NA`)/sum(count)), by=.(variable,employment)]

ghsp.wg.disabled.working <- as.data.table(ghsp.wg.overall)[working.age=="Yes", .(disabled=sum(count*Disabled),not.disabled=sum(count*`Not disabled`)),by=employment]
ghsp.wg.disabled.working$disabled <- ghsp.wg.disabled.working$disabled/sum(ghsp.wg.overall$Disabled[ghsp.wg.overall$working.age=="Yes"]*ghsp.wg.overall$count[ghsp.wg.overall$working.age=="Yes"])
ghsp.wg.disabled.working$not.disabled <- ghsp.wg.disabled.working$not.disabled/sum(ghsp.wg.overall$`Not disabled`[ghsp.wg.overall$working.age=="Yes"]*ghsp.wg.overall$count[ghsp.wg.overall$working.age=="Yes"])

write.csv(ghsp.wg.domains.working,"output/GHSP WG WA domains.csv")
write.csv(ghsp.wg.overall.working,"output/GHSP WG WA overall.csv")
write.csv(ghsp.wg.disabled.working,"output/GHSP WG WA disabled employment.csv")

rm(list=c("ghsp.wg.cut","ghsp.wg.melt","ghsp.other.cut","ghsp.other.melt","ghsp.wg","ghsp.health","ghsp.hhr","ghsp.employ","ghsp.health.employ","ghsp.wg.domains","ghsp.wg.overall"))
