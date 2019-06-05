required.packages <- c("reshape2","ggplot2","data.table","foreign")
lapply(required.packages, require, character.only=T)

setwd("G:/My DriVe/Work/GitHub/inclusion-works/")

#Pull post-harvest household roster and section 4a (health)
ghsp.hhr <- read.spss("project-data/NGA_2012_GHSP-W2_v02_M_SPSS/Post Harvest Wave 2/Household/sect1_harvestw2.sav", to.data.frame=T)
names(ghsp.hhr) <- attributes(ghsp.hhr)[4]$variable.labels
ghsp.health <- read.spss("project-data/NGA_2012_GHSP-W2_v02_M_SPSS/Post Harvest Wave 2/Household/sect4a_harvestw2.sav", to.data.frame=T)
names(ghsp.health) <- attributes(ghsp.health)[4]$variable.labels

#Merge and convert to data.table
ghsp.health <- merge(ghsp.hhr,ghsp.health, by.x=c("HOUSEHOLD IDENTIFICATION","LINE NO."),by.y=c("HOUSEHOLD IDENTIFICATION","INDIVIDUAL ID"))
ghsp.health <- as.data.table(ghsp.health)

#Create age groups of 5 year intervals
ghsp.health$age.group <- floor(ghsp.health$`AGE IN COMPLETED YEARS`/5)

#Create set of null entries for each age group and sex and append - this ensures that
#ghsp.groups <- unique(subset(ghsp.health,select=c("age.group","SEX")))
#ghsp.health <- rbind(ghsp.health,ghsp.groups,fill=T)

#Separate two sets of impairment questions
ghsp.wg.cut <- ghsp.health[,c(151,8,111,113,115,117,119,121)]
ghsp.other.cut <- ghsp.health[,c(151,8,106:110)]

#Replace "CANNOT SEE" and "CANNOT HEAR" responses with "CANNOT DO"
levels(ghsp.wg.cut$`DO YOU HAVE DIFFICULTY SEEING, EVEN IF YOU ARE WEARING GLASSES?`)[match("CANNOT SEE", levels(ghsp.wg.cut$`DO YOU HAVE DIFFICULTY SEEING, EVEN IF YOU ARE WEARING GLASSES?`))] <- "CANNOT DO"
levels(ghsp.wg.cut$`DO YOU HAVE DIFFICULTY HEARING, EVEN IF YOU ARE WEARING A HEARING AID?`)[match("CANNOT HEAR", levels(ghsp.wg.cut$`DO YOU HAVE DIFFICULTY HEARING, EVEN IF YOU ARE WEARING A HEARING AID?`))] <- "CANNOT DO"

#Create "Disabled/Not disabled/NA" indicator based on WG criteria
ghsp.wg.cut$disabled <- "Not disabled"
ghsp.wg.cut[which(rowSums(ghsp.wg.cut=="CANNOT DO"|ghsp.wg.cut=="YES, A LOT",na.rm=T)>0L)]$disabled <- "Disabled"
ghsp.wg.cut[which(rowSums(is.na(ghsp.wg.cut),na.rm=T)>0L)]$disabled <- "NA"

#Melt and cast
ghsp.wg.melt <- melt(ghsp.wg.cut, id.vars = c(1,2), na.rm=F)
ghsp.other.melt <- melt(ghsp.other.cut, id.vars = c(1,2), na.rm=F)
ghsp.wg <- dcast(ghsp.wg.melt, variable + age.group + SEX ~ value, fun=length)
ghsp.other <- dcast(ghsp.other.melt, variable + age.group + SEX ~ value, fun=length)
 
#Revert to data.frame as data.table doesn't like being coerced by sapply
ghsp.wg <- as.data.frame(ghsp.wg)
ghsp.wg[,4:11] <- sapply(ghsp.wg[,4:11], as.double)

#Tidy columns
ghsp.wg <- ghsp.wg[,c(1,2,3,6,9,7,5,10,11,8,4)]

#Calculate percentages across rows
ghsp.wg$count <- rowSums(ghsp.wg[,c(4:11)])
for(i in 1:length(ghsp.wg$age.group)){
  ghsp.wg[i,4:11] <- ghsp.wg[i,4:11]/sum(ghsp.wg[i,4:11])
}

#Split dichotomous disability from domains
ghsp.wg.domains <- ghsp.wg[,c(1,2,3,7:12)]
ghsp.wg.domains <- subset(ghsp.wg.domains, variable != "disabled")
ghsp.wg.overall <- subset(ghsp.wg, variable == "disabled")[,c(1:6,12)]

write.csv(ghsp.wg.domains,"output/GHSP WG domains.csv")
write.csv(ghsp.wg.overall,"output/GHSP WG overall.csv")

rm(list=c("ghsp.wg.cut","ghsp.wg.melt","ghsp.other.cut","ghsp.other.melt","ghsp.wg","ghsp.health","ghsp.hhr"))
