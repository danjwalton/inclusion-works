required.packages <- c("reshape2","ggplot2","data.table","foreign")
lapply(required.packages, require, character.only=T)

setwd("G:/My DriVe/Work/GitHub/inclusion-works/")

nga.ghsp.hhr <- read.spss("project-data/NGA_2012_GHSP-W2_v02_M_SPSS/Post Harvest Wave 2/Household/sect1_harvestw2.sav", to.data.frame=T)
names(nga.ghsp.hhr) <- attributes(nga.ghsp.hhr)[4]$variable.labels
nga.ghsp.health <- read.spss("project-data/NGA_2012_GHSP-W2_v02_M_SPSS/Post Harvest Wave 2/Household/sect4a_harvestw2.sav", to.data.frame=T)
names(nga.ghsp.health) <- attributes(nga.ghsp.health)[4]$variable.labels

nga.ghsp.health <- merge(nga.ghsp.hhr,nga.ghsp.health, by.x=c("HOUSEHOLD IDENTIFICATION","LINE NO."),by.y=c("HOUSEHOLD IDENTIFICATION","INDIVIDUAL ID"))

nga.ghsp.health <- as.data.table(nga.ghsp.health)

nga.ghsp.health$age.group <- floor(nga.ghsp.health$`AGE IN COMPLETED YEARS`/5)

#Create set of null entries for each age group and sex and append - this ensures that
#nga.ghsp.groups <- unique(subset(nga.ghsp.health,select=c("age.group","SEX")))
#nga.ghsp.health <- rbind(nga.ghsp.health,nga.ghsp.groups,fill=T)

nga.ghsp.health.cut1 <- nga.ghsp.health[,c(151,8,113,115,117,119,121)]
nga.ghsp.health.cut2 <- nga.ghsp.health[,c(151,8,106:110)]

nga.ghsp.health.melt1 <- melt(nga.ghsp.health.cut1, id.vars = c(1,2))
nga.ghsp.health.melt2 <- melt(nga.ghsp.health.cut2, id.vars = c(1,2))
nga.ghsp.health.melt1[which(nga.ghsp.health.melt1$value=="CANNOT HEAR")]$value <- "CANNOT DO"
nga.ghsp.health.agg1 <- dcast(nga.ghsp.health.melt1, variable + age.group + SEX ~ value, fun=length)
nga.ghsp.health.agg2 <- dcast(nga.ghsp.health.melt2, variable + age.group + SEX ~ value, fun=length)

nga.ghsp.health.agg1$`NA` <- as.double(nga.ghsp.health.agg1$`NA`)
nga.ghsp.health.agg1$`CANNOT DO` <- as.double(nga.ghsp.health.agg1$`CANNOT DO`)
nga.ghsp.health.agg1$`NO, NO DIFFICULTY` <- as.double(nga.ghsp.health.agg1$`NO, NO DIFFICULTY`)
nga.ghsp.health.agg1$`YES, A LOT` <- as.double(nga.ghsp.health.agg1$`YES, A LOT`)
nga.ghsp.health.agg1$`YES, SOME` <- as.double(nga.ghsp.health.agg1$`YES, SOME`)
nga.ghsp.health.agg1$count <- rowSums(nga.ghsp.health.agg1[,4:8])

for(i in 1:length(nga.ghsp.health.agg1$age.group)){
  nga.ghsp.health.agg1[i]
  nga.ghsp.health.agg1[i,4:8] <- nga.ghsp.health.agg1[i,4:8]/sum(nga.ghsp.health.agg1[i,4:8])
}

nga.ghsp.health.agg1 <- nga.ghsp.health.agg1[,c(1,2,3,5,7,8,6,4,9)]
