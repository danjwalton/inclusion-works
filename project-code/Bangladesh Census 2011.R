required.packages <- c("reshape2","ggplot2","data.table","foreign","ipumsr")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/inclusion-works")
ddi <- read_ipums_ddi("project-data/Bangladesh Census 2011/ipumsi_00003.xml")
data <- as.data.table(read_ipums_micro(ddi))

data$age.group <- NA 
data$age.group[which(data$AGE != 999)] <- ceiling(data$AGE/5)

data$employment <- "unemployed"
data$employment[which(data$BD2011A_EMPSTAT == 1)] <- "employed"
#data$employment[which(data$BD2011A_EMPSTAT == 3)] <- "employed" #This is house work
data$employment[which(data$BD2011A_EMPSTAT == 9)] <- NA

labs <- attributes(data$BD2011A_DISAB)$labels
data$domain <- names(labs)[match(data$BD2011A_DISAB,labs)]

data$disabled <- "Not disabled"
data$disabled[which(data$BD2011A_DISAB > 0)] <- "Disabled"

labs <- attributes(data$DHS_IPUMSI_BD)$labels
data$region <- names(labs)[match(data$DHS_IPUMSI_BD,labs)]

data$education <- "No education or preschool"
data$education[which(data$BD2011A_EDATTAN >= 5)] <- "Primary"
data$education[which(data$BD2011A_EDATTAN >= 10)] <- "Secondary"
data$education[which(data$BD2011A_EDATTAN >= 12)] <- "Higher"
data$education[which(data$BD2011A_EDATTAN == 99 | data$AGE < 15)] <- NA

data$sex.lab <- "Male"
data$sex.lab[which(data$SEX == 2)] <- "Female"
data$sex.lab[which(data$SEX == 9)] <- NA

data$working.age <- "No"
data$working.age[data$age.group>=4&data$age.group<=13] <- "Yes"

data.melt <- data[,c("PERWT","employment","domain","disabled","working.age","sex.lab","education","region")]
data.domains <- dcast.data.table(data.melt, employment + working.age + sex.lab + education + region ~ domain, fun=sum, value.var = "PERWT")
data.domains$count <- rowSums(data.domains[,c(6:12)])
data.domains[,c(6:12)] <- data.domains[,c(6:12)]/rowSums(data.domains[,c(6:12)])

#data.domains <- subset(melt(data.domains, c(1:3,11)), variable != "No disability")
#data.domains$no.value <- 1-data.domains$value

data.overall <- dcast.data.table(data.melt, employment + working.age + sex.lab + education + region ~ disabled, fun=sum, value.var="PERWT")
data.overall$count <- rowSums(data.overall[,c(6:7)])
data.overall[,c(6:7)] <- data.overall[,c(6:7)]/rowSums(data.overall[,c(6:7)])

#Create dummy label for TOTAL
totallabel <- as.data.frame("Total")
names(totallabel) <- "sex.lab"

###OUTPUTS###

#OVERALL PREVALENCE RATE
data.overall.all <- data.overall[, .(Disabled = sum(Disabled*count)/sum(count), `Not disabled` = sum(`Not disabled`*count)/sum(count)), by=.(sex.lab)]
data.overall.all <- rbind(data.overall.all, cbind(totallabel, data.overall[, .(Disabled = sum(Disabled*count)/sum(count), `Not disabled` = sum(`Not disabled`*count)/sum(count))]))
fwrite(data.overall.working, "output/BGD CEN all ages overall.csv")

#SHARE OF WORKING AGE POPULATION WHO ARE DISABLED, BY SEX
data.overall.working <- data.overall[working.age=="Yes", .(Disabled = sum(Disabled*count)/sum(count),
                                                           `Not disabled` = sum(`Not disabled`*count)/sum(count)), by=.(sex.lab)]
data.overall.working <- rbind(data.overall.working,
                              cbind(totallabel,
                                    data.overall[working.age=="Yes", .(Disabled = sum(Disabled*count)/sum(count),
                                                                       `Not disabled` = sum(`Not disabled`*count)/sum(count))]))
fwrite(data.overall.working, "output/BGD CEN WA overall.csv")

#SHARE OF WORKING AGE POPULATION WHO ARE DISABLED, BY SEX AND DOMAIN
data.domains.working <- data.domains[working.age=="Yes",
                                     .(Autistic = sum(Autistic*count)/sum(count),
                                       Hearing = sum(Hearing*count)/sum(count),
                                       Mental = sum(Mental*count)/sum(count),
                                       Physical = sum(Physical*count)/sum(count),
                                       Speech = sum(Speech*count)/sum(count),
                                       Vision = sum(Vision*count)/sum(count)),
                                     by=.(sex.lab)]

data.domains.working <- rbind(data.domains.working,
                              cbind(totallabel,
                                    data.domains[working.age=="Yes",
                                                 .(Autistic = sum(Autistic*count)/sum(count),
                                                   Hearing = sum(Hearing*count)/sum(count),
                                                   Mental = sum(Mental*count)/sum(count),
                                                   Physical = sum(Physical*count)/sum(count),
                                                   Speech = sum(Speech*count)/sum(count),
                                                   Vision = sum(Vision*count)/sum(count))]
                                    )
                              )
fwrite(data.domains.working, "output/BGD CEN WA domains.csv")

#WORKING AGE POPULATION WHO ARE DISABLED, BY GEOGRAPHY AND DOMAIN
data.overall.geog <- data.overall[working.age=="Yes", .(Disabled = sum(Disabled*count)/sum(count),
                                                           `Not disabled` = sum(`Not disabled`*count)/sum(count)), by=.(region)]
data.domains.geog <- data.domains[working.age=="Yes",
                                     .(Autistic = sum(Autistic*count)/sum(count),
                                       Hearing = sum(Hearing*count)/sum(count),
                                       Mental = sum(Mental*count)/sum(count),
                                       Physical = sum(Physical*count)/sum(count),
                                       Speech = sum(Speech*count)/sum(count),
                                       Vision = sum(Vision*count)/sum(count)),
                                     by=.(region)]

data.geog <- cbind(data.domains.geog,data.overall.geog[,-1])
fwrite(data.geog, "output/BGD CEN WA regions.csv")

#EMPLOYMENT RATE FOR WORKING AGE PWDS AND PWODS, BY SEX AND EDUCATION LEVEL
data.overall.employ1 <- data.overall[working.age=="Yes",
                                    .(Disabled = sum(Disabled*count),
                                      `Not disabled` = sum(`Not disabled`*count)),
                                    by=.(employment,education,sex.lab)]

data.overall.employ1 <- cbind(data.table(employment=c("employed","unemployed")),
                              data.overall.employ1[,.(Disabled = Disabled/sum(Disabled),
                                                      `Not disabled` = `Not disabled`/sum(`Not disabled`)),
                                                   by=.(sex.lab, education)])

data.overall.employ2 <- data.overall[working.age=="Yes",
                                     .(Disabled = sum(Disabled*count),
                                      `Not disabled` = sum(`Not disabled`*count)),
                                     by=.(employment, education)]

data.overall.employ2 <- cbind(data.table(employment=c("employed","unemployed")),
                              totallabel,
                              data.overall.employ2[,.(Disabled = Disabled/sum(Disabled),
                                                      `Not disabled` = `Not disabled`/sum(`Not disabled`)), by=.(education)]
                              )

data.overall.employ <- rbind(data.overall.employ1,data.overall.employ2)
data.overall.employ <- melt(data.overall.employ, c(1:3))
data.overall.employ <- data.overall.employ[complete.cases(data.overall.employ)]
data.overall.employ.edu <- dcast.data.table(data.overall.employ[employment=="employed" & sex.lab=="Total"], variable ~ education)
order <- c("variable","No education or preschool","Primary","Secondary","Higher")
data.overall.employ.edu <- data.overall.employ.edu[,..order]
fwrite(data.overall.employ.edu, "output/BGD CEN WA education employment.csv")


data.overall.employ1 <- data.overall[working.age=="Yes",
                                     .(Disabled = sum(Disabled*count),
                                       `Not disabled` = sum(`Not disabled`*count)),
                                     by=.(employment,sex.lab)]

data.overall.employ1 <- cbind(data.table(employment=c("employed","unemployed")),
                              data.overall.employ1[,.(Disabled = Disabled/sum(Disabled),
                                                      `Not disabled` = `Not disabled`/sum(`Not disabled`)),
                                                   by=.(sex.lab)])

data.overall.employ2 <- data.overall[working.age=="Yes",
                                     .(Disabled = sum(Disabled*count),
                                       `Not disabled` = sum(`Not disabled`*count)),
                                     by=.(employment)]

data.overall.employ2 <- cbind(data.table(employment=c("employed","unemployed")),
                              totallabel,
                              data.overall.employ2[,.(Disabled = Disabled/sum(Disabled),
                                                      `Not disabled` = `Not disabled`/sum(`Not disabled`))]
)

data.overall.employ <- rbind(data.overall.employ1,data.overall.employ2)
data.overall.employ <- melt(data.overall.employ, c(1:2))
data.overall.employ <- data.overall.employ[complete.cases(data.overall.employ)]
data.overall.employ.sex <- dcast.data.table(data.overall.employ[employment=="employed"], variable ~ sex.lab)
fwrite(data.overall.employ.sex, "output/BGD CEN WA overall employment.csv")


#EMPLOYMENT RATE FOR WORKING AGE PWDS, BY SEX AND DOMAIN
data.domains.employ1 <- data.domains[working.age=="Yes",
                                     .(Autistic = sum(Autistic*count),
                                       Hearing = sum(Hearing*count),
                                       Mental = sum(Mental*count),
                                       Physical = sum(Physical*count),
                                       Speech = sum(Speech*count),
                                       Vision = sum(Vision*count)),
                                     by=.(employment,sex.lab)]

data.domains.employ1 <- cbind(data.table(employment=c("employed","unemployed")),
                              data.domains.employ1[,
                                                   .(Autistic = Autistic/sum(Autistic),
                                                     Hearing = Hearing/sum(Hearing),
                                                     Mental = Mental/sum(Mental),
                                                     Physical = Physical/sum(Physical),
                                                     Speech = Speech/sum(Speech),
                                                     Vision = Vision/sum(Vision)),
                                                   by=.(sex.lab)])

data.domains.employ2 <- data.domains[working.age=="Yes",
                                     .(Autistic = sum(Autistic*count),
                                       Hearing = sum(Hearing*count),
                                       Mental = sum(Mental*count),
                                       Physical = sum(Physical*count),
                                       Speech = sum(Speech*count),
                                       Vision = sum(Vision*count)),
                                     by=.(employment)]

data.domains.employ2 <- cbind(data.table(employment=c("employed","unemployed")),
                              totallabel,
                              data.domains.employ2[,.(Autistic = Autistic/sum(Autistic),
                                                      Hearing = Hearing/sum(Hearing),
                                                      Mental = Mental/sum(Mental),
                                                      Physical = Physical/sum(Physical),
                                                      Speech = Speech/sum(Speech),
                                                      Vision = Vision/sum(Vision))])

data.domains.employ <- rbind(data.domains.employ1,data.domains.employ2)
data.domains.employ <- melt(data.domains.employ, c(1:2))
data.domains.employ <- data.domains.employ[complete.cases(data.domains.employ)]
data.domains.employ.sex <- dcast.data.table(data.domains.employ[employment=="employed"], variable ~ sex.lab)
fwrite(data.domains.employ.sex, "output/BGD CEN WA domains employment.csv")