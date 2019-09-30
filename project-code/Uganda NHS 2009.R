required.packages <- c("reshape2","ggplot2","data.table","bit64")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/inclusion-works")

unhs <- fread("project-data/UNHS/UNHS labour + disability.csv")
#unhs <- unique(fread("project-data/UNHS/UNHS disability.csv")
unhs[unhs=="Don't know"] <- 999
unhs[is.na(unhs)] <- 999
unhs <- data.table(sapply(unhs, as.numeric))

unhs$`Employment status` <- as.numeric(NA)
unhs[
  #CHK1 == 2 | 
  CHK2 == 2 | 
  CHK3 == 2
  ]$`Employment status` <- 0

unhs[
  #CHK1 == 1 |
  CHK2 == 1 |
  CHK3 == 1
  ]$`Employment status` <- 1

unhs <- unhs[!is.na(`Employment status`)]

unhs$Sex <- "Male"
unhs[which(unhs$Sex_code==2)]$Sex <- "Female"
unhs$working.age <- "No"
unhs[which(unhs$Age>=14 & unhs$Age<=65)]$working.age <- "Yes"
unhs$employed <- "unemployed"
unhs[which(unhs$`Employment status`==1)]$employed <- "employed"
unhs$Region.chr <- as.character(NA)
unhs[which(unhs$Region==1)]$Region.chr <- "Central"
unhs[which(unhs$Region==2)]$Region.chr <- "Eastern"
unhs[which(unhs$Region==3)]$Region.chr <- "Northern"
unhs[which(unhs$Region==4)]$Region.chr <- "Western"
#unhs$District.chr <- "NA"

unhs$weight <- unhs$mult

unhs[unhs==999] <- NA

unhs <- unhs[complete.cases(unhs[,c(3:9)])]

#unhs <- merge(unhs, unhs[,c(1:2,27:29)], by.x=c("hh", "h2q1"), by.y=c("hh", "h6q1"))

unhs.overall.wa <- rbind(cbind(data.frame(Sex="Overall")
                               ,(unhs[working.age=="Yes", .(Disabled=sum(Disability*weight)/sum(weight))]))
                         ,(unhs[working.age=="Yes", .(Disabled=sum(Disability*weight)/sum(weight)), by=Sex]))
unhs.overall.wa$`Not disabled` <- 1-unhs.overall.wa$Disabled
fwrite(unhs.overall.wa, "output/UNHS WA overall.csv")

unhs.domains <- rbind(cbind(data.frame(Sex="Overall")
                            ,(unhs[working.age=="Yes", .(Seeing=sum(Seeing*weight)/sum(weight)
                                                         ,Hearing=sum(Hearing*weight)/sum(weight)
                                                         ,Walking=sum(Walking*weight)/sum(weight)
                                                         ,Remembering=sum(Remembering*weight)/sum(weight)
                                                         ,`Self care`=sum(`Self care`*weight)/sum(weight)
                                                         ,Communication=sum(Communication*weight)/sum(weight))]))
                      ,(unhs[working.age=="Yes", .(Seeing=sum(Seeing*weight)/sum(weight)
                                                   ,Hearing=sum(Hearing*weight)/sum(weight)
                                                   ,Walking=sum(Walking*weight)/sum(weight)
                                                   ,Remembering=sum(Remembering*weight)/sum(weight)
                                                   ,`Self care`=sum(`Self care`*weight)/sum(weight)
                                                   ,Communication=sum(Communication*weight)/sum(weight)), by=Sex]))
fwrite(unhs.domains, "output/UNHS WA domains.csv")

unhs.domains.employ<- rbind(cbind(data.frame(Sex="Overall")
                                  ,(unhs[working.age=="Yes", .(Seeing=sum(Seeing*weight)
                                                               ,Hearing=sum(Hearing*weight)
                                                               ,Walking=sum(Walking*weight)
                                                               ,Remembering=sum(Remembering*weight)
                                                               ,`Self care`=sum(`Self care`*weight)
                                                               ,Communication=sum(Communication*weight)), by=employed]))
                            ,unhs[working.age=="Yes", .(Seeing=sum(Seeing*weight)
                                                        ,Hearing=sum(Hearing*weight)
                                                        ,Walking=sum(Walking*weight)
                                                        ,Remembering=sum(Remembering*weight)
                                                        ,`Self care`=sum(`Self care`*weight)
                                                        ,Communication=sum(Communication*weight)), by=.(employed,Sex)])

unhs.domains.employ <- cbind(data.table(employment=c("employed","unemployed"))
                             ,unhs.domains.employ[, .(Seeing=Seeing/sum(Seeing)
                                                      ,Hearing=Hearing/sum(Hearing)
                                                      ,Walking=Walking/sum(Walking)
                                                      ,Remembering=Remembering/sum(Remembering)
                                                      ,`Self care`=`Self care`/sum(`Self care`)
                                                      ,Communication=Communication/sum(Communication))
                                                  ,by=.(Sex)])[employment=="employed"]
fwrite(unhs.domains.employ, "output/UNHS WA domains employment.csv")

unhs.overall.employ <- rbind(cbind(data.frame(Sex="Overall")
                                   ,(unhs[working.age=="Yes", .(Disabled=sum(Disability*weight)
                                                                , `Not disabled`=sum((1-Disability)*weight)), by=employed]))
                             ,unhs[working.age=="Yes", .(Disabled=sum(Disability*weight)
                                                         , `Not disabled`=sum((1-Disability)*weight)), by=.(employed, Sex)])

unhs.overall.employ <- cbind(data.table(employment=c("employed","unemployed"))
                             ,unhs.overall.employ[, .(Disabled=Disabled/sum(Disabled)
                                                      ,`Not disabled`=`Not disabled`/sum(`Not disabled`))
                                                  ,by=.(Sex)])[employment=="employed"]
fwrite(unhs.overall.employ, "output/UNHS WA overall employment.csv")

unhs.overall.regions <- unhs[working.age=="Yes", .(Disabled=sum(Disability*weight)/sum(weight)
                                                   ,Seeing=sum(Seeing*weight)/sum(weight)
                                                   ,Hearing=sum(Hearing*weight)/sum(weight)
                                                   ,Walking=sum(Walking*weight)/sum(weight)
                                                   ,Remembering=sum(Remembering*weight)/sum(weight)
                                                   ,`Self care`=sum(`Self care`*weight)/sum(weight)
                                                   ,Communication=sum(Communication*weight)/sum(weight)), by=.(Region.chr)]
fwrite(unhs.overall.regions, "output/UNHS WA regions.csv")

# unhs.overall.districts <- unhs[working.age=="Yes", .(Disabled=sum(Disability*weight)/sum(weight)
#                                                    ,Seeing=sum(Seeing*weight)/sum(weight)
#                                                    ,Hearing=sum(Hearing*weight)/sum(weight)
#                                                    ,Walking=sum(Walking*weight)/sum(weight)
#                                                    ,Remembering=sum(Remembering*weight)/sum(weight)
#                                                    ,`Self care`=sum(`Self care`*weight)/sum(weight)
#                                                    ,Communication=sum(Communication*weight)/sum(weight)), by=.(District.chr)][District.chr!="NA"]
# fwrite(unhs.overall.districts, "output/UNHS WA districts.csv")
