required.packages <- c("reshape2","ggplot2","data.table","bit64")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/inclusion-works")

unhs <- fread("project-data/UNHS/UNHS.csv")
unhs.regions <- unique(fread("project-data/UNHS/UNHS regions.csv"))

unhs <- merge(unhs, unhs.regions, by="hh")

unhs$Sex.chr <- "Male"
unhs[which(unhs$Sex==2)]$Sex.chr <- "Female"
unhs$working.age <- "No"
unhs[which(unhs$h2q2a>=14 & unhs$h2q2a<=65)]$working.age <- "Yes"
unhs$employed <- "unemployed"
unhs[which(unhs$`Employment status`==1)]$employed <- "employed"
unhs$Region.chr <- "Central"
unhs[which(unhs$Region==2)]$Region.chr <- "Eastern"
unhs[which(unhs$Region==3)]$Region.chr <- "Northern"
unhs[which(unhs$Region==4)]$Region.chr <- "Western"

unhs$weight <- 1

unhs.overall.wa <- rbind(cbind(data.frame(Sex.chr="Overall")
                               ,(unhs[working.age=="Yes", .(Disabled=sum(Any_impairment*weight)/sum(weight))]))
                         ,(unhs[working.age=="Yes", .(Disabled=sum(Any_impairment*weight)/sum(weight)), by=Sex.chr]))
unhs.overall.wa$`Not disabled` <- 1-unhs.overall.wa$Disabled
fwrite(unhs.overall.wa, "output/UNHS WA overall.csv")

unhs.domains <- rbind(cbind(data.frame(Sex.chr="Overall")
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
                                                   ,Communication=sum(Communication*weight)/sum(weight)), by=Sex.chr]))
fwrite(unhs.domains, "output/UNHS WA domains.csv")

unhs.domains.employ<- rbind(cbind(data.frame(Sex.chr="Overall")
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
                                                        ,Communication=sum(Communication*weight)), by=.(employed,Sex.chr)])

unhs.domains.employ <- cbind(data.table(employment=c("employed","unemployed"))
                             ,unhs.domains.employ[, .(Seeing=Seeing/sum(Seeing)
                                                      ,Hearing=Hearing/sum(Hearing)
                                                      ,Walking=Walking/sum(Walking)
                                                      ,Remembering=Remembering/sum(Remembering)
                                                      ,`Self care`=`Self care`/sum(`Self care`)
                                                      ,Communication=Communication/sum(Communication))
                                                  ,by=.(Sex.chr)])[employment=="employed"]
fwrite(unhs.domains.employ, "output/UNHS WA domains employment.csv")

unhs.overall.employ <- rbind(cbind(data.frame(Sex.chr="Overall")
                                   ,(unhs[working.age=="Yes", .(Disabled=sum(Any_impairment*weight)
                                                                , `Not disabled`=sum((1-Any_impairment)*weight)), by=employed]))
                             ,unhs[working.age=="Yes", .(Disabled=sum(Any_impairment*weight)
                                                         , `Not disabled`=sum((1-Any_impairment)*weight)), by=.(employed, Sex.chr)])

unhs.overall.employ <- cbind(data.table(employment=c("employed","unemployed"))
                             ,unhs.overall.employ[, .(Disabled=Disabled/sum(Disabled)
                                                      ,`Not disabled`=`Not disabled`/sum(`Not disabled`))
                                                  ,by=.(Sex.chr)])[employment=="employed"]
fwrite(unhs.overall.employ, "output/UNHS WA overall employment.csv")

unhs.overall.regions <- unhs[working.age=="Yes", .(Disabled=sum(Any_impairment*weight)/sum(weight)
                                                   ,Seeing=sum(Seeing*weight)/sum(weight)
                                                   ,Hearing=sum(Hearing*weight)/sum(weight)
                                                   ,Walking=sum(Walking*weight)/sum(weight)
                                                   ,Remembering=sum(Remembering*weight)/sum(weight)
                                                   ,`Self care`=sum(`Self care`*weight)/sum(weight)
                                                   ,Communication=sum(Communication*weight)/sum(weight)), by=.(Region.chr)]
fwrite(unhs.overall.regions, "output/UNHS WA regions.csv")
