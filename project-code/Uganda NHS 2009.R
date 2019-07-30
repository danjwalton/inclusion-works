required.packages <- c("reshape2","ggplot2","data.table","bit64")
lapply(required.packages, require, character.only=T)

setwd("G:/My Drive/Work/GitHub/inclusion-works")

unhs.lab.dis <- fread("project-data/UNHS/UNHS labour + disability.csv")
unhs <- unique(fread("project-data/UNHS/UNHS disability.csv"))

unhs$Sex <- "Male"
unhs[which(unhs$Sex_code==2)]$Sex <- "Female"
unhs$working.age <- "No"
unhs[which(unhs$Age>=14 & unhs$Age<=65)]$working.age <- "Yes"
unhs.lab.dis$employed <- "unemployed"
unhs.lab.dis[which(unhs.lab.dis$`Employment status`==1)]$employed <- "employed"
unhs.lab.dis$Sex.chr <- "Male"
unhs.lab.dis[which(unhs.lab.dis$Sex==2)]$Sex.chr <- "Female"
unhs$Region.chr <- "Central"
unhs[which(unhs$Region==2)]$Region.chr <- "Eastern"
unhs[which(unhs$Region==3)]$Region.chr <- "Northern"
unhs[which(unhs$Region==4)]$Region.chr <- "Western"
unhs$District.chr <- "NA"
unhs[which(unhs$District==102)]$District.chr <- "Kampala"
unhs[which(unhs$District==105)]$District.chr <- "Masaka"
unhs[which(unhs$District==108)]$District.chr <- "Mukono"
unhs[which(unhs$District==113)]$District.chr <- "Wakiso"
unhs[which(unhs$District==203)]$District.chr <- "Iganga"
unhs[which(unhs$District==204)]$District.chr <- "Jinja"
unhs[which(unhs$District==209)]$District.chr <- "Mbale"
unhs[which(unhs$District==215)]$District.chr <- "Sironko"
unhs[which(unhs$District==406)]$District.chr <- "Kasese"
unhs[which(unhs$District==410)]$District.chr <- "Mbarara"

unhs$weight <- 1

unhs.lab.dis <- merge(unhs.lab.dis, unhs[,c(1:2,27:30)], by.x=c("hh", "h2q1"), by.y=c("hh", "h6q1"))

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

unhs.domains.employ<- rbind(cbind(data.frame(Sex.chr="Overall")
                                  ,(unhs.lab.dis[working.age=="Yes", .(Seeing=sum(Seeing*weight)
                                                               ,Hearing=sum(Hearing*weight)
                                                               ,Walking=sum(Walking*weight)
                                                               ,Remembering=sum(Remembering*weight)
                                                               ,`Self care`=sum(`Self care`*weight)
                                                               ,Communication=sum(Communication*weight)), by=employed]))
                            ,unhs.lab.dis[working.age=="Yes", .(Seeing=sum(Seeing*weight)
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
                                   ,(unhs.lab.dis[working.age=="Yes", .(Disabled=sum(Any_impairment*weight)
                                                                , `Not disabled`=sum((1-Any_impairment)*weight)), by=employed]))
                             ,unhs.lab.dis[working.age=="Yes", .(Disabled=sum(Any_impairment*weight)
                                                         , `Not disabled`=sum((1-Any_impairment)*weight)), by=.(employed, Sex.chr)])

unhs.overall.employ <- cbind(data.table(employment=c("employed","unemployed"))
                             ,unhs.overall.employ[, .(Disabled=Disabled/sum(Disabled)
                                                      ,`Not disabled`=`Not disabled`/sum(`Not disabled`))
                                                  ,by=.(Sex.chr)])[employment=="employed"]
fwrite(unhs.overall.employ, "output/UNHS WA overall employment.csv")

unhs.overall.regions <- unhs[working.age=="Yes", .(Disabled=sum(Disability*weight)/sum(weight)
                                                   ,Seeing=sum(Seeing*weight)/sum(weight)
                                                   ,Hearing=sum(Hearing*weight)/sum(weight)
                                                   ,Walking=sum(Walking*weight)/sum(weight)
                                                   ,Remembering=sum(Remembering*weight)/sum(weight)
                                                   ,`Self care`=sum(`Self care`*weight)/sum(weight)
                                                   ,Communication=sum(Communication*weight)/sum(weight)), by=.(Region.chr)]
fwrite(unhs.overall.regions, "output/UNHS WA regions.csv")

unhs.overall.districts <- unhs[working.age=="Yes", .(Disabled=sum(Disability*weight)/sum(weight)
                                                   ,Seeing=sum(Seeing*weight)/sum(weight)
                                                   ,Hearing=sum(Hearing*weight)/sum(weight)
                                                   ,Walking=sum(Walking*weight)/sum(weight)
                                                   ,Remembering=sum(Remembering*weight)/sum(weight)
                                                   ,`Self care`=sum(`Self care`*weight)/sum(weight)
                                                   ,Communication=sum(Communication*weight)/sum(weight)), by=.(District.chr)][District.chr!="NA"]
fwrite(unhs.overall.districts, "output/UNHS WA districts.csv")
