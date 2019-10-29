required.packages <- c("reshape2", "ggplot2", "data.table", "networkD3")
lapply(required.packages, require, character.only = T)

wd <- "G:/My Drive/Work/GitHub/inclusion-works/"
setwd(wd)

nodes <- data.table(
  name = c(sectors$V1, "major", "minor", "inclusion", "assistance")
)

links <- data.table(
  source = c(sectors$V1, sectors$V1, "major", "major", "minor", "minor"),
  target = c(rep("major", length(sectors$V1)), rep("minor", length(sectors$V1)), "inclusion", "assistance", "inclusion", "assistance"),
  value  = c(sectors$major, sectors$minor, sum(sectors$major.inclusive), sum(sectors$major)-sum(sectors$major.inclusive), sum(sectors$minor.inclusive), sum(sectors$minor)-sum(sectors$minor.inclusive))
)

links <- links[value > 0]

p <- sankeyNetwork(links,nodes,"source","target","value")
