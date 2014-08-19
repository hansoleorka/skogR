library(roxygen2)
roxygenize(package.dir="D:\\WorkSpace2\\Packages\\skogR\\")
#exportPattern("^[[:alpha:]]+")

#library(devtools)
#install("D:\\WorkSpace2\\NorForFunc\\NorForFunc")
# 
detach("package:skogR")
remove.packages("skogR")
# setwd("D:\\WorkSpace2\\NorForFunc")
# system("R CMD INSTALL NorForFunc --build")
# 
# 
# install.packages("D:\\WorkSpace2\\NorForFunc\\NorForFunc_0.1.1.zip",repos = NULL)
# library(NorForFunc)
# ?calcSingleTreeVolume
# 

#Example
library(skogR)
load("D:\\WorkSpace2\\Packages\\skogR\\data\\trees.rda")
?volumeTree
Species <- as.character(retentionTrees$Otrsl)
sp <- ifelse(Species == "G",1,ifelse(Species == "F",2,ifelse(Species == "L",3,NA)))

d <- retentionTrees$Odbh/10
h <- retentionTrees$Ohoyde/10

v <- volumeTree(retentionTrees$Odbh/10,retentionTrees$Ohoyde/10,Species)