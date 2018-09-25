rm(list = ls())

source("R/diameterIncrementTree.R")
source("R/mortalityTree.R")
source("R/HeightGrowth.R")
source("R/heightRelativGrowthTree.R")
source("R/diameterIncrementStand.R")

load(file="data/skogRtrees.rda")
attach(skogRtrees)
#Compute BAL
BAL <- c()
for(i in c(1:length(ba))){
     BAL[i] <- sum(ba[ba>ba[i]])
}
BA <- sum(ba)*(10000/250)
region = 5
LAT = 60.8
SI = 17

#Diameter increment 
id5 <- diameterIncrementTree(sp,d,BAL,SI,BA,region,LAT)

#Height 
HO <- mean(h[rev(order(d))][1:2])/10
POT <- HeightGrowth(HO, SI, 5, 1 ) - HO
ihrel <- heightRelativGrowthTree(d,sp)

Growth = POT*ihrel

h2 <- h/10 + Growth
d2 <- d+(id5/10)
HO <- mean(h2[rev(order(d2))][1:2])

m <- mortalityTree(d,sp,BA)
p <- runif(length(m),0,1)
cbind(m,p,m>p)

tmp <- data.frame(sp=sp,d=d2,h=h2,m=m,plotID=1)
dim(tmp)
tmp <- subset(tmp,m<p)
dim(tmp)

tmp


library(devtools)
#install_github("hansoleorka/skogrover",auth_token='ae68208b16ea80a128a860c49f3a2a01b31dd6ad',dependencies=TRUE)

library(skogrover)
nmbu2015_1000<-fieldPlot(plotid=tmp$plotID,
                         d=tmp$d,
                         sp=tmp$sp,
                         h=tmp$h,
                         correction=FALSE,
                         area=250)



diameterIncrementStand(1,17,150,20,20,80)


