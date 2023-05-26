library(roxygen2)
#roxygenize(package.dir="C:\\hao\\github\\skogR\\")
roxygenize(package.dir="/home/hanso/Dropbox/GitHub/skogRII/skogR/")

remove.packages("skogR")
library(devtools)
install_github("hansoleorka/skogR")

?devtools::document()

library(skogR)
