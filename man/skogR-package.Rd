\name{skogR-package}
\alias{skogR-package}
\alias{skogR}
\docType{package}
\title{
What the package does (short line)
~~ package title ~~
}
\description{
More about what it does (maybe more than one line)
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab skogR\cr
Type: \tab Package\cr
Version: \tab 0.3\cr
Date: \tab 2013-05-16\cr
License: \tab What license is it under?\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Who wrote it

Maintainer: Who to complain to <yourfault@somewhere.net>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ 
skog
Norge
}
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
library(skogR)
data(trees)
?volumeTree
Species <- as.character(retentionTrees$Otrsl)
sp <- ifelse(Species == "G",1,ifelse(Species == "F",2,ifelse(Species == "L",3,NA)))

d <- retentionTrees$Odbh/10
h <- retentionTrees$Ohoyde/10

v <- volumeTree(retentionTrees$Odbh/10,retentionTrees$Ohoyde/10,Species)
}
