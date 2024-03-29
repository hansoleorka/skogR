% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/heightStandDominant.R
\name{heightStandDominant}
\alias{heightStandDominant}
\title{Dominant height}
\usage{
heightStandDominant(H40, age, SP)
}
\arguments{
\item{H40}{a vector of site indeces (m)}

\item{age}{age at breast height (years)}

\item{SP}{species code (1=spruce, 2= pine, 3 = birch, 4= aspen)}
}
\value{
Dominant tree height (m)
}
\description{
Calculates dominant height based on site index and age
}
\details{
Index age is 40
}
\examples{
H40 <- c(11,14,17,20,23)
age <- c(40,40,40,50,60)
heightStandDominant( H40 = H40, age = age , SP = 2 )
}
\references{
Sharma et. al. (2011), Eriksson et. al. (1997), Opdahl (1992)
Sharma, R. P., Brunner, A., Eid, T., & Øyen, B.-H. (2011). Modelling dominant height growth from national forest inventory individual tree data with short time series and large age errors. Forest Ecology and Management, 262(12), 2162–2175. https://doi.org/10.1016/j.foreco.2011.07.037
Eriksson H, Johansson U, Kiviste A. 1997. A site-index model for pure and mixed stands of Betula pendula and Betula pubescens in Sweden. Scand J For Res. 12(2):149–156. doi:10.1080/02827589709355396
Opdahl, H. 1992. Bonitet, vekst og produksjon hos osp (Popolus Tremula L.) i Sør-Norge = Site-index, growth and yield in Aspen (Popolus tremula L.) stands in South Norway 44 s. : ill. (Meddelelser fra Skogforsk ; 44.11) ISBN: 82-7169-527-4
}
\seealso{
SiteIndex, HeightDiffSi, HeightGrowth, BhAge
}
\author{
Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
}
