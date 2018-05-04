#' Calculates: Five-year diameter increment of a single tree (mm per 5 yr).
#' 
#' Ole Martin Bollandsås 2007. Manuscript USED IN MATRIX MODEL
#' 
#' @param speciesCode species code (1=spruce, 2= pine, 3= brich)
#' @param d Diameter of the single tree at breast height (cm)
#' @param BA Stand basal area (m2ha-1)
#' @param BAL Sum grunnflate på trær større enn treet man beregner for. 
#' @param SI Site index of spruce (H40-system m.)
#' @param region Dummy variable for region. =1 --> County nr >=10
#' @param LAT latitude
#' @return V Volume including bark (m3/ha)
#' @details ?
#' @author Hans Ole Ørka \email{hans.ole.orka@@gmail.org}
#' @references Vestjordet 1959,Brantseg 1959,Næsset 1994a, Næsset 1995a,b
#' @export
#' @examples
#' load(file="data/skogRtrees.rda")
#' attach(skogRtrees)
#' #Compute BAL
#' BAL <- c()
#' for(i in c(1:length(ba))){
#'      BAL[i] <- sum(ba[ba>ba[i]])
#' }
#' BA <- sum(ba)*(10000/250)
#' region = 5
#' LAT = 60.8
#' SI = 17
#' diameterIncrementTree(sp,d,BAL,SI,BA,region,LAT)
diameterIncrementTree <- function(speciesCode, d, BAL, SI, BA, region,LAT){
     #Spruce(speciesCode==1)
     id51 = (17.8393 + 0.04762 * d - 0.00011585 * d * d - 0.34116 * BAL + 0.90604 * SI - 0.02414 * BA - 0.26781 * LAT);
     #Pine (speciesCode==2)
     id52 = (25.5426 + 0.02509 * d - 0.0000566 * d * d - 0.21622 * BAL + 0.69814 * SI - 0.12318 * BA - 0.33626 * LAT);
     #Birch (speciesCode==3)
     id53 = (11.8084 + 0.00009616 * d * d - 0.0000000958457 * d * d * d + 0.5185 * SI - 0.15176 * BA - 0.16052 * LAT);
     #Broadleaves (speciesCode==4)
     id54 = (2.20413 + 0.0631 * d - 0.0000832 * d * d + 0.35904 * SI - 0.17678 * BA);
     
     id5=rep(NA,length(speciesCode));
     id5[speciesCode==1] <- id51[speciesCode==1]
     id5[speciesCode==2] <- id52[speciesCode==2]
     id5[speciesCode==3] <- id53[speciesCode==3]
     id5[speciesCode==4] <- id54[speciesCode==4]
     
     id5[id5<0] <- 0

     return (id5)
}
