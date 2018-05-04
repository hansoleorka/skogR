#' Dominant height development (Sharma et al. 2011)
#' 
#' Calculates dominant height for Norway spruce (Picea abies (L.) Karst.) and Scots pine (Pinus sylvestris L.) in Norway. 
#' 
#' @param H40 Site index of spruce (H40-system m.)
#' @param AGE Stand age
#' @param DominantSpecies Dominant species of the stand (species code: 1=spruce, 2= pine)
#' @return Dominant hegith (m) 
#' @details See reference
#' @author Hans Ole Ørka \email{hans.ole.orka@@gmail.org}
#' @references Sharma, R.P. et al., 2011. Modelling dominant height growth from national forest inventory individual tree data with short time series and large age errors. Forest ecology and management, 262(12), pp.2162–2175.
#' @export
#' @examples
#' dominantHeightDevelopment(70,17,1)
#' dominantHeightDevelopment(70,17,2)
#' dominantHeightDevelopment(70,17,3)
heightStandDominantDevelopment <- function(AGE,H40,DominantSpecies){
     t=AGE;          
     if(DominantSpecies %in% c(1,2)){
          #Spruce
          if(DominantSpecies == 1){
               b1=18.9206       
               b2=5175.18       
               b3=1.1576
          }
          
          #Pine
          if(DominantSpecies==2){
               b1=12.8361
               b2=3263.99
               b3=1.1758
          }
          
          
          X0=0.5*(H40-b1+((H40-b1)^2+4*b2*H40*40^(-b3))^0.5)
          HO=(b1+X0)/(1+(b2/X0*t^(-b3)))
          
          return(HO)
          
          
          
     }
     else
     {
          HO=NA
          warning("Warning: Only developed for Norway spruce and Scots pine....")
          return(HO)}
}
