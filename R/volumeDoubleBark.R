#' Helpfunctions for volume: Doublebarkthickness
#' 
#' Functions for calculation of doublebark in milimeter 
#' 
#' @param d Diameter in breast hegiht i centimeter including bark
#' @param h Tree height in meter
#' @param sp species codes (1=spruce, 2=pine, 3=Birch) 
#' @param Fitje1995 logical indicating to use the functions by Fitje 1995 rather than Brantseg 1967
#' @details bla bla
#' @return b doublebark i mm
#' @author Hans Ole Ørka \email{hans.ole.orka@@gmail.org}
#' @references Braastad, H. 1966. Volume tables for birch. Reports of the Norwegian Forest Research Institute, 21, 265-365.
#' Brantseg, A. 1967. Volume functions and tables for Scots pine. South Norway. Reports of the Norwegian Forest Research Institute, 12, 689-739.
#' Vestjordet, E. 1967. Functions and tables for volume of standing trees. Norway spruce. Reports of the Norwegian Forest Research Institute, 22, 539-574.
#' Fitje A. 1995. 
#' @export 



volumeDoubleBark <- function(d,h,sp,Fitje1995=FALSE){
     
          # DOBBEL BARK I MM. Gran
          Bs <- -0.34 + 0.831648 * d - 0.002832 * d * d - 0.010112 * h * h + 0.700203 * d * d / (h * h)
          
          # DOBBEL BARK I MM. Furu #Brantseg(1967)
          Bp <- 2.9571 + 1.1499 * d - 0.7304 * d / h 
          
          #Dobbelbark  Furu Fitje 1995
          if(Fitje1995) {
               Bp <- 2.3 + 1.13*d
          }
          
          #Dobbelbark Bjørk i mm
          Bb <- 1.046 * d

          #Dobbelbark i mm
          b <- ifelse(sp == 1, Bs, ifelse(sp == 2, Bp, ifelse(sp == 3,Bb,NA)))          
          return(b)
}
