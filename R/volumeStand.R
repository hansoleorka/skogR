#' Stand volume
#' 
#' Compute stand volume based on Norwegian stand volume functions
#' 
#' @name volumeStand
#' 
#' @param G Basal area (m2/ha)
#' @param HL Lorey's Mean heigh (m)
#' @param SP species code (1=spruce, 2= pine, 3= brich)
#' @param Vestlandet character for using functions representative for western-Norway either "coast" or "interior"
#' @return V Volume including bark (m3/ha)
#' @details ?
#' @author Hans Ole Ørka \email{hans.ole.orka@@gmail.org}
#' @references Vestjordet 1959,Brantseg 1959,Næsset 1994a, Næsset 1995a,b
#' @export

volumeStand <- function(G,HL,SP,Vestlandet=NULL){
	
     #Spruce Stands (Vestjordet 1959)
	VS <- -3.4837 + 0.4451 * G * HL + 1.052 * G
	
     #Pine Stands (Brandtseg 1959)
	VP <- 24.4832 + 0.5015 * G * HL - 1.9794 * HL
	
     #Birch Stands (Vestjordet 1959)
	VB <- -0.0660 + 0.3863 * G * HL + 1.0203 * G
     
     
	V = ifelse(SP==1,VS,ifelse(SP==2,VP,ifelse(SP==3,VB,NA)))
     
     #Vestlandet
     if(is.null(Vestlandet)==FALSE){

     #Næsset 1994a
     VpVN <- 0.8505 * G^0.9943 * HL^0.8021
     
     if(Vestlandet == "coast") {
          VsVN <- -36.367 + 1.207 * G + 5.869 * HL + 0.413 * G * HL - 0.191 * HL^20
     }
          if(Vestlandet == "interior") {
          VsVN <- -38.+13 + 1.485 * G + 5.162 * HL + 0.396 * G * HL - 0.131 * HL^2
          }
     V = ifelse(SP==1,VS,ifelse(SP==2,VpVN,ifelse(SP ==1 & SP ==2,VB,NA)))
     }
	
	
	return(V)
}

