#' Single Tree volume
#' 
#' Compute stem volume of single trees using Norwegian single tree volume functions
#' 
#' @param d Diameter in breast height in centimeter 
#' @param h Tree height in meter
#' @param sp vektor med treslag; 1=gran, 2=furu, 3=lauv
#' @param aboveBark a logical value indicating if volume above (TRUE) or below bark (FALSE).
#' @return v volum in liters (dm3)
#' @seealso volumeDoubbleBark
#' @details Dette er en hovedfuksjon. 
#' @rdname volumeTree
#' @author Hans Ole Oerka \email{hans.ole.orka@@gmail.org}
#' @export

volumeTree <- function(d,h,sp,aboveBark=TRUE){
     vs <-volumeTreeSpruce(d,h,aboveBark=aboveBark)
     vp <-volumeTreePine(d,h,aboveBark=aboveBark)
     vb <-volumeTreeBirch(d,h,aboveBark=aboveBark)
     v <- ifelse(sp == 1,vs,ifelse(sp == 2,vp,ifelse(sp == 3,vb,NA)))
	return(v)
}

