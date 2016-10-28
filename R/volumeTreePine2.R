#' @param k crownheight 
#' @param b doublebark 
#' @seealso volumeDoubleBark crownHeightPine
#' @rdname volumeTree
#' @references Brantseg, A. 1967. Volume functions and tables for Scots pine. South Norway. Reports of the Norwegian Forest Research Institute, 12, 689-739.
#' @export

volumeTreePine2 <- function(d,h,k,b,aboveBark=TRUE){

     # VOLUM MED BARK I DM3. D <= 12CM ER VFB1 OG D > 12CM ER VFB2.
	if(aboveBark){
	     VFB2 <- -9.9793 + 0.204787*d*d + 0.029966*d*d*h + 0.003539*d*d*k - 0.002918*d*d*b # > 10 cm
          VFB1 <- 2.0044 + 0.029886*d*d + 0.036972*d*d*h # <12 cm
          v<- ifelse (d < 11, VFB1,VFB2)
	}
		
     # VOLUM UNDER BARK I DM3. D <= 12CM ER VFU1 OG D > 12CM ER VFU2.
	if(aboveBark == FALSE){
		du <- d
		VFB2 <- -3.7967 + 0.137902*du*du + 0.026031*du*du*h + 0.005498*du*h*h + 0.006482*du*du*k # >= 10 cm
		VFB1 <- 2.2922 + 0.040072*du*du*h + 0.00216*du*h*h # <12 cm
		v<- ifelse (du < 11, VFB1,VFB2)	}
	
	return(v)
}
