#' 2
#' @details The shift of pine function according to diameter is on <11
#' @rdname volumeTree
#' @references Brantseg, A. 1967. Volume functions and tables for Scots pine. South Norway. Reports of the Norwegian Forest Research Institute, 12, 689-739.
#' @export

volumeTreePine <- function(d,h,aboveBark=TRUE){
	
     # VOLUM MED BARK I DM3. D <= 12CM ER VFB1 OG D > 12CM ER VFB2.
	if(aboveBark){
		v<- ifelse (d <= 11, {VFB1 <- 2.912 + 0.039994 * d * d * h - 0.001091 * d * h * h},
				ifelse (d > 11, {VFB2 <- 8.6524 + 0.076844 * d * d + 0.031573 * d * d * h},NA))
	}
	
	
     # VOLUM UNDER BARK I DM3. D <= 12CM ER VFU1 OG D > 12CM ER VFU2.
	if(aboveBark == FALSE){
		du <- d
		v<-	ifelse (du <= 11, {VFU1 <- 2.2922 + 0.040072 * du * du * h + 0.00216 * du * h * h},
				ifelse (du > 11, {VFU2 <- -3.5425 + 0.128182 * du * du + 0.028268 * du * du * h + 0.008216 * du * h * h},NA))
	}
	
	return(v)
}
