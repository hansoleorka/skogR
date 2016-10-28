#' @rdname volumeTree
#' @references Braastad, H. 1966. Volume tables for birch. Reports of the Norwegian Forest Research Institute, 21, 265-365.
#' @export

volumeTreeBirch <- function(d,h,aboveBark=TRUE){
     
     # VOLUM MED BARK I DM3.
	if(aboveBark){
	     B <- volumeDoubleBark(d,h,sp=3)
		v = -1.25409 + 0.12739 * d * d + 0.03166 * d * d * h + 0.0009752 * d * h * h - 0.01226 * h * h - 0.004214 * d * d * B
	}
	
     # VOLUM UNDER BARK I DM3.
	if(aboveBark == FALSE){
		dub <- d
		v = -1.48081 + 0.16945 * dub * dub + 0.01834 * dub * dub * h + 0.01018 * dub * h * h - 0.0451 * h * h
	}
	
	return(v)

}




#FUNKSJON FOR LAUV (Brastad, 1966)
#VLL2 = -1.86827 + 0.21461*d*d + 0.01283 * d * d * h + 0.0138 * d * h *h - 0.06311 * h * h


