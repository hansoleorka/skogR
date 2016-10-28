#' @rdname volumeTree
#' @references Vestjordet, E. 1967. Functions and tables for volume of standing trees. Norway spruce. Reports of the Norwegian Forest Research Institute, 22, 539-574.
#' @export

volumeTreeSpruce <- function(d,h,aboveBark=TRUE){
     # VOLUM MED BARK I DM3. D <= 10CM ER VGB1, 10CM < D <= 13CM ER VGB2 OG D > 13CM ER VGB3
	if(aboveBark){
		v<-		ifelse (d <= 10, {VGB1 = 0.52 + 0.02403 * d * d * h + 0.01463 * d * h * h - 0.10983 * h * h + 0.15195 * d * h},
						ifelse (d > 10 & d <= 13, {VGB2 = -31.57 + 0.0016 * d * h * h + 0.0186 * h * h + 0.63 * d * h - 2.34 * h + 3.2 * d},
								ifelse (d > 13, {VGB3 = 10.14 + 0.0124 * d * d * h + 0.03117 * d * h * h - 0.36381 * h * h + 0.28578 * d * h}, NA)))
	}
	
	
     # VOLUM UNDER BARK I DM3. D <= 10CM ER VGU1, 10CM < D <= 13CM ER VGU2 OG D > 13CM ER VGU3.
	if(aboveBark == FALSE){
		dub <- d
		v<-
				ifelse (dub <= 10, {VGU1 = 0.38 + 0.02524 * dub * dub * h + 0.01269 * dub * h * h - 0.07726 * h * h + 0.11671 * dub * h},
						ifelse (dub > 10 & d <= 13, {VGU2 = -27.19 + 0.0073 * dub * h * h - 0.0228 * h * h + 0.5667 * dub * h - 1.98 * h + 2.75 * dub},
								ifelse (dub > 13, {VGU3 = 8.66 + 0.01218 * dub * dub * h + 0.02976 * dub * h * h - 0.31373 * h * h + 0.25452 * dub * h},NA)))
	}
	
	return(v)
}

