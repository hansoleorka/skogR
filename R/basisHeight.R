#' basisHeight
#' 
#' Calculate basis height as described in Eid, Fitje & Hoen 2002.
#'
#' @param d dbh in cm. Might be a vector.
#' @param sp species. 1 = spruce, 2 = pine, 3 = decideous. Should be of same length as h.
#'
#' @return a vector of the same length as the input values.
#'
#'
#' @references Eid, T.,Fitje A. og Hoen, H.F. (2002) Økonomi og Planlegging. Gan Forlag, Oslo. 205 p.
#'
#' @author Marius Hauglin 2016 \email{marius.hauglin@@nmbu.no}
#' @export



basisHeight<-function(d, sp){
	
	h<-rep(NA,length(d))
	
	# Gran dmb 
	h[sp == 1 & d >= 15]<- -14.17 + (20.86*log10(d[sp == 1 & d >= 15]) )
	h[sp == 1 & d < 15]<- 1.3 + (0.6*d[sp == 1 & d < 15]) 
	
	# Furu, lauv
	h[sp > 1 & d < 35]<- 0.39 + (0.852*d[sp > 1 & d < 35]) - (0.010644*d[sp > 1 & d < 35]*d[sp > 1 & d < 35])
	h[sp > 1 & d >= 35]<- 13.35 + (0.107*d[sp > 1 & d >= 35]) 
	
return(h)
	
}
