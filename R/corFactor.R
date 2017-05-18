#' corFactor
#' 
#' Calculate correction factor by converting the correction factor from another species.
#'
#' @param sp input species: 1 = spruce, 2 = pine, 3 = deciduous
#' @param cf correction factor
#'
#' @return a vector of length three with the correction factor for the three species (spruce, pine, deciduous). 
#'
#'@note the input correction factor is also returned.
#' 
#' @references Eid, T. & Fitje, A. 1993. Variasjoner innen bestand for volum, grunnflate, treantall, middeldiameter og middelhøyde.
#' Meddelelser fra Skogforsk 46.10: 44 s.
#'
#' @author Marius Hauglin 2017 \email{marius.hauglin@@nmbu.no}
#' @export

corFactor<-function(sp,cf){
	
	spruce<-pine<-deciduous<-NA
	if (sp == 1) { pine<-0.5+0.49*cf; deciduous<-0.79+0.31*cf; spruce<-cf }
	if (sp == 2) { spruce<-0.27+0.92*cf; deciduous<-0.46+0.59*cf; pine<-cf }
	if (sp == 3) { spruce<-0.56+0.65*cf; pine<-0.48+0.52*cf; deciduous <- cf }

	return(c(spruce, pine, deciduous))
}




