#' @rdname volumeTree
#' @references Braastad, H. 1966. Volume tables for birch. Reports of the Norwegian Forest Research Institute, 21, 265-365.
#' @export

volumeTreeBroadleaved <- function(d,h){
     #FUNKSJON FOR LAUV (Brastad, 1966)
     # VOLUM MED BARK I DM3.
	v = -1.86827 + 0.21461*d*d + 0.01283 * d * d * h + 0.0138 * d * h *h - 0.06311 * h * h
	return(v)

}






