#' Show the NEWS file.
#' 
#' Show the NEWS file of the NorForFunc package.     
#' 
#' @return None
#' @author Hans Ole Ørka \email{hans.ole.orka@@gmail.org}
#' @examples 
#' skogR.News()

skogR.News <- function () 
{
	newsfile <- file.path(system.file(package = "skogR"),"NEWS")
	file.show(newsfile)
}
