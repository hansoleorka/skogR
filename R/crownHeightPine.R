#' Crown height of pine
#' 
#' Compute crown height of single trees using Norwegian only for pine... 
#' 
#' @param d Diameter in breast height (cm) 
#' @param h Tree height (m)
#' @return k crownheight (m)
#' @seealso volumeTreePine2
#' @author Hans Ole Ørka \email{hans.ole.orka@@gmail.org}
#' @references Brantseg 1967
crownHeightPine <- function(d,h){
    k <- 4.1203 - 0.002817*d*d + 0.26234*h*h - 0.3184*(d/h)*(d/h)
    return(k)
}

