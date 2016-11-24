#' domHeight
#' 
#' Calculate dominant height
#'
#' @param h numerical vector; heights in m.
#' @param d numerical vector; diameters in cm.
#' @param area numerical; area from which the trees dsecribed by d and h are taken. Given in m2.
#'
#' @return the dominant height in m.
#'
#' @references Fitje, A. (1989). Tremåling.
#'
#' @author Marius Hauglin 2016 \email{marius.hauglin@@nmbu.no}
#' @export



# funksjon for å beregne overhøyde (tilsv 10 største (diameter) per da)
domHeight<-function(h,d,area) mean(h[order(d,decreasing=TRUE)][ 1:(round(0.01*area,0)) ])


