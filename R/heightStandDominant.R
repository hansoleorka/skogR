#' Dominant height 
#'
#' Calculates dominant height based on site index and age
#'
#' @param H40 a vector of site indeces (m)
#' @param age age at breast height (years)
#' @param SP species code (1=spruce, 2= pine, 3 = birch, 4= aspen)
#' @return Dominant tree height (m)
#' @details Index age is 40
#' @seealso SiteIndex, HeightDiffSi, HeightGrowth, BhAge
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @references Sharma et. al. (2011), Eriksson et. al. (1997), Opdahl (1992)
#' Sharma, R. P., Brunner, A., Eid, T., & Øyen, B.-H. (2011). Modelling dominant height growth from national forest inventory individual tree data with short time series and large age errors. Forest Ecology and Management, 262(12), 2162–2175. https://doi.org/10.1016/j.foreco.2011.07.037
#' Eriksson H, Johansson U, Kiviste A. 1997. A site-index model for pure and mixed stands of Betula pendula and Betula pubescens in Sweden. Scand J For Res. 12(2):149–156. doi:10.1080/02827589709355396
#' Opdahl, H. 1992. Bonitet, vekst og produksjon hos osp (Popolus Tremula L.) i Sør-Norge = Site-index, growth and yield in Aspen (Popolus tremula L.) stands in South Norway 44 s. : ill. (Meddelelser fra Skogforsk ; 44.11) ISBN: 82-7169-527-4     
#' @examples
#' H40 <- c(11,14,17,20,23)
#' age <- c(40,40,40,50,60)
#' heightStandDominant( H40 = H40, age = age , SP = 2 )
#' @export
#'
heightStandDominant <- function( H40 , age , SP ){
     
     H40 <- H40 - 1.3
     
     Hcalc <- rep( NA, length( age ) )
     
     #spruce
     b1 <- 18.9206
     b2 <- 5175.18
     b3 <- 1.1576
     R <- 0.5 * ( H40 - b1 + ( ( H40 - b1 )^2 + 4 * b2 * H40 * 40^( -b3 ) )^0.5 )
     H_sp <- ( b1 + R ) / ( 1 + ( b2 / R * age^( -b3 ) ) ) + 1.3
     
     #pine
     b1 <- 12.8361
     b2 <- 3263.99
     b3 <- 1.1758
     R <- 0.5 * ( H40 - b1 + ( ( H40 - b1 )^2 + 4 * b2 * H40 * 40^( -b3 ) )^0.5 )
     H_pi <- ( b1 + R ) / ( 1 + ( b2 / R * age^( -b3 ) ) ) + 1.3
     
     #birch
     b1 <- 394
     b2 <- 1.387
     k <- 7
     d1 <- b1 / ( k^b2 )
     R <- ( ( H40 - d1 )^2 + 4 * b1 * H40 / 40^b2 )^0.5
     H_bi <- ( H40 + d1 + R ) / ( 2 + 4 * b1 * age^( -1 * b2 ) / ( H40 - d1 + R )) + 1.3
     
     #aspen
     H40 <- H40 - 1.3
     OSP20 <- ((age+5.94064)/(2.19443+0.64260*(age+5.94064)))^8.07005
     OSP23 <- ((age+4.89477)/(2.25222+0.55797*(age+4.89477)))^6.30208
     DIFF <- (OSP23+0.0262)-(OSP20+0.1103)
     H_as <- ((-20*DIFF)/3)+(OSP20+0.1103)+((H40*DIFF)/3)
     
     #result
     Hcalc[ SP == 1 ] <- H_sp[ SP == 1 ]
     Hcalc[ SP == 2 ] <- H_pi[ SP == 2 ]
     Hcalc[ SP == 3 ] <- H_bi[ SP == 3 ]
     Hcalc[ SP == 4 ] <- H_as[ SP == 4 ]
     Hcalc
}