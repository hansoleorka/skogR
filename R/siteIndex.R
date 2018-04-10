#' SiteIndex
#' 
#' calculate site index according to the Norwegian H40 system
#' 
#' @param age age at breast height (years)
#' @param HO top height, i.e. mean of the 100 largest trees with respect to dbh per hectar (m)
#' @param SP species code (1=spruce, 2= pine, 3= birch)
#' @param method character for choosing the intended functions, default or "TVEITE-BRAASTAD"
#' @return H40 site index (m)
#' @details Reference age of 40 years
#' @seealso HeightGrowth, HeightDiffSi
#' @author Lennart Noordermeer \email{lennart.noordermeer@gmail.com}
#' @references Tveite (1977), Tveite & Braastad (1981), Eriksson (1997), Sharma et al (2011)
#' @examples
#' a <- c(55,55) # ages at breast height
#' b <- c(10.23744, 24.81046) # dominant heights (m)
#' SiteIndex( a, b, 1) 
#' @export

SiteIndex <- function( age, HO, SP, method = "SHARMA-BRUNNER" ) {

  HO <- HO - 1.3
  
  SIcalc <- rep( NA, length( age ) )
  
  if (method == "TVEITE-BRAASTAD") {
    
    a <- ( age - 40 ) / 10
    
    #spruce
    b <- age * 0.1 + 0.55
    diff <- 3.0 + 0.40183 * a - 0.104701 * a^2 + 0.679104 * a^3 / 100 + 0.184402 * a^4 / 100 - 0.224249 * a^5 / 1000
    h17 <- ( b / ( 0.430606 + 0.164818 * b ) )^2.1
    diff[ age > 100 ] <- 3.755
    SIspruce <- 17.0 + 3.0 * ( ( HO - h17 ) / diff ) + 1.3
    
    #pine
    diff <- 3.0 + 0.394624 * a - 0.0649695 * a^2 + 0.487394 * a^3 / 100 - 0.141827 * a^4 / 1000
    h14 <- 1.3 + ( 24.7 * ( 1 - exp( -0.02105 * age ) )^1.18029 )
    diff[ age > 119 ] <- 3.913
    SIpine <- 14 + 3.0 * ( ( HO - h14 ) / diff ) + 1.3
    
    #birch
    b1 <- 394
    b2 <- 1.387
    k <- 7
    d1 <- b1 / ( k^b2 )
    r1 <- ( ( HO - d1 )^2 + 4 * b1 * ( HO ) / age^b2 )^0.5
    SIbirch <- ( HO + d1 + r1 ) / ( 2 + 4 * b1 * 40^( -1 * b2 ) / ( HO - d1 + r1 ) ) + 1.3  
    
    SIcalc[ SP == 1 ] <- SIspruce[ SP == 1 ]
    SIcalc[ SP == 2 ] <- SIpine[ SP == 2 ]
    SIcalc[ SP == 3 ] <- SIbirch[ SP == 3 ]
    
  }
  
  if ( method == "SHARMA-BRUNNER" ) {
    
    #spruce
    b1 <- 18.9206
    b2 <- 5175.18
    b3 <- 1.1576
    R <- 0.5 * ( HO - b1 + ( ( HO - b1 )^2 + 4 * b2 * HO * age^( -b3 ) )^0.5 )
    H_sp <- ( b1 + R ) / ( 1 + ( b2 / R * 40^( -b3 ) ) ) + 1.3
    
    #pine
    b1 <- 12.8361
    b2 <- 3263.99
    b3 <- 1.1758
    R <- 0.5 * ( HO - b1 + ( ( HO - b1 )^2 + 4 * b2 * HO * age^( -b3 ) )^0.5 )
    H_pi <- ( b1 + R ) / ( 1 + ( b2 / R * 40^( -b3 ) ) ) + 1.3
    
    #birch 
    b1 <- 394
    b2 <- 1.387
    k <- 7
    d1 <- b1 / ( k^b2 )
    R <- ( ( HO - d1 )^2 + 4 * b1 * HO / age^b2 )^0.5
    H_bi <- ( HO + d1 + R ) / ( 2 + 4 * b1 * 40^( -1 * b2 ) / ( HO - d1 + R )) + 1.3
    
    SIcalc[ SP == 1 ] <- H_sp[ SP == 1 ]
    SIcalc[ SP == 2 ] <- H_pi[ SP == 2 ]
    SIcalc[ SP == 3 ] <- H_bi[ SP == 3 ]
    
  }
  
  return( SIcalc )
  
}
