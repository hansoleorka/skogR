#' HeightGrowth
#' 
#' Function for future dominant height prognoses
#' 
#' @param Hdt1 a numeric vector of initial dominant heights (m) 
#' @param H40 site index (m)
#' @param years observation period (nr of years between time points 1 and 2)
#' @param ts species code (1=spruce, 2= pine, 3= birch)
#' @return Expected dominant heights at time point 2
#' @details Reference age of 40 years
#' @seealso SiteIndex, HeightDiffSi
#' @author Lennart Noordermeer \email{lennart.noordermeer@gmail.com}
#' @references Eriksson (1997), Sharma et al (2011)
#' @examples
#' a <- c(8,20) # initial dominant heights
#' b <-c(8,20) # H40 site index values
#' HeightGrowth(Hdt1=a, H40=b, years=15, ts=1)
#' @export

HeightGrowth <- function( Hdt1, H40, years, ts ) {
  
  Hdt1 <- Hdt1 - 1.3
  
  ageint <- seq( 1 , 1000 , .01 )
  
  ages <- Hdt2 <- rep( NA , length( Hdt1 ) ) 
  
  if ( ts == 1 ) {
    b1 <- 18.9206        
    b2 <- 5175.18         
    b3 <- 1.1576         
  }
  
  if ( ts == 2 ) {
    b1 <- 12.8361        
    b2 <- 3263.99        
    b3 <- 1.1758          
  }
  
  if ( ts == 3 ) {     
    b1 <- 394
    b2 <- 1.387
    k <- 7
    d1 <- b1 / ( k^b2 )         
  }
  
  for ( i in 1 : length( Hdt1 ) ) { 
    for ( a in 1 : length( ageint ) ) { 
      if ( ts == 1 | ts ==2 ) {
        X0 <- 0.5 * ( H40[ i ] - b1 + ( (H40[ i ] - b1 )^2 + 4 * b2 * H40[ i ] * 40^( -b3 ) )^0.5 ) 
        h1 <- ( ( b1 + X0 ) / ( 1 + ( b2 / X0 * ageint[ a ]^( -b3 ) ) ) ) 
      }
      if ( ts == 3 ) {
        X0 <- ( ( H40[ i ] - d1 )^2 + 4 * b1 * H40[i] / 40^b2 )^0.5
        h1 <- ( H40[ i ] + d1 + X0 ) / ( 2 + 4 * b1 * ageint[ a ]^( -1 * b2 ) / ( H40[ i ] - d1 + X0 )) 
      }
      if ( h1 - 1.3 > Hdt1[ i ] ) {  
        ages[ i ] <- ageint[ a ]  
        break
      }
    }
  }
  
  if ( ts == 1 | ts ==2 ) {
    X0 <- 0.5 * ( Hdt1 - b1 + ( ( Hdt1 - b1 )^2 + 4 * b2 * Hdt1 * ages^( -b3 ) )^0.5 )
    Hdt2 <- ( b1 + X0 ) / ( 1 + ( b2 / X0 * ( ages + ( years ) )^( -b3 ) ) ) + 1.3
  }
  
  if ( ts == 3 ) {
    X0 <- ( ( Hdt1 - d1 )^2 + 4 * b1 * Hdt1 / ( ages )^b2 )^0.5
    Hdt2 <- ( Hdt1 + d1 + X0 ) / ( 2 + 4 * b1 * ( ages +  years )^( -1 * b2 ) / ( Hdt1 - d1 + X0 ) ) + 1.3
  }
  
  return( Hdt2 )
  
}
