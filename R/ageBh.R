## ageBh ####
#'
#' Calculate age at breast height from site index, top height and species
#'
#' @param H40 site index
#' @param Ho top height, i.e. mean o the 100 largest trees with respect to dbh per hectar (m) 
#' @param SP species code (1=spruce, 2= pine, 3= birch)
#' @return age at breast height
#' @details reference age of 40 years
#' @author Lennart Noordermeer \email{lennart.noordermeer@gmail.com} 
#' modified by Maria Aasnes Moan \email{maria.asnes.moan@nmbu.no}
#' @examples
#' H40 <- c(11,14,17,20,23) 
#' Ho <- c(13.995,11.169,21.267,24.81046,28.32)
#' ageBh( H40 , Ho, 1 )
#' @export

ageBh <- function(H40, Ho, SP) { 
     
     H40 <- H40 - 1.3
     
     if ( SP == 1 ) {
          b1 <- 18.9206        
          b2 <- 5175.18         
          b3 <- 1.1576         
     }
     
     if ( SP == 2 ) {
          b1 <- 12.8361        
          b2 <- 3263.99        
          b3 <- 1.1758          
     }
     
     ages <- rep( NA, length( H40 ) )
     
     ageint <- seq( 1 , 1000 , 1 )
     
     for ( i in 1 : length( Ho ) ) { 
          for ( a in 1 : length( ageint ) ) { 
               X0 <- 0.5 * ( H40[ i ] - b1 + 
                                  ( (H40[ i ] - b1 )^2 + 4 * b2 * H40[ i ] * 40^( -b3 ) )^0.5 ) 
               h1 <- ( ( b1 + X0 ) / ( 1 + ( b2 / X0 * ageint[ a ]^( -b3 ) ) ) ) + 1.3
               if ( h1 == Ho[ i ]  |  h1 > Ho[ i ]) { 
                    ages[ i ] <- ageint[ a ]  
                    break
               }
          }
     }
     
     return( ages )
     
}