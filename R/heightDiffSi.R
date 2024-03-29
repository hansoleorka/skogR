#' HeightDiffSi
#' 
#' Function for computing height-differential site index (DEPRECIATED)
#' 
#' @name heightDiffSi
#' 
#' @param Hdt1 a numeric vector of initial dominant heights (m) 
#' @param Hdt2 a numeric vector of dominant heights (m) at the second point in time
#' @param t observation period (nr of years between time points 1 and 2)
#' @param ts species code (1=spruce, 2= pine)
#' @param depreciated force to use function by setting FALSE
#' @return H40 site index (m)
#' @details Reference age of 40 years
#' @seealso SiteIndex, HeightDiffSi
#' @author Lennart Noordermeer \email{lennart.noordermeer@gmail.com}
#' @references Sharma et al (2011)
#' @examples
#' a <- c(8,20) # initial dominant heights
#' b <- c(10.23744,24.81046) # heights at time point 2
#' heightDiffSi(a, b, 15, 1)
#' @export

heightDiffSi <- function( Hdt1, Hdt2, t, ts, depreciated=TRUE) {
     if(depreciated){
          return("Function depreciated. Use faster alternative siteIndexMultiTemp instead")
          }
     
     if(depreciated == FALSE){ 
          Hdt1 <- Hdt1 - 1.3
          Hdt2 <- Hdt2 - 1.3
          
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
          
          boninterv <- 1 
          
          H40 <- seq( 5 , 29 , boninterv ) 
          
          ageint <- seq( 1 , 1000 , 1 ) 
          
          arrayt1 <- array( dim = c( length( Hdt1 ) , length( ageint ), length( H40 ) ) ) 
          agest1 <- matrix( nrow = length( Hdt1 ), ncol = length( H40 ) ) 
          
          for ( y in ( 1 : length( H40 ) ) ) { 
               for ( i in (1 : nrow( agest1 ) ) ) { 
                    for ( a in 1: length( ageint ) ) {  
                         X02 <- 0.5 * ( H40[ y ] - b1 + ( ( H40[ y ] - b1 )^2 + 4 * b2 * H40[ y ] * 40^( -b3 ) )^0.5 ) 
                         arrayt1[ i,a,y ] <- ( ( b1 + X02 ) / ( 1 + ( b2 / X02 * ( ageint[ a ] )^( -b3 ) ) ) )             
                         if ( arrayt1[ i,a,y ] - 1.3 >  Hdt1[ i ]  ) { 
                              agest1[ i,y ] <- ageint[ a ]  
                              break
                         }
                    }
               }
          }
          
          heights.t2 <- matrix( NA, nrow = nrow( agest1 ), ncol = ncol( agest1 ) ) 
          
          colnames( heights.t2 ) <- H40
          
          ovrebon <- rep( NA, length( Hdt2 ) ) 
          
          for( i in 1 : nrow( heights.t2 ) ) {
               for( j in 1 : ncol( heights.t2 ) ) {
                    heights.t2[ i,j ] <- heightGrowth(Hdt1[ i ] + 1.3, H40[ j ], t, ts)
                    if( is.na( heights.t2[ i,j ] ) ) {
                         j = j + 1
                    } else if(  heights.t2[ i,j ] - 1.3 > Hdt2[ i ] ) {
                         ovrebon[ i ] <- H40[ j ] 
                         break
                    }
               }
          }
          
          nedrebon <- ovrebon - boninterv 
          
          heightsnedrebon <- heightsovrebon <- rep( NA, length( Hdt1 ) ) 
          
          for( z in 1 : length( heightsnedrebon ) ) {
               idxnedre <- which( colnames( heights.t2 ) == nedrebon[ z ] )
               if ( identical( idxnedre, integer( 0 ) ) ) {
                    heightsnedrebon[ z ] <- 1 
               } else {
                    heightsnedrebon[ z ] <- heights.t2[ z, idxnedre ]
               }
          }
          
          for( y in 1: length( heightsovrebon ) ) {
               idxovre <- which( colnames( heights.t2 ) == ovrebon[ y ] )
               heightsovrebon[ y ] <- heights.t2[ y, idxovre ]
          }
          
          diffx <- heightsovrebon - heightsnedrebon 
          diffmine <- Hdt2 + 1.3 - heightsnedrebon
          ratio <- diffmine/diffx
          result <- nedrebon + ratio * boninterv 
          
          return ( result )
     }
     
}

