#' siteIndexMultiTemp
#' 
#' Calculate site index from time series of top heights and years of measurement
#' 
#' @param Ho values of top heights in m
#' @param yrs numeric values of years of measurement (e.g. 2020)
#' @param SP species code (1=spruce, 2= pine, 3= birch)
#' @return list of three elements: (1) H40 site index (m), (2) calculated ages for the input values of height, and (3) absolute residual (m)
#' @details Reference age of 40 years
#' @seealso HeightGrowth, HeightDiffSi, SiteIndex
#' @author Lennart Noordermeer \email{lennart.noordermeer@gmail.com}
#' @references Sharma et al (2011)
#' @examples
#' Ho = c(17, 20, 23)
#' yrs = c(1999, 2010, 2022)
#' SP = 1
#' siteIndexMultiTemp( Ho , yrs , SP )
#' @export

siteIndexMultiTemp <- function( Ho , yrs , SP ){
     
     if( length( Ho ) < 2 ) { return( list( NA , NA , NA ) ) } # There needs to be at least two points in time 
     if(any(diff( Ho ) < 0 )) { return( list( NA , NA , NA ) ) } # All heights need to be increasing 
     if(any(diff( yrs ) < 0 )) { return( list( NA , NA , NA ) ) } # All years need to be increasing
     
     # rough indication
     residuals <- res <- a <- c()
     ages <- list()
     for( si in 1 : 40 ){
          lastage <- ageDBH(  si, Ho[ length( Ho ) ] , SP )
          lastyr <- yrs[ length( Ho ) ]
          lastyr
          r=1
          for( r in 1 : length( Ho ) ){
               if( length( lastage ) == 0 ){ residuals[ r ] <- a[ r ] <- NA 
               } else { residuals[ r ] <- Ho[ r ] - heightStandDominant( H40 = si, 
                                                            age = lastage - ( lastyr - yrs[ r ] ), 
                                                            SP = SP ) 
               a[ r ] <- lastage - ( lastyr - yrs[ r ] )
               }
          }
          ages[[ si  ]] <- a
          
          res[ si ] <- sum( abs( residuals ) ) 
     }
     
     Min <- which.min( res  ) - 1
     Max <- which.min( res  ) + 1
     
     # more precise
     residuals <- residual <- a <- c()
     ages <- list()
     
     si=2
     SI_seq <- seq( Min , Max , .1 )
     length( SI_seq )
     for( si in 1:length( SI_seq ) ){
          
          lastage <- ageDBH( SI_seq[ si ], Ho[ length( Ho ) ] , SP )
          lastyr <- yrs[ length( Ho ) ]
          lastage
          lastyr
          r=1
          for( r in 1 : length( Ho ) ){
               if( length( lastage ) == 0 ){ residuals[ r ] <- a[ r ] <- NA 
               } else { residuals[ r ] <- Ho[ r ] - heightStandDominant( H40 = SI_seq[ si ], 
                                                            age = lastage - ( lastyr - yrs[ r ] ), 
                                                            SP = SP ) 
               a[ r ] <- lastage - ( lastyr - yrs[ r ] )
               }
          }
          ages[[ si ]] <- a
          residual[ si ] <- sum( abs( residuals ) ) 
     }
     # minimize residuals
     #plot( residual, type = "l" ), ylim = c( 0 , 100 ) )
     return( list( SI_seq[ which.min( residual  ) ] , 
                   ages[[ which.min( residual ) ]], 
                   min( residual[ ! is.na( residual ) ] ) / length( Ho ) ) )
}

