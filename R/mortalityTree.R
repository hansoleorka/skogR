#' Calculates 5 year mortality rate for tree-species
#' 
#' #' 'Model for survival. Original model by Eid & Tuhus (2005). Later (april 2007) the model was modified to a more simple model and to include basal area. Eid, T. & Tuhus, E., (2005). Models for individual tree mortality in Norway. For. Ecol. Manage. 154: 69-84
#' 
#' @name mortalityTree
#' 
#' @param species species code (1=spruce, 2= pine, 3= brich, 4 = other broadleaves )
#' @param d  Diameter of the single tree at breast height (cm), Diameter (cm)(Publication uses cm, parameter estimate adjusted)
#' @param BA Stand basal area (m2ha-1)
#' @return 5 year mortality rate
#' @details Based on SAS code from 'Ole Martin Bollandsås, 16.01.2007
#' @author Hans Ole Ørka \email{hans.ole.orka@@gmail.org}
#' @references Vestjordet 1959,Brantseg 1959,Næsset 1994a, Næsset 1995a,b
#' @examples
#' sp <- c(1,1,2,4)
#' d <- c(20,25,10,5)
#' BA <- 14
#' mortalityTree(d,sp,BA)
#' BA <- c(14,14,10,10)
#' mortalityTree(d,sp,BA)
#' @export
mortalityTree <- function(d,sp, BA){
     
     d=d*10.;
     #Spruce ( sp==1)
     MortalityR1 = (1 + exp(-1 * (-2.4916 - (0.02 * d) + (0.000032 * d * d) + (0.0308 * BA))))^ (-1.)
     #}else if ( sp == 2){
     MortalityR2 = (1 + exp(-1 * (-1.8079 - (0.0267 * d) + (0.000033 * d * d) + (0.055 * BA))))^(-1.)
     #}else if ( sp == 3){
     MortalityR3 =(1 + exp(-1 * (-2.1876 - (0.0157 * d) + (0.000027 * d * d) + (0.0295 * BA))))^(-1.)
     #}else if ( sp == 4){
     MortalityR4 =(1 + exp(-1 * (-1.5512 - (0.0111 * d) + (0.000014 * d * d) + (0.0159 * BA))))^(-1.)

     
     MortalityR=rep(NA,length( sp ));
     MortalityR[ sp==1] <- MortalityR1[ sp==1]
     MortalityR[ sp==2] <- MortalityR2[ sp==2]
     MortalityR[ sp==3] <- MortalityR3[ sp==3]
     MortalityR[ sp==4] <- MortalityR4[ sp==4]
     
     return(MortalityR)
}