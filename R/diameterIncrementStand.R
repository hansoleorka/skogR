#' Annual diameter increment with bark 
#' 
#' THIS FUNCTION CALCULATES ANNUAL DIAMETER INCREMENT IN MM WITH BARK FOR THE NORWEGIAN SPECIES 
#' SPRUCE, PINE, BIRCH AND ASPEN (GRAN, FURU, BJERK ELLER OSP)
#' 
#' @param SP species code (1=spruce, 2= pine, 3= brich)
#' @param Ho Topheight (m)
#' @param H40 Site index
#' @param N2 Number of stems after thining
#' @param T13 Age
#' @param Dmb Diamter under bark
#' @return Annual diameter increment with bark (mm)
#' @details TODO: Need to check if function is correct. 
#' @author Hans Ole Ørka \email{hans.ole.orka@@gmail.org}
#' @references Blingsmo, K. R. 1984. Diametertilvekstfunksjoner for bjork-,furu- og granbestand. NISK-rapport 7/84.
#' @export
#' diameterIncrementStand(1,17,150,20)
#' diameterIncrementStand(1,17,150,20,20,80)
diameterIncrementStand <- function(TS,H40,N2,Dmb,Ho=NULL,T13=NULL){
     
     if((is.null(Ho) & is.null(T13))==FALSE){
          #GRAN
          ViDBli1 = exp(7.65355+0.1315*log(Ho)-0.1901*log(H40)-0.4713*log(N2)-0.8364*log(T13)-0.0101*log(Dmb))
          #FURU
          ViDBli2 = exp(6.19685+0.1883*log(Ho)+0.3886*log(H40)-0.4927*log(N2)-0.6634*log(T13)-0.3355*log(Dmb))
          #BJØRK
          ViDBli3 = exp(8.38323+1.6871*log(Ho)-1.1495*log(H40)-0.4455*log(N2)-1.5151*log(T13)-0.1373*log(Dmb))
          
          ViDBli <- c()
          ViDBli[TS == 1] <- ViDBli1[TS == 1] 
          ViDBli[TS == 2] <- ViDBli2[TS == 2] 
          ViDBli[TS == 3] <- ViDBli3[TS == 3] 
     }     
     
     if(is.null(Ho) & is.null(T13)){
          #Men siden alder kan være trøblete så har også Blingsmo følgende funksjoner uten overhøyde og alder:
          #Gran
          ViDBli1 = exp(4.28193+0.7950*log(H40)-0.4836*log(N2)-0.7186*log(Dmb))
          #Furu
          ViDBli2 = exp(4.99200+1.2866*log(H40)-0.6457*log(N2)-1.1093*log(Dmb))
          #Lauv
          ViDBli3 = exp(3.90258+1.3080*log(H40)-0.5821*log(N2)-1.0051*log(Dmb))
          
          ViDBli <- c()
          ViDBli[TS == 1] <- ViDBli1[TS == 1] 
          ViDBli[TS == 2] <- ViDBli2[TS == 2] 
          ViDBli[TS == 3] <- ViDBli3[TS == 3] 
     }
     
     
     return(ViDBli)
}


