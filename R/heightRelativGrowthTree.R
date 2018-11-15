#' Calculates relative height growth of single trees
#' 
#' Calculate relative height growth of singler trees based on Sharma et al. 2017
#' 
#' @name heightRelativGrowthTree
#' 
#' @param d Diameter of the single tree at breast height (cm)
#' @param sp species code (1=spruce, 2= pine, 3= brich)
#' @param CIndex competition index to be used only "CI4" implemented
#' @return Height growth for the period (m) 
#' @details ?
#' @author Hans Ole Ørka \email{hans.ole.orka@@gmail.org}
#' @references Sharma, R.P. & Brunner, A., 2017. Modeling individual tree height growth of Norway spruce and Scots pine from national forest inventory data in Norway. Scandinavian journal of forest research / issued bimonthly by the Nordic Forest Research Cooperation Committee , 32(6), pp.501–514.
#' @examples
#' sp <- c(1,1,2,4)
#' d <- c(20,25,10,5)
#' heightRelativGrowthTree(d,sp)
#' @export
heightRelativGrowthTree <- function(d,sp,CIndex = "CI4"){
     #Single tree stuff
     ba <- pi*(d/200)^2
     #Compute BAL
     BAL <- c()
     for(i in c(1:length(ba))){
          BAL[i] <- sum(ba[ba>ba[i]])
     }
     
     
     #Parameters for Sharma and Brunner 2017
     param1 = structure(list(Species = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2), 
                             CI = c("CI1","CI2", "CI3", "CI4", "CI5", "CI1", "CI2", "CI3", "CI4", "CI5"), 
                             a = c(2.55, 2.97, 2.44, 3.14, 2.05, 8.23, 8.52, 7.99, 5.29,6.71), 
                             a_se = c(0.363, 0.41, 0.375, 0.433, 0.515, 0.699, 0.703, 0.628, 0.292, 0.713), 
                             b = c(-0.91, -1.077, -0.868, -1.16, -0.683, -2.96, -3.05, -2.87, -1.9, -2.35), 
                             b_se = c(0.156, 0.161, 0.165, 0.129, 0.282, 0.251, 0.251, 0.217, 0.0737, 0.242), 
                             c = c(0.745, 0.428, 0.745, -0.354, 0.335, 0.505, 0.46, 0.517, 0.525, 0.874), 
                             c_se = c(0.0998, 0.0531, 0.11, 0.0526, 0.166, 0.113, 0.0905, 0.0839, 0.0482, 0.114), 
                             p = c(2.02, 1.66, 2.07, 0, 0.038, 2.92, 2.25, 2.88, 0, 0.0579), 
                             p_se = c(0.251, 0.351, 0.255, NA, 0.0146, 0.466, 0.462, 0.409, NA, 0.012), 
                             q = c(2.53, 1.59, -0.829, 0,21.8, 0.755, 0.59, -0.203, 0, 9.42), 
                             q_se = c(0.189, 0.122, 0.101,  NA, 10.9, 0.324, 0.273, 0.0471, NA, 6.04), 
                             psi = c(-0.187, -0.185, -0.187, -0.184, -0.18, -0.336, -0.336, -0.336, -0.333, -0.336), 
                             psi_se = c(0.00498, 0.00497, 0.00497, 0.00498, 0.005, 0.00558,0.00555, 0.00561, 0.00551, 0.00557)), 
                              .Names = c("Species", "CI",  "a", "a_se", "b", "b_se", "c", "c_se", "p", "p_se", "q", "q_se","psi", "psi_se"), row.names = c(NA, -10L), class = "data.frame")
     
     #Competition index
     k <- ifelse(sp==1,500,100)
     CI4 <- BAL/(BAL+k*ba)
      

     #Relativ height growth
     CI <- CI4
     
     #Spruce
     param <- subset(param1, Species == 1 & CI==CIndex)
     ihrel1 = param$a * exp(param$b * (1 - CI)^param$c) *  (1 - CI)^param$c
     
     #Pine
     param <- subset(param1, Species == 2 & CI==CIndex)
     ihrel = param$a * exp(param$b * (1 - CI)^param$c) *  (1 - CI)^param$c
     
     ihrel[sp==1] <- ihrel1[sp==1]
     
     return(ihrel)
}
     
