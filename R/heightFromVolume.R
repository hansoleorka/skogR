#' heigthFromVolume
#' 
#' Calculate estimated heights by reversing the single-tree volume functions.
#'
#' @param vol numerical vector; single tree volume.
#' @param d numerical vector; diameters in cm.
#' @param sp numerical vector; species.
#' @param interval bounds of the possible height values. Used in the optimization function.
#'
#' @return a vector of the same length as the input with estimated heights.
#'
#'
#' @author Marius Hauglin 2016 \email{marius.hauglin@@gmail.com}
#' @export


heigthFromVolume<-function(vol,d,sp,interval=c(2,60)){
	
	esth<-rep(NA,length(vol))

	for (i in 1:length(vol)){	

	# function to minimize
	if (sp[i] == 1 & d[i] > 13) {
		minfun<-function(h) abs( vol[i] - (10.14 + 0.0124 * d[i] * d[i] * h + 0.03117 * d[i] * h * h - 0.36381 * h * h + 0.28578 * d[i] * h) )
	}
	
	if (sp[i] == 1 & d[i] > 10  & d[i] <= 13){
		minfun<-function(h) abs( vol[i] - (-31.57 + 0.0016 * d[i] * h * h + 0.0186 * h * h + 0.63 * d[i] * h - 2.34 * h + 3.2 * d[i]) )
	}
	
	if (sp[i] == 1 & d[i] <= 10) {
		minfun<-function(h) abs( vol[i] - (0.52 + 0.02403 * d[i] * d[i] * h + 0.01463 * d[i] * h * h - 0.10983 * h * h + 0.15195 * d[i] * h) )
	}
	
	if (sp[i] == 2 & d[i] <= 11) {
		minfun<-function(h) abs( vol[i] - (2.912 + 0.039994 * d[i] * d[i] * h - 0.001091 * d[i] * h * h) )
	}
	
	if (sp[i] == 2 & d[i] > 11){
		minfun<-function(h) abs( vol[i] - (8.6524 + 0.076844 * d[i] * d[i] + 0.031573 * d[i] * d[i] * h) )
	}
	
	
	if (sp[i] == 3){
		minfun<-function(h) abs( vol[i] - (-1.25409 + 0.12739 * d[i] * d[i] + 0.03166 * d[i] * d[i] * h + 0.0009752 * d[i] * h * h - 0.01226 * h * h - 0.004214 * d[i] * d[i] * (1.046 * d[i])) )
	}
	
	
	# optimization
	esth[i]<-optimize(minfun,interval=interval)$minimum
	
	}
	
	
	return(esth)
}



