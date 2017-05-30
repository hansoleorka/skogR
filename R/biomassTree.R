#' BIOMASS FUNCTIONS (Marklund, 1988)
#' 
#' Single-tree biomass functions for spruce, pine and birch.
#'
#' @param d Numerical vector with diameter at breast height (cm)
#' @param h Numerical vector with tree heights (m)
#' @param sp Numerical vector with tree species (1 = Norway spruce, 2 = Scots Pine, 3 = Birch)
#' @param components String vector specifying which biomass components to return:
#' 
#'  \itemize{
#'  \item \bold{sb}  Biomass of stem bark
#'  \item \bold{sw}  Stem wood biomass
#'  \item \bold{st}  Total stem biomass (sw+sb)
#'  \item \bold{fl}  Foliage biomass
#'  \item \bold{cr}  Crown biomass 
#'  \item \bold{br}  Branch biomass (cr-fl)
#'  \item \bold{db}  Biomass of dead branches
#'  \item \bold{su}  Stump biomass *
#'  \item \bold{rf}  Biomass of fine roots *
#'  \item \bold{rc}  Biomass of coarse roots *
#'  \item \bold{rs}  Biomass of roots (rc+rf) *
#'  \item \bold{sr}  Biomass of stump-root system *
#'  \item \bold{ab}  Total aboveground biomass (st + cr + su + db)
#'  \item \bold{bg}  Total belowground biomass (Petterson & Ståhl 2006)
#'  \item \bold{tb}  Total tree biomass (ab + rs)

#' 
#' 
#'  \item \bold{all}  All the above components
#'	}
#' 
#' @return A data frame with the dry weight biomass (kg) for the specified components. Column names and order will be the same as in components.
#'
#' @note \code{h} is optional. Marklund include functions with diameter alone, and with diameter 
#' and height as explanatory variables for most biomass components. 
#' Note that for some components only functions with 
#' diameter as single explanatory variable are available (marked with * in the list above). 
#' For these biomass components the 
#' values from the functions with diameter as single explanatory variable will 
#' be returned for spruce and pine, \emph{even if height values are provided.}
#' For birch there are no functions for these biomass components 
#' (marked with *) so NAs will be returned. 
#' In the case of the crown biomass component (\bold{cr}) for birch the function with 
#' diameter as single explanatory variable is the only  
#' available function, thus the value from this is returned, 
#' \emph{even if height values are provided.}
#'
#' @references Marklund, L.G., 1988. Biomass functions for pine, 
#' spruce and birch in Sweden (Report No. 45). Swedish University of Agricultural Sciences, Umeå.
#' @references Petersson, H. & Ståhl, G. 2006. Functions for below-ground biomass of Pinus sylvestris, Picea abies, Betula pendula and Betula pubescens in Sweden. 
#' Scandinavian Journal of Forest Research 21(Suppl 7): 84-93

#'
#' @author Marius Hauglin (2013-2017) \email{marius.hauglin@@nmbu.no}
#' @export


biomassTree<-function(d,h=NA,sp,components=c('all')){
	
# test data consistency
	if (length(sp) != 1 & length(sp) != length(d))stop('sp not of length one, or same length as d.')
	if (length(sp) == 1) sp<-rep(sp,length(d))
	
	if (!is.na(h[1])) {
		includeheights<-TRUE
		if (length(h) != length(d)) stop ('d and h not of the same length.')
	} else {includeheights<-FALSE}	
	
	
	# create logical variables from components string vector
	br<-db<-fl<-cr<-su<-rf<-rc<-rs<-sb<-sr<-sw<-st<-ab<-bg<-tb<- FALSE
	for (i in components) eval( parse(text=paste("if ('",i,"' %in% components) ",i," <- TRUE",sep='')) )
	if ('all' %in% components) br<-db<-fl<-cr<-su<-rf<-rc<-rs<-sb<-sr<-sw<-st<-ab<-bg<-tb<- TRUE
	
	
	# Template functions
	dfun<-function(dAdd,dCoef,const) exp(const+dCoef*(d/(d+dAdd)))
	dhfun<-function(dAdd,dCoef,hCoef,lnhCoef,const)exp(const+dCoef*(d/(d+dAdd))+hCoef*h+lnhCoef*log(h))	
	

	if (includeheights){
		
		# spruce
		cr_1<- dhfun(13,10.9708,-0.0124,-0.4923,-1.2063)
		idb_1<- dhfun(18,3.6518,0.0493,1.0129,-4.6351) 
		sw_1<- dhfun(14,7.2309,0.0355,0.7030,-2.3032)  
		fl_1<-dhfun(12,9.7809,0,-0.4873,-1.8551)
		sb_1<-dhfun(15,8.3089,0.0147,0.2295,-3.4020)
		st_1<-dhfun(14,7.4690,0.0289,0.6828,-2.1702)
	
		# pine
		cr_2<- dhfun(10,13.3955,0,-1.1955,-2.5413)
		db_2<- dhfun(10,7.1270,-0.0465,1.1060,-5.8926) 
		sw_2<- dhfun(14,7.6066,0.0200,0.8658,-2.6864)  
		fl_2<-dhfun(7,12.1095,0.0413,-1.5650,-3.4781)
		sb_2<-dhfun(16,7.2482,0,0.4487,-3.2765)
		st_2<-dhfun(13,7.5939,0.0151,0.8799,-2.6768)
		
		# birch
		sw_3<- dhfun(11,8.1184,0,0.9783,-3.3045) 
		db_3<- dhfun(30,11.2872,-0.3081,2.6821,-6.6237) 
		sb_3<-dhfun(14,8.3019,0,0.7433,-4.0778)
		st_3<-dhfun(7,8.2827,0.0393,0.5772,-3.5686)
		
	} else {
		
		# spruce
		db_1<- dfun(18,9.9550,-4.3308) 
		sw_1<- dfun(14,11.4873,-2.2471)  
		cr_1<- dfun(13,8.5242,-1.2804)
		fl_1<-dfun(12,7.8171,-1.9602)
		sb_1<-dfun(15,9.8364,-3.3912)
		st_1<-dfun(14,11.3341,-2.0571)

		# pine
		cr_2<- dfun(10,9.1015,-2.8604)
		sw_2<-dfun(14,11.4219,-2.2184)
		db_2<- dfun(10,9.5938,-5.3338) 
		fl_2<-dfun(7,7.77681,-3.7983)
		sb_2<-dfun(16,8.8489,-2.9748)
		st_2<-dfun(13,11.3264,-2.3388)
		
		# birch
		sw_3<- dfun(11,10.8109,-2.3327) 
		db_3<- dfun(5,7.9266,-5.9507) 
		sb_3<-dfun(14,10.3876,-3.2518)
		st_3<-dfun(8,11.0735,-3.0932)
		
	}
	
	# likt for både d og d+h

	# spruce	
	sr_1<-dfun(14,10.5381,-2.4447) # Not h
	su_1<-dfun(17,10.6686,-3.3645)# Not h
	rc_1<-dfun(8,13.3703,-6.3851) # Not h
	rf_1<-dfun(12,7.6283,-2.5706) # Not h
	
	# pine
	sr_2<-dfun(12,11.1106,-3.3913) # Not h
	su_2<-dfun(15,11.0481,-3.9657) # Not h
	rc_2<-dfun(9,13.2902,-6.3413) # Not h
	rf_2<-dfun(10,8.8795,-3.8375) # Not h
	
	# birch
	cr_3<- dfun(10,10.2806,-3.3633) # Not h	
	su_3<-su_2 # bruker funksjon for furu
	fl_3<-sw_3 * 0.011/0.52	   # faktor fra ? Zianis?
	rf_3<-sw_3 * 0.042/0.52   # faktor fra ? Zianis?      
	rc_3<-sw_3 * 0.042/0.52   # faktor fra ? Zianis?
	sr_3<-sr_2 # bruker funksjon for furu

	
	# under jorden - Petterson & Ståhl
	bg_1<-(exp((0.32308^2)  / 2 + 4.58761 + (d*10) / ((d*10) + 138) * 10.44035))/1000 # Petterson og Sthål 2006 Root limit 2 mm	
	bg_2<-(exp((0.35449^2)  / 2 + 3.44275 + (d*10) / ((d*10) + 113) * 11.06537))/1000
	bg_3<-(exp((0.36266^2)  / 2 + 6.1708  + (d*10) / ((d*10) + 225) * 10.01111))/1000 
	
	# Calculate combined components
	br_1<-cr_1-fl_1
	br_2<-cr_2-fl_2
	br_3<-cr_3-fl_3

	rs_1<-rc_1+rf_1
	rs_2<-rc_2+rf_2
	rs_3<-rc_3+rf_3

	ab_1<-sw_1+sb_1+cr_1+su_1+db_1 
	ab_2<-sw_2+sb_2+cr_2+su_2+db_2
	ab_3<-sw_3+sb_3+cr_3+su_3+db_3
	
	tb_1<-ab_1+rs_1
	tb_2<-ab_2+rs_2
	tb_3<-ab_3+rs_3
	
	
	# Subset according to species
	if ('all' %in% components) {
		out.colums<-c('br','db','fl','cr','su','rf','rc','rs','sb','sr','sw','st','ab','bg','tb')
	} else {out.colums<-components}
	out<-data.frame(matrix(nrow=length(d),ncol=length(out.colums)))
	colnames(out)<-out.colums

	for (i in out.colums) {
		eval( parse(text=paste('out$',i,'[sp == 1]<-',i,'_1[sp == 1]',sep='')) )
		eval( parse(text=paste('out$',i,'[sp == 2]<-',i,'_2[sp == 2]',sep='')) )
		eval( parse(text=paste('out$',i,'[sp == 3]<-',i,'_3[sp == 3]',sep='')) )
	}
	

	return(out)
	
} # End biomasseTree function	



