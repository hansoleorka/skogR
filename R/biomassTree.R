#' BIOMASS FUNCTIONS (Marklund, 1988)
#' 
#' Single-tree biomass functions for spruce, pine and birch.
#'
#' @param d Numerical vector with diameter at breast height (cm)
#' @param h Numerical vector with tree heights (m)
#' @param sp Numerical vector with tree species (1 = Norway spruce, 2 = Scots Pine, 3 = Birch)
#' @param components String vector specifying which biomass components to return:
#' 
#'  \itemize {
#'  \item \bold{sb} \hspace{20 mm} Biomass of stem bark
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
#'  \item \bold{ab}  Total aboveground biomass (st+cr)
#' 
#'  \item \bold{all }  All the above components
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
#' spruce and birch in Sweden (Report No. 45). Swedish University of Agricultural Sciences, Ume?.

#'
#' @author Marius Hauglin (2013) \email{marius.hauglin@@umb.no}



biomassTree<-function(d,h=NA,sp,components=c('all')){
	
# test data consistency
	if (length(sp) != 1 & length(sp) != length(d))stop('sp not of length one, or same length as d.')
	if (length(sp) == 1) sp<-rep(sp,length(d))
	
	if (!is.na(h[1])) {
		includeheights<-TRUE
		if (length(h) != length(d)) stop ('d and h not of the same length.')
	} else {includeheights<-FALSE}	
	
	
	# create logical variables from components string vector
	br<-db<-fl<-cr<-su<-rf<-rc<-rs<-sb<-sr<-sw<-st<-ab<- FALSE
	for (i in components) eval( parse(text=paste("if ('",i,"' %in% components) ",i," <- TRUE",sep='')) )
	if ('all' %in% components) br<-db<-fl<-cr<-su<-rf<-rc<-rs<-sb<-sr<-sw<-st<-ab<- TRUE
	
	
	# Template functions
	dfun<-function(dAdd,dCoef,const) exp(const+dCoef*(d/(d+dAdd)))
	dhfun<-function(dAdd,dCoef,hCoef,lnhCoef,const)exp(const+dCoef*(d/(d+dAdd))+hCoef*h+lnhCoef*log(h))	
	
	
# Norway spruce	
	if (includeheights){
		
		if (cr) cr_1<- dhfun(13,10.9708,-0.0124,-0.4923,-1.2063)
		if (db) db_1<- dhfun(18,3.6518,0.0493,1.0129,-4.6351) 
		if (sw) sw_1<- dhfun(14,7.2309,0.0355,0.7030,-2.3032)  
		if (fl) fl_1<-dhfun(12,9.7809,0,-0.4873,-1.8551)
		if (sb) sb_1<-dhfun(15,8.3089,0.0147,0.2295,-3.4020)
		if (sr) sr_1<-dfun(14,10.5381,-2.4447) # Not h
		if (su) su_1<-dfun(17,10.6686,-3.3645)# Not h
		if (rc) rc_1<-dfun(8,13.3703,-6.3851) # Not h
		if (rf) rf_1<-dfun(12,7.6283,-2.5706) # Not h
		if (st) st_1<-dhfun(14,7.4690,0.0289,0.6828,-2.1702)
	} else {
		
		if (db) db_1<- dfun(18,9.9550,-4.3308) 
		if (sw) sw_1<- dfun(14,11.4873,-2.2471)  
		if (cr) cr_1<- dfun(13,8.5242,-1.2804)
		if (fl) fl_1<-dfun(12,7.8171,-1.9602)
		if (sb) sb_1<-dfun(15,9.8364,-3.3912)
		if (sr) sr_1<-dfun(14,10.5381,-2.4447)
		if (su) su_1<-dfun(17,10.6686,-3.3645)
		if (rc) rc_1<-dfun(8,13.3703,-6.3851)
		if (rf) rf_1<-dfun(12,7.6283,-2.5706)
		if (st) st_1<-dfun(14,11.3341,-2.0571)

	}
	


# Scots pine	
	if (includeheights){
		
		if (cr) cr_2<- dhfun(10,13.3955,0,-1.1955,-2.5413)
		if (db) db_2<- dhfun(10,7.1270,-0.0465,1.1060,-5.8926) 
		if (sw) sw_2<- dhfun(14,7,6066,0.0200,0.8658,-2.6864)  
		if (fl) fl_2<-dhfun(7,12.1095,0.0413,-1.5650,-3.4781)
		if (sb) sb_2<-dhfun(14,7.6066,0.0200,0.8658,-2.6864)
		if (st) st_2<-dhfun(13,7.5939,0.0151,0.8799,-2.6768)
		if (sr) sr_2<-dfun(12,11.1106,-3.3913) # Not h
		if (su) su_2<-dfun(15,11.0481,-3.9657) # Not h
		if (rc) rc_2<-dfun(9,13.2902,-6.3413) # Not h
		if (rf) rf_2<-dfun(10,8.8795,-3.8375) # Not h
		
	} else {
		
		if (cr) cr_2<- dfun(10,9.1015,-2.8604)
		if (sw) sw_2<-dfun(14,11.4219,-2.2184)
		
		if (db) db_2<- dfun(10,9.5938,-5.3338) 
		if (fl) fl_2<-dfun(7,7.77681,-3.7983)
		if (sb) sb_2<-dfun(16,8.8489,-2.9748)
		if (sr) sr_2<-dfun(12,11.1106,-3.3913)
		if (su) su_2<-dfun(15,11.0481,-3.9657)
		if (rc) rc_2<-dfun(9,13.2902,-6.3413)
		if (rf) rf_2<-dfun(10,8.8795,-3.8375)
		if (st) st_2<-dfun(13,11.3264,-2.3388)
	}
	
	
# Birch	
	if (includeheights){
		
		if (cr) cr_3<- dfun(10,10.2806,-3.3633) # Not h	
		if (sw) sw_3<- dhfun(11,8.1184,0,0.9783,-3.3045) 
		if (db) db_3<- dhfun(30,11.2872,-0.3081,2.6821,-6.6237) 
		if (sb) sb_3<-dhfun(14,8.3019,0,0.7433,-4.0778)
		if (st) st_3<-dhfun(7,8.2827,0.0393,0.5772,-3.5686)

		if (su) su_3<-rep(NA,length(d)) # Functions do not exist for these components
		if (rc) rc_3<-rep(NA,length(d))
		if (rf) rf_3<-rep(NA,length(d))
		if (fl) fl_3<-rep(NA,length(d))
		if (sr) sr_3<-rep(NA,length(d))
		
		
		
	} else {
		
		if (cr) cr_3<- dfun(10,10.2806,-3.3633)
		if (sw) sw_3<- dfun(11,10.8109,-2.3327) 
		if (db) db_3<- dfun(5,7.9266,-5.9507) 
		if (sb) sb_3<-dfun(14,10.3876,-3.2518)
		if (st) st_3<-dfun(8,11.0735,-3.0932)
		
		if (su) su_3<-rep(NA,length(d)) # Functions do not exist for these components
		if (rc) rc_3<-rep(NA,length(d))
		if (rf) rf_3<-rep(NA,length(d))
		if (fl) fl_3<-rep(NA,length(d))
		if (sr) sr_3<-rep(NA,length(d))
	}
	
	
	
	# Calculate combined components
	if (br) {
		br_1<-cr_1-fl_1
		br_2<-cr_2-fl_2
		br_3<-cr_3-fl_3
	}
	
	if (rs) {
		rs_1<-rc_1+rf_1
		rs_2<-rc_2+rf_2
		rs_3<-rc_3+rf_3
	}
	
	if (ab) {
		ab_1<-st_1+cr_1
		ab_2<-st_2+cr_2
		ab_3<-st_3+cr_3
	}
	
	
	
	# Subset according to species
	if ('all' %in% components) {
		out.colums<-c('br','db','fl','cr','su','rf','rc','rs','sb','sr','sw','st','ab')
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



