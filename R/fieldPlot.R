#' fieldPlot
#' 
#' Calculates volume, basal area, stem number and mean and dominant height on field plots
#'
#' @param plotid vector giving the plotid for each tree
#' @param d numerical vector; diameters in cm.
#' @param sp numerical vector; species.
#' @param h numerical vector; heights in m.
#' @param correction logical; Should be TRUE if sample trees are selected from only inside the plot.
#' @param rel_factor numerical; if correction is TRUE a relascope factor must be given.
#' @param plot_radius numerical; if correction is TRUE a plot radius must be given. 
#' @param outside_plot logical vector; indicating if a tree is outside the plot. Optional.
#' @param esth_method numerical; giving the method to use for estimation of tree heights. See details.
#' 
#' @return a data frame with the following columns:
#' 
#'
#' @note Trees outside the plot will be used to calculate the correction factor.
#'
#' @references Eid, T., Fitje, A., and Hoen, H.F. (2002) Økonomi og planlegging. Gan Forlag, Oslo. 205 s.
#' @references Bollandsås, O.M. (2007). Calculating volume on the field plots of the Norwegian National Forest Inventory. Unpublished. 
#'
#' 
#' @author Marius Hauglin 2016 \email{marius.hauglin@@nmbu.no}
#' @export



fieldPlot<-function(plotid,d,sp,h,correction,rel_factor=NA,plot_radius=NA,outside_plot=NA,esth_method=1){

	
if (is.na(outside_plot[1])) outside_plot<-rep(FALSE,length(plotid))
if (is.na(rel_factor[1])) rel_factor<-rep(NA,length(plotid))
if (is.na(plot_radius[1])) plot_radius<-rep(NA,length(plotid))

if (length(rel_factor) == 1) rel_factor<-rep(rel_factor,length(plotid))
if (length(plot_radius) == 1) plot_radius<-rep(plot_radius,length(plotid))


# check consistency
if (!all.equal(length(plotid),length(d),length(sp),length(h),length(outside_plot))) stop ('all input vectors must be of equal length.')
if (correction) if (is.na(rel_factor[1]) | is.na(plot_radius[1])) stop ('radius and relascope factor needed for correction.')




klaving<-data.frame(plotid=plotid,d=d,sp=sp,h=h,outside_plot=outside_plot,rel_factor=rel_factor,radius=plot_radius)

# grunnflate alle trær
klaving$grfl<-((klaving$d/1000)*(klaving$d/1000) )*(pi/4) # m2

# basisvolum
klaving$basish<-basisHeight(d=klaving$d,sp=klaving$sp)
klaving$basisvol<-volumeTree(d=klaving$d,sp=klaving$sp,h=klaving$basish)

# volum prøvetrær (andre får NA)
klaving$vol<-volumeTree(d=klaving$d,sp=klaving$sp,h=klaving$h)


# correction: calculate weights
klaving$weight<-1
if (correction){
	klaving$ps_ha<-( (klaving$radius * klaving$radius) * pi ) /10000 # m2
	klaving$limit_diam<-klaving$radius/(50/sqrt(klaving$rel_factor))
	klaving$weight[klaving$d < klaving$limit_diam]<-(klaving$rel_factor*klaving$ps_ha)/klaving$grfl
}



# korreksjonsfaktor prøvetrær (andre får NA)
klaving$kf<-klaving$vol/klaving$basisvol
klaving$kf_h<-klaving$h/klaving$basish


# sett korreksjonsfaktor for volum
for (i in unique(klaving$plotid)) {
	for (j in unique(klaving$sp)) {
		
	klaving$kf[klaving$plotid == i & klaving$sp == j & is.na(klaving$kf)]<-
			weighted.mean(klaving$kf[klaving$plotid == i & klaving$sp == j],
						    klaving$weight[klaving$plotid == i & klaving$sp == j],na.rm=TRUE)
	
	klaving$kf[klaving$plotid == i & 
				klaving$sp == j & 
				is.na(klaving$h) & 
				nrow(klaving[klaving$plotid == i & klaving$sp == j & !is.na(klaving$h),]) < 3]<-NA # hvis mindre enn tre prøvtær
	}
}



# korreksjonsfaktor høyde, til bruk i beregning av estimert høyde -> beregning av overhøyde
for (i in unique(klaving$plotid)) {
	for (j in unique(klaving$sp)) {
		
		klaving$kf_h[klaving$plotid == i & klaving$sp == j & is.na(klaving$kf_h)]<-
		weighted.mean(klaving$kf_h[klaving$plotid == i & klaving$sp == j],
				klaving$weight[klaving$plotid == i & klaving$sp == j],na.rm=TRUE)
		
		klaving$kf_h[klaving$plotid == i & 
						klaving$sp == j & 
						is.na(klaving$h) & 
						nrow(klaving[klaving$plotid == i & klaving$sp == j & !is.na(klaving$h),]) < 3]<-NA # hvis mindre enn tre prøvtær
	}
}



# global korreksjonsfaktor for de med mindre enn tre trær av et treslag på flata
for (i in unique(klaving$sp) ) {
	
	klaving$kf[is.na(klaving$kf) & klaving$sp == i]<-weighted.mean(klaving$kf[klaving$sp == i],
			klaving$weight[klaving$sp == i],
			na.rm=TRUE)	
	
	klaving$kf_h[is.na(klaving$kf_h) & klaving$sp == i]<-weighted.mean(klaving$kf_h[klaving$sp == i],
			klaving$weight[klaving$sp == i],
			na.rm=TRUE)	
}


# volum alle trær
klaving$vol[is.na(klaving$vol)]<-klaving$basisvol[is.na(klaving$vol)]*klaving$kf[is.na(klaving$vol)]



# 1) beregnede høyder alle trær - korreksjonsfaktor
if (esth_method == 1) klaving$h_est<-klaving$basish*klaving$kf_h



# 2) beregnede høyder alle trær - baklengs i volumfunksjoner
if (esth_method == 2) klaving$h_est<-heigthFromVolume(klaving$vol,klaving$d,klaving$sp)


	
	
	

# setter inn feltmålt høyde for prøvetrær
klaving$h_est[!is.na(klaving$h)]<-klaving$h[!is.na(klaving$h)]

# ikke trær utenfor flatene (bare i beregning av korreksjonsfaktor over)
klaving<-klaving[!klaving$outside_plot,]




# beregne flatevariabler
resvars<-c('plotID','plotSIZE','G_s','G_p','G_d','G_tot','V_s','V_p','V_d','V_tot','N_s','N_p','N_d','N_tot','H_lor','H_dom')
restab<-data.frame(matrix(nrow=length(unique(klaving$plotid)),ncol=length(resvars)))
colnames(restab)<-resvars
for (i in 1:nrow(restab)) {
 
	fn<-unique(klaving$plotid)[i]
	a<-klaving[klaving$plotid == fn,] # alle
	g<-klaving[klaving$plotid == fn & klaving$sp == 1,] # gran
	f<-klaving[klaving$plotid == fn & klaving$sp == 2,] # furu
	l<-klaving[klaving$plotid == fn & klaving$sp == 3,] # lauv
	
	area<-klaving$area[klaving$plotid == fn][1]
	
 restab$plotID[i]<-fn
 restab$plotSIZE[i]<-round(area,0) # m2
 
 # grunnflate
 restab$G_s[i]<-(sum(g$grfl)/area) * 10000  # m2/ha
 restab$G_p[i]<-(sum(f$grfl)/area) * 10000 # m2/ha
 restab$G_d[i]<-(sum(l$grfl)/area) * 10000 # m2/ha
 restab$G_tot[i]<-(sum(a$grfl)/area) * 10000 # m2/ha
 
 # volum
 restab$V_s[i]<-(sum(g$vol/1000)/area) * 10000  # m3/ha
 restab$V_p[i]<-(sum(f$vol/1000)/area) * 10000  # m3/ha
 restab$V_d[i]<-(sum(l$vol/1000)/area) * 10000  # m3/ha
 restab$V_tot[i]<-(sum(a$vol/1000)/area) * 10000  # m3/ha
 
 # treantall
 restab$N_s[i]<-round((nrow(g)/area)*10000,0)  
 restab$N_p[i]<-round((nrow(f)/area)*10000,0)
 restab$N_d[i]<-round((nrow(l)/area)*10000,0)
 restab$N_tot[i]<-round((nrow(a)/area)*10000,0)
 
 # høyder
 restab$H_lor[i]<-mean(a$h,na.rm=TRUE) # trær plukket ut med relaskop  
 restab$H_dom[i]<-domHeight(a$h_est,a$DBH,area) 
	
  
 
}

return (restab)

} # end fieldPlot function










		
		
		
		
		
		
		
		
		
		
		