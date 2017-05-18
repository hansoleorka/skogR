#' fieldPlot
#' 
#' Calculates volume, basal area, stem number and mean and dominant height on field plots
#'
#' @param plotid vector giving the plotid for each tree
#' @param d numerical vector; diameters in cm.
#' @param sp numerical vector; species. (1 = spruce, 2 = pine, 3 = deciduous).
#' @param h numerical vector; heights in m.
#' @param correction logical; Should be TRUE if sample trees are selected with relascope but from only inside the plot.
#' @param rel_factor numerical; if correction is TRUE a relascope factor must be given.
#' @param plot_radius numerical; the plot radius in m. 
#' @param outside_plot logical vector; indicating if a tree is outside the plot. Optional. 
#' @param biomass_components character vector; calculate plotwise biomass for the given components. See help 
#' in function biomassTree. 
#' Trees outside the plot will only be used to calculate the correction factor.
#' @param separate_sampletrees logical; Determines if sample trees are only used for 
#' @param corf_method numeric; Method to use if there are missing sample trees for one or more species on a sample plot:
#' (1) Convert the correction factor from one of the other species on the plot, using the corFactor function. In this alternative no data 
#' from other plots are used.
#' (2) Use the mean correction factor for the species for all plots. 
#' @param  min_samtr numeric; Minimum number of sample trees allowed for each species in one plot. If the number of sample trees for one species 
#' is below this number then the mean correction factor for all plots are used instead of the local correction factor.
#' @param output_heights logical; Estimated heights are returned for all trees. 
#' 
#' @return a data frame with the following columns:
#' if output_heights is TRUE: a list with two items, a data.frame with plotwise results, and a data.frame with the heights for all trees in the dataset.
#' 
#'
#' @references Eid, T., Fitje, A., and Hoen, H.F. (2002) Økonomi og planlegging. Gan Forlag, Oslo. 205 s.
#' @references Bollandsås, O.M. (2007). Calculating volume on the field plots of the Norwegian National Forest Inventory. Unpublished. 
#'
#' 
#' @author Marius Hauglin 2017 \email{marius.hauglin@@gmail.com}
#' @export



fieldPlot<-function(plotid,d,sp,h,
		correction=FALSE,
		rel_factor=NA,
		plot_radius=NA,
		outside_plot=NA,
		biomass_components=NA,
		separate_sampletrees=FALSE,
		corf_method=1,
		min_samtr=3,
		output_heights=FALSE){

	
if (length(outside_plot) == 1) outside_plot<-rep(FALSE,length(plotid))
if (is.na(rel_factor[1])) rel_factor<-rep(NA,length(plotid))
if (is.na(plot_radius[1])) plot_radius<-rep(NA,length(plotid))

if (length(rel_factor) == 1) rel_factor<-rep(rel_factor,length(plotid))
if (length(plot_radius) == 1) plot_radius<-rep(plot_radius,length(plotid))

if (!is.na(biomass_components[1]))  {
	if (biomass_components[1] == 'all') biomass_components <- c('sb','sw','st','fl','cr','br','db','su','rf','rc','rs','sr','ab')
	bmvars<-paste0('BM_',c(biomass_components,paste0(biomass_components,'_s'),paste0(biomass_components,'_p'),paste0(biomass_components,'_d')))
}

# check consistency / give warnings
if ( length(unique(c(length(plotid),length(d),length(sp),length(h),length(outside_plot)) )) >1 )stop ('all input vectors must be of equal length.')
if (correction) if (is.na(rel_factor[1]) | is.na(plot_radius[1])) stop ('radius and relascope factor needed for correction.')
if (correction) if (any(outside_plot)) warning ('correction=TRUE and trees outside_plot')


prtre<-!is.na(h)
klaving<-data.frame(plotid=plotid,d=d,sp=sp,h=h,outside_plot=outside_plot,rel_factor=rel_factor,radius=plot_radius,prtre=prtre)

# grunnflate alle trær
klaving$grfl<-((klaving$d/100)*(klaving$d/100) )*(pi/4) # m2

# basisvolum
klaving$basish<-basisHeight(d=klaving$d,sp=klaving$sp)
klaving$basisvol<-volumeTree(d=klaving$d,sp=klaving$sp,h=klaving$basish)

# volum prøvetrær (andre får NA)
klaving$vol<-volumeTree(d=klaving$d,sp=klaving$sp,h=klaving$h)


# correction: calculate weights
klaving$weight<-1
if (correction){
	klaving$ps_ha<-( (klaving$radius * klaving$radius) * pi ) /10000 # m2
	klaving$limit_diam<-klaving$radius/(50/sqrt(klaving$rel_factor)) # m
	klaving$weight[klaving$d/100 < klaving$limit_diam]<-( (klaving$rel_factor*klaving$ps_ha)/klaving$grfl )[klaving$d/100 < klaving$limit_diam]
}



# korreksjonsfaktor prøvetrær (andre får NA)
klaving$kf<-klaving$vol/klaving$basisvol


# sett korreksjonsfaktor for volum flatevis (metode 1)
if (corf_method == 1){
	for (i in unique(klaving$plotid)) {
		
		kf1<-kf2<-kf3<-NA
		
		kf1<-weighted.mean(klaving$kf[klaving$plotid == i & klaving$sp == 1],klaving$weight[klaving$plotid == i & klaving$sp == 1],na.rm=TRUE)
		kf2<-weighted.mean(klaving$kf[klaving$plotid == i & klaving$sp == 2],klaving$weight[klaving$plotid == i & klaving$sp == 2],na.rm=TRUE)
		kf3<-weighted.mean(klaving$kf[klaving$plotid == i & klaving$sp == 3],klaving$weight[klaving$plotid == i & klaving$sp == 3],na.rm=TRUE)
		
		# setter til NA hvis det er for få prøvetrær av ett treslag
		if (sum(!is.na(klaving$kf[klaving$plotid == i & klaving$sp == 1])) < min_samtr) kf1<-NA
		if (sum(!is.na(klaving$kf[klaving$plotid == i & klaving$sp == 2])) < min_samtr) kf2<-NA
		if (sum(!is.na(klaving$kf[klaving$plotid == i & klaving$sp == 3])) < min_samtr) kf3<-NA
		
		
		# hvis det mangler prøvetre av et treslag, konverter fra kor.faktor fra et av de andre treslagene
		if (is.na(kf1))kf1<-corFactor(sp=2,cf=kf2)[1]
		if (is.na(kf1))kf1<-corFactor(sp=3,cf=kf3)[1]
		
		if (is.na(kf2))kf2<-corFactor(sp=1,cf=kf1)[2]
		if (is.na(kf2))kf2<-corFactor(sp=3,cf=kf3)[2]
		
		if (is.na(kf3))kf3<-corFactor(sp=1,cf=kf1)[3]
		if (is.na(kf3))kf3<-corFactor(sp=2,cf=kf2)[3]
		
		klaving$kf[klaving$plotid == i & klaving$sp == 1]<-kf1
		klaving$kf[klaving$plotid == i & klaving$sp == 2]<-kf2
		klaving$kf[klaving$plotid == i & klaving$sp == 3]<-kf3
		
	}
} else if (corf_method == 2){
	
	
# sett korreksjonsfaktor for volum metode 2 (se hjelp)
	for (i in unique(klaving$plotid)) {
		for (j in unique(klaving$sp)) {
			
			klaving$kf[klaving$plotid == i & klaving$sp == j & is.na(klaving$kf)]<-
					weighted.mean(klaving$kf[klaving$plotid == i & klaving$sp == j],
							klaving$weight[klaving$plotid == i & klaving$sp == j],na.rm=TRUE)
			
			klaving$kf[klaving$plotid == i & 
							klaving$sp == j & 
							is.na(klaving$h) & 
							nrow(klaving[klaving$plotid == i & klaving$sp == j & !is.na(klaving$h),]) < min_samtr]<-NA # hvis mindre enn X prøvetær
		}
	}
	
	
	
# global korreksjonsfaktor for de med mindre enn X trær av et treslag på flata
	for (i in unique(klaving$sp) ) {
		
		klaving$kf[is.na(klaving$kf) & klaving$sp == i]<-weighted.mean(klaving$kf[klaving$sp == i],
				klaving$weight[klaving$sp == i],
				na.rm=TRUE)	
		
	}
} # end corf_method == 2


# volum alle trær (prøvetrær beholder tidligere beregnet volum)
klaving$vol[is.na(klaving$vol)]<-klaving$basisvol[is.na(klaving$vol)]*klaving$kf[is.na(klaving$vol)]



# beregnede høyder alle trær - baklengs i volumfunksjoner
klaving$h_est<-heightFromVolume(klaving$vol,klaving$d,klaving$sp)


# setter inn feltmålt høyde for prøvetrær
klaving$h_est[!is.na(klaving$h)]<-klaving$h[!is.na(klaving$h)]

# hvis estimerte høyder skal returneres
if (output_heights) outh<-subset(klaving,select=c(plotid,d,h_est,kf,vol))

# ikke trær utenfor flatene (bare i beregning av korreksjonsfaktor over)
klaving<-klaving[!klaving$outside_plot,]


# ikke prøvetrær hvis dette er angitt (bare bruk til korr. faktor)
if (separate_sampletrees) klaving<-klaving[!klaving$prtre,]


# beregne biomasse hvis dette er oppgitt
if (!is.na(biomass_components[1])) klaving<-cbind(klaving,biomassTree(klaving$d,klaving$h,klaving$sp,biomass_components))	


# beregne per flate
resvars<-c('plotID','plotSIZE','G_s','G_p','G_d','G_tot','V_s','V_p','V_d','V_tot','N_s','N_p','N_d','N_tot','H_lor','H_dom')
if (!is.na(biomass_components[1])) resvars<-c(resvars,bmvars)
restab<-data.frame(matrix(nrow=length(unique(klaving$plotid)),ncol=length(resvars)))
colnames(restab)<-resvars
for (i in 1:nrow(restab)) {
 
	fn<-unique(klaving$plotid)[i]
	a<-klaving[klaving$plotid == fn,] # alle
	g<-klaving[klaving$plotid == fn & klaving$sp == 1,] # gran
	f<-klaving[klaving$plotid == fn & klaving$sp == 2,] # furu
	l<-klaving[klaving$plotid == fn & klaving$sp == 3,] # lauv
	
	area<-round((klaving$radius[klaving$plotid == fn][1])^2 * pi,0)
	
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
 
 # grunnflateveid middelhøyde
 # restab$H_lor[i]<-mean(a$h,na.rm=TRUE) # middelhøyde som snitt av trær plukket ut med relaskop  
   restab$H_lor[i]<-weighted.mean(a$h_est,a$grfl) # middelhøyde som vektet snitt av (estimerte) høyder
	
 # overhøyde
 restab$H_dom[i]<-domHeight(a$h_est,a$d,area) 
	
  
# biomasse (tonn/ha)
if (!is.na(biomass_components[1])) {
	spl1<-c('a','g','f','l')
	spl2<-c('','_s','_p','_d')
	for (j in biomass_components){
		for (k in 1:4) eval(parse(text=paste0('restab$BM_',j,spl2[k],'[i]<-(sum(',spl1[k],'$',j,'/1000)/area) * 10000') )) 
	}	
}
	
 
	
	
 
}

if (!output_heights) {

	return (restab)
	
} else {
	
	return (list(results=restab,heights=outh))
	
	
}



} # end fieldPlot function










		
		
		
		
		
		
		
		
		
		
		