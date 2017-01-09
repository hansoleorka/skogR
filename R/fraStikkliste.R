#' fraStikkliste
#' 
#' creates table of trees from a 'stikkliste'
#'
#' @param id vector of ids
#' @param sp numerical vector; species: 1 = spruce, 2 = pine, 3 = deciduous
#' @param stikkliste data.frame or matrix; number of trees in each diameter class. The columns are the diameter classes in increasing order. 
#' Number of rows equal to the length of id and sp vectors. 
#' @param classes numerical vector; diameter for each diameter class. Length must equal number of columns in stikkliste. 
#'
#' @return a data.frame with the columns: id, species and dbh (diameter).
#'
#'
#' @author Marius Hauglin 2017 \email{marius.hauglin@@gmail.com}
#' @export


fraStikkliste<-function(id,sp,stikkliste,classes=NA){

	if (is.na(classes[1])) classes<-1:ncol(stikkliste)
	
	# check consistency
	if (length(classes) != ncol(stikkliste)) stop ('classes != ncol(stikkliste)')
	
	st<-as.matrix(stikkliste)
	
	vars<-c('id','species','dbh')
	treliste<-data.frame(matrix(nrow=sum(st,na.rm=TRUE),ncol=length(vars)))
	colnames(treliste)<-vars	
	lin<-1
	
	for (i in 1:length(id)){
			
			for (k in 1:length(classes)){ # diameterklasse
				
				n<-st[i,k]
				if (!is.na(n)) for (m in 1:n) { treliste[lin,]<-c(id[i],sp[i],classes[k]) ; lin<-lin+1}
				
			}
		}
	
treliste<-treliste[order(treliste$id),]	
	
return (treliste)	
	
} # end function fraStikkliste
