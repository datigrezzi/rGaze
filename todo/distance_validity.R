# functions to get distance and validity averages from eye tracking data file
# based on read file which should contain distance and validity columns
# Iyad Aldaqre
# 26.11.2015
#########################
# to do:
# add keyword for column name to look for when selecting relevant columns
#########################
get.distance<-function(data,keyword='distance',ignore.case=T,na.rm=T){
	distData<-data[,grep(keyword,names(data),ignore.case=ignore.case)]
	meansDistLR<-rowMeans(distData,na.rm=na.rm)
	avgDistance<-mean(meansDistLR,na.rm=na.rm)
	return(avgDistance)
}

#######
get.validity<-function(data,keyword='validity',ignore.case=T,na.rm=T){
	valData<-data[,grep(keyword,names(data),ignore.case=ignore.case)]
	meansValLR<-rowMeans(valData,na.rm=na.rm)
	avgValidity<-mean(meansValLR,na.rm=na.rm)
	return(avgValidity)
}