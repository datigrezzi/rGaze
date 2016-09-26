# script to mark fixations after velocities are calculated
# and creating a fixation matrix and save it
# created 19.11.2012
# updated 25.03.2014
# by Iyad Aldaqre

# make as function(data,threshold=minthresh,aois=c(circular,polygon),write=T,writeDir=getwd())

# include a conditional statement to check if velocities were calculated, otherwise run simple i-vt

# create columns of previous samples
# use data$startTime data$startX and data$startY from simple i.vt

# mark stimulus changes, to split fixations spanning different stimuli  <-!!!!!!
data$preStim<-c(NA,as.character(data$event[1:length(data$event)-1]))
data$preStim[data$event==data$preStim]<-1
data$preStim[data$preStim!='1']<-0
data$preStim<-as.numeric(data$preStim)

# mark consecutive and slow samples as fixations
data$X<-0
data$X[data$dtime<=34 & data$vel<=velthresh & data$preStim==1]<-1 # making sure the samples are consecutive

# index and group fixations
data$preFix<-c(0,data$X[1:(length(data$X)-1)])
data$postFix<-c(data$X[2:length(data$X)],0)

data$fixation<-0
data$fixation[data$X==1&data$preFix==1]<-NA
fixCount<-seq(1:length(data$fixation[data$X==1&data$preFix==0]))
data$fixation[data$X==1&data$preFix==0]<-fixCount
data$fixation<-na.locf(data$fixation,fromLast=FALSE)

# prepare for export fixations table, similar to that of saccades
# x and y of fixations
xMeans<-tapply(data$gazePointX,data$fixation,mean,na.rm=T) # to avoid the mean of 0
yMeans<-tapply(data$gazePointY,data$fixation,mean,na.rm=T) # same
data$fixX[data$X==1&data$preFix==0]<-xMeans[2:length(xMeans)]
data$fixY[data$X==1&data$preFix==0]<-yMeans[2:length(yMeans)]

data$fixEndTime[nrow(data)]<-data$timeStamp[nrow(data)]
data$fixEndTimeStamp[data$X==1&data$preFix==0]<-data$timeStamp[data$X==1&data$postFix==0]
data$fixDuration[data$X==1&data$preFix==0]<-data$fixEndTimeStamp[data$X==1&data$preFix==0]-data$startTime[data$X==1&data$preFix==0]
# endTimeStamp and duration
fixations<-data[data$X==1&data$preFix==0,c('fixation','timeStamp','fixDuration','fixX','fixY','segment','subject','trial','event')]


## PLOT HERE and OVERLAY AFTER i>1
# if (i==1){plot(data$vel,type='p',col=i,cex=0.2)} else {points(data$vel,col=i, cex=0.2)}

names(fixations)<-c("fixationIndex","timeStamp","duration","gazePointX","gazePointY","segment","subject","trial","event")
# fixations<-subset(fixations, subset = Duration >= timethreshold)

# ####################
# ### marking AOIs ### making errors when there are no fixations
# ####################
fixations$aoi<-'out'
if(aoiType=='polygon'){
	for(ia in 1:naoi){fixations[point.in.polygon(fixations$gazePointX,fixations$gazePointY,aoimat[,1,ia],aoimat[,2,ia])>0,'aoi']<-aoinames[ia]}
	} else if(aoiType=='circular'){
		for(ia in 1:naoi){fixations$aoi[sqrt((fixations$gazePointX-aoimat[1,1,ia])^2+(fixations$gazePointY-aoimat[1,2,ia])^2)<=aoicorr[1,1,ia]]<-aoinames[ia]}
	}

## centroid check
## points(mean(temp$GazePointX),mean(temp$GazePointY),col='red')
## plot(temp$GazePointX,temp$GazePointY,col='blue')

# Saving fixation file
fixationFile<-paste(subject_names[i],'_fixations.txt',sep='')
setwd(workdir4)
write.table(fixations,fixationFile, quote=FALSE, sep="\t", na="NA",row.names=FALSE,col.names=TRUE)

# check point
print('Fixations Identified!')