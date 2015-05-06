# A script to apply an I-VT fixation filter on "data"
# created 19.11.2012
# updated 25.03.2014
# Iyad Aldaqre
# make as function(data,threshold=velthresh,write=T,writeDir=getwd())

# include a conditional statement to check if velocities were calculated, otherwise run simple i-vt

##############################
### calculating velocities ###
##############################
# create columns of previous samples
data$startTime<-c(0,data$timeStamp[1:(length(data$timeStamp)-1)]) # saccades starting time
data$startX<-c(data$gazePointX[1],data$gazePointX[1:(length(data$gazePointX)-1)]) # saccades starting position X
data$startY<-c(data$gazePointY[1],data$gazePointY[1:(length(data$gazePointY)-1)]) # saccades starting position Y
data$dtime<-data$timeStamp-data$startTime # should we keep this?

# euclodian distance -> sqrt((x1-x2)^2+(y1-y2)^2)
data$vel<-sqrt((data$gazePointX-data$startX)^2+(data$gazePointY-data$startY)^2)/data$dtime # pixel/ms

# aoi of saccade start
data$startAoi<-c(data$aoi[1],data$aoi[1:length(data$aoi)-1])
# data$postAoi<-c(data$aoi[2:length(data$aoi)],0)

# group saccades
data$X<-0
data$X[data$vel>=velthresh&data$dtime<=34]<-1 #mark all saccade samples
data$preSac<-c(0,data$X[1:length(data$X)-1])
data$postSac<-c(data$X[2:length(data$X)],0)

data$saccade<-0
data$saccade[data$X==1&data$preSac==1]<-NA # maximum there are two missing samples before; 34 is ceiling for sample time * 2
saccount<-seq(1:length(data$vel[data$X==1&data$preSac==0]))
data$saccade[data$X==1&data$preSac==0]<-saccount
data$saccade<-na.locf(data$saccade,fromLast=FALSE)

# prepare for export saccades table, similar to that of fixations
data$endX<-NA
data$endX[nrow(data)]<-data$gazePointX[nrow(data)]
data$endX[data$X==1&data$postSac==0]<-data$gazePointX[data$X==1&data$postSac==0]
data$endX<-na.locf(data$endX,fromLast=TRUE)

data$endY<-NA
data$endY[nrow(data)]<-data$gazePointY[nrow(data)]
data$endY[data$X==1&data$postSac==0]<-data$gazePointY[data$X==1&data$postSac==0]
data$endY<-na.locf(data$endY,fromLast=TRUE)

data$endTimeStamp<-NA
data$endTimeStamp[nrow(data)]<-data$timeStamp[nrow(data)]
data$endTimeStamp[data$X==1&data$postSac==0]<-data$timeStamp[data$X==1&data$postSac==0]
data$endTimeStamp<-na.locf(data$endTimeStamp,fromLast=TRUE)

data$endAoi<-NA
data$endAoi[nrow(data)]<-data$aoi[nrow(data)]
data$endAoi[data$X==1&data$postSac==0]<-data$aoi[data$X==1&data$postSac==0]
data$endAoi<-na.locf(data$endAoi,fromLast=TRUE)

saccadeData<-data[data$X==1&data$preSac==0,]

# rbind(data[is.element(data$timeStamp,unique(data$stimtime)),])

# Saving data file with velocities
dataFile<-paste(workdir3,'/',subject_names[i],'_preprocessed-data.txt',sep='')
write.table(data,dataFile, quote=FALSE, sep="\t", na="NA",row.names=FALSE,col.names=TRUE)

saccadeFile<-paste(workdir5,'/',subject_names[i],'_saccades.txt',sep='')
write.table(saccadeData,saccadeFile, quote=FALSE, sep="\t", na="NA",row.names=FALSE,col.names=TRUE)

print('Saccades Identified!')
