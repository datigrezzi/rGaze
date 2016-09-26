# Script to interpolate successive missing values up to interpthresh samples
# Created by Iyad Aldaqre on the 15th of November 2012
####################################
### Interpolating Missing Values ###
####################################
##REPLACE BY USING CUSTOM FUNCTION col.na.approx(cbind(x,y,maxgap=interpThresh,na.rm=T))
# resetting row names
rownames(data) <- seq(length=nrow(data))

# interpolating NAs introduced by adding Event Info ### THESE RAWS SHOULD BE REMOVED BEFORE!
# IN ANY CASE, KEEPING THEM WON'T BE BAD!
data$prex<-c(data$gazePointX[1],data$gazePointX[1:(length(data$gazePointX)-1)])
data$prey<-c(data$gazePointY[1],data$gazePointY[1:(length(data$gazePointY)-1)])
data$gazePointX[is.na(data$validityRight)&is.na(data$validityRight)]<-data$prex[is.na(data$validityRight)&is.na(data$validityRight)]
data$gazePointY[is.na(data$validityRight)&is.na(data$validityRight)]<-data$prey[is.na(data$validityRight)&is.na(data$validityRight)]

# marking missing gaze data
data$X<-0
data$gazePointX[data$validityLeft==4&data$validityRight==4]<-NA
data$gazePointY[data$validityLeft==4&data$validityRight==4]<-NA
data$X[is.na(data$gazePointX)&is.na(data$gazePointY)]<-1

# interpolating missing gaze data
datanarows<-as.numeric(row.names(data[data$X==1,]))

data$gazePointX<-na.approx(data$gazePointX,maxgap=interpthresh,na.rm=T) # find way to index interpolated values and mark them as '5' in validity
data$gazePointY<-na.approx(data$gazePointY,maxgap=interpthresh,na.rm=T) # na.rm=T deletes remaining NAs


data<-subset(data,select=-c(prex,prey))

# Check!
print('Interpolating missing values - done!')