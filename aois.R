#############################
# marking AOIs in one of two ways:
# circular: falling in the circle with radius x; distance from center
# polygon: falling into a polygon set by corners' coordinates
# Iyad Aldaqre
# 02.12.2015
#############################
# to add:static or dynamic (add correction and time in AOImat?)
# dynamicSegmentIndex=c(3,5) # use is.element
# segmentIndex # vector with the indices of all segments

#############################
# attributes:
# aoiCoordinateMatrix
# aoiNames=c()
# data[gazeX,gazeY]
# aoiType='polygon' # or 'circular'
# aoiCorrectedCoordinates # for dynamic (matrix) or for circular (vector)
# input:
# aoi coordinates matrix
# data frame with gaze coords and segments

# output:
# an AOI vector with the length of the data file from which coordinates was taken
aoi<-function(aoiCoordinates,aoiNames,gazeData,aoiType='polygon',aoiCorr){ # aoiCorr for circular radius (later deployment for dynamic AOIs)
	require(sp)
	aoiVector<-rep('out',nrow(gazeData))
	if(aoiType=='polygon'){
		aoiMatrixX<-aoiCoordinates[,c(1,2,2,1)]
		aoiMatrixY<-aoiCoordinates[,c(3,3,4,4)]
		for(v in 1:nrow(aoiCoordinates)){aoiVector[point.in.polygon(gazeData[,1], gazeData[,2],aoiMatrixX[v,],aoiMatrixY[v,])>0]<-aoiNames[v]}
	} else if(aoiType=='circular'){
		for(v in 1:nrow(aoiCoordinates)){aoiVector[sqrt((gazeData[,1]-aoiCoordinates[v,1])^2+(gazeData[,2]-aoiCoordinates[v,3])^2)<=aoiCorr[v]]<-aoiNames[v]}
	}
	return(aoiVector)
}


### DEBUG
# aoiCoordinateMatrix<-aoiMat
# gazeData<-inData[,c('GazepointX','GazepointY','segment')]
# aoiVector<-rep('out',nrow(gazeData))
# aoiType<-'polygon'
# if(aoiType=='polygon'){
	# aoiMatrixX<-cbind(aoiCoordinateMatrix[,1:2],aoiCoordinateMatrix[,2:1])
	# aoiMatrixY<-cbind(aoiCoordinateMatrix[,3],aoiCoordinateMatrix[,3],aoiCoordinateMatrix[,4],aoiCoordinateMatrix[,4])
	# for(v in 1:nrow(aoiMatrixX)){aoiVector[point.in.polygon(gazeData[,1], gazeData[,2],aoiMatrixX[v,],aoiMatrixY[v,])>0]<-aoiNames[v]}
# } else if(aoiType=='circular'){
	# aoiMatrix<-aoiCoordinateMatrix
	# for(v in 1:nrow(aoiMatrix)){aoiVector[sqrt((gazeData[,1]-aoiMatrix[v,1])^2+(gazeData[,2]-aoiMatrix[v,3])^2)<=aoiCorr[v]]<-aoinames[v]}
# }