# A funciton to parse Tobii Glasses 2 JSON data and output a more familiar dataframe

read.tobii.glasses <- function(file, normalized = FALSE){
	require(reshape2)
	require(rjson)
	
	glassesJSON<-readLines(file)
	
	videoTime <- NULL
	gazeData <- NULL
	pupilData <- NULL
	gyroData <- NULL
	accelerometerData <- NULL
	syncOutData <- NULL
	syncInData <- NULL
	
	for(i in glassesJSON){
		if(grepl('vts', i) & !grepl('evts', i)){
			videoTime <- rbind(videoTime, t(unlist(fromJSON(i))))
		} else if(grepl('gp',i) & !grepl('gp3', i)){
			gazeData<-rbind(gazeData,t(unlist(fromJSON(i))))
		} else if(grepl('pd',i)){
			pupilData<-rbind(pupilData,t(unlist(fromJSON(i))))
		} else if(grepl('gy',i)){
			gyroData<-rbind(gyroData,t(unlist(fromJSON(i))))
		} else if(grepl('ac',i)){
			accelerometerData<-rbind(accelerometerData,t(unlist(fromJSON(i))))
		} else if(grepl('sig',i) & grepl('out', i)){
			syncOutData<-rbind(syncOutData,t(unlist(fromJSON(i))))
		} else if(grepl('sig',i) & grepl('in', i)){
			syncInData<-rbind(syncInData,t(unlist(fromJSON(i))))
		}
	}
	
	### Prepare video time
	videoTime <- as.data.frame(videoTime[,c(1,3)], stringsAsFactors = F)
	videoTime$ts <- as.numeric(videoTime$ts)

	### VIDEO AND GAZE
	gazeData <- as.data.frame(gazeData[,-4], stringsAsFactors = F)
	gazeData$ts <- as.numeric(gazeData$ts)
	gazeData <- merge(videoTime, gazeData, by="ts", all=T)
	names(gazeData)[5:6] <- c("gpx", "gpy")

	### PUPIL
	pupilData<-as.data.frame(pupilData,row.names = as.character(seq(1,nrow(pupilData),1)), stringsAsFactors = F)
	pupilData$pd <- as.numeric(pupilData$pd)
	pupilData$ts <- as.numeric(pupilData$ts)
	pupilData <- recast(pupilData, ts ~ eye, id.var=c("ts","eye"), measure.var="pd", mean)
	names(pupilData) <- c("ts","pdl","pdr")
	# merge with gaze data
	gazeData <- merge(gazeData, pupilData, by="ts", all=T)

	### GYRO
	gyroData <- as.data.frame(gyroData, stringsAsFactors = F)
	gyroData$ts <- as.numeric(gyroData$ts)
	gyroData <- merge(videoTime, gyroData, by="ts", all=T)

	### ACCELEROMETER
	accelerometerData <- as.data.frame(accelerometerData[,-2], stringsAsFactors = F)
	accelerometerData$ts <- as.numeric(accelerometerData$ts)
	accelerometerData <- merge(videoTime, accelerometerData, by="ts", all=T)

	### SYNC OUT
	syncOutData<-data.frame(syncOutData,row.names = as.character(seq(1,nrow(syncOutData),1)), stringsAsFactors=F)
	syncOutData$ts <- as.numeric(syncOutData$ts)
	syncOutData$sig <- as.numeric(syncOutData$sig)
	### SYNC IN
	if(!is.null(syncInData)){
		syncInData<-data.frame(syncInData,row.names = as.character(seq(1,nrow(syncInData),1)), stringsAsFactors=F)
		syncInData <- syncInData[,c(1,4)]
	} else {
		syncInData <- as.data.frame(cbind(syncOutData$ts, rep(0, nrow(syncOutData))), stringsAsFactors=F)
		names(syncInData) <- c("ts", "sig")
	}
	syncInData$ts <- as.numeric(syncInData$ts)
	syncInData$sig <- as.numeric(syncInData$sig)
	syncData <- merge(syncOutData[,-3], syncInData, by="ts", all=T)
	names(syncData)[3:4]<-c('sync.out','sync.in')

	return(list("gaze" = gazeData, "gyro" = gyroData, "accelerometer" = accelerometerData, "sync" = syncData))
}