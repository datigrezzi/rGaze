# A funciton to parse Tobii Glasses 2 JSON data and output a more familiar dataframe
# TDOO: keep vts in separate array and add s value for each variable separately (or encode it like tobii did before)

read.tobii.glasses <- function(file, normalized = FALSE){
	require(reshape2)
	require(rjson)
	
	listJS2df <- function(list_of_JSON){
		if(length(list_of_JSON) > 0){
			tmp <- lapply(lapply(list_of_JSON, FUN=fromJSON), FUN=unlist)
			dfOut <- as.data.frame(matrix(unlist(tmp), byrow=T, nrow=length(tmp)), stringsAsFactors = F)
			names(dfOut) <- names(fromJSON(list_of_JSON[1]))
			return(dfOut)
		} else{
			return(NULL)
		}
	}
	
	glassesJSON<-readLines(file)
		
	# extract video time data
	tmp <- glassesJSON[grepl('vts', glassesJSON) & !grepl('evts', glassesJSON)]
	videoTime <-listJS2df(tmp)[,c(1,3)]
	videoTime$ts <- as.numeric(videoTime$ts)

	# extract gaze data
	tmp <- glassesJSON[grepl('gp', glassesJSON) & !grepl('gp3', glassesJSON)]
	gazeData <- listJS2df(tmp)[,-4]
	names(gazeData)[4:5] <- c("gpx", "gpy")
	gazeData$ts <- as.numeric(gazeData$ts)

	# extract pupil data
	tmp <- glassesJSON[grepl('pd',glassesJSON)]
	pupilData <- listJS2df(tmp)
	
	# extract gyroscope data
	tmp <- glassesJSON[grepl('gy', glassesJSON)]
	gyroData <- listJS2df(tmp)
	gyroData$ts <- as.numeric(gyroData$ts)

	# extract accelerometer data
	tmp <- glassesJSON[grepl('ac', glassesJSON)]
	accelerometerData <- listJS2df(tmp)
	accelerometerData$ts <- as.numeric(accelerometerData$ts)
	
	# extract sync out data
	tmp <- glassesJSON[grepl('sig', glassesJSON) & grepl('out', glassesJSON)]
	syncOutData <- listJS2df(tmp)
	
	# extract sync in data
	tmp <- glassesJSON[grepl('sig', glassesJSON) & grepl('in', glassesJSON)]
	syncInData <- listJS2df(tmp)
	
	##########################
	### Preparing datasets ###
	##########################
	### PUPIL
	pupilData$pd <- as.numeric(pupilData$pd)
	pupilData$ts <- as.numeric(pupilData$ts)
	pupilData$s <- as.numeric(pupilData$s)
	pupilData <- cbind(recast(pupilData, ts ~ eye, id.var=c("ts","eye"), measure.var="pd", mean),recast(pupilData, ts ~ eye, id.var=c("ts","eye"), measure.var="s", mean)[,c(2,3)])
	names(pupilData) <- c("ts","pdl","pdr","s_pdl","s_pdr")
	# merge with gaze data
	gazeData <- merge(gazeData, pupilData, by="ts", all=T)

	### SYNC OUT
	syncOutData$ts <- as.numeric(syncOutData$ts)
	syncOutData$sig <- as.numeric(syncOutData$sig)
	
	### SYNC IN
	if(!is.null(syncInData)){
		syncInData <- syncInData[,c(1,4)]
	} else {
		syncInData <- as.data.frame(cbind(syncOutData$ts, rep(0, nrow(syncOutData))), stringsAsFactors=F)
		names(syncInData) <- c("ts", "sig")
	}
	syncInData$ts <- as.numeric(syncInData$ts)
	syncInData$sig <- as.numeric(syncInData$sig)
	syncData <- merge(syncOutData[,-3], syncInData, by="ts", all=T)
	names(syncData)[3:4]<-c('sync.out','sync.in')
	
	### RETURN LIST OF SEPARATE DATASETS
	return(list("gaze" = gazeData, "gyro" = gyroData, "accelerometer" = accelerometerData, "vts" = videoTime, "sync" = syncData))
}