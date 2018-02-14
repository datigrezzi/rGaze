# a script to calculate average pupil baseline during certain time segments
# after calculating average baseline and subtracting it from each sample
# 
# sketch written on 11.2.2015
# initial version on 23.11.2017
# iyad aldaqre

# interpolate pupil missing for each eye and filter before averaging
# averaging might introduce false change between samples

# outlier functions to use inside pupil
stdOutlierIdx <- function(x, nSD = 3, na.rm=T){
	idx <- rep(0, length(x))
	idx[x < (mean(x, na.rm=na.rm) - (sd(x, na.rm=na.rm) * nSD)) | x > (mean(x, na.rm=na.rm) + (sd(x, na.rm=na.rm) * nSD))] <- 1
	return(idx)
}

diffOutlierIdx <- function(x, threshold = 0.5) {
	xDiff <- c(0, diff(x))
	idx <- rep(0, length(x))
	idx[abs(xDiff) > threshold] <- 1
	return(idx)
}

threshOutlierIdx <- function(x, min=0, max=6){
	idx <- rep(0, length(x))
	idx[x < min | x > max] <- 1
	return(idx)
}

# rm(list=ls())
# baselineIndex=NULL if no baseline is needed
# same with outlierParam
pupil <- function(data,timestampVar='ts', pupilVar='pupil', baselineIndex="baselineSegment", pupilIndex="pupilSegment", averageOutput=FALSE, samplingRate=60, interpolate=T, maxgap=Inf, outlierParam = list("threshold" = c(0,6), "sd" = 3, "change" = 0.05)){
	require(zoo)
	
	# check if baseline index exists in data, otherwise set baseline to zero
	if(is.null(baselineIndex)){
		baselineValue <- 0
		warning("No baseline index variable provided; Setting baseline to 0")
	}
	### get variables
	# pupil diameter
	pupilDiameter <- data[,pupilVar]
	# replace zeros with NAs
	pupilDiameter[pupilDiameter == 0] <- NA
	outlierIdx <- rep(0, length(pupilDiameter))
	if(length(pupilVar) == 2){
		pupilDiameter2 <- data[,pupilVar[2]]
		pupilDiameter2[pupilDiameter2 == 0] <- NA
		outlierIdx2 <- rep(0, length(pupilDiameter2))
	}
	
	# timestamp
	timestamp <- data[,timestampVar]
	# making sure data is consecutive (verifying timestamps)
	if(max(range(diff(timestamp)<=ceiling(1000/samplingRate))) > 1){
		warning("Data might not be continuous!")
	}
	
	# mark outliers for rejection when calculating means
	# outlier identifiers are inclusive of previous outlier type
	if(!is.null(outlierParam)){
		outlierType <- match.arg(names(outlierParam), c("threshold", "sd", "change"), several.ok=TRUE)
		if(is.element("threshold", outlierType)){
			outlierIdx[threshOutlierIdx(pupilDiameter, min=outlierParam$threshold[1])] <- 1
			# for second pupil data if available
			if(exists('pupilDiameter2')){
				outlierIdx2[threshOutlierIdx(pupilDiameter2, min=outlierParam$threshold[2])] <- 1
			}
		}
		if(is.element("sd", outlierType)){
			# should be done for baseline and pupil segments separately
			outlierIdx[stdOutlierIdx(pupilDiameter, nSD=outlierParam$sd)] <- 1
			# for second pupil data if available
			if(exists('pupilDiameter2')){
				outlierIdx2[stdOutlierIdx(pupilDiameter2, nSD=outlierParam$sd)] <- 1
			}
		}
		if(is.element("change", outlierType)){
			outlierIdx[diffOutlierIdx(pupilDiameter, outlierParam$change)] <- 1
			if(exists('pupilDiameter2')){
				outlierIdx2[diffOutlierIdx(pupilDiameter2, outlierParam$change)] <- 1
			}
		}
	}
	
	# interpolate if explicitly asked to
	if(interpolate==T){
		data[,variables[3]]<-na.approx(data[,variables[3]], maxgap=maxgap, na.rm=F)
	}
	
	# get unique stimulus names
	stimuli <- unique(as.character(data[data[,variables[2]] != "" & !is.na(data[,variables[2]]), variables[2]]))
	for(i in stimuli){
		# based on stimulus name, get start and end time
		startTime <- min(data[data[,variables[2]] == i,variables[1]])
		endTime <- max(data[data[,variables[2]] == i,variables[1]])
		# calculate relative timeline
		data$RelativeTimestamp[data[,variables[2]]==i] <- data[data[,variables[2]]==i,variables[1]]-startTime
		
		# calculate baseline
		data$Baseline[data[,variables[2]]==i] <- mean(data[data[,variables[1]] >= (startTime + baselineTime[1]) & data[,variables[1]] <= (startTime + baselineTime[2]),variables[3]], na.rm=T)
		# correct pupil
		data$CorrectedPupil[data[,variables[2]]==i] <- data[data[,variables[2]]==i,variables[3]]-data$Baseline[data[,variables[2]]==i]
	}
	return(data[!is.na(data$RelativeTimestamp),])
}

###########
## DEBUG ##
###########
# setwd('~/SRLABS_Drive/projects/pupilMarketing')
# # setwd('C:/Users/Aldaqre/Google Drive/projects/pupilMarketing')
# rawData <- read.table("IPSOS_Kinder-Pack_Data_Export.tsv",sep='\t',header=T,dec=",")
# inData <- rawData[rawData$ParticipantName=="A001",]

# pupilData <- pupil(inData, variables <- c('RecordingTimestamp','MediaName','PupilRight'))
# plot(pupilData$RelativeTimestamp, pupilData$CorrectedPupil, type='l')

##########
## TODO ##
##########
# add outlier rejection

# pupilTime = c(1000, 2000), bin=ceil(sampling.rate) to be used for means below
# output variables: raw average pupil, absolute difference form baseline, percent difference from baseline, pupil.segment absolute means and relative changes from baseline and definitely sems for each time point

# media_var shuld be unique names of all media available; if media was repeated then name should be modified to be unique for each repetition, e.g. by pasting index number at the end

# mediaIndex_var are the indices of media which we want to analyze pupil dilation for

# baseline.segmentTime is the start-stop time relative to media start from which to calculate baseline for each media
# pupil.segmentTime is the start-stop time relative to media start from which to calculate the pupil diameter data
# bin is important to standardize time to beginning of segment; to use max resolution, ceil(samplingRate) is recommended
# this time should be adjusted, by subtracting event start time form raw timestamp (check pG pupil analyses)
# interpolate will use na.approx with maxgap
# saveOutput will create a short version of the data with only averages
# mediaIndex_var and media name should be included in exported dataset
# at beginning of function, print the medias that will be analyzed and the time of baseline selected as warnings

# work on the same dataframe: use apply(min) to add average baseline at first sample of each trial.index
# then from zoo, na.locf() to fill variable and apply vector operations!