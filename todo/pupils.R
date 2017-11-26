# a script to calculate average pupil baseline during certain time segments
# after calculating average baseline and subtracting it from each sample
# sketch written on 11.2.2015
# iyad aldaqre
require(zoo)
rm(list=ls())

pupil <- function(data,variables=c('Timestamp','Stimulus','Pupil'),baselineTime=c(-1500, -500), pupilTime = c(1000, 2000), bin=ceil(sampling.rate), samplingRate=60, interpolate=T, maxgap=5){
	# making sure data is consecutive (verifying timestamps)
	if(max(range(diff(data[,variables[1]])<=ceiling(1000/samplingRate))) > 1){
		warning("Data might not be continuous!")
	}
	# interpolate if explicitly asked to
	if(interpolate==T){
		data[,variables[3]]<-na.locf(data[,variables[3]], maxgap=maxgap, na.rm=F)
	}
	# get unique stimulus names
	stimuli <- unique(as.character(data[data[,variables[2]] != "" & !is.na(data[,variables[2]]), variables[2]]))
	for(i in stimuli){
		# based on stimulus name, get start and end time
		startTime <- min(data[data[,variables[2]] == i,variables[1]])
		endTime <- max(data[data[,variables[2]] == i,variables[1]])
		# calculate baseline
		data$Baseline[data[,variables[2]]==i] <- mean(data[data[,variables[1]] >= (startTime + baselineTime[1]) & data[,variables[1]] <= (startTime + baselineTime[2]),variables[3]], na.rm=T)
		# correct pupil
		data$CorrectedPupil[data[,variables[2]]==i] <- data[data[,variables[2]]==i,variables[3]]-data$Baseline[data[,variables[2]]==i]
		# calculate relative timeline
		data$RelativeTimestamp[data[,variables[2]]==i] <- data[data[,variables[2]]==i,variables[1]]-startTime
	}
	return(data[!is.na(data$RelativeTimestamp),])
}

setwd('~/SRLABS_Drive/projects/pupilMarketing')
# setwd('C:/Users/Aldaqre/Google Drive/projects/pupilMarketing')
rawData <- read.table("IPSOS_Kinder-Pack_Data_Export.tsv",sep='\t',header=T,dec=",")
inData <- rawData[rawData$ParticipantName=="A001",]

pupilData <- pupil(inData, variables <- c('RecordingTimestamp','MediaName','PupilRight'))
plot(pupilData$RelativeTimestamp, pupilData$CorrectedPupil, type='l')

# add outlier rejection
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