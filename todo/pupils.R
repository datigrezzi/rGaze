# a script to calculate average pupil baseline during certain time segments
# after calculating average baseline and subtracting it from each sample
# sketch written on 11.2.2015
# iyad aldaqre

pupils<-function(data,variables=('recordingTimestamp','mediaName','pupilDiameter'),mediaList, mediaIndex, baseline.segmentTime, pupil.segmentTime, interpolate=T,maxgap=5,bin=ceil(sampling.rate), sampling.rate=60,saveOutput=F,outputDir){
	require(zoo)
	require(reshape2)
	# get studio events data and choose mediaIndex of those (with start and end time)
	pupilMedia<-mediaList[is.element(mediaList$Index,mediaIndex),variables[2]]
	warning('Analyzing media: ', paste(pupilMedia, collapse=' '))
	# interpolate if explicitly asked to
	if(interpolate==T){
		data[,variables[3]]<-na.locf(data[,variables[3]],maxgap=maxgap, na.rm=F)
	}
	# making sure data is consecutive (verifying timestamps)
	data$consecutive<-diff(data[,variables[1]])<=ceiling(1000/sampling.rate)
	# based on trial.index, calculate adjTime starting at trial beginning
	
	
}


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