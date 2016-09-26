###########################
# adding segment information to data, based on event file, trial number and segment length
# Iyad Aldaqre
# 26.11.2015
# updated: 2.12.2015
###########################
# to add
# segment length in a matrix for variable segments for each scene?
###########################

# all scenes are to be treated the same, and scene timings will be treated as segments within each scene. e.g. wordLearning
time.segments<-function(dataTimeStamp,eventsList,segmentedEventIndex,segmentTimeVector,nSegments=(length(segmentTimeVector)-1)){
	segmentVector<-rep(1,length(dataTimeStamp))
	for(r in 1:nrow(eventsList)){ ###### MAKE SURE IT'S IN MS INSTEAD OF SECONDS
		if(is.element(r,segmentedEventIndex)){
			for(k in 1:nSegments){
				segmentVector[dataTimeStamp>=eventsList[r,grepl('start',names(eventsList),ignore.case=T)]+segmentTimeVector[k] & dataTimeStamp<= eventsList[r,grepl('start',names(eventsList),ignore.case=T)]+segmentTimeVector[k+1]]<-k
			}
		}
	}
	return(segmentVector)
}

########
# DEBUG
# eventsList<-inEvents
# segmentVector<-rep(1,length(inData$Timestamp))
# for(r in 1:nrow(eventsList)){ ###### MAKE SURE IT'S IN MS INSTEAD OF SECONDS
	# if(is.element(r,segmentedEventIndex)){
		# for(k in 1:length(segmentTimeVector)-1){
			# segmentVector[inData$Timestamp>=eventsList[r,grepl('start',names(eventsList),ignore.case=T)]+segmentTimeVector[k] & inData$Timestamp<= eventsList[r,grepl('start',names(eventsList),ignore.case=T)]+segmentTimeVector[k+1]]<-k
		# }
	# }
# }