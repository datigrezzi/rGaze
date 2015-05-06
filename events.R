###########################################
# SCRIPT START
###########################################
# set working directory
setwd(workdir1)

# import table
#j<-1
for (j in 1:length(subjects)){
file <- subjects[j]

mainmat_events <- read.table(file, header = T, sep = "\t", dec = ",",fill = FALSE, na.strings="NA", blank.lines.skip = FALSE)
mainmat_events<-mainmat_events[,c('RecordingTimestamp','GazePointIndex','DistanceLeft','ValidityLeft','DistanceRight','ValidityRight','GazePointX..ADCSpx.','GazePointY..ADCSpx.','StudioEvent','StudioEventData','MediaName')]

names(mainmat_events)<-c('Timestamp','Number','DistanceLeft','ValidityLeft','DistanceRight','ValidityRight','GazePointX','GazePointY','Event','Descriptor','StimuliName')


#create events matrix (for movie start, end and name)
event<-as.character(mainmat_events$Descriptor[mainmat_events$Event=="MovieStart"|mainmat_events$Event=="ImageStart"])
start<-mainmat_events$Timestamp[mainmat_events$Event=="MovieStart"|mainmat_events$Event=="ImageStart"]
end<-mainmat_events$Timestamp[mainmat_events$Event=="MovieEnd"|mainmat_events$Event=="ImageEnd"]
events<-data.frame(cbind(event,start,end))
events[,1]<-as.character(events[,1])
events[,2:3]<-apply(events[,2:3],2,as.numeric)


###########################################
# DECREASING MAIN MATRIX SIZE
###########################################
# starting an index for deleting irrelevent media
events[,4]<-0
events[events$event=="No media",4]<-1
for (d in 1:numintermedia){
events[events$event==intermedia[d],4]<-1
}
events<-subset(events,subset=V4!=1)
events<-events[,1:3]

# adding stimulus order number (which video comes before which!)
for (e in 1:nrow(events)){	
	mainmat_events[mainmat_events$Timestamp>=events[e,2] & mainmat_events$Timestamp<=events[e,3],"Number"]<-as.character(e)
	}

events[,4]<-subject_names[j]

if (j==1){
eventsmat<-events
	} else{
		eventsmat<-rbind(eventsmat,events)
		}

print(paste(subject_names[j],'-',j))

}

setwd(workdir2)

names(eventsmat)[4]<-"subject"

eventsmat$seg<-0 # remember to check trials that need segmentation

eventsfile<-(paste(experiment,"-event-info.txt", sep=""))
write.table(eventsmat, eventsfile, quote=FALSE, sep="\t", na="NA",row.names=FALSE,col.names=TRUE)
