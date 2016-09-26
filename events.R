###########################################
# getting event details function
# Iyad Aldaqre
# 19.11.2015
# updated 11.7.2016
# Updated 26.9.2016
###########################################
# to add:
# percentage progress bar
# based on number of items in fileList and i
###########################################
events<-function(fileList,dataDir,exportDir,variables=c('Timestamp','Event','Descriptor'),segmented=T,segment.keyword="",skip.keyword="",skip.ignore.case=T,video.ends.by.press=F,saveFile=T,...){
	curDir<-getwd()
	setwd(dataDir)
	# i<-2 ## for debugging
	for (i in 1:length(fileList)){ ## quote here for debugging
# print(paste(i,'check',sep='_'))
		# reading individual combined data file
		tempFile<-read.table(fileList[i], header = T, sep = "\t", dec = ",",fill = T, na.strings="NA",blank.lines.skip = FALSE, ...)[,variables]
		# getting event info
		eventNames<-as.character(tempFile[(grepl('start',tempFile[,2],ignore.case=T)|grepl('show',tempFile[,2],ignore.case=T)),3])
		eventStart<-tempFile[(grepl('start',tempFile[,2],ignore.case=T)|grepl('show',tempFile[,2],ignore.case=T)),1]
		# in case video was looped until a key was pressed
		if(video.ends.by.press){
			eventEnd<-tempFile[(grepl('key',tempFile[,2],ignore.case=T)|grepl('press',tempFile[,2],ignore.case=T)),1]
		}else{
			eventEnd<-tempFile[(grepl('end',tempFile[,2],ignore.case=T)),1]
			}
		# combining variables, replacing tempFile, setting mode
		tempFile<-data.frame(cbind(eventNames,eventStart,eventEnd))
		tempFile[,2:3]<-apply(tempFile[,2:3],2,as.numeric)
		
		ifelse(skip.keyword!="",tempFile<-tempFile[!grepl(skip.keyword,tempFile$eventNames,ignore.case=skip.ignore.case),],tempFile<-tempFile)
		
		tempFile$eventNumber<-1:nrow(tempFile)
		tempFile$subject<-fileList[i]
		# combining subjects' event files
		if(i==1){allEvents<-tempFile}else{allEvents<-rbind(allEvents,tempFile)}
	} ## quote here for debugging
	# add event duration
	allEvents$eventDuration<-allEvents$eventEnd-allEvents$eventStart
	# add segmentation marker: which events are segmented?
	if(segmented){
		if(segment.keyword==""){
			allEvents$seg<-1
			}else{
			allEvents$seg<-0	
			allEvents$seg[grepl(segment.keyword,allEvents$eventNames,ignore.case=T)]<-1
			}
		} else {
		allEvents$seg<-0
		}	
	# shall a copy be saved in export folder?
	if(saveFile){
		setwd(exportDir)
		write.table(allEvents, 'eventsfile.txt', quote=FALSE, sep="\t",na="NA",row.names=FALSE,col.names=TRUE)
	}
	setwd(curDir)
	return(allEvents)#eventsMat instead
}

### debugging section
# variables=c('Timestamp','Event','Description')
# setwd(workdir1)
# fileList<-evList
# i<-2
# tempFile<-read.table(fileList[i], header = T, sep = "\t", dec = ",",fill = T, na.strings="NA",blank.lines.skip = FALSE, skip=18)[,variables]
# video.ends.by.press=T
# skip.keyword='red_screen'
# skip.ignore.case=T