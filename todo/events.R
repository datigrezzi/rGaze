###########################################
# getting event details function
# Iyad Aldaqre
# 19.11.2015
# updated 11.7.2016
# Updated 8.10.2016
###########################################
# to add:
# percentage progress bar
# based on number of items in fileList and i
###########################################
events<-function(fileList,dataDir,exportDir,variables=c('Timestamp','Event','Descriptor'),segmented=T,segment.keyword="",skip.keyword="",skip.ignore.case=T,video.ends.by.press=F,saveFile=F,dec = ".",...){
	curDir<-getwd()
	if(is.element('allEvents',ls())){rm(allEvents)}
	# setwd(dataDir)
	# i<-2 ## for debugging
	for (i in 1:length(fileList)){ ## quote here for debugging
		print(paste(i,'check',sep='_'))
		# reading individual combined data file
		tempFile<-read.table(paste(dataDir,fileList[i],sep=.Platform$file.sep), header = T, sep = "\t",fill = T, na.strings="NA",blank.lines.skip = FALSE,...)[,variables]
		# getting event info
		eventNames<-as.character(tempFile[(grepl('start',tempFile[,2],ignore.case=T)|grepl('show',tempFile[,2],ignore.case=T)),3])
		eventStart<-tempFile[(grepl('start',tempFile[,2],ignore.case=T)|grepl('show',tempFile[,2],ignore.case=T)),1]
		# in case video was looped until a key was pressed
		# change to restricting keypresses to those occuring after events start. loop event start and take minimum greater than each event start
		if(video.ends.by.press){
			# eventEnd<-tempFile[(grepl('key',tempFile[,2],ignore.case=T)|grepl('press',tempFile[,2],ignore.case=T)),1]
			eventEnd<-rep(0,length(eventStart))
			for(evS in 1:length(eventStart)){
				eventEnd[evS]<-min(tempFile$Timestamp[tempFile$Timestamp>eventStart[evS]&(grepl('key',tempFile$Event,ignore.case=T)|grepl('press',tempFile$Event,ignore.case=T))],na.rm=T)
			}
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
		write.table(allEvents, paste(exportDir,'eventsfile.txt',sep=.Platform$file.sep), quote=FALSE, sep="\t",na="NA",row.names=FALSE,col.names=TRUE)
	}

	setwd(curDir) #change to full path without setwd
	return(allEvents)#eventsMat instead

}

### debugging section
# variables=c('Timestamp','Event','Description')
# dataDir<-workdir1
# exportDir<-workdir2
# fileList<-evList
# i<-2
# tempFile<-read.table(fileList[i], header = T, sep = "\t", dec = ",",fill = T, na.strings="NA",blank.lines.skip = FALSE, skip=18)[,variables]
# video.ends.by.press=T
# skip.keyword='red_screen'
# skip.ignore.case=T
