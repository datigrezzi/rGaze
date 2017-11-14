# browsing and reading hdf5 files
# using package h5r
# download and usage from: http://stackoverflow.com/questions/15974643/how-to-deal-with-hdf5-files-in-r
# 22.5.2014
# updated: 14.05.2015

# load library
read.hdf5<-function(file, structurePreview=T){
	isRhdf5Installed<-require(rhdf5)
	isZooInstalled<-require(zoo)
	if(isRhdf5Installed==F){
		print('install package by executing:')
		print('source(\'http://bioconductor.org/biocLite.R\')')
		print('biocLite(\'rhdf5\')')
	}
	# read file
	gazeData<-h5read(file,'/data_collection/events/eyetracker/BinocularEyeSampleEvent')[,c('time','left_gaze_x','left_gaze_y','left_eye_cam_z','left_pupil_measure1','right_gaze_x','right_gaze_y','right_eye_cam_z','right_pupil_measure1','status')]
	eventData<-h5read(file,'/data_collection/events/experiment/MessageEvent')[,c('time','text')]
	# combine files using merge()
	allData<-merge(gazeData, eventData, by='time', all=T)
	# add event info using na.locf() then subset (check ost4?)
#	allData$text[1]<-'NA'
	allData$text<-as.factor(allData$text)
	allData$text<-na.locf(allData$text,fromLast=F,na.rm=F)
#	allData<-subset(allData,subset=is.na(allData$left_gaze_x)==F)
	# add particiapnt name
	allData$participant<-file
	# preview file
	if(structurePreview==T){print(h5ls(file))}
	# return file
	return(allData)
	# close hdf5 file
	H5close()
}
