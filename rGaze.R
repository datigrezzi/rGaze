# Basic functions for raw gaze data processing
# Iyad Aldaqre
# Updated 07.03.2017

###########
### IVT ###
###########
# Function to run a velocity based threshold to filter raw gaze data into fixations and saccades; saving the output is optional; This function expects data of a single participant
ivt<-function(data, variables = c('Timestamp', 'GazePointX', 'GazePointY'), threshold = 1.5, export.processed = F, export.dir = getwd(), sampling.rate = 60){
	require(zoo,quietly=T)
	velocity<-sqrt((diff(data[,variables[2]])^2)+(diff(data[,variables[3]])^2))/diff(data[,variables[1]]) # pixel/ms
	# saccades
	saccade_indices<-velocity>=threshold&diff(data[,variables[1]])<=ceiling(1000/sampling.rate)
	saccade_indices[is.na(saccade_indices)]<-0
	if(velocity[1]>=threshold & !is.na(velocity[1])){ # to categorize first sample
		saccade_indices_diff<-c(1,diff(saccade_indices)) # to insure continuous data
	} else {
		saccade_indices_diff<-c(-1,diff(saccade_indices))
	}
	saccade_count<-seq(1:length(saccade_indices_diff[saccade_indices_diff==1]))
	saccades<-rep(NA,length(velocity))
	saccades[saccade_indices_diff==1]<-saccade_count
	saccades[saccade_indices_diff==-1]<-0
	saccades<-na.locf(saccades,fromLast=F,na.rm=F)	
	data$Saccades<-c(0,saccades)
	
	# fixations
	fixation_indices<-velocity<=threshold&diff(data[,variables[1]])<=ceiling(1000/sampling.rate)
	fixation_indices[is.na(fixation_indices)]<-0
	if(velocity[1]<=threshold & !is.na(velocity[1])){
		fixation_indices_diff<-c(1,diff(fixation_indices)) # to insure continuous data
	} else {
		fixation_indices_diff<-c(-1,diff(fixation_indices))
	}
	fixation_count<-seq(1:length(fixation_indices_diff[fixation_indices_diff==1]))
	fixations<-rep(NA,length(velocity))
	fixations[fixation_indices_diff==1]<-fixation_count
	fixations[fixation_indices_diff==-1]<-0
	fixations<-na.locf(fixations,fromLast=F,na.rm=F)
	data$Fixations<-c(0, fixations)

	# adding sample duration
	data$SamplingRate<-sampling.rate

	if(export.processed){
		write.table(data,paste(export.dir,'/processed_data.csv',sep=''),sep=',',dec='.',row.names=F)
		feedback.text='Gaze Processed and Exported!'
	} else{
		feedback.text='Gaze Processed!'
		return(data)
		}
}

#################
### FIXATIONS ###
#################
# Function to group fixations processed by the ivt function above; saving the output is optional
fixations<-function(data = processed_data, export.processed = F, export.dir = getwd(), variables = c('Timestamp', 'GazePointX', 'GazePointY', 'Fixations'), variablesToKeep = NULL){
	data<-data[data[,variables[4]]!=0,]
	row.names(data)<-1:nrow(data)

	fixation_data<-cbind(aggregate(data[,variables[2]],by=list(data[,variables[4]]),FUN=mean),aggregate(data[,variables[3]],by=list(data[,variables[4]]),FUN=mean)[,2],aggregate(data[,variables[1]],by=list(data[,variables[4]]),FUN=min)[,2],(aggregate(data[,variables[1]],by=list(data[,variables[4]]),FUN=max)[2]-aggregate(data[,variables[1]],by=list(data[,variables[4]]),FUN=min)[,2])+ceiling(1000/data$SamplingRate[1]))
	names(fixation_data)<-c('Fixation','GazePointX','GazePointY','Timestamp','Duration')
	
	if(!is.null(variablesToKeep)){
		data$rows<-as.numeric(as.character(row.names(data)))
		fixation_rows<-t(aggregate(data$rows,by=list(data[,variables[4]]),FUN=min)[2])
		other_variables<-data[fixation_rows,variablesToKeep]
		fixation_data<-cbind(fixation_data,other_variables)
	}
	
	if(export.processed){
		write.table(data,paste(export.dir,'/fixation_data.csv',sep=''),sep=',',dec='.',row.names=F)
		return(fixation_data)
		feedback.text='Gaze Processed and Exported!'
	} else{
		feedback.text='Gaze Processed!'
		return(fixation_data)
	}
}

################
### SACCADES ###
################
# Function to group saccades processed by the ivt function above; saving the output is optional
saccades<-function(data = processed_data, export.processed = F, export.dir = getwd(), variables = c('Timestamp', 'GazePointX', 'GazePointY', 'Saccades'), variablesToKeep = NULL){
	data<-data[data[,variables[4]]!=0,]
	row.names(data)<-1:nrow(data)
	data$rows<-as.numeric(as.character(row.names(data)))
	function_rows_min<-t(aggregate(data$rows,by=list(data[,variables[4]]),FUN=min)[2])
	function_rows_max<-t(aggregate(data$rows,by=list(data[,variables[4]]),FUN=max)[2])

	saccade_data<-cbind(aggregate(data[,variables[1]],by=list(data[,variables[4]]),FUN=min),(aggregate(data[,variables[1]],by=list(data[,variables[4]]),FUN=max)[2]-aggregate(data[,variables[1]],by=list(data[,variables[4]]),FUN=min)[2])+ceiling(1000/data$SamplingRate[1]),data[function_rows_min,variables[2]],data[function_rows_min,variables[3]],data[function_rows_max+1,variables[2]],data[function_rows_max+1,variables[3]])
	names(saccade_data)<-c('Saccade','Timestamp','Duration','GazePointX_Start','GazePointY_Start','GazePointX_End','GazePointY_End')
	
	if(!is.null(variablesToKeep)){
		other_variables<-data[function_rows_min,variablesToKeep]
		saccade_data<-cbind(saccade_data,other_variables)
	}
	
	if(export.processed){
		write.table(data,paste(export.dir,'/saccade_data.csv',sep=''),sep=',',dec='.',row.names=F)
		return(saccade_data)
		feedback.text='Gaze Processed and Exported!'
	} else{
		feedback.text='Gaze Processed!'
		return(saccade_data)
	}
}

############
### AOIS ###
############
# Function to create an AOI variable to a data set, given the x and y coordinates.
# This function expects a single-subject dataframe with aoinames as column name, a column for each aoi and x,x,y,y OR x,y,radius
aois<-function(data, aoi_coordinates, variables = c('GazePointX', 'GazePointY'), aoiType = 'polygon'){ 
	require(sp,quietly=T)
	AOI<-rep('out',nrow(data))
	if(aoiType=='polygon'){
		aoiMatrixX<-aoi_coordinates[c(1,2,2,1),]
		aoiMatrixY<-aoi_coordinates[c(3,3,4,4),]
		for(v in 1:ncol(aoi_coordinates)){AOI[point.in.polygon(data[,variables[1]], data[,variables[2]],aoiMatrixX[,v],aoiMatrixY[,v])>0]<-names(aoi_coordinates)[v]}
	} else if(aoiType=='circular'){
		for(v in 1:ncol(aoi_coordinates)){AOI[sqrt((data[,variables[1]]-aoi_coordinates[1,v])^2+(data[,variables[2]]-aoi_coordinates[2,v])^2)<= aoi_coordinates[3,v]]<-names(aoi_coordinates)[v]}
	}
	data<-cbind(data,AOI)
	return(data)
}

# Function to create a heatmap from x and y coordinates.
# On macOS the heatmap is exported with a transparent background and therefore can be overlayed on the stimulus picture using an image processing software
### debug
n<-200
color.levels<-20
color.palette<-rf
flip.y.axis<-T
export.plot<-F
size<-c(10.02,5.36)
export.dir<-getwd()
export.file.name<-'heatmap'
data<-milanoData
variables<-c('GazePointRightX..ADCSpx.','GazePointRightY..ADCSpx.')
###

heatmap <- function(data, variables = c('GazePointX', 'GazePointY'), n = 200, color.palette=rainbow, color.levels=20, flip.y.axis = T, export.plot = F, size=c(10.02,5.36), export.dir = getwd(), export.file.name='heatmap'){
	require(zoo)
	xCoordinates<-na.locf(data[,variables[1]], na.rm=T)
	yCoordinates<-na.locf(data[,variables[2]], na.rm=T)
	if(flip.y.axis){yCoordinates<-(yCoordinates*-1)+max(yCoordinates)}
	# cat('Please wait while data is processed; this can take some time!')
	heatmapData<-kde2d(xCoordinates,yCoordinates,n=n)
	plot(xCoordinates,yCoordinates)
	par(mai=c(0,0,0,0))
	if(export.plot){pdf(paste(export.dir,'/', export.file.name,'.pdf',sep=''),width=size[1],height=size[2])}
	filled.contour(heatmapData, axes=F, color.palette=color.palette, nlevels=color.levels, frame.plot = F, key.axes = F, asp = 0.5)
	if(export.plot){dev.off()}
}