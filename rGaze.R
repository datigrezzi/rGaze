# Basic functions for raw gaze data processing
# Iyad Aldaqre
# Updated 03.11.2017

# load needed libraries for this package
require(zoo,quietly = T)
require(sp,quietly=T)

### Calculate Euclidean Distance Between Two Points ###
eu.dist <- function(x1, y1, x2, y2){
	return(sqrt(((x2-x1)^2)+((y2-y1)^2)))
}

# A function to shift a vector by "shift" elements
shift<-function (x, by = 1){
	if(by > 0){
		shifted <- c(rep(NA, times=by), x[1:(length(x)-by)])
	} else if(by < 0){
		shifted <- c(x[(abs(by)+1):length(x)],rep(NA, times=abs(by)))
	}
	return(shifted)
}

#########################
### Grouping Saccades ###
#########################
# function to group consecutive saccades into a single saccade if they are within max.dist radius (in pixels), max.delay apart. Soon a verification of whether they are going with the same trajectory will be added.

groupSaccades <- function(saccades, max.dist = 50, max.delay = 30){	
	# shift saccade start position and column-bind it to saccades data, to have next saccade start point on same row
	saccades$NextSaccadeStartX <- shift(saccades$GazePointX_Start, by = -1)
	saccades$NextSaccadeStartY <- shift(saccades$GazePointY_Start, -1)
	saccades$dTime <- c(diff(saccades$Timestamp), 0) - saccades$Duration
	saccades$sac_dist <- eu.dist(saccades$GazePointX_End, saccades$GazePointY_End, saccades$NextSaccadeStartX, saccades$NextSaccadeStartY)
	
	# dummy variable for saccades instead of looping [check]
	grouped_indices <- saccades$sac_dist <= max.dist & saccades$dTime <= max.delay
	grouped_indices[is.na(grouped_indices)] <- 0

	# managing first saccade
	if(grouped_indices[1] == 1){
		grouped_indices_diff <- c(1, diff(grouped_indices))
	} else{
		grouped_indices_diff <- c(0, diff(grouped_indices))
	}
	
	# indexing remaining separate saccades
	grouped_saccades <- rep(NA, nrow(saccades))
	grouped_saccades[grouped_indices_diff == 1 | (grouped_indices_diff != -1 & grouped_indices == 0)] <- 1
	# giving them a new saccade number as we will have less saccades after grouping
	grouped_saccades_count <- seq(1:length(grouped_saccades[!is.na(grouped_saccades)]))
	grouped_saccades[!is.na(grouped_saccades)] <- grouped_saccades_count
	# add new saccades numbers to saccades dataset
	saccades$grouped<-na.locf(grouped_saccades,fromLast=F,na.rm=F)
	
	# do the actual grouping, creating new dataset	
	grouped_saccades <- cbind(aggregate(saccades$Timestamp,by=list(saccades$grouped),FUN=min), ((aggregate(saccades$Timestamp,by=list(saccades$grouped),FUN=max)[2] + saccades$Duration[is.element(saccades$Timestamp,tapply(saccades$Timestamp, INDEX = saccades$grouped, FUN=max))])-aggregate(saccades$Timestamp,by=list(saccades$grouped),FUN=min)[2]), saccades$GazePointX_Start[is.element(saccades$Timestamp, tapply(saccades$Timestamp, INDEX = saccades$grouped, FUN = min))], saccades$GazePointY_Start[is.element(saccades$Timestamp, tapply(saccades$Timestamp, INDEX = saccades$grouped, FUN = min))],saccades$GazePointX_End[is.element(saccades$Timestamp, tapply(saccades$Timestamp, INDEX = saccades$grouped, FUN = max))],saccades$GazePointY_End[is.element(saccades$Timestamp, tapply(saccades$Timestamp, INDEX = saccades$grouped, FUN = max))])
	
	# rename variables
	names(grouped_saccades)<-c('Saccade','Timestamp','Duration','GazePointX_Start','GazePointY_Start','GazePointX_End','GazePointY_End')

	# return new dataset
	return(grouped_saccades)
}

##########################
### Grouping Fixations ###
##########################
# function to group consecutive fixations into a single fixation if they are within max.dist radius (in pixels) and max.delay apart

groupFixations <- function(fixations, max.dist = 50, max.delay = 75){
	# shift saccade start position and column-bind it to fixations data, to have next saccade start point on same row
	fixations$NextFixationX <- shift(fixations$GazePointX, by = -1)
	fixations$NextFixationY <- shift(fixations$GazePointY, -1)
	fixations$dTime <- c(diff(fixations$Timestamp), 0) - fixations$Duration
	fixations$fix_dist <- eu.dist(fixations$GazePointX, fixations$GazePointY, fixations$NextFixationX, fixations$NextFixationY)
	
	# dummy variable for fixations instead of looping [check]
	grouped_indices <- fixations$fix_dist <= max.dist & fixations$dTime <= max.delay
	grouped_indices[is.na(grouped_indices)] <- 0

	# managing first saccade
	if(grouped_indices[1] == 1){
		grouped_indices_diff <- c(1, diff(grouped_indices))
	} else{
		grouped_indices_diff <- c(0, diff(grouped_indices))
	}
	
	# indexing remaining separate fixations
	grouped_fixations <- rep(NA, nrow(fixations))
	grouped_fixations[grouped_indices_diff == 1 | (grouped_indices_diff != -1 & grouped_indices == 0)] <- 1
	# giving them a new saccade number as we will have less fixations after grouping
	grouped_fixations_count <- seq(1:length(grouped_fixations[!is.na(grouped_fixations)]))
	grouped_fixations[!is.na(grouped_fixations)] <- grouped_fixations_count
	# add new fixations numbers to fixations dataset
	fixations$grouped<-na.locf(grouped_fixations,fromLast=F,na.rm=F)
	
	# do the actual grouping, creating new dataset	
	grouped_fixations <- cbind(aggregate(fixations$Timestamp,by=list(fixations$grouped),FUN=min), aggregate(fixations$GazePointX,by=list(fixations$grouped),FUN=mean)[2], aggregate(fixations$GazePointY,by=list(fixations$grouped),FUN=mean)[2], ((aggregate(fixations$Timestamp,by=list(fixations$grouped),FUN=max)[2] + fixations$Duration[is.element(fixations$Timestamp,tapply(fixations$Timestamp, INDEX = fixations$grouped, FUN=max))])-aggregate(fixations$Timestamp,by=list(fixations$grouped),FUN=min)[2]))
	
	# rename variables
	names(grouped_fixations)<-c('Fixation','Timestamp','GazePointX','GazePointY','Duration')

	# return new dataset
	return(grouped_fixations)
}

###########
### IVT ###
###########
# Function to run a velocity based threshold to filter raw gaze data into fixations and saccades
# This function expects data of a single participant. Threshold is in pixels per timestamp unit (probably millisecond, depending on data collection software)
# Threshold in degrees can be estimated before hand for each participant and then converted to pixels and passed to this function (see deg2pix)

ivt<-function(data, variables = c('Timestamp', 'GazePointX', 'GazePointY'), threshold = 1.5, output = 'saccades', samplingRate = 'auto', variablesToKeep = NULL, groupConsecutive = FALSE, max.dist = 50, max.delay = 50, minFixDuration = NA, verbose = FALSE){

	velocity <- sqrt((diff(data[,variables[2]])^2) + (diff(data[,variables[3]])^2)) / diff(data[,variables[1]]) # pixel/ms
	
	if(samplingRate == 'auto'){
		samplingRate = round(1000/mean(diff(data[,variables[1]]), na.rm = T))
		if(verbose){
		  print(paste("Sampling rate detected:", samplingRate))
		}

	}
	
	if(output == 'velocities'){
		feedback.text='Velocities calculated!'
		return(velocity)
	}

	# saccades
	else if(output == 'saccades'){
		# following condition only takes consecutive samples; can change here to tolerate missing samples by increasing the last part into multiples of sample duration
		saccade_indices <- velocity >= threshold & diff(data[,variables[1]]) <= ceiling(1000/samplingRate)
		saccade_indices[is.na(saccade_indices)] <- 0
		if(velocity[1] >= threshold & !is.na(velocity[1])){ # to categorize first sample
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
		
		# creating saccades dataset
		data<-data[data$Saccades!=0,]
		row.names(data)<-1:nrow(data)
		data$rows<-as.numeric(as.character(row.names(data)))
		function_rows_min<-t(aggregate(data$rows,by=list(data$Saccades),FUN=min)[2])
		function_rows_max<-t(aggregate(data$rows,by=list(data$Saccades),FUN=max)[2])

		saccade_data<-cbind(aggregate(data[,variables[1]],by=list(data$Saccades),FUN=min), (aggregate(data[,variables[1]],by=list(data$Saccades),FUN=max)[2]-aggregate(data[,variables[1]],by=list(data$Saccades),FUN=min)[2])+ceiling(1000/samplingRate), data[function_rows_min,variables[2]],data[function_rows_min,variables[3]],data[function_rows_max,variables[2]],data[function_rows_max,variables[3]])
		
		names(saccade_data)<-c('Saccade','Timestamp','Duration','GazePointX_Start','GazePointY_Start','GazePointX_End','GazePointY_End')
		
		if(groupConsecutive){
			saccade_data <- groupSaccades(saccade_data, max.dist = max.dist, max.delay = max.delay)
		}
		
		if(!is.null(variablesToKeep)){
			other_variables<-data[is.element(data[,variables[1]], saccade_data$Timestamp),variablesToKeep]
			saccade_data<-cbind(saccade_data,other_variables)
		}
		if(verbose){
		  print('Saccades identified!')
		}
		return(saccade_data)
	}
	else if(output == 'fixations'){
		# fixations
		fixation_indices <- velocity <= threshold&diff(data[,variables[1]]) <= ceiling(1000/samplingRate)
		fixation_indices[is.na(fixation_indices)] <- 0
		if(velocity[1] <= threshold & !is.na(velocity[1])){
			fixation_indices_diff <- c(1,diff(fixation_indices)) # to insure continuous data
		} else {
			fixation_indices_diff <- c(-1,diff(fixation_indices))
		}
		fixation_count <- seq(1:length(fixation_indices_diff[fixation_indices_diff==1]))
		fixations <- rep(NA,length(velocity))
		fixations[fixation_indices_diff==1] <- fixation_count
		fixations[fixation_indices_diff==-1] <- 0
		fixations <- na.locf(fixations,fromLast=F,na.rm=F)
		data$Fixations <- c(0, fixations)
		
		data<-data[data$Fixations!=0,]
		row.names(data)<-1:nrow(data)

		fixation_data<-cbind(aggregate(data[,variables[2]],by=list(data$Fixations),FUN=mean),aggregate(data[,variables[3]],by=list(data$Fixations),FUN=mean)[,2],aggregate(data[,variables[1]],by=list(data$Fixations),FUN=min)[,2],(aggregate(data[,variables[1]],by=list(data$Fixations),FUN=max)[2]-aggregate(data[,variables[1]],by=list(data$Fixations),FUN=min)[,2])+ceiling(1000/samplingRate))
	
		names(fixation_data)<-c('Fixation','GazePointX','GazePointY','Timestamp','Duration')
	
		if(groupConsecutive){
			fixation_data <- groupFixations(fixation_data, max.dist = max.dist, max.delay = max.delay)
		}
	
		if(!is.null(variablesToKeep)){
			other_variables<-data[is.element(data[,variables[1]], fixation_data$Timestamp),variablesToKeep]
			fixation_data<-cbind(fixation_data,other_variables)
		}
		if(verbose){
		  print('Fixations identified!')
		}
		if(!is.na(minFixDuration)){
		  fixation_data <- fixation_data[fixation_data$Duration >= minFixDuration,]
		  fixation_data$Fixation <- seq(1, nrow(fixation_data))
		  row.names(fixation_data) <- seq(1, nrow(fixation_data))
		}
		return(fixation_data)
	}
	else{
		warning("Please choose valid output (fixations, saccades or velocities)")
	}

}

############
### AOIS ###
############
# Function to create an AOI variable to a data set, given the x and y coordinates.
# This function expects a single-subject dataframe with aoinames as column name, a column for each aoi and x,x,y,y OR x,y,radius
aois<-function(data, aoi_coordinates, variables = c('GazePointX', 'GazePointY'), aoiType = 'rect'){ 
	AOI<-rep('out',nrow(data))
	if(aoiType=='rect'){
		aoiMatrixX<-aoi_coordinates[c(1,2,2,1),]
		aoiMatrixY<-aoi_coordinates[c(3,3,4,4),]
		for(v in 1:ncol(aoi_coordinates)){AOI[point.in.polygon(data[,variables[1]], data[,variables[2]],aoiMatrixX[,v],aoiMatrixY[,v])>0]<-names(aoi_coordinates)[v]}
	} else if(aoiType=='circle'){
		for(v in 1:ncol(aoi_coordinates)){AOI[sqrt((data[,variables[1]]-aoi_coordinates[1,v])^2+(data[,variables[2]]-aoi_coordinates[2,v])^2)<= aoi_coordinates[3,v]]<-names(aoi_coordinates)[v]}
	}
	# data<-cbind(data,AOI)
	return(AOI)
}

#############################
### Proportion of looking ###
#############################
# function to calculate proportion of looking at each aoi and its dynamic changes over a period of time.
# data is binned and proportions are calculated for each bin for a single participant. Should be ready to plot.
# afterwards, data from all participants can be concatenated to plot means for bins with error bars (see error.line)

######################
### Pupil Dilation ###
######################
# Function to process pupil data, subtracting baseline as an option and preparing for plot by binning data (even at sampling rate).
# afterwards, data from all participants can be concatenated to plot means for bins with error bars (see error.line)

###############
### Heatmap ###
###############

# Function to create a heatmap from x and y coordinates.
# On macOS the heatmap is exported with a transparent background and therefore can be overlayed on the stimulus picture using an image processing software

# debug
# n<-200
# color.levels<-20
# color.palette<-rf
# flip.y.axis<-T
# export.plot<-F
# size<-c(10.02,5.36)
# export.dir<-getwd()
# export.file.name<-'heatmap'
# data<-milanoData
# variables<-c('GazePointRightX..ADCSpx.','GazePointRightY..ADCSpx.')


heatmap <- function(data, variables = c('GazePointX', 'GazePointY'), n = 200, color.palette=rainbow, color.levels=20, flip.y.axis = T, export.plot = F, size=c(10.02,5.36), export.dir = getwd(), export.file.name='heatmap'){
  require('MASS')
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

heatmap2 <- function(data, variables = c('GazePointX', 'GazePointY'), n = 200, color.palette=rainbow, color.levels=20, flip.y.axis = T, export.plot = F, size=c(10.02,5.36), export.dir = getwd(), export.file.name='heatmap', ylims = c(1024, 0), xlims = c(0, 1280)){
  require('MASS')
  xCoordinates<-na.approx(data[,variables[1]], na.rm=T)
  yCoordinates<-na.approx(data[,variables[2]], na.rm=T)
  heatmapData<-kde2d(xCoordinates,yCoordinates,n=n)
  par(mai=c(0,0,0,0))
  filled.contour(heatmapData, axes=F, color.palette=color.palette, nlevels=color.levels, frame.plot = F, key.axes = F, asp = 0.5, ylim = ylims, xlim = xlims)
}

# TODO: add clustering; check https://www.rdocumentation.org/packages/spatialEco/versions/1.1-1/topics/nni