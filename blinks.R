# based on Mathot's algorithm description in A Simple Way To Reconstruct Pupil Size During Eye Blinks (2013)
# Modified to fit noisy data with many missing samples recorded at lower frequency
# Iyad Aldaqre
# 2018
# IDEA: split in two functions, one to return indices of blinks, other to interpolate; this allows to do a second check whether a blink is present for both eyes (more conservative)

remove.blinks <- function(x, ts, threshold = 0.04, smoothing_window_length=2, sampling_rate = 50, blink_duration_range = c(100, 400), smooth = T){
	require(zoo, quietly=T)
	# cubic spline interpolation
	spline.interpolation <- function(x, ts, t1, t2, t3, t4, plotSignal=F){
		# select data
		tmp <- cbind(x[ts >= t1 & ts <= t4], ts[ts >= t1 & ts <= t4])
		# na blink data
		tmp[tmp[,2] >= t2 & tmp[,2] <= t3, 1] <- NA
		# Select portion of UN PROCESSED DATA AND CALCULATE CUBIC SPLINE from t1 to t4
		interpolatedData <- spline(tmp[,1], n=length(tmp[,1]))$y
		
		if(plotSignal){
			plot(tmp[,2], tmp[,1])
			points(tmp[,2],interpolatedData, col='red', pch=4)
		}
		
		# return interpolation
		return(interpolatedData[tmp[,2] >= t2 & tmp[,2] <= t3])
	}
	
	if(smooth){
		# smooth the data by a moving average window
		x <- c(rollapply(x, width = smoothing_window_length, mean),rep(x[length(x)], smoothing_window_length - 1)) # make sure to keep original length		
	}
	
	# calculate velocities
	vel <- c(diff(x)/diff(ts),NA)
	
	# categorize different data patterns
	idx <- matrix(c(seq(1,length(x)),rep(0,length(x))), nrow = length(x), ncol = 2)
	idx[vel < threshold * -1, 2] <- 1 # neg_spike
	idx[vel > threshold, 2] <- 2 # pos_spike
	# idx[abs(vel) <= 0.001, 2] <- 3 # almost zero velocity
	idx[is.na(vel), 2] <- 4 # NAs
	# add timestamps for reference
	idx <- cbind(idx, ts[idx[,1]]) # timestamps
	# idxCutoff <- idx[idx[,2] == 1 | idx[,2] == 2, ]
	idxRle <- rle(idx[,2])
	idxRleOffsets <- cumsum(idxRle$lengths)
	idxRleOnsets <- idxRleOffsets-(idxRle$lengths-1)
	
	counter <- 0
	# do separate loops for each condition to reduce cycles
	# if current is neg_spike and within blink duration there is a positive spike, until next zero if within blink duration
	# get indices of negative spikes in the RLE object
	idx_neg_spikes <- which(idxRle$values == 1, useNames = F)
	if(length(idx_neg_spikes) > 0){
		for(i in 1:length(idx_neg_spikes)){
			# mark blink onset
			thisBlinkOnset <- idx[idxRleOnsets[idx_neg_spikes[i]],3] # t2, according to Mathot's article
			# for this negative spike, check if there is a positive spike that follows within a duration of a blink
			nextUpSpikeIdxs <- idx[idx[,3] >= (thisBlinkOnset + blink_duration_range[1]) & idx[,3] <= (thisBlinkOnset + blink_duration_range[2]) & idx[,2] == 2, 1]
			# check also the following near-zero velocities
			nextZeroIdxs <- idx[idx[,3] >= (thisBlinkOnset + blink_duration_range[1]) & idx[,3] <= (thisBlinkOnset + blink_duration_range[2]) & idx[,2] == 0, 1]
		 	# if we have a positive spike within blink duration
			if(length(nextUpSpikeIdxs) > 0){
				# get the index of first postive spike
				nextUpSpikeIdx <- min(nextUpSpikeIdxs)
				nextSpikeOffsetIdx <- which(idxRleOnsets <= nextUpSpikeIdx & idxRleOffsets >= nextUpSpikeIdx , useNames = F)
				# check if velocity goes to near-zero within blink duration
				if(length(nextZeroIdxs) > 0){
					nextZeroIdx <- min(nextZeroIdxs)
					thisBlinkOffset <- idx[nextZeroIdx,3] # t3, according to Mathot's article
				}
				if(length(nextZeroIdxs) == 0 | (thisBlinkOffset - thisBlinkOnset) > blink_duration_range[2]){
					thisBlinkOffset <- idx[idxRleOffsets[nextSpikeOffsetIdx],3] # t3
				}
				
				preBlinkOnset <- thisBlinkOnset - thisBlinkOffset + thisBlinkOnset # t1, according to Mathot's article
				postBlinkOffset <- thisBlinkOffset - thisBlinkOnset + thisBlinkOffset # t4, according to Mathot's article
				
				# interpolate blink data
				x[ts >= thisBlinkOnset & ts <= thisBlinkOffset] <- spline.interpolation(x, ts, preBlinkOnset, thisBlinkOnset, thisBlinkOffset, postBlinkOffset, plotSignal=F)
				
				# increase blink counter by one
				counter <- counter + 1
			}
		}
	}

	# else if current is NA and within blink duration no neg_spike and yes pos_spike, until zero	
	# get indices of negative spikes in the RLE object
	idx_nas_up_spikes <- which(rollapply(idxRle$values, 2, identical, c(4,2)))
	if(length(idx_nas_up_spikes) > 0){
		for(i in 1:length(idx_nas_up_spikes)){
			# mark blink onset
			thisBlinkOnset <- idx[idxRleOnsets[idx_nas_up_spikes[i]],3] # t2, according to Mathot's article
			nextSpikeOffset <- idx[idxRleOffsets[idx_nas_up_spikes[i]+1], 3]
			
			if((nextSpikeOffset - thisBlinkOnset) >= blink_duration_range[1] & (nextSpikeOffset - thisBlinkOnset) <= blink_duration_range[2]){
				nextZeroIdxs <- idx[idx[,3] >= (thisBlinkOnset + blink_duration_range[1]) & idx[,3] <= (thisBlinkOnset + blink_duration_range[2]) & idx[,2] == 0, 1]
				# check if velocity goes to near-zero within blink duration
				if(length(nextZeroIdxs) > 0){
					nextZeroIdx <- min(nextZeroIdxs)
					thisBlinkOffset <- idx[nextZeroIdx,3] # t3, according to Mathot's article
				}
				if(length(nextZeroIdxs) == 0 | (thisBlinkOffset - thisBlinkOffset) > blink_duration_range[2]){
					thisBlinkOffset <- nextSpikeOffset # t3
				}
				preBlinkOnset <- thisBlinkOnset - thisBlinkOffset + thisBlinkOnset # t1, according to Mathot's article
				postBlinkOffset <- thisBlinkOffset - thisBlinkOnset + thisBlinkOffset # t4, according to Mathot's article
				
				# interpolate blink data
				x[ts >= thisBlinkOnset & ts <= thisBlinkOffset] <- spline.interpolation(x, ts, preBlinkOnset, thisBlinkOnset, thisBlinkOffset, postBlinkOffset)
				
				# increase blink counter by one
				counter <- counter + 1
			}
		}
	}
	
	# TODO
	# if current is neg_spike and within blink duration all NAs afterwards, until next near-zero
	
	print(paste("Found", counter, "blinks!"))
	return(x)
}

artifact.idx <- function(x, ts, threshold = 0.01){
	require(zoo, quietly=T)
	x <- na.approx(x, na.rm=F)
	vel <- abs(c(diff(x)/diff(ts),NA))
	idx <- rep(0,length(x))
	idx[vel > threshold] <- 1 # spike
	return(idx)
}
