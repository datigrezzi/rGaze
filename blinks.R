rm(list=ls())

# based on Mathot's algorithm description in A Simple Way To Reconstruct Pupil Size During Eye Blinks (2013)
# Modified to fit noisy data with many missing samples recorded at lower frequency
# Iyad Aldaqre
# 2018

# remove.blinks <- function(x, ts, threshold = 0.01){
  # require(zoo)
  # # normX <- scale(x)
  # vel <- c(diff(x)/diff(ts),NA)
  # # mark different phases of blins (onset, reversal and offset)
  # idx <- matrix(c(seq(1,length(x)),rep(0,length(x))), nrow = length(x), ncol = 2)
  # idx[vel <= threshold * -1, 2] <- 1 # onset
  # idx[vel >= threshold, 2] <- 2 # reversal
  # idx[abs(vel) <= 0.001, 2] <- 3 # almost zero velocity, offsets
  # idx <- idx[idx[,2] != 0,]
  # blinkRule <- which(rollapply(idx[,2], 3, identical, c(1, 2, 3)))
  # onsets <- matrix(idx[blinkRule,], ncol=2)
  # print(paste("Found", length(blinkRule), "blinks!"))
  # if(length(blinkRule) > 0){
	  # for(ii in 1:length(blinkRule)){
	    # # get t2 = onset and t3 = offset times
	    # blinkIdx <- idx[which(idx[,1]==onsets[ii,1]):(which(idx[,1]==onsets[ii,1])+2),]
	    # onsetTs <- ts[blinkIdx[blinkIdx[,2]==1,1]]
	    # offsetTs <- ts[blinkIdx[blinkIdx[,2]==3,1]]
	    # # calculate t1 = t2 - t3 + t2 and t4 = t3 - t2 + t3
	    # preOnsetTs <- onsetTs - offsetTs + onsetTs
	    # postOffsetTs <- offsetTs - onsetTs + offsetTs
	    # # select data
	    # tmp <- cbind(x[ts >= preOnsetTs & ts <= postOffsetTs], ts[ts >= preOnsetTs & ts <= postOffsetTs])
	    # # na blink data
	    # tmp[tmp[,2] >= onsetTs & tmp[,2] <= offsetTs, 1] <- NA
	    # # Select portion of UN PROCESSED DATA AND CALCULATE CUBIC SPLINE from t1 to t4
	    # interpolatedData <- spline(tmp[,1], n=length(tmp[,1]))$y
	    # # replace ONLY BLINK PORTION (from t2 to t3) WITH ITS PARALLEL FROM SPINE OUTPUT
	    # x[ts >= onsetTs & ts <= offsetTs] <- interpolatedData[tmp[,2] >= onsetTs & tmp[,2] <= offsetTs]
	    # # mark onset-offset times
	  # }
  # }
  # return(x)
# }


# cubic spline interpolation
spline.interpolation <- function(x, ts, t1, t2, t3, t4){
	# select data
	tmp <- cbind(x[ts >= t1 & ts <= t4], ts[ts >= t1 & ts <= t4])
	# na blink data
	tmp[tmp[,2] >= t2 & tmp[,2] <= t3, 1] <- NA
	# Select portion of UN PROCESSED DATA AND CALCULATE CUBIC SPLINE from t1 to t4
	interpolatedData <- spline(tmp[,1], n=length(tmp[,1]))$y
	# return interpolation
	return(interpolatedData[tmp[,2] >= t2 & tmp[,2] <= t3])
}

remove.blinks <- function(x, ts, norm_threshold = 0.04, initial_interpolation_maxgap=2, sampling_rate = 50, blink_duration_range = c(100, 400)){
  require(zoo, quietly=T)
  sampleDur <- 1000/sampling_rate
  blink_samples_range <- round(blink_duration_range / sampleDur)
  normX <- na.approx(scale(x), maxgap= initial_interpolation_maxgap, na.rm=F)
  vel <- c(diff(normX)/diff(ts),NA)

  # categorize different data patterns
  idx <- matrix(c(seq(1,length(x)),rep(0,length(x))), nrow = length(x), ncol = 2)
  idx[vel < norm_threshold * -1, 2] <- 1 # neg_spike
  idx[vel > norm_threshold, 2] <- 2 # pos_spike
  idx[abs(vel) <= 0.001, 2] <- 3 # almost zero velocity
  idx[is.na(vel), 2] <- 4 # NAs
  # add timestamps for reference
  idx <- cbind(idx, ts[idx[,1]]) # timestamps
  # idxCutoff <- idx[idx[,2] == 1 | idx[,2] == 2, ]
  idxRle <- rle(idx[,2])
  idxRleOffsets <- cumsum(idxRle$lengths)
  idxRleOnsets <- idxRleOffsets-(idxRle$lengths-1)
  
  # loop over idx
  counter <- 0
  # do separate loops for each condition to reduce cycles
  # if current is neg_spike and within blink duration there is a positive spike, until zero
  idx_neg_spikes <- which(idxRle$values == 1, useNames = F)
  for(i in 1:length(idx_neg_spikes)){
  	nextUpSpikeIdxs <- idx[idx[,3] >= (idx[idxRleOnsets[idx_neg_spikes[i]],3] + blink_duration_range[1]) & idx[,3] <= (idx[idxRleOnsets[idx_neg_spikes[i]],3] + blink_duration_range[2]) & idx[,2] == 2, 1]
  	if(length(nextUpSpikeIdxs) > 0){
  		nextUpSpikeIdx <- min(nextUpSpikeIdxs)
  		nextSpikeOffsetIdx <- which(idxRleOnsets <= nextUpSpikeIdx & idxRleOffsets >= nextUpSpikeIdx , useNames = F)
  		thisBlinkOnset <- idx[idxRleOnsets[idx_neg_spikes[i]],3] # t2, according to Mathot's article
  		thisBlinkOffset <- idx[idxRleOffsets[nextSpikeOffsetIdx],3] # t3, according to Mathot's article
  		preBlinkOnset <- thisBlinkOnset - thisBlinkOffset + thisBlinkOnset # t1, according to Mathot's article
  		postBlinkOffset <- thisBlinkOffset - thisBlinkOnset + thisBlinkOffset # t4, according to Mathot's article
  		
  		# interpolate blink data
  		x[ts >= thisBlinkOnset & ts <= thisBlinkOffset] <- spline.interpolation(x, ts, preBlinkOnset, thisBlinkOnset, thisBlinkOffset, postBlinkOffset)
  		
  	}
  	# else if current is neg_spike and within blink duration all NAs, until zero
  	# else if current is NA and within blink duration no neg_spike and yes pos_spike, until zero
  	# else if current is NA for a duration of blink, until zero
  }

  
  # for(i in 2:nrow(idxCutoff)){
  	# if(idxCutoff[i-1,2] == 1 & idxCutoff[i,2] == 2 & idxCutoff[i,3]-idxCutoff[i-1,3] <= blink_duration){ # blink
  		# blinkOnset <- idxCutoff[i-1, 3] # last zero before threshold cut
  		# blinkReversal <- idxCutoff[i, 3]
  		# nextZero <- min(idxNoZero[idxNoZero[,2] == 3 & idxNoZero[,3] > blinkReversal, 3])
	    # if(diff(c(blinkOnset,nextZero)) <= blink_duration + 20){
	    	# blinkOffset <- nextZero # first zero after threshold cut
	    # }
	    # else{
	    	# blinkOffset <- blinkReversal + (diff(c(blinkOnset,blinkReversal)/2))
	    # }
	    # # calculate t1 = t2 - t3 + t2 and t4 = t3 - t2 + t3
	    # preBlinkOnset <- blinkOnset - blinkOffset + blinkOnset
	    # postBlinkOffset <- blinkOffset - blinkOnset + blinkOffset
	    
	    # # select data
	    # tmp <- cbind(x[ts >= preBlinkOnset & ts <= postBlinkOffset], ts[ts >= preBlinkOnset & ts <= postBlinkOffset])
	    # # na blink data
	    # tmp[tmp[,2] >= blinkOnset & tmp[,2] <= blinkOffset, 1] <- NA
	    # # Select portion of UN PROCESSED DATA AND CALCULATE CUBIC SPLINE from t1 to t4
	    # interpolatedData <- spline(tmp[,1], n=length(tmp[,1]))$y
	    # # replace ONLY BLINK PORTION (from t2 to t3) WITH ITS PARALLEL FROM SPINE OUTPUT
	    # x[ts >= blinkOnset & ts <= blinkOffset] <- interpolatedData[tmp[,2] >= blinkOnset & tmp[,2] <= blinkOffset]
  		# counter <- counter + 1
  	# }
  # }
  
  ### get consecutive NAs that are long enough to be part of a blink
  # lenEnc <- rle(idxNoZero[,2])
  # lenEnc$values <- lenEnc$values == 4 & abs(lenEnc$lengths - round(blink_duration / (1000/sampling_rate))) <= 4
  # cumsum(lenEnc$lengths)
  # idxNoZero[inverse.rle(lenEnc),]
  ### drop in vel below threshold with consecutive NAs; from last NA get center and do as above
  ### increase in vel above threshold with preceding NAs; from first NA get center and do as above
  ### consecutive NAs that span the length of a blink (from 150 to 250ms)


  print(paste("Found", counter, "blinks!"))
  return(x)
}

### smoothing data by averaging consecutive samples
# x_smooth <- c(NA, rollapply(x, width = 2, FUN = mean))
# x_norm <- scale(x)
### checking NAs
### blinks should be NA that are between 100 and 200ms in both eyes
# lenEnc <- rle(is.na(x))
# lenEncS <- rle(thisData$s!=0)
# lenEnc$values <- lenEnc$values & lenEnc$lengths >= 6
# str(lenEnc)
# thisData[inverse.rle(lenEnc),]
# diff(range(ts))
# ts[cumsum(lenEnc$lengths)[lenEnc$values]] # NA blink offset
# ts[(cumsum(lenEnc$lengths)-(lenEnc$lengths-1))[lenEnc$values]] # NA blink onset
###

vel <- abs(c(diff(x_norm)/diff(ts), NA))
par(mar=c(3,3,2,1), mgp=c(2,0.4,0), las=1,tck=-.01, xaxs="i",yaxs="i", mfrow=c(2,1))
lims <- c(2, 5)
plot(ts, x, type = "l", ylab='raw pupil (mm)', xlab='timestamp (ms)')
plot(ts, thisData$pdl, type = "l", ylab='norm pupil', xlab='timestamp (ms)')
plot(ts, vel, type = "l", ylab='velocity change', xlab='timestamp (ms)')

plot(ts, x_norm, type = "l", ylab='norm pupil', xlab='timestamp (ms)')

rawData <- cbind(ts,x,x_norm, vel)
rawData <- rawData[rawData[,1] >= 70000 & rawData[,1] <= 80000,]
plot(rawData[,1], rawData[,2], type = "l", ylab='raw pupil (mm)', xlab='timestamp (ms)')
plot(rawData[,1], rawData[,3], type = "l", ylab='norm pupil', xlab='timestamp (ms)')
plot(rawData[,1], rawData[,4], type = "l", ylab='absolute velocity of change', xlab='timestamp (ms)')

# plot(tmp[,1], na.locf(tmp[,4],na.rm=F, fromLast = T), type = "l", ylim=lims)
# plot(tmp[,1], na.approx(tmp[,4],na.rm=F), type = "l", ylim=lims)
# plot(tmp[,1], tmpBlinks, type = "l", ylim=lims)
# plot(tmp[,1], tmp[,3], type = "l", ylim=c(-0.02,0.02))
