# a script to calculate average pupil baseline during certain time segments
# after calculating average baseline and subtracting it from each sample
# sketch written on 11.2.2015
# iyad aldaqre

pupils<-function(data,trial.index,baseline.index,pupil.index,bin=ceil(samplingRate),interpolate=T,maxgap=5,saveOutput=F,outputDir)

# output variables: raw average pupil, absolute difference form baseline, percent difference from baseline
# trial.index is trial variable; very important to segregate trials. condition or event names recommended!
# basline.index is dummy variable to indicate segments from which baseline will be taken
# pupil.index is dummey variable to indicate segments from which pupil dilation will be calculated
# bin is important to standardize time to beginning of segment; to use max resolution, ceil(samplingRate) is recommended
# this time should be adjusted, by subtracting event start time form raw timestamp (check pG pupil analyses)
# interpolate will use na.approx with maxgap
# saveOutput will create a short version of the data with only averages
# if trial.index was prepared to include stimulus name or condition name, this will be included in exported dataset

# work on the same dataframe: use apply(min) to add average baseline at first sample of each trial.index
# then from zoo, na.locf() to fill variable and apply vector operations!