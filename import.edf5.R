# browsing and reading hdf5 files
# using package h5r
# download and usage from:http://stackoverflow.com/questions/15974643/how-to-deal-with-hdf5-files-in-r
# 22.5.2014

# get to their sources
source('http://bioconductor.org/biocLite.R')
# get specific library
biocLite('rhdf5')
# load library
require(rhdf5)

# browse file structure
setwd("~/Dropbox/BabyLab/Iyads Experiments/EMORES/psychopy/data")
h5ls('tobi_emores_2015_Feb_02_1424.hdf5')

# read specific data set from file
allData<-h5read('tobi_emores_2015_Feb_02_1424.hdf5','/data_collection/events/eyetracker/BinocularEyeSampleEvent')