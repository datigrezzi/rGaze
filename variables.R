# Script for importing variables file and setting all variables
# created by Iyad Aldaqre on the 15th of November 2012
########################################
### SET VARIABLES FROM IMPORTED FILE ###
########################################
variables<-read.table(file.choose(), header =FALSE, sep = ",", dec = ".",fill =TRUE, na.strings="NA",stringsAsFactors=FALSE)

experiment <- as.character(variables[1,2]) # experiment name
numintermedia<-variables[7,4] # media to be ignored and deleted (e.g. intertrial displays)
intermedia<-rep(0,numintermedia)
for(a in 1:numintermedia){
	intermedia[a]<-as.character(variables[2,a+1])
}
workdir1<- paste(sysdir,as.character(variables[3,2]),sep='') # where to get raw-data files
workdir2<- paste(sysdir,as.character(variables[4,2]),sep='') # where to save data

dir.create(paste(workdir2,'/preprocessed data',sep=''))
workdir3<-paste(workdir2,'/preprocessed data',sep='') # where to save preprocessed data
dir.create(paste(workdir2,'/fixation data',sep=''))
workdir4<-paste(workdir2,'/fixation data',sep='') # where to save fixation data
dir.create(paste(workdir2,'/saccade data',sep=''))
workdir5<-paste(workdir2,'/saccade data',sep='') # where to save saccades data
dir.create(paste(workdir2,'/processed data',sep=''))
workdir6<-paste(workdir2,'/processed data',sep='') # where to save postprocessed data

resolution<-as.numeric(variables[5,2:3]) # display resolution (or stimulus) x,y and anythign outside is ignored
pixthresh<-as.numeric(variables[6,2]) # for I-DT
timethreshold<-as.numeric(variables[6,3]) # temporal threshold for accepting a fixation (ms)/ minimum fixation duration
velthresh<-as.numeric(variables[6,4])/16.6 # velocity threshold for I-VT in pixel/ms (velocity per sample divided by sample rate)

interpolation<-as.numeric(variables[6,5]) # whether to interpolate missing values (1) or not (0)
interpthresh<-as.numeric(variables[6,6]) # threshold of successive missing values which will be interpolated

eyes<-as.character(variables[6,7]) # 'avg' or 'left' or 'right'

naoi<-as.numeric(variables[7,2]) # number of AOIs
nscene<-as.numeric(variables[7,3]) # number of scenes to analyze
nfam<-as.numeric(variables[8,2]) # number of familiarisation trials IF time segments are different compared to test trials. otherwise, it MUST be 0
nseg<-as.numeric(variables[7,5])

dynamicres<-100 # 100ms resolution

sceneCond<-as.numeric(variables[8,3])
scenes<-variables[(9+(naoi*2)):(8+(naoi*2)+nscene),2:3]
scenes<-t(as.matrix(apply(scenes,2,as.numeric)))

dynaScene<-variables[(9+(naoi*2)):(8+(naoi*2)+nscene),4]

segments<-variables[(8+(naoi*2)+nscene+1):(8+(naoi*2)+nscene+nseg),2:4]
segments<-as.matrix(apply(segments,2,as.numeric))

# override nscene - CHECK!
aoimat<-t(variables[9:(8+(naoi*2)),3:(6+(nscene))])
aoimat<-apply(aoimat,2,as.numeric)
dim(aoimat)<-c((4+(nscene)),2,naoi)
aoinames<-unique(variables[9:(8+(naoi*2)),2])
if(is.element(2,dynaScene)){
	aoicorr<-aoimat[5:(4+(nscene)),,] # if some scenes are dynamic, then check aoi correction values
}

aoiType<-'circular' # overwrite in file before fixations if otherwise (e.g. in i.vt.R for wordLearning)
print(paste('AOIs are:',aoiType))
aoimat<-aoimat[1:4,,]

source(paste(fundir,'error.bar.R',sep=''))