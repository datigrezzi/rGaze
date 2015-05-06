# mark AOIs as distance from their centers ========================== CHECK NEW AOIS
# make function
data$aoi <- "out"
for(ia in 1:naoi){
data$aoi[sqrt((data$gazePointX-aoimat[1,1,ia])^2+(data$gazePointY-aoimat[1,2,ia])^2)<=aoicorr[1,ia]]<-aoinames[ia]
}

# data$aoi[sqrt((data$gazePointX - aoimat[1, 1, 1])^2 + (data$gazePointY - aoimat[1, 2, 1])^2) <= aoicorr[1, 1]] <- aoinames[1]
# data$aoi[sqrt((data$gazePointX - aoimat[1, 1, 2])^2 + (data$gazePointY - aoimat[1, 2, 2])^2) <= aoicorr[1, 2]] <- aoinames[2]
# data$aoi[sqrt((data$gazePointX - aoimat[1, 1, 3])^2 + (data$gazePointY - aoimat[1, 2, 3])^2) <= aoicorr[1, 3]] <- aoinames[3]
# data$aoi[(sqrt((data$gazePointX - aoimat[1, 1, 3])^2 + (data$gazePointY - aoimat[1, 2, 3])^2) <= aoicorr[1, 4]) & (sqrt((data$gazePointX - aoimat[1, 
	# 1, 3])^2 + (data$gazePointY - aoimat[1, 2, 3])^2) > aoicorr[1, 1])] <- aoinames[4]