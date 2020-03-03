# A funciton to parse Tobii Glasses 2 JSON data and output a more familiar dataframe
# TDOO: keep vts in separate array and add s value for each variable separately (or encode it like tobii did before)

require(rjson)
require(reshape2)

listJS2df <- function(list_of_JSON){
    if(length(list_of_JSON) > 0){
        tmp <- lapply(lapply(list_of_JSON, FUN=fromJSON), FUN=unlist)
        dfOut <- as.data.frame(matrix(unlist(tmp), byrow=T, nrow=length(tmp)), stringsAsFactors = F)
        names(dfOut) <- names(fromJSON(list_of_JSON[1]))
        return(dfOut)
    } else{
        return(NULL)
    }
}

read.tobii.glasses <- function(g_file, normalized = FALSE){
	glassesJSON<-readLines(g_file)
		
	# extract video time data
	tmp <- glassesJSON[grepl('vts', glassesJSON) & !grepl('evts', glassesJSON)]
	videoTime <-listJS2df(tmp)[,c(1,3)]
	videoTime$ts <- as.numeric(videoTime$ts)

	# extract gaze data
	tmp <- glassesJSON[grepl('gp', glassesJSON) & !grepl('gp3', glassesJSON)]
	gazeData <- listJS2df(tmp)[,-4]
	names(gazeData)[4:5] <- c("gpx", "gpy")
	gazeData$ts <- as.numeric(gazeData$ts)
	
	# extract gaze data 3d
	tmp <- glassesJSON[grepl('gp3', glassesJSON)]
	gazeData3D <- listJS2df(tmp)
	names(gazeData3D)[4:6] <- c("gpx", "gpy", "gpz")
	gazeData3D$ts <- as.numeric(gazeData3D$ts)
	

	# extract pupil data
	tmp <- glassesJSON[grepl('pd',glassesJSON)]
	pupilData <- listJS2df(tmp)
	
	# extract gyroscope data
	tmp <- glassesJSON[grepl('gy', glassesJSON)]
	gyroData <- listJS2df(tmp)
	gyroData$ts <- as.numeric(gyroData$ts)
	names(gyroData)[3:5] <- c("gyroy", "gyrox", "gyroz")

	# extract accelerometer data
	tmp <- glassesJSON[grepl('ac', glassesJSON)]
	accelerometerData <- listJS2df(tmp)
	accelerometerData$ts <- as.numeric(accelerometerData$ts)
	
	# extract sync out data
	tmp <- glassesJSON[grepl('sig', glassesJSON) & grepl('out', glassesJSON)]
	syncOutData <- listJS2df(tmp)
	
	# extract sync in data
	tmp <- glassesJSON[grepl('sig', glassesJSON) & grepl('in', glassesJSON)]
	syncInData <- listJS2df(tmp)
	
	##########################
	### Preparing datasets ###
	##########################
	### PUPIL
	pupilData$pd <- as.numeric(pupilData$pd)
	pupilData$ts <- as.numeric(pupilData$ts)
	pupilData$s <- as.numeric(pupilData$s)
	pupilData <- cbind(recast(pupilData, ts ~ eye, id.var=c("ts","eye"), measure.var="pd", mean),recast(pupilData, ts ~ eye, id.var=c("ts","eye"), measure.var="s", mean)[,c(2,3)])
	names(pupilData) <- c("ts","pdl","pdr","s_pdl","s_pdr")
	# merge with gaze data
	gazeData <- merge(gazeData, pupilData, by="ts", all=T)

	### SYNC OUT
	syncOutData$ts <- as.numeric(syncOutData$ts)
	syncOutData$sig <- as.numeric(syncOutData$sig)
	
	### SYNC IN
	if(!is.null(syncInData)){
		syncInData <- syncInData[,c(1,4)]
	} else {
		syncInData <- as.data.frame(cbind(syncOutData$ts, rep(0, nrow(syncOutData))), stringsAsFactors=F)
		names(syncInData) <- c("ts", "sig")
	}
	syncInData$ts <- as.numeric(syncInData$ts)
	syncInData$sig <- as.numeric(syncInData$sig)
	syncData <- merge(syncOutData[,-3], syncInData, by="ts", all=T)
	names(syncData)[3:4]<-c('sync.out','sync.in')
	
	### RETURN LIST OF SEPARATE DATASETS
	return(list("gaze" = gazeData, "gaze3" = gazeData3D, "gyro" = gyroData, "accelerometer" = accelerometerData, "vts" = videoTime, "sync" = syncData))
}

# function to calculate angles from velocity, can be used to revert i-vt vector or to estimate angles for gyroscope, assuming starting point is 0
# velocity = distance / time
# distance = velocity * time
velocity2degrees <- function(velocity_vector, ts_vector, baseline_correction = TRUE){
    time_delta <- c(0,diff(ts_vector))
    if(baseline_correction){velocity_vector <- velocity_vector - velocity_vector[1]}
    steps_degree <- velocity_vector * time_delta
    # print(steps_degree[1])
    return(cumsum(steps_degree))
}

rad2deg <- function(angle){
    return((angle * 180) / pi)
}

deg2rad <- function(angle){
    return((angle * pi) / 180)
}

vectors2angle <- function(a, b, absolute = TRUE, degree = TRUE){
    # a <- c(4, -3, 5)
    # b <- c(9, 7, -10)
    # https://www.youtube.com/watch?v=QWIZXRjMspI
    # https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r
    
    a <- as.matrix(a)
    b <- as.matrix(b)
    dotProd <- sum(a*b)
    aNorm <- sqrt(sum(a * a))
    bNorm <- sqrt(sum(b * b))
    
    theta <- acos(dotProd / ( aNorm * bNorm))
    
    if(degree){
        theta <- rad2deg(theta)
    }
    if(b[1,1] < a[1,1] & !absolute){
        theta <- theta * -1
    }
    return(theta)
}

car2pol <- function(x, y, z, degree = TRUE, adjust = TRUE){
    # x <- 300; y <- 600; z <- 300
    # x <- gd$gaze3$gpx[100] #181.19
    # y <- gd$gaze3$gpz[100] #714.19
    # z <- gd$gaze3$gpy[100] #210.31
    
    # reference: http://dynref.engr.illinois.edu/rvs.html
    # xyz coordinates of a point
    # r = sqrt(x^2+y^2+z^2)
    # theta = atan2(y,x)
    # phi = arccos(z/r)
    if(z > 0){
        rho <- sqrt(x^2 + y^2 + z^2) #766.2424
        theta <- atan2(y, x) #0.859642
        phi <- acos(z/rho) #0.3707167
        if(degree){
            theta <- rad2deg(theta) #49.25386
            phi <- rad2deg(phi) #21.2405
        }
        if(adjust){
            theta <- (90 - abs(theta)) * -1 #-40.74614
            phi <- (90 - abs(phi)) * -1 #-68.7595
        }
        return(c(rho, theta, phi))
    }
    else{
        return(c(NA,NA,NA))
    }
}

pol2car <- function(rho, theta, phi, degree = TRUE, adjust = TRUE){
    # reference: http://dynref.engr.illinois.edu/rvs.html
    # r or rho is radius, or the is distance from origin
    # theta is azimuth or polar angle from x in the x/y plane
    # phi is inclination or the angle fromt he positive z axis
    if(adjust){
        theta = 90 + theta
        phi = 90 + phi
    }
    if(degree){
        theta <- deg2rad(theta)
        phi <- deg2rad(phi)
    }
    x = rho * cos(theta) * sin(phi)
    y = rho * sin(theta) * sin(phi)
    z = rho * cos(phi)
    return(c(x,y,z))
}

### TESTING ###
# plot(x,y, xlim = c(150,-150), ylim = c(-150,150))
# gd$gaze3[gd$gaze3$gaze_x_deg == max(gd$gaze3$gaze_x_deg, na.rm=T),]

# i <- 1
# coords <- c(gd$gaze_gyro$gpx[i], gd$gaze_gyro$gpz[i], gd$gaze_gyro$gpy[i])
# angles <- car2pol(coords[1], coords[2], coords[3])
# coords2 <- pol2car(angles[1], angles[2], angles[3])
# coords; coords2
