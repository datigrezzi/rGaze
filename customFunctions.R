# A set of useful custom functions
# Iyad Aldaqre
# 17.01.2017

# for 
# require(zoo)
require(aspace)

# A function to get the maximum n values (instead of just the single maximum value)
maxN<-function(x,n){
	return(sort(x)[(length(x)-n):length(x)])
}

# A function to plot error bars in barplots
# from monkeysuncle.stanford.edu/?p=485
error.bar <- function(x, y, upper, lower = upper, length = 0.1, ...) {
	if (length(x) != length(y) | length(y) != length(lower) | length(lower) != length(upper)){
		stop("Vectors must be of the same length")
		} else {
			arrows(x, y + upper, x, y - lower, angle = 90, code = 3, length = length, ...)
			}
}

# A function to calculate the standard error of the mean
sem <- function(x, na.rm = TRUE) {
	if (na.rm) {stdError <- sd(x, na.rm = T)/sqrt(length(x))} else {stdError <- sd(x)/sqrt(length(x))}
	return(stdError)
}

# A function to plot line and shaded error area around the line
error.line<-function(x,y,error,type='l',lineCol='black',ylim=c(min(y),max(y)),ylab='',xlab='',errorCol=lineCol,errorAlpha=40,predict=T,add=F,...){
	if(length(error)==1){
		warning('Length of error was expanded to match length of x')
		error<-rep(error,length(x))
	}else if(length(error)>1&length(error)!=length(x)){
		stop('Error and x lengths differ!')
	}
	if(predict==T){
		lineData<-predict(smooth.spline(x,y))
		errorData<-predict(smooth.spline(x,error))
	} else {
		lineData<-as.data.frame(cbind(x,y))
		errorData<-as.data.frame(cbind(x,error))
		names(errorData)<-c('x','y')
	}
	
	if(add==F){plot(x,y,type = 'n',ylim = ylim,ylab = ylab, xlab = xlab)}
	lines(lineData, col = lineCol,type=type,...)
	colVal<-paste(rgb(t(col2rgb(errorCol)),maxColorValue=255),as.character(errorAlpha),sep='')
	polygon(c(lineData$x,rev(lineData$x)), c(lineData$y+errorData$y, rev(lineData$y-errorData$y)),col = colVal, border = NA)
}

# A function to calculate cohen's d effect size for t.tests
cohen.d<-function(M1,SD1,M2,SD2){
	sPooled=sqrt(((SD1^2)+(SD2^2))/2)
	es<-M1-M2/sPooled
	return(es)
}

# A function to interpolate ColNAs # col.na.approx(cbind(x,y,maxgap=interpThresh,na.rm=T))
col.na.approx<-function(data,maxgap=5,na.rm=T){
	for(colNum in 1:ncol(data)){
		data[,colNum]<-na.approx(data[,colNum],...)
	}
	return(data)
}

# A function to convert pixels into degrees of visual angle
pix2deg<-function(pixels,screenSize=22,screenResolution=c(1920,1080),distance=600){
	# dpi: diagnoal pixels, as calculated by pythagorean theorem, divided by screen size in inches
	dpi<-sqrt((screenResolution[1]^2)+(screenResolution[2]^2))/screenSize
	mm<-(pixels*25.4)/dpi
	deg<-2*atan_d(mm/(2*distance))
	return(deg)
}

# A function to convert degrees of visual angle into pixels
deg2pix <- function(degrees, screenSize=22, screenResolution=c(1920, 1080), distance = 600){
	# dpi: diagnoal pixels, as calculated by pythagorean theorem, divided by screen size in inches
	dpi <- sqrt((screenResolution[1]^2)+(screenResolution[2]^2))/screenSize # dot per inch
	dpmm <- dpi / 25.4 # converted to mm
	pix <- (2*distance*tan_d(degrees/2)) * dpmm
	return(round(pix)) # because pixels ar integers!
}

deg2pix2 <- function(degrees, dotPerMm, distance = 600){
	pix <- (2*distance*tan_d(degrees/2)) * dotPerMm
	return(round(pix)) # because pixels ar integers!
}

# A function to convert mm to degrees of visual angle
mm2deg <- function(mm, distance){
	deg <- 2*atan_d(mm/(2*distance))
	return(deg)
}

deg2mm <- function(deg, distance){
	mm <- 2*distance*tan_d(deg/2)
	return(mm)
}