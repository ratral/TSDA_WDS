##-----------------------------------------------------------------------------
## Box Plot: Display of Distribution
##-----------------------------------------------------------------------------
# Box and whisker plots are uniform in their use of the box: the bottom and top
# of the box are always the first and third quartiles, and the band inside the 
# box is always the second quartile (the median). But the ends of the whiskers
# can represent several possible alternative values, among them:
# - the minimum and maximum of all of the data.
# - the lowest datum still within 1.5 IQR of the lower quartile, 
#   and the highest datum still within 1.5 IQR of the upper quartile
#   (often called the Tukey boxplot)
# - one standard deviation above and below the mean of the data
# - the 9th percentile and the 91st percentile
# - the 2nd percentile and the 98th percentile.
##-----------------------------------------------------------------------------


box_plot_data_ts <- function(x,xtyp="days"){
  if(xtyp=="days") {
    x <- data.frame(time=format(round(index(x),"days"),format="%Y-%m-%d"),coredata(x))
    colnames(x)[2]<-'value' 
    x <- boxplot(value~time,data=x,plot=FALSE)
    x <- data.frame(time=as.Date(x$name),stats=t(x$stats),n=x$n)
    colnames(x) <- c("time","min","quant.25","median","quant.75","max","records")
    x}
  
  if(xtyp=="hours"){
    x <- data.frame(time=format(round(index(x),"hours"),format="%H:%M:%S"),coredata(x))
    colnames(x)[2]<-'value'
    x <- boxplot(value~time,data=x,plot=FALSE)
    x <- data.frame(time=as.POSIXct(x$name,format="%H:%M:%S", tz = "Europe/London" ),stats=t(x$stats),n=x$n)
    colnames(x) <- c("time","min","quant.25","median","quant.75","max","records")
    x}
  
  if(xtyp=="mins") {
    x<-data.frame(time=format(round(index(x),"mins"),format="%H:%M:%S"),coredata(x))
    colnames(x)[2]<-'value'
    x <- boxplot(value~time,data=x,plot=FALSE)
    x <- data.frame(time=as.POSIXct(x$name,format="%H:%M:%S", tz = "Europe/London"),stats=t(x$stats),n=x$n)
    colnames(x) <- c("time","min","quant.25","median","quant.75","max","records")
    x}
  
  if(xtyp=="weekdays") {    
    x<-data.frame(weekday=weekdays(as.POSIXct(index(x)),abbreviate=FALSE),
                  time=format(round(index(x),"hours"),format="%H:%M:%S"),
                  coredata(x))
    
    colnames(x)[3]<-'value'
    x<-boxplot(value~paste(weekday,"-",time,sep=""),data=x,plot=FALSE)
    
    weekdays <- c( rep("1-Monday",12),
                   rep("2-Tuesday",12),
                   rep("3-Wednesday",12),
                   rep("4-Thursday",12),
                   rep("5-Friday",12),
                   rep("6-Saturday",12),
                   rep("7-Sunday",12))
    
    time <- (paste(c(0:23),':',rep('00',23),':',rep('00',23),sep=''))
    time <- c(rep(time,7)) ;time <- as.POSIXct(time,format="%H:%M:%S", tz = "Europe/London")
    
    x <- data.frame(weekdays=weekdays,time=time,stats=t(x$stats),n=x$n)}
  
  return(x)
} 

##-----------------------------------------------------------------------------
