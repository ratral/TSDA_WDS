#===============================================================================
#
#------------------------------------------------------------------------------- 
plot_time_serie<-function(x,xtyp="days"){
  
  x    <- box_plot_data_ts(x,xtyp)
  graf <- ggplot(x,aes(x=time,y=median))
  
  if(xtyp=="days"){graf<-graf+
    geom_errorbar(colour="gray",size=1.0,aes(ymin=min,ymax=max))+
    geom_errorbar(colour="blue",size=1.0,aes(ymin=quant.25,ymax=quant.75))+
    geom_point(colour="red",size=3)}
  
  
  if(xtyp=="hours"){graf<-graf+
    geom_errorbar(colour="gray",size=1.0,aes(ymin=min,ymax=max))+
    geom_errorbar(colour="blue",size=1.0,aes(ymin=quant.25,ymax=quant.75))+
    geom_point(colour="red",size=3)+
    scale_x_datetime(breaks = date_breaks("2 hour"),
                     minor_breaks = date_breaks("1 hour"),
                     labels = date_format("%H:%M", tz = "Europe/London"))}
  
  if(xtyp=="mins"){graf<-graf+
    geom_errorbar(colour="gray",size=0.5,aes(ymin=min,ymax=max))+
    geom_errorbar(colour="blue",size=0.5,aes(ymin=quant.25,ymax=quant.75))+
    geom_point(colour="red",size=1.5)+
    scale_x_datetime(breaks = date_breaks("2 hour"),
                     minor_breaks = date_breaks("1 hour"),
                     labels = date_format("%H:%M", tz = "Europe/London"))}
  graf
}

#===============================================================================
