#------------------------------------------------------------------------------- 
#  Rounding zoo times series
#------------------------------------------------------------------------------- 
round_time<-function(x,rtyp="01mins"){
  if(rtyp=="01mins") {x <- aggregate(x,time(x)-as.numeric(as.POSIXlt(time(x)))%%(60*1), median)}
  if(rtyp=="05mins") {x <- aggregate(x,time(x)-as.numeric(as.POSIXlt(time(x)))%%(60*5), median)}
  if(rtyp=="10mins") {x <- aggregate(x,time(x)-as.numeric(as.POSIXlt(time(x)))%%(60*10),median)}
  if(rtyp=="15mins") {x <- aggregate(x,time(x)-as.numeric(as.POSIXlt(time(x)))%%(60*15),median)}  
  if(rtyp=="30mins") {x <- aggregate(x,time(x)-as.numeric(as.POSIXlt(time(x)))%%(60*30),median)}
  if(rtyp=="60mins") {x <- aggregate(x,time(x)-as.numeric(as.POSIXlt(time(x)))%%(60*60),median)}
  return(x)}
#===============================================================================
