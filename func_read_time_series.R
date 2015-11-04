read_time_series <- function(file , sep = "," , time_format = "%H:%M:%S" , date_format="'%d.%m.%Y" , value="" , units="" , ...){
  # Loading Packages
  require(zoo,chron)
  
  f1 <- function(d, t) as.POSIXct(paste(d, t), format = paste(date_format, time_format))
  
  z1 <- read.zoo(file,header = FALSE,index = 1:2,FUN = f1)
  
}
