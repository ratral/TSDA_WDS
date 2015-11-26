
read_time_series <- function(data_file,
                             date_format = "%H:%M:%S",
                             time_format = "%d.%m.%Y" , 
                             time_zone   = Sys.timezone(),
                             roundtyp = "01mins",
                             sep = ",",
                             header = TRUE,...){
  ## Loading Packages
  require(zoo,chron)
  
  data_serie <- read.csv(file = data_file)
  
  ## checked for duplicated
  data_serie <- unique(data_serie)
  
  date_time <- as.POSIXct(paste(data_serie$Date,data_serie$Time),
                         format = paste(date_format,time_format),
                         tz = time_zone)
  
  data_serie <- data.frame(date_time = date_time,
                           value = data_serie[,3])
  
  data_serie <- read.zoo(data_serie)
  data_serie <- round_time(data_serie , rtyp=roundtyp)
  
}
