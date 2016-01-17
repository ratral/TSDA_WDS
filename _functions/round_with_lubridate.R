# Function Round --------------------------------------------------------------
# The code handle well all the tricky situations thanks to lubridate. 
# Of course this function works only for precision expressed in minutes
# but you can easily extend it to other units, and even create a generic
# function if you really need it.


round_minute <- function(x,precision){
        m    <- minute(x) + second(x)/60
        m.r  <- round(m /precision)*precision
        minute(x) <- m.r
        second(x) <- 0
        x
}
