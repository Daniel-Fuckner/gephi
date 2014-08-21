createFeatures <- function(clickTimestamp, touchpointType){
  len <- length(clickTimestamp)
  # Spezialfall wenn funnelLength == 1 
  if (len==1){
    First <- Last <- as.numeric(1)
    Position <- funnelLength <- as.integer(1)
    timeSinceLastDay <- as.numeric(NA)
    timeSinceLastHour <- as.numeric(NA)
    timeSinceLastMinute <- as.numeric(NA)
    timeSinceFirstDay <- as.numeric(NA)
    timeSinceFirstHour <- as.numeric(NA)
    timeSinceFirstMinute <-as.numeric(NA) 
  }else{
    First <- c(1,rep(0,len-1))
    Last <- c(rep(0,len-1),1)
    Position <- c(1:len)
    funnelLength <- c(rep(len,len))
    
    # Hier werden die zeitdifferenzen zwischen dem aktuellen und dem vorherigen touchpoint berechnet
    # Kann auch noch andere differenzen berechnen 
    x <- as.numeric(as.difftime(c(clickTimestamp),format="%Y-%m-%d %H:%M:%S", units = "days"))
    timeSinceLastDay <-c(as.numeric(NA),x[2:len]-x[1:(len-1)])
    x2 <- as.numeric(as.difftime(c(clickTimestamp),format="%Y-%m-%d %H:%M:%S", units = "hours"))
    timeSinceLastHour <- c(as.numeric(NA),x2[2:len]-x2[1:(len-1)])
    x3 <- as.numeric(as.difftime(c(clickTimestamp),format="%Y-%m-%d %H:%M:%S", units = "mins"))
    timeSinceLastMinute <- c(as.numeric(NA),x3[2:len]-x3[1:(len-1)])
    
    # die gesamte Zeit eines Funnels in Tagen, wenn man x2 oder x3 verwendet kann man die Zeit in Stunden und/oder Minuten haben. 
    
    # Funnel duration in Days, Minutes and hours
    timeSinceFirstDay <- c(rep(0,len))
    timeSinceFirstHour <- c(rep(0,len))
    timeSinceFirstMinute <- c(rep(0,len))
    
    timeSinceFirstDay <-  cumsum(timeSinceLastDay)
    timeSinceFirstHour <-  cumsum(timeSinceLastHour)
    timeSinceFirstMinute <-  cumsum(timeSinceLastMinute)
  }
  
  # weekdays
  weekday <- weekdays(as.POSIXct(clickTimestamp))
  weekday <- (factor(weekday,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))
  levels(weekday) <- c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag")
  
  hour  <- hour(as.POSIXct(clickTimestamp))
  
  # Has clicked variable and clickCount
  hasClicked <- c(rep(0,len))
  clickCount <- c(rep(0,len))
  
  if(sum(touchpointType==1)>=1){
    hasClicked[min(which(touchpointType==1)):len] <- 1
    clickCount <- cumsum(as.numeric(touchpointType==1))
  }
  
  Transaction <- Last
  data.table(First,Last,Position,Transaction,funnelLength,timeSinceLastDay
             ,timeSinceLastHour,timeSinceLastMinute
             ,timeSinceFirstDay,timeSinceFirstHour,timeSinceFirstMinute,weekday,hour,hasClicked,clickCount)
}