rm(list = ls())

#load packages
library(data.table)
library(foreach)
library(doSNOW)

###support function
source("/zpool1/s10859017/consulting/scripts/createFeatures.R")
source("/zpool1/s10859017/consulting/scripts/createFeaturesClicksOnly.R")
source("/zpool1/s10859017/consulting/scripts/createPosition.R")

load(file = "/zpool1/s10859017/consulting/r_data/sets.RData")
levelName = sets[[2]]
sets = sets[[1]]

failList <- list()
system.time(
  for (i in 1:8){
    ############################################
    ## campaign teil start
    pathFail <- paste("/zpool1/s10859017/consulting/raw_data/fail_funnel_structure",i,".csv",sep="")
    system.time(fail<- fread(input=pathFail ,stringsAsFactors=FALSE, na.strings="\\N", sep=",", header=FALSE))
    setnames(fail, c('id','projectIdFunnelKeyword','clickTimestamp','touchpointType','isLast','isTransaction','creationtime'))

    maxId <- max(fail$id)
    system.time(newFeat <- fail[, createFeatures(clickTimestamp, touchpointType, id, maxId), by="id"])
    newFeat$vid <- c(1 : length(newFeat$id))
    fail$vid <- c(1 : length(newFeat$id))
    
    setkey(newFeat,vid)
    setkey(fail,vid)
    fail <- merge(fail, newFeat, by="vid", suffixes = c("", ".y"))
    fail[, c("vid","isLast","isTransaction","id.y"):=NULL]
    
    ############################################
    ## funnel teil ende
    print("fail funnel zu ende")
    ############################################
    
    fail$campaign <- rep(NA, nrow(fail))
        
    el2 <- fail$projectIdFunnelKeyword 

    for(j in 1 : length(levelName)){
      idx <- is.element(el = el2, set = sets[[j]])
      fail$campaign[idx] <- levelName[j]
    }

    failList[[i]] <- fail
    #delete not needed items
    state <- paste("Iteration",i,"ist zu ende")
    print(state)
  }
)

fail <- rbindlist(failList)

# parFunc = function(sets){
#   createFeatures <- function(clickTimestamp, touchpointType,id,maxId){
#     if((id %% 10000)==0){
#       state <- paste(round(id/maxId*100,2), "% is done")
#       print(state)
#     }
#     len <- length(clickTimestamp)
#     # Spezialfall wenn funnelLength == 1 
#     if (len==1){
#       First <- Last <- as.numeric(1)
#       Position <- funnelLength <- as.integer(1)
#       timeSinceLastDay <- as.numeric(NA)
#       timeSinceLastHour <- as.numeric(NA)
#       timeSinceLastMinute <- as.numeric(NA)
#       timeSinceFirstDay <- as.numeric(NA)
#       timeSinceFirstHour <- as.numeric(NA)
#       timeSinceFirstMinute <-as.numeric(NA) 
#     }else{
#       First <- c(1,rep(0,len-1))
#       Last <- c(rep(0,len-1),1)
#       Position <- c(1:len)
#       funnelLength <- c(rep(len,len))
#       
#       # Hier werden die zeitdifferenzen zwischen dem aktuellen und dem vorherigen touchpoint berechnet
#       # Kann auch noch andere differenzen berechnen 
#       x <- as.numeric(as.difftime(c(clickTimestamp),format="%Y-%m-%d %H:%M:%S", units = "days"))
#       timeSinceLastDay <-c(as.numeric(NA),x[2:len]-x[1:(len-1)])
#       x2 <- as.numeric(as.difftime(c(clickTimestamp),format="%Y-%m-%d %H:%M:%S", units = "hours"))
#       timeSinceLastHour <- c(as.numeric(NA),x2[2:len]-x2[1:(len-1)])
#       x3 <- as.numeric(as.difftime(c(clickTimestamp),format="%Y-%m-%d %H:%M:%S", units = "mins"))
#       timeSinceLastMinute <- c(as.numeric(NA),x3[2:len]-x3[1:(len-1)])
#       
#       # die gesamte Zeit eines Funnels in Tagen, wenn man x2 oder x3 verwendet kann man die Zeit in Stunden und/oder Minuten haben. 
#       
#       # Funnel duration in Days, Minutes and hours
#       timeSinceFirstDay <- c(rep(0,len))
#       timeSinceFirstHour <- c(rep(0,len))
#       timeSinceFirstMinute <- c(rep(0,len))
#       
#       timeSinceFirstDay <-  c(as.numeric(NA),cumsum(replace(x=timeSinceLastDay,list=c(1),values=0))[-1])
#       timeSinceFirstHour <-  c(as.numeric(NA),cumsum(replace(x=timeSinceLastHour,list=c(1),values=0))[-1])
#       timeSinceFirstMinute <- c(as.numeric(NA),cumsum(replace(x=timeSinceLastMinute,list=c(1),values=0))[-1])
#     }
#     
#     # weekdays
#     weekday <- weekdays(as.POSIXct(clickTimestamp))
#     weekday <- (factor(weekday,levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))
#     levels(weekday) <- c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag")
#     
#     hour  <- hour(as.POSIXct(clickTimestamp))
#     
#     # Has clicked variable and clickCount
#     hasClicked <- c(rep(0,len))
#     clickCount <- c(rep(0,len))
#     
#     if(sum(touchpointType==1)>=1){
#       hasClicked[min(which(touchpointType==1)):len] <- 1
#       clickCount <- cumsum(as.numeric(touchpointType==1))
#     }
#     
#     Transaction <- Last
#     data.table(First,Last,Position,Transaction,funnelLength,timeSinceLastDay
#                ,timeSinceLastHour,timeSinceLastMinute
#                ,timeSinceFirstDay,timeSinceFirstHour,timeSinceFirstMinute,weekday,hour,hasClicked,clickCount)
#   } 
#   
#   fail = foreach(i = 1:8, .packages='data.table', .inorder=FALSE) %dopar% {
#     ############################################
#     ## campaign teil start
#     pathFail <- paste("/zpool1/s10859017/consulting/raw_data/fail_funnel_structure",i,".csv",sep="")
#     system.time(fail <- fread(input=pathFail ,stringsAsFactors=FALSE, na.strings="\\N", sep=",", header=FALSE))
#     setnames(fail, c('id','projectIdFunnelKeyword','clickTimestamp','touchpointType','isLast','isTransaction','creationtime'))
#     
#     maxId <- max(fail$id)
#     system.time(newFeat <- fail[, createFeatures(clickTimestamp, touchpointType, id, maxId), by="id"])
#     newFeat$vid <- c(1 : length(newFeat$id))
#     fail$vid <- c(1 : length(newFeat$id))
#     newFeat = as.data.table(newFeat)
#     fail = as.data.table(fail)
#     
#     setkey(newFeat,vid)
#     setkey(fail,vid)
#     fail <- merge(fail, newFeat, by="vid", suffixes = c("", ".y"))
#     fail[, c("vid","isLast","isTransaction","id.y"):=NULL]
#     
#     ############################################
#     ## funnel teil ende
#     print("fail funnel zu ende")
#     ############################################
#     
#     fail$campaign <- rep(NA, nrow(fail))
#     
#     el2 <- fail$projectIdFunnelKeyword 
#     
#     levelName = c("SEM","Generic","SEO","Direct","Social Media","Newsletter","E-Mailing","Kooperationen","Affiliate","Display","TV","No Match")
#     
#     for(j in 1 : length(levelName)){
#       idx <- is.element(el = el2, set = sets[[j]])
#       fail$campaign[idx] <- levelName[j]
#     }
#     
#     fail
#   }
#   setkey(fail, id)
#   return(fail)
# }
# 
# ###apply function with parallel computing
# #stopCluster(cl)
# cl = makeCluster(8, type = "SOCK")
# registerDoSNOW(cl)
# getDoParWorkers()
# getDoParName()
# fail = parFunc(sets)
# stopCluster(cl)


nrow(subset(fail, funnelLength>250)) # touchpoints mit Laenge groesser 250
length(unique(subset(fail, funnelLength>250)$id)) # success funnels mit Laenge groesser 250
length(unique(subset(fail, funnelLength>300)$id)) #
length(unique(subset(fail, funnelLength>1000)$id)) #

fail = subset(fail, funnelLength<=250) #nach 250 abschneiden

failWithViews = fail
save(failWithViews, file="/zpool1/s10859017/consulting/r_data/failWithViews.RData")

nrow(subset(fail, touchpointType == 2)) # views
fail = subset(fail, touchpointType == 1) #data table, dass nur clicks enthaelt
fail[, c("touchpointType", "hasClicked", "clickCount", "First", "Last", "Position", "Transaction",
         "funnelLength", "timeSinceLastDay", "timeSinceLastHour", "timeSinceLastMinute",
         "timeSinceFirstDay", "timeSinceFirstHour", "timeSinceFirstMinute", "weekday", "hour") := NULL] #nicht mehr brauchbare features loeschen

####features anpassen
maxId <- max(fail$id)
system.time(newFeat <- fail[, createFeaturesClicksOnly(clickTimestamp, id, maxId), by="id"])
newFeat$vid <- c(1:length(newFeat$id))
fail$vid <- c(1:length(newFeat$id))

setkey(newFeat,vid)
setkey(fail,vid)
fail <- merge(fail, newFeat, by="vid", suffixes = c("", ".y"))
fail[, c("vid","id.y") := NULL]

save(fail, file="/zpool1/s10859017/consulting/r_data/fail.RData")
















































