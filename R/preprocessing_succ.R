rm(list = ls())

library(data.table)

###support function
source("/zpool1/s10859017/consulting/scripts/createFeatures.R")
source("/zpool1/s10859017/consulting/scripts/createFeaturesClicksOnly.R")
source("/zpool1/s10859017/consulting/scripts/createPosition.R")

load(file = "/zpool1/s10859017/consulting/r_data/sets.RData")
levelName = sets[[2]]
sets = sets[[1]]

pathSucc <- "/zpool1/s10859017/consulting/raw_data/success_funnel_structure.csv"
succ <- fread(input=pathSucc, stringsAsFactors=FALSE, na.strings="\\N", sep=",")
    
maxId <- max(succ$id)
system.time(newFeat <- succ[, createFeatures(clickTimestamp, touchpointType, id, maxId), by="id"])
newFeat$vid <- c(1:length(newFeat$id))
succ$vid <- c(1:length(newFeat$id))

setkey(newFeat,vid)
setkey(succ,vid)
succ <- merge(succ, newFeat, by="vid", suffixes = c("", ".y"))
succ[, c("vid","isLast","isTransaction","id.y") := NULL]

succ$campaign <- rep(NA, nrow(succ))

el2 <- succ$projectIdFunnelKeyword 
    
for(j in 1 : length(levelName)){
  idx <- is.element(el = el2, set = sets[[j]])
  succ$campaign[idx] <- levelName[j]
}


nrow(subset(succ, funnelLength>250)) #71423 touchpoints mit Laenge groesser 250
length(unique(subset(succ, funnelLength>250)$id)) #189 success funnels mit Laenge groesser 250
length(unique(subset(succ, funnelLength>300)$id)) #106
length(unique(subset(succ, funnelLength>1000)$id)) #4

succ = subset(succ, funnelLength<=250) #nach 250 abschneiden

succWithViews = succ
save(succWithViews, file="/zpool1/s10859017/consulting/r_data/succWithViews.RData")

nrow(subset(succ, touchpointType == 2)) #1429336 views
succ = subset(succ, touchpointType == 1) #data table, dass nur clicks enthaelt
succ[, c("touchpointType", "hasClicked", "clickCount", "First", "Last", "Position", "Transaction",
        "funnelLength", "timeSinceLastDay", "timeSinceLastHour", "timeSinceLastMinute",
        "timeSinceFirstDay", "timeSinceFirstHour", "timeSinceFirstMinute", "weekday", "hour") := NULL] #nicht mehr brauchbare features loeschen

####features anpassen
maxId <- max(succ$id)
system.time(newFeat <- succ[, createFeaturesClicksOnly(clickTimestamp, id, maxId), by="id"])
newFeat$vid <- c(1:length(newFeat$id))
succ$vid <- c(1:length(newFeat$id))

setkey(newFeat,vid)
setkey(succ,vid)
succ <- merge(succ, newFeat, by="vid", suffixes = c("", ".y"))
succ[, c("vid","id.y") := NULL]

save(succ, file="/zpool1/s10859017/consulting/r_data/succ.RData")



