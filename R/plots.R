rm(list=ls())

#load packages
library(ggplot2)
library(scales)
library(data.table)
library(plyr)
library(reshape)

#load data
load("/zpool1/s10859017/consulting/r_data/succWithViews.RData")
load("/zpool1/s10859017/consulting/r_data/succ.RData")

succWithViews = succWithViews[, c("touchpointType", "hasClicked", "clickCount") := NULL]
data = rbind(cbind(succWithViews, Views = "Ja"), cbind(succ, Views = "Nein"), use.names = TRUE)

pdf(file="/zpool1/s10859017/consulting/r_results/plotsSuccWithViewsAndSucc.pdf")

#timeSinceFirst: nur mit Last=1 => Dauer vom ersten bis letzten
p1 = ddply(data[Last == 1], .(Views), summarise, prop=prop.table(table(timeSinceFirstDay)), timeSinceFirstDay=names(table(timeSinceFirstDay)))
p1$timeSinceFirstDay = floor(as.numeric(p1$timeSinceFirstDay))
p1 = aggregate(prop ~ Views + timeSinceFirstDay, data = p1, FUN = sum)
p2 = p1[p1$timeSinceFirstDay == 0, ]
p2$prop = round(p2$prop, 2)
ggplot(p1, aes(x = timeSinceFirstDay, y = prop, fill = Views)) +
  geom_bar(stat="identity", position='dodge') + 
  scale_fill_discrete(name="Views?", labels=c("ja", "nein")) +
  labs(x = "Beobachtungsdauer in Tagen", y = "Relative Haeufigkeit") +
  facet_wrap(~Views, nrow=2, ncol=1) +
  scale_x_continuous(limits = c(0,50), oob=squish) + 
  scale_y_continuous(limits = c(0,.05), oob=squish) +
  geom_text(data = p2, aes(x=5, y=.045, label=prop))
  
#timeSinceLast: Frequenz der Kontaktpunkte
p1 = ddply(data, .(Views), summarise, prop=prop.table(table(timeSinceLastDay)), timeSinceLastDay=names(table(timeSinceLastDay)))
p1$timeSinceLastDay = floor(as.numeric(p1$timeSinceLastDay))
p1 = aggregate(prop ~ Views + timeSinceLastDay, data = p1, FUN = sum)
p2 = p1[p1$timeSinceLastDay == 0, ]
p2$prop = round(p2$prop, 2)
ggplot(p1, aes(x = timeSinceLastDay, y = prop, fill = Views)) +
  geom_bar(stat="identity", position='dodge') + 
  scale_fill_discrete(name="Views?", labels=c("ja", "nein")) +
  labs(x = "Dauer zwischen Kontaktpunkten in Tagen", y = "Relative Haeufigkeit") + 
  facet_wrap(~Views, nrow=2, ncol=1) +
  scale_y_continuous(limits = c(0,.25), oob=squish) +
  scale_x_continuous(limits = c(0,50), oob=squish) +
  geom_text(data = p2, aes(x=5, y=.225, label=prop))

#hour
p1 = ddply(data, .(Views), summarise, prop=prop.table(table(hour)), hour=names(table(hour)))
p1$hour = as.integer(p1$hour)
ggplot(p1, aes(x = hour, y = prop, fill = Views)) +
  geom_bar(stat="identity", position='dodge') + 
  scale_fill_discrete(name="Views?", labels=c("ja", "nein")) +
  labs(x = "Uhrzeit", y = "Relative Haeufigkeit") +
  facet_wrap(~Views, nrow=2, ncol=1)

#weekday
p1 = ddply(data, .(Views), summarise, prop=prop.table(table(weekday)), weekday=names(table(weekday)))
p1$weekday <- (factor(p1$weekday, levels=c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag")))
ggplot(p1, aes(x = weekday, y = prop, fill = Views)) +
  geom_bar(stat="identity", position='dodge') + 
  labs(x = "Wochentag", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Views?", labels=c("ja", "nein")) +
  facet_wrap(~Views, nrow=2, ncol=1)

#touchPointType
load("/zpool1/s10859017/consulting/r_data/succWithViews.RData")
p = ggplot(data = succWithViews, mapping = aes(x = factor(touchpointType)))
p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge") +
  labs(x = "Art des Kontakts", y = "Relative Haeufigkeit") +
  scale_x_discrete(labels=c("Click", "View"))

#touchPointType with First=1 only
p = ggplot(data = succWithViews[First==1], mapping = aes(x = factor(touchpointType)))
p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge") +
  labs(x = "Art des ersten Kontakts", y = "Relative Haeufigkeit") +
  scale_x_discrete(labels=c("Click", "View"))

#touchPointType with Last=1 only
p = ggplot(data = succWithViews[Last==1], mapping = aes(x = factor(touchpointType)))
p + geom_bar(aes(y = (..count..)/sum(..count..)), stat = "bin", position = "dodge") +
  labs(x = "Art des letzten Kontakts", y = "Relative Haeufigkeit") +
  scale_x_discrete(labels=c("Click", "View"))

#clickCount:
clickCountMean = succWithViews[, mean(clickCount), by="Position"]

p = ggplot(data = clickCountMean, mapping = aes(x = Position, y = V1))
p + geom_line() +
  labs(x = "Position", y = "Haeufigkeit der Clicks im Mittel") +
  geom_abline(aes(intercept=0, slope=1), color = "red") +
  scale_y_continuous(limits = c(0,250), oob=squish) +
  scale_x_continuous(limits = c(0,250), oob=squish)

#hasClicked:
hasClickedProb = succWithViews[, mean(hasClicked), by="Position"]

p = ggplot(data = hasClickedProb, mapping = aes(x = Position, y = V1))
p + geom_line() +
  labs(x = "Position", y = "Anteil mit mindestens einem Click") +
  scale_y_continuous(limits = c(0,1), oob=squish)

#funnel length
p1 = ddply(data[First==1], .(Views), summarise, prop=prop.table(table(funnelLength)), funnelLength=names(table(funnelLength)))
p1$funnelLength = as.integer(p1$funnelLength)
p2 = p1[p1$funnelLength == 1, ]
p2$prop = round(p2$prop, 2)
p2[1,2] = "" #fuer Views=ja ist keine Anzeige noetig, da Wert <= ylim=.25
ggplot(p1, aes(x = funnelLength, y = prop, fill = Views)) +
  geom_bar(stat="identity", position='dodge') + 
  labs(x = "Funnel Length", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Views?", labels=c("ja", "nein")) +
  facet_wrap(~Views, nrow=2, ncol=1) +
  scale_y_continuous(limits = c(0,.25), oob=squish) +
  scale_x_continuous(limits = c(0,20), oob=squish) +
  geom_text(data = p2, aes(x=5, y=.225, label=prop))

#campaign
p1 = ddply(data, .(Views), summarise, prop=prop.table(table(campaign)), campaign=names(table(campaign)))
p2 = p1[p1$campaign == "Display", ]
p2$prop = round(p2$prop, 2)
p2[2,2] = ""
ggplot(p1, aes(x = campaign, y = prop, fill = Views)) +
  geom_bar(stat="identity", position='dodge') + 
  labs(x = "Kampagnen", y = "Relative Haeufigkeit") +
  coord_flip() +
  scale_fill_discrete(name="Views?", labels=c("ja", "nein")) +
  facet_wrap(~Views, nrow=2, ncol=1) +
  scale_y_continuous(limits = c(0,.4), oob=squish) +
  geom_text(data = p2, aes(x=5, y=.35, label=prop))
  
dev.off()
rm(list=ls())

#load data
load("/zpool1/s10859017/consulting/r_data/succ.RData")
load("/zpool1/s10859017/consulting/r_data/fail.RData")

data = rbind(cbind(succ, success = "Ja"), cbind(fail, success = "Nein"))
rm(succ, fail)

pdf(file="/zpool1/s10859017/consulting/r_results/plotsSuccAndFail.pdf")

#timeSinceFirst: nur mit Last=1 => Dauer vom ersten bis letzten
p1 = ddply(data[Last == 1], .(success), summarise, prop=prop.table(table(timeSinceFirstDay)), timeSinceFirstDay=names(table(timeSinceFirstDay)))
p1$timeSinceFirstDay = floor(as.numeric(p1$timeSinceFirstDay))
p1 = aggregate(prop ~ success + timeSinceFirstDay, data = p1, FUN = sum)
p2 = p1[p1$timeSinceFirstDay == 0, ]
p2$prop = round(p2$prop, 2)
ggplot(p1, aes(x = timeSinceFirstDay, y = prop, fill = success)) +
  geom_bar(stat="identity", position='dodge') + 
  labs(x = "Beobachtungsdauer in Tagen", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  facet_wrap(~success, nrow=2, ncol=1) +
  scale_y_continuous(limits = c(0,.05), oob=squish) + 
  scale_x_continuous(limits = c(0,50), oob=squish) +
  geom_text(data = p2, aes(x=5, y=.045, label=prop))

#timeSinceLast: Frequenz der Kontaktpunkte
p1 = ddply(data, .(success), summarise, prop=prop.table(table(timeSinceLastDay)), timeSinceLastDay=names(table(timeSinceLastDay)))
p1$timeSinceLastDay = floor(as.numeric(p1$timeSinceLastDay))
p1 = aggregate(prop ~ success + timeSinceLastDay, data = p1, FUN = sum)
p2 = p1[p1$timeSinceLastDay == 0, ]
p2$prop = round(p2$prop, 2)
ggplot(p1, aes(x = timeSinceLastDay, y = prop, fill = success)) +
  geom_bar(stat="identity", position='dodge') + 
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  labs(x = "Dauer zwischen Kontaktpunkten in Tagen", y = "Relative Haeufigkeit") + 
  facet_wrap(~success, nrow=2, ncol=1) +
  scale_y_continuous(limits = c(0,.1), oob=squish) + 
  scale_x_continuous(limits = c(0,50), oob=squish) +
  geom_text(data = p2, aes(x=5, y=.09, label=prop))

#hour
p1 = ddply(data, .(success), summarise, prop=prop.table(table(hour)), hour=names(table(hour)))
p1$hour = as.integer(p1$hour)
ggplot(p1, aes(x = hour, y = prop, fill = success)) +
  geom_bar(stat="identity", position='dodge') + 
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  labs(x = "Uhrzeit", y = "Relative Haeufigkeit") +
  facet_wrap(~success, nrow=2, ncol=1)

#weekday
p1 = ddply(data, .(success), summarise, prop=prop.table(table(weekday)), weekday=names(table(weekday)))
p1$weekday <- (factor(p1$weekday, levels=c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag")))
ggplot(p1, aes(x = weekday, y = prop, fill = success)) +
  geom_bar(stat="identity", position='dodge') + 
  labs(x = "Wochentag", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  facet_wrap(~success, nrow=2, ncol=1)

#funnel length
p1 = ddply(data[First==1], .(success), summarise, prop=prop.table(table(funnelLength)), funnelLength=names(table(funnelLength)))
p1$funnelLength = as.integer(p1$funnelLength)
p2 = p1[p1$funnelLength == 1, ]
p2$prop = round(p2$prop, 2)
ggplot(p1, aes(x = funnelLength, y = prop, fill = success)) +
  geom_bar(stat="identity", position='dodge') + 
  labs(x = "Funnel Length", y = "Relative Haeufigkeit") +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  facet_wrap(~success, nrow=2, ncol=1) +
  scale_y_continuous(limits = c(0,.3), oob=squish) +
  scale_x_continuous(limits = c(0,20), oob=squish) +
  geom_text(data = p2, aes(x=5, y=.275, label=prop))

#campaign
p1 = ddply(data, .(success), summarise, prop=prop.table(table(campaign)), campaign=names(table(campaign)))
ggplot(p1, aes(x = campaign, y = prop, fill = success)) +
  geom_bar(stat="identity", position='dodge') + 
  labs(x = "Kampagnen", y = "Relative Haeufigkeit") +
  coord_flip() +
  scale_fill_discrete(name="Konvertiert?", labels=c("ja", "nein")) +
  facet_wrap(~success, nrow=2, ncol=1) 
  #scale_y_continuous(limits = c(0,.1), oob=squish) +

dev.off()
