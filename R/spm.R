###################
###succWithViews###
###################
rm(list=ls())

###load packages
library(arules)
library(arulesSequences)
library(data.table)

#load data
load("/zpool1/s10859017/consulting/r_data/succWithViews.RData")
succWithViews = succWithViews[!is.na(succWithViews$campaign), ] #NAs in campaign loeschen
succWithViews$campaign = factor(succWithViews$campaign)
levels(succWithViews$campaign) = c("AffiliatePartnerprogramm", "AffiliateRest", "Direct", 
                          "Display", "EMail", "Generic", "KooperationenFocus", 
                          "KooperationenImmonet", "KooperationenImmoscout24", 
                          "KooperationenImmowelt", "KooperationenRest", "Newsletter", 
                          "SEMBrand", "SEMGenerisch", "SEMRemarketing", "SEO", "SocialMedia")

#save as a text file
succWithViews = succWithViews[Position != 1, ]
succWithViews$count = 1
succWithViews = succWithViews[, c("id", "Position", "count", "campaign"), with=F]
write.table(succWithViews, file = "/zpool1/s10859017/consulting/r_data/succWithViews.txt", quote = F, row.names = F, col.names = F)

#load data in the format that cspade needs
succWithViews = read_baskets(con = "/zpool1/s10859017/consulting/r_data/succWithViews.txt", info = c("sequenceID","eventID","SIZE"))
# as(succWithViews, "data.frame")

#apply cspade algorithm
spadeSuccViews = cspade(succWithViews, parameter = list(support = 0.05), control = list(verbose = TRUE))
# summary(spadeSuccViews)
# as(spadeSuccViews, "data.frame")

#save result
save(spadeSuccViews, file = "/zpool1/s10859017/consulting/r_results/spadeSuccViews.RData")

##########
###succ###
##########
rm(list=ls())
library(arules)
library(arulesSequences)
library(data.table)
load("/zpool1/s10859017/consulting/r_data/succ.RData")
succ = succ[!is.na(succ$campaign), ] #NAs in campaign loeschen
succ$campaign = factor(succ$campaign)
levels(succ$campaign) = c("AffiliatePartnerprogramm", "AffiliateRest", "Direct", 
                                   "Display", "EMail", "Generic", "KooperationenFocus", 
                                   "KooperationenImmonet", "KooperationenImmoscout24", 
                                   "KooperationenImmowelt", "KooperationenRest", "Newsletter", 
                                   "SEMBrand", "SEMGenerisch", "SEMRemarketing", "SEO", "SocialMedia")
succ = succ[Position != 1, ]
succ$count = 1
succ = succ[, c("id", "Position", "count", "campaign"), with=F]
write.table(succ, file = "/zpool1/s10859017/consulting/r_data/succ.txt", quote = F, row.names = F, col.names = F)
succ = read_baskets(con = "/zpool1/s10859017/consulting/r_data/succ.txt", info = c("sequenceID","eventID","SIZE"))
spadeSucc = cspade(succ, parameter = list(support = 0.05), control = list(verbose = TRUE))
save(spadeSucc, file = "/zpool1/s10859017/consulting/r_results/spadeSucc.RData")

##########
###fail###
##########
rm(list=ls())
library(arules)
library(arulesSequences)
library(data.table)
load("/zpool1/s10859017/consulting/r_data/fail.RData")
fail = fail[!is.na(fail$campaign), ] #NAs in campaign loeschen
fail$campaign = factor(fail$campaign)
levels(fail$campaign) = c("AffiliatePartnerprogramm", "AffiliateRest", "Direct", 
                          "Display", "EMail", "Generic", "KooperationenFocus", 
                          "KooperationenImmonet", "KooperationenImmoscout24", 
                          "KooperationenImmowelt", "KooperationenRest", "Newsletter", 
                          "SEMBrand", "SEMGenerisch", "SEMRemarketing", "SEO", "SocialMedia")
fail = fail[Position != 1, ]
fail$count = 1
fail = fail[, c("id", "Position", "count", "campaign"), with=F]
write.table(fail, file = "/zpool1/s10859017/consulting/r_data/fail.txt", quote = F, row.names = F, col.names = F)
fail = read_baskets(con = "/zpool1/s10859017/consulting/r_data/fail.txt", info = c("sequenceID","eventID","SIZE"))
spadeFail = cspade(fail, parameter = list(support = 0.05), control = list(verbose = TRUE))
save(spadeFail, file = "/zpool1/s10859017/consulting/r_results/spadeFail.RData")

###look at results
load("Z:/consulting/r_results/spadeSuccViews.RData")
summary(spadeSuccViews)
as(spadeSuccViews, "data.frame")[order(as(spadeSuccViews, "data.frame")$support, decreasing = T), ]

load("Z:/consulting/r_results/spadeSucc.RData")
summary(spadeSucc)
as(spadeSucc, "data.frame")[order(as(spadeSucc, "data.frame")$support, decreasing = T), ]

load("Z:/consulting/r_results/spadeFail.RData")
summary(spadeFail)
as(spadeFail, "data.frame")[order(as(spadeFail, "data.frame")$support, decreasing = T), ]
