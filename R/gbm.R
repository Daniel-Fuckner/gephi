rm(list=ls())

#load packages
library(data.table)
library(gbm)
library(foreach)
library(doSNOW)

#load data
load("/zpool1/s10859017/consulting/r_data/succ.RData")
load("/zpool1/s10859017/consulting/r_data/fail.RData")
fail$Transaction = 0

data = rbind(succ, fail)
#data = replace(data, is.na(data), 0) # alle NAs auf 0 setzen
rm(fail, succ)

data[, c("projectIdFunnelKeyword", "clickTimestamp", "creationtime", "timeSinceLastDay", "timeSinceLastHour", 
         "timeSinceFirstDay", "timeSinceFirstHour") := NULL]

###function
gbmFunction = function(data){
  trainIdx = foreach(i = 1:12, .packages=c("gbm", "data.table"), .inorder=TRUE) %dopar% {
    data1 = data[Position==i]
    set.seed(i) #results reproducable (at least on 64-bit computers)
    sample(1 : nrow(data1), (0.5 * nrow(data1)))
  }
  model = foreach(i = 1:12, .packages=c("gbm", "data.table"), .inorder=TRUE) %dopar% {
    data1 = data[Position==i]
    dataTrain = data1[trainIdx[[(i-4)]], ]
    if(i == 1){
      gbm(Transaction ~ factor(campaign) + weekday + hour, # formula
          data = dataTrain, # dataset
          #weights = w,
          #var.monotone = c(0), # -1: monotone decrease, +1: monotone increase, 0: no monotone restrictions
          distribution = "bernoulli",
          n.trees = 1000, # number of trees
          shrinkage = 0.001, # shrinkage or learning rate, 0.001 to 0.1 usually work
          interaction.depth = 1, # 1: additive model, 2: two-way interactions, etc
          bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
          train.fraction = 1, # fraction of data for training, first train.fraction*N used for training
          cv.folds = 0, # do 5-fold cross-validation
          n.minobsinnode = 5, # minimum total weight needed in each node
          keep.data = TRUE,
          verbose = FALSE) # don't print progress
    }else{
      gbm(Transaction ~ factor(campaign) + timeSinceLastMinute + timeSinceFirstMinute + weekday + hour, # formula
          data = dataTrain, # dataset
          #weights = w,
          #var.monotone = c(0), # -1: monotone decrease, +1: monotone increase, 0: no monotone restrictions
          distribution = "bernoulli",
          n.trees = 1000, # number of trees
          shrinkage = 0.005, # shrinkage or learning rate, 0.001 to 0.1 usually work
          interaction.depth = 1, # 1: additive model, 2: two-way interactions, etc
          bag.fraction = 0.5, # subsampling fraction, 0.5 is probably best
          train.fraction = 1, # fraction of data for training, first train.fraction*N used for training
          cv.folds = 0, # do 5-fold cross-validation
          n.minobsinnode = 5, # minimum total weight needed in each node
          keep.data = TRUE,
          verbose = FALSE) # don't print progress
    }
  }
  pred = foreach(i = 1:12, .packages=c("gbm", "data.table"), .inorder=TRUE) %dopar% {
    data1 = data[Position==i]
    dataTest = data1[-trainIdx[[(i-4)]], ]
    cbind(id = dataTest$id, Position = dataTest$Position, Transaction = dataTest$Transaction,
          p = predict(model[[(i-4)]], newdata = dataTest, n.trees = 1000, type = "response"))
  }
  return(list(trainIdx, model, pred))
}

###apply function with parallel computing
#stopCluster(cl)
cl = makeCluster(12, type = "SOCK")
registerDoSNOW(cl)
getDoParWorkers()
getDoParName()
res = gbmFunction(data)
save(res, file = "/zpool1/s10859017/consulting/r_results/gbm1_12.RData")
stopCluster(cl)



