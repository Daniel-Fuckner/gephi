rm(list=ls())

#load packages
library(data.table)
library(gbm)
library(ggplot2)
library(scales)
library(reshape)

#load results
# load("C:/Users/Markus/Desktop/plotsData/gbm5_8.RData")
load("Z:/consulting/r_results/gbm5_8.RData")
trainIdx = res[[1]]
model = res[[2]]
pred = res[[3]]

res1 = res[[1]]
res2 = res[[2]]
res3 = res[[3]]
res4 = res[[4]]

model = res[[2]]
model1 = model[[4]]


# ?gbm.object
sum(res1$train.error < res1$valid.error) # = n.trees, as expected, but:
sum(res2$train.error < res2$valid.error) # = 0

# ?pretty.gbm.tree
pretty.gbm.tree(res2, 1000)

# ?gbm.perf

# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
bestIter = gbm.perf(model1, method = "OOB")
print(bestIter)

# check performance using a 50% heldout test set
best.iter <- gbm.perf(res2, method="test")
print(best.iter)

# check performance using 5-fold cross-validation
best.iter <- gbm.perf(res2,method="cv")
print(best.iter)

# plot the performance # plot variable influence
summary.gbm(model1, n.trees=1)         # based on the first tree
summary.gbm(model1, n.trees=bestIter) # based on the estimated best number of trees

# compactly print the first and last trees for curiosity
print(pretty.gbm.tree(res2,1))
print(pretty.gbm.tree(res2,res2$n.trees))


plot.gbm(res2, 3)

#########
###ROC###
#########
pdf(file="/zpool1/s10859017/consulting/r_results/roc.pdf")
pred = res[[3]]
for(j in 1 : length(pred)){ # for each Position
  pred1 = as.data.table(pred[[j]])
  grid = seq(min(pred1$p), max(pred1$p), length.out = 1000) #Gitter von min vorhergesagter Wkeit und max Wkeit
  roc = data.table(sensi = numeric(1000), spezi = numeric(1000))
  for(i in 1:1000){
    p1 = as.numeric(pred1$p > grid[i]) #wenn groesser, dann wird als Transaktion vorhergesagt
    table = as.data.table(cbind(y = pred1$Transaction, prediction = p1))
    table1 = table[table$y == 1, ]
    table0 = table[table$y == 0, ]
    roc$sensi[i] = sum(table1$prediction == 1) / nrow(table1) #TPF
    roc$spezi[i] = sum(table0$prediction == 0) / nrow(table0) #TNF
  }
  ggplot(roc, aes(x = (1 - spezi), y = sensi)) +
    geom_line() +
    geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1), color = "red") +
    labs(x = "1 - Spezifitaet", y = "Sensitivitaet", title = paste("Position", j, sep = " ")) +
    scale_x_continuous(limits = c(0,1), oob=squish) + 
    scale_y_continuous(limits = c(0,1), oob=squish)
}
dev.off()



