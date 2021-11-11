install.packages("Tree")
library(tree)
View(Admission_Predict)
set.seed(1)
#Train and test set
train = sample(1:nrow(Admission_Predict),nrow(Admission_Predict)/2)
tree.AP = tree(Chance.of.Admit ~.,Admission_Predict,subset = train)


#Variables and nodes
summary(tree.AP)

#Plotting of Tree
plot(tree.AP)
text(tree.AP,pretty = 0)

#Prunning of Tree (WE ARE LOOKING FOR THE LOWEST NODES)
cv.AP = cv.tree(tree.AP)
plot(cv.AP$size,cv.AP$dev,type = "b")
#4 to 6 nodes is best


par(mfrow=c(3,2))
#Prunned Tree Plot
for (i in 4:6){
prune.AP = prune.tree(tree.AP,best = i)
plot(prune.AP)
text(prune.AP,pretty = 0)

yhat = predict(prune.AP,newdata = Admission_Predict[-train,])
AP.test = Admission_Predict[-train,"Chance.of.Admit"]
plot(yhat,AP.test)
abline(0,1)

#Prunned MSE
prunnedMean = mean((yhat - AP.test)^2)
print(prunnedMean)
}


# MSE
yhat = predict(tree.AP,newdata = Admission_Predict[-train,])
AP.test = Admission_Predict[-train,"Chance.of.Admit"]
plot(yhat,AP.test)
abline(0,1)
mean((yhat - AP.test)^2)


