#Final Project Script
set.seed(2000)
train<-sample(nrow(LSsliders), 0.75*nrow(LSsliders))
test<-(-train)
fit.log<-glm(type~.,data=LSsliders[train,],family = binomial())
summary(fit.log)


prob<-predict(fit.log, type='response',LSsliders[test,])
high.pred<-factor(prob>.5, levels=c(TRUE,FALSE), labels=c('strike','ball'))
high.pred
sum.table<-table(LSsliders[test,]$type,high.pred,dnn=c('Actual','Predicted'))
sum.table
accuracy<-(123+48)/245; accuracy
error.rate<-(1-accuracy); error.rate


set.seed(2000)
mytree<-rpart(type~.,data=LSsliders[train,],minsplit = 3, minbucket = 2, method='class')
fancyRpartPlot(mytree)
mytree$variable.importance

printcp(mytree)
plotcp(mytree)
reduced.tree<-prune(mytree, cp=.016731)
fancyRpartPlot(reduced.tree)

#reduced model
fit.red<-glm(type~release_speed+stand+balls+vy0+sz_top+release_spin_rate, data=LSsliders[train,],family=binomial())
summary(fit.red)

prob.red<-predict(fit.red, type='response',LSsliders[test,])
high.pred.red<-factor(prob.red>.5, levels=c(TRUE,FALSE), labels=c('strike','ball'))
high.pred.red
sum.table.red<-table(LSsliders[test,]$type,high.pred.red,dnn=c('Actual','Predicted'))
sum.table.red
accuracy.red<-(129+33)/245; accuracy.red
error.rate.red<-(1-accuracy.red); error.rate.red

set.seed(2000)
mytree.red<-rpart(type~release_speed+stand+balls+vy0+sz_top+release_spin_rate,data=LSsliders[train,],minsplit = 3, minbucket=2,method='class')
fancyRpartPlot(mytree.red)
mytree.red$variable.importance

printcp(mytree.red)
reduced.tree.red<-prune(mytree.red, cp=.03088)
fancyRpartPlot(reduced.tree.red)


#Fit a model without the location of the pitch at the plate
fit.log2<-glm(type~.-plate_x-plate_z,data=LSsliders[train,],family = binomial())
summary(fit.log2)

prob2<-predict(fit.log2, type='response',LSsliders[test,])
high.pred2<-factor(prob2>.5, levels=c(TRUE,FALSE), labels=c('strike','ball'))
high.pred2
sum.table2<-table(LSsliders[test,]$type,high.pred2,dnn=c('Actual','Predicted'))
sum.table2
accuracy2<-(122+49)/245; accuracy2
error.rate2<-(1-accuracy); error.rate2

set.seed(2000)
mytree2<-rpart(type~.-plate_x-plate_z,data=LSsliders[train,],minsplit = 3, minbucket = 2, method='class')
fancyRpartPlot(mytree2)
mytree2$variable.importance

printcp(mytree2)
plotcp(mytree2)
reduced.tree2<-prune(mytree2, cp=.046332)
fancyRpartPlot(reduced.tree2)

#reduced model without location of pitch at plate
fit.log2.red<-glm(type~release_speed+stand+balls+vx0+vy0+sz_top+release_spin_rate,data=LSsliders[train,],family=binomial())
summary(fit.log2.red)

prob2.red<-predict(fit.log2.red, type='response', LSsliders[test,])
high.pred2.red<-factor(prob2.red>.5, levels=c(TRUE,FALSE), labels=c('strike','ball'))
high.pred2.red
sum.table2.red<-table(LSsliders[test,]$type,high.pred2.red,dnn=c('Actual','Predicted'))
sum.table2.red
accuracy2.red<-(129+35)/245; accuracy2.red
error.rate2.red<-(1-accuracy); error.rate2.red

set.seed(2000)
mytree2.red<-rpart(type~release_speed+stand+balls+vx0+vy0+sz_top+release_spin_rate ,data=LSsliders[train,],minsplit = 3, minbucket = 2, method='class')
fancyRpartPlot(mytree2.red)
mytree2.red$variable.importance

printcp(mytree2.red)
plotcp(mytree2.red)
reduced.tree2.red<-prune(mytree2.red, cp=.011583)
fancyRpartPlot(reduced.tree2.red)


#Random Forest
set.seed(2000)
LSsliders$stand<-as.factor(LSsliders$stand)
rf.sliders<-randomForest(type~., data=LSsliders, subset=train, mtry=4, importance=TRUE)
print(rf.sliders)


