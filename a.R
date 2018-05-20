library("quantmod")
library("lubridate")
library("e1071")
library("ROCR")
library("pROC")
library("caret")
startDate = as.Date("2012-01-01")
endDate = as.Date("2014-01-01")
getSymbols("AAPL", src = "yahoo", from = startDate, to = endDate)
DayofWeek<-wday(AAPL, label=TRUE)
PriceChange<- Cl(AAPL) - Op(AAPL)
Class<-ifelse(PriceChange>0,"UP","DOWN")
DataSet<-data.frame(DayofWeek,Class)
MyModel<-naiveBayes(DataSet[,1],DataSet[,2])

EMA5<-EMA(Op(AAPL),n = 5)
EMA10<-EMA(Op(AAPL),n = 10)
EMACross <- EMA5 - EMA10
EMACross<-round(EMACross,2)
DataSet2<-data.frame(DayofWeek,EMACross, Class)
DataSet2<-DataSet2[-c(1:10),]
TrainingSet<-DataSet2[1:328,]
TestSet<-DataSet2[329:492,] 
EMACrossModel<-naiveBayes(TrainingSet[,1:2],TrainingSet[,3])
X<-predict(EMACrossModel,TestSet)
predvec<-ifelse(X=="UP", 1, 0)
realvec<-ifelse(TestSet[,3]=="UP", 1, 0)
pr <- prediction(predvec, realvec)
prf <- performance(pr, "prec", "rec")
auc.tmp <- performance(pr,"auc");
result <- confusionMatrix(predvec,realvec)
auc <- as.numeric(auc.tmp@y.values)

