#######
####### XGBoosting with cross validation
#######

library(mlbench)
library(caret)
library(caTools)
library(caret)
library(klaR)
library("readxl")

pathRead <- "C:/Users/markos/Desktop/Markos Viggiato/DCC - Mestrado/Disciplinas 2018.1/Machine Learning/Projeto Final/notebook/new-labeled-with-maintenance-activities.xlsx"
mydata <- readxl::read_xlsx(pathRead)
mydata<- mydata[-(1:3)]
mydata <- mydata[complete.cases(mydata),]
#mydata[1:nrow(mydata),2:ncol(mydata)] <- as.numeric(as.matrix(mydata[1:nrow(mydata),2:ncol(mydata)]))
mydata <- as.data.frame(mydata)
mydata[,-1] <- as.data.frame(sapply(mydata[,-1], as.numeric)) #<- sapply is here
mydata$label <- as.factor(mydata$label)

ind = createDataPartition(mydata$label, p = 0.85, list = FALSE)
trainDF<-mydata[ind,]
testDF<-mydata[-ind,]

ControlParamteres <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats=5,
                                  savePredictions = TRUE,
                                  classProbs = TRUE
)

parametersGrid <-  expand.grid(eta = 0.1, 
                               colsample_bytree=c(0.5,0.7),
                               max_depth=c(3,6),
                               nrounds=100,
                               gamma=1,
                               min_child_weight=2,
                               subsample = 1
)

parametersGrid

modelxgboost <- train(label~., 
                      data = trainDF,
                      method = "xgbTree",
                      trControl = ControlParamteres,
                      tuneGrid=parametersGrid,
                      na.action = na.pass)

modelxgboost
#modelxgboost$xlevels[["additions"]] <- union(modelxgboost$xlevels[["additions"]], levels(testDF$additions))
predictions<-predict(modelxgboost,testDF, na.action = na.pass)
t<-table(predictions=predictions,actual=testDF$label)
acc <- (t[1,1] + t[2,2] + t[3,3])/(sum(t))
