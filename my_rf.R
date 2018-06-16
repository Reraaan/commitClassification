#######
####### Random Forest with cross validation
#######
# author: Markos Viggiato

library(mlbench)
library(caret)
library(caTools)
library(caret)
library(klaR)
library("readxl")

pathRead <- "C:/Users/markos/Desktop/Markos Viggiato/DCC - Mestrado/Disciplinas 2018.1/Machine Learning/Projeto Final/notebook/new-labeled-with-maintenance-activities.xlsx"
pathRead2 <- "C:/Users/markos/Desktop/Markos Viggiato/DCC - Mestrado/Disciplinas 2018.1/Machine Learning/Projeto Final/notebook/1151-labeled-commits.xlsx"

#mydata2 <- read.csv(pathRead2, quote = "", header = TRUE, sep = "#")

mydata2 <- readxl::read_xlsx(pathRead2)

mydata2<- mydata2[-(1:3)]
#mydata <- mydata[complete.cases(mydata),]
#mydata[1:nrow(mydata),2:ncol(mydata)] <- as.numeric(as.matrix(mydata[1:nrow(mydata),2:ncol(mydata)]))
mydata2 <- as.data.frame(mydata2)
mydata2[,-1] <- as.data.frame(sapply(mydata2[,-1], as.numeric)) #<- sapply is here
mydata2$label <- as.factor(mydata2$label)

ind = createDataPartition(mydata2$label, p = 0.85, list = FALSE)
trainDF<-mydata2[ind,]
testDF<-mydata2[-ind,]

ControlParamteres <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats=5,
                                  savePredictions = TRUE,
                                  classProbs = TRUE)

rf_model<-train(label~.,
                data=trainDF,
                method="rf",
                trControl= ControlParamteres,
                prox=TRUE,allowParallel=TRUE)

print(rf_model)

print(rf_model$finalModel)


predictions<-predict(rf_model,testDF)
t<-table(predictions=predictions,actual=testDF$label)
acc <- (t[1,1] + t[2,2] + t[3,3])/(sum(t))

plot(modelxgboost)



