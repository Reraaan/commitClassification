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
myseed = set.seed(7)
ControlParamteres <- trainControl(method = "repeatedcv",
                                  number = 10,
                                  repeats=5,
                                  savePredictions = TRUE,
                                  classProbs = TRUE, seeds = myseed)


#featurePlot(x=mydata2[,2:10], y=mydata2[,1], plot="box", scales=list(x=list(relation="free"), y=list(relation="free")), auto.key=list(columns=3), outline=FALSE)

# best mtry = 36 - acc = 76%
#mtry <- sqrt(ncol(mydata2)-1)
#mytunegrid <- expand.grid(mtry=4)

tunegrid <- expand.grid(mtry=c(1:15))

rf_model<-train(label~.,
                data=trainDF,
                method="rf",
                trControl= ControlParamteres,
                tunegrid=mytunegrid)

print(rf_model)
print(rf_model$finalModel)
plot(rf_model)


predictions<-predict(rf_model,testDF)
t<-table(predictions=predictions,actual=testDF$label)
acc <- (t[1,1] + t[2,2] + t[3,3])/(sum(t))



# Algorithm Tune (tuneRF)
bestmtry <- randomForest::tuneRF(mydata2[,-1], mydata2$label, stepFactor=0.15, improve=1e-5, ntree=500, plot = TRUE)
print(bestmtry)




