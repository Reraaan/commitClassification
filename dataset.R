library("jsonlite")
library("plyr")
library("readxl")
library(tibble)

pathRead <- "C:/Users/markos/Desktop/Markos Viggiato/DCC - Mestrado/Disciplinas 2018.1/Machine Learning/Projeto Final/notebook/1151-labeled-commits-raw.csv"
#mydata <- read.csv2(pathRead, stringsAsFactors = FALSE)
mydata <- read.csv(pathRead, quote = "", header = TRUE, sep = "#", stringsAsFactors = FALSE)
mydata <- add_column(mydata, additions = array("", dim=nrow(mydata)), .after = 4)
mydata <- add_column(mydata, deletions = array("", dim=nrow(mydata)), .after = 5)

for (i in 1:nrow(mydata)){
    commitSHA <- mydata[[i,1]]
    
    switch(mydata[[i,2]],
           "ReactiveX-RxJava" = {fullname = "ReactiveX/RxJava"},
           "hbase" = {fullname = "apache/hbase"},
           "elasticsearch" = {fullname = "elastic/elasticsearch"},
           "intellij-community" = {fullname = "JetBrains/intellij-community"},
           "hadoop" = {fullname = "apache/hadoop"},
           "drools" = {fullname = "kiegroup/drools"},
           "kotlin" = {fullname = "JetBrains/kotlin"},
           "restlet-framework-java" = {fullname = "restlet/restlet-framework-java"},
           "orientdb" = {fullname = "orientechnologies/orientdb"},
           "camel" = {fullname = "apache/camel"},
           "spring-framework" = {fullname = "spring-projects/spring-framework"} )
       
    commits <- fromJSON(paste0("https://api.github.com/repos/", fullname,"/commits/",commitSHA,"?access_token=e785ba8c5eb78b241528a5e7c0199b9e8c40a72b"))
    addition <- commits$stats$additions
    deletion <- commits$stats$deletions
    mydata[[i,5]] <- addition
    mydata[[i,6]] <- deletion

}
xlsx::write.xlsx(mydata, "C:/Users/markos/Desktop/Markos Viggiato/DCC - Mestrado/Disciplinas 2018.1/Machine Learning/Projeto Final/notebook/1151-labeled-commits.xlsx")


### plot the 3 types of maintenance
plot(as.numeric(mydata[1,5]), as.numeric(mydata[1,6]), xlim=range(0:500), ylim=range(0:500))
par(new=TRUE)

for (i in 2:nrow(mydata)){
    if(mydata[[i,4]] == "p")
        color="blue"
    else if(mydata[[i,4]] == "c")
        color = "green"
    else
        color = "red"
    plot(as.numeric(mydata[i,5]), as.numeric(mydata[i,6]), xlim=range(0:500), ylim=range(0:500), ann=FALSE, xaxt='n', yaxt='n', col = color)
    par(new=TRUE)
    
}

### check correlation between added lines and deleted lines for each types of maintenance
vecPadd <- numeric()
vecPdel <- numeric()
vecCadd <- numeric()
vecCdel <- numeric()
vecAadd <- numeric()
vecAdel <- numeric()

for (i in 1:nrow(mydata)){
    if(mydata[[i,4]]=="p"){
        vecPadd <- append(vecPadd, as.numeric(mydata[[i,5]]))
        vecPdel <- append(vecPdel, as.numeric(mydata[[i,6]]))
    }
    if(mydata[[i,4]]=="c"){
        vecCadd <- append(vecCadd, as.numeric(mydata[[i,5]]))
        vecCdel <- append(vecCdel, as.numeric(mydata[[i,6]]))
    }
    if(mydata[[i,4]]=="a"){
        vecAadd <- append(vecAadd, as.numeric(mydata[[i,5]]))
        vecAdel <- append(vecAdel, as.numeric(mydata[[i,6]]))
    }
}


### Boxplots of added LOC and deleted LOC for each maintenance activity type
pathRead2 <- "C:/Users/markos/Desktop/Markos Viggiato/DCC - Mestrado/Disciplinas 2018.1/Machine Learning/Projeto Final/notebook/1151-labeled-commits.xlsx"

mydata <- readxl::read_xlsx(pathRead2)
mydata <- mydata[,4:6]

dataC <- mydata[mydata[,1]=="c",]
dataC[,2:3] <- as.data.frame(sapply(dataC[,2:3], as.numeric)) 
dataA <- mydata[mydata[,1]=="a",]
dataA[,2:3] <- as.data.frame(sapply(dataA[,2:3], as.numeric)) 
dataP <- mydata[mydata[,1]=="p",]
dataP[,2:3] <- as.data.frame(sapply(dataP[,2:3], as.numeric)) 

#additions
par(mfrow = c(1, 3))
boxplot(dataA[,2], outline = FALSE, main = "Adaptive", cex.axis=2, cex.main=2.5)
boxplot(dataC[,2], outline = FALSE, main = "Corrective", cex.axis=2, cex.main=2.5)
boxplot(dataP[,2], outline = FALSE, main = "Perfective", cex.axis=2, cex.main=2.5)

# deletions
par(mfrow = c(1, 3))
boxplot(dataA[,3], outline = FALSE, main = "Adaptive", cex.axis=2, cex.main=2.5)
boxplot(dataC[,3], outline = FALSE, main = "Corrective", cex.axis=2, cex.main=2.5)
boxplot(dataP[,3], outline = FALSE, main = "Perfective", cex.axis=2, cex.main=2.5)

# One axis
add <- c(dataC[,2], dataA[,2], dataP[,2])
dataJoint <- add$additions
gp <- c(rep("C", 500), rep("A", 247), rep("P", 404))
boxplot(dataJoint ~ gp, pch=19)


# Using ggplot2
library(ggplot2)
d <- data.frame(d.type=c(rep(0,15),rep(1,15)),sub.type=rep(c('A','B','C'),10),val=rnorm(30))

p <- ggplot(d, aes(factor(sub.type), val)) 
p + geom_boxplot() + facet_grid(. ~ d.type)


d <- data.frame(d.type=c(dataC[,2], dataA[,2], dataP[,2]), sub.type=rep(c('A','B','C'),10))


set.seed(1234)
x1 = rnorm(15, 100, 15);  x2 = rnorm(20, 110, 15)
x = c(x1, x2);  gp = c(rep(1,15), rep(2,20))
boxplot(x ~ gp, col=c("skyblue2","green2"), pch=19)

