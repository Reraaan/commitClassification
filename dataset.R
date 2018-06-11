library("jsonlite")
library("plyr")
library("readxl")

pathRead <- "C:/Users/markos/Desktop/Markos Viggiato/DCC - Mestrado/Disciplinas 2018.1/Machine Learning/Projeto Final/1151-commits-labeled-with-maintenance-activities.csv"
mydata <- read.csv2(pathRead, stringsAsFactors = FALSE)

fullname <- mydata[[1,2]]
commitSHA <- mydata[[1,1]]
commits <- fromJSON(paste0("https://api.github.com/repos/ReactiveX/RxJava/commits/",commitSHA))
print(commits$stats$deletions)
