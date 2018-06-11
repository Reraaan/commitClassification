library("jsonlite")
library("plyr")
library("readxl")
library(tibble)

pathRead <- "C:/Users/markos/Desktop/Markos Viggiato/DCC - Mestrado/Disciplinas 2018.1/Machine Learning/Projeto Final/1151-commits-labeled-with-maintenance-activities.csv"
mydata <- read.csv2(pathRead, stringsAsFactors = FALSE)
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
       
    commits <- fromJSON(paste0("https://api.github.com/repos/", fullname,"/commits/",commitSHA,"?access_token=2cd4f1a3761d27c6051b1d991d7d861649c6705b"))
    addition <- commits$stats$additions
    deletion <- commits$stats$deletions
    mydata[[i,5]] <- addition
    mydata[[i,6]] <- deletion

}
xlsx::write.xlsx(mydata, "C:/Users/markos/Desktop/Markos Viggiato/DCC - Mestrado/Disciplinas 2018.1/Machine Learning/Projeto Final/new-labeled-with-maintenance-activities.xlsx")






