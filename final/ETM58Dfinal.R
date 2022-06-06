#install.packages("farff")
#install.packages("rpart")
#install.packages("rattle")
#install.packages("mlr")
#install.packages("data.table")
#install.packages("lubricate")

library("farff")
library("tidyverse")
library("factoextra")
library("openxlsx")
library("rpart")
library("rattle")
library("caret")
library("mlr")
library("data.table")
library("lubridate")


set.seed(123)

kidneydf <- readARFF("C:/ETM58D/final/db3_kidney/Chronic_Kidney_Disease/chronic_kidney_disease_full.arff")
kidneydfmod <- kidneydf

kidneydfmod <- data.frame(lapply(kidneydf,function(x){gsub("abnormal","1",x)} ))
kidneydfmod <- data.frame(lapply(kidneydfmod,function(x){gsub("normal","0",x)} ))
kidneydfmod <- data.frame(lapply(kidneydfmod,function(x){gsub("notckd","0",x)} ))
kidneydfmod <- data.frame(lapply(kidneydfmod,function(x){gsub("ckd","1",x)} ))
kidneydfmod <- data.frame(lapply(kidneydfmod,function(x){gsub("notpresent","0",x)} ))
kidneydfmod <- data.frame(lapply(kidneydfmod,function(x){gsub("present","1",x)} ))
kidneydfmod <- data.frame(lapply(kidneydfmod,function(x){gsub("good","0",x)} ))
kidneydfmod <- data.frame(lapply(kidneydfmod,function(x){gsub("poor","1",x)} ))
kidneydfmod <- data.frame(lapply(kidneydfmod,function(x){gsub("no","0",x)} ))
kidneydfmod <- data.frame(lapply(kidneydfmod,function(x){gsub("yes","1",x)} ))
kidneydfmod[] <- lapply(kidneydfmod, function(x) as.numeric(as.character(x)))
kidneydfmod$class <- as.factor(as.character(kidneydfmod$class))

imp1 <- impute(kidneydfmod, classes = list(numeric=imputeMedian(), factor=imputeMode()))
kidneydfmod <- imp1$data

str(kidneydfmod)

kidneytrain <- kidneydfmod[1:320,]
kidneytest <- kidneydfmod[321:400,]


#create and test decision tree

dtree <- rpart(class~.,data = kidneytrain, method = "class")

fancyRpartPlot(dtree)

kidneypredict <- predict(dtree,kidneytest,type="class")
kidneypredict

tableaccuracy <- table(kidneytest$class,kidneypredict)

accuracytest<- sum(diag(tableaccuracy))/sum(tableaccuracy)
accuracytest

#prune the decision tree

printcp(dtree)
plotcp(dtree)
p1 <- dtree$cptable[which.min(dtree$cptable[, "xerror"]), "CP"]

pdtree <- prune(dtree, cp=p1)

fancyRpartPlot(pdtree)

#Start random forest

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)
str(kidneydfmod)
rf_1 <- caret::train(class~., data = kidneytrain, method = "ranger", trControl = fitControl)

plot(rf_1)
print(rf_1)


#mrty is chosen as 2

rf1pred <- predict(rf_1, kidneytest)
rf1pred

rf1accu <- table(kidneytest$class,rf1pred)

rf1test <- sum(diag(rf1accu))/sum(rf1accu)
rf1test

#DATASET2

#Predict VHD diagnosis and level using medical data

data2 <- read.xlsx("C:/ETM58D/final/lungsdata/Z-Alizadehsanidataset.xlsx")

#change sex as fmale= 1 and male=0

data2 <- data.frame(lapply(data2,function(x){gsub("Fmale","1",x)} ))
data2 <- data.frame(lapply(data2,function(x){gsub("Male","0",x)} ))

#change CAD data as Cad=1 and no as =0
data2 <- data.frame(lapply(data2,function(x){gsub("Cad","1",x)} ))
data2 <- data.frame(lapply(data2,function(x){gsub("Normal","0",x)} ))

#Cahnge all No to 0 and yeses to 1
data2 <- data.frame(lapply(data2,function(x){gsub("N","0",x)} ))
data2 <- data.frame(lapply(data2,function(x){gsub("Y","1",x)} ))

#Change VHD data on 0-3 scale
data2 <- data.frame(lapply(data2,function(x){gsub("mild","1",x)} ))
data2 <- data.frame(lapply(data2,function(x){gsub("Moderate","2",x)} ))
data2 <- data.frame(lapply(data2,function(x){gsub("Severe","3",x)} ))

#change BBB positive data as LBBB=1 and RBBB=2
data2 <- data.frame(lapply(data2,function(x){gsub("LBBB","1",x)} ))
data2 <- data.frame(lapply(data2,function(x){gsub("RBBB","2",x)} ))

#make storage tyepes suitable
data2[] <- lapply(data2, function(x) as.numeric(as.character(x)))
data2$VHD <- as.factor(as.character(data2$VHD))

data2train <- data2[1:260,]
data2test <- data2[262:303,]

#train and test decision tree

dtree_2 <-rpart(VHD~.,data = data2train, method = "class")

fancyRpartPlot(dtree_2)

case2pre <- predict(dtree_2, data2test, type = "class")

case2tabacc <- table(data2test$VHD,case2pre)
case2tabacc

case2acctest <- sum(diag(case2tabacc))/sum(case2tabacc) 
case2acctest

#prune the decision tree

printcp(dtree_2)
plotcp(dtree_2)
p2 <- dtree_2$cptable[which.min(dtree$cptable[, "xerror"]), "CP"]

pdtree_2 <- prune(dtree_2, cp=p2)

fancyRpartPlot(pdtree_2)

#train and test random forest

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

rf_2 <- caret::train(VHD~.,
              data = data2train,
              method = "ranger",
              trControl = fitControl,
              )

plot(rf_2)
print(rf_2)

#mrty is chosen as 55

rf2pred <- predict(rf_2, data2test)

rf2accu <- table(data2test$VHD,rf2pred)

rf2test <- sum(diag(rf2accu))/sum(rf2accu)
rf2test



#DATASET 3

#predict grades based on student and family questionerres

data3 <- read.csv("C:/ETM58D/final/studentgrade/student-mat.csv", sep = ";")

str(data3)

data3 <- data.frame(lapply(data3,function(x){gsub("GP","0",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("MS","1",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("F","1",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("M","0",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("U","0",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("R","1",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("LE3","0",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("GT3","1",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("A","0",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("T","1",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("mother","0",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("father","1",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("other","0",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("teacher","1",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("health","2",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("services","3",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("at_home","4",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("home","1",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("reputation","2",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("course","3",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("no","0",x)} ))
data3 <- data.frame(lapply(data3,function(x){gsub("yes","1",x)} ))
data3[] <- lapply(data3, function(x) as.integer(as.character(x)))
data3$G3 <- as.factor(as.character(data3$G3))
str(data3)

data3train <- data3[1:320,]
data3test <- data3[321:395,]


#create decision tree

dtree_3 <- rpart(G3~., data3train, method = "class")

fancyRpartPlot(dtree_3)

case3pre <- predict(dtree_3, data3test, type = "class")

case3tabacc <- table(data3test$G3,case3pre)
case3tabacc

case3acctest <- sum(diag(case3tabacc))/sum(case3tabacc) 
case3acctest

#prune the decision tree

printcp(dtree_3)
plotcp(dtree_3)
p3 <- dtree_3$cptable[which.min(dtree$cptable[, "xerror"]), "CP"]

pdtree_3 <- prune(dtree_3, cp=p3)

fancyRpartPlot(pdtree_3)

#create randomforest

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

rf_3 <- caret::train(G3~.,
                     data = data3train,
                     method = "ranger",
                     trControl = fitControl
)

plot(rf_3)
print(rf_3)

#mrty is chosen as 32

rf3pred <- predict(rf_3, data3test)

rf3accu <- table(data3test$G3,rf3pred)

rf3test <- sum(diag(rf3accu))/sum(rf3accu)
rf3test

