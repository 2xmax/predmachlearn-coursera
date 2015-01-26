#includes
library(dplyr)
library(caret)
library(randomForest)
#seed
set.seed(42)

if (!file.exists("pml-training.csv")){
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile = "pml-training.csv", method="curl")  
}

raw <- read.csv("pml-training.csv", na.strings = c("NA",""), stringsAsFactors = FALSE)

cleanupData = function(raw){  
  cleaned <- subset(raw, select = -c(X, user_name, raw_timestamp_part_1, raw_timestamp_part_2, cvtd_timestamp, new_window, num_window))  
  naThreshold = as.integer(nrow(raw) * 0.5)
  cleaned <- subset(cleaned, select = -which(colSums(is.na(cleaned)) > naThreshold))
  cleaned
}

data = cleanupData(raw)

data$classe <- as.factor(data$classe)

testIndex = createDataPartition(cleaned$classe, p = 0.7,list=FALSE)
train = cleaned[-testIndex,]
test = cleaned[testIndex,]

predictor <- randomForest(classe ~ ., data = train)
trainPred <- predict(predictor, newdata = train)
testPred <- predict(predictor, newdata = test)

confusionMatrix <- confusionMatrix(test$classe, testPred)
accurancy = confusionMatrix$overall[1]