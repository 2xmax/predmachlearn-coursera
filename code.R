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

testIndex = createDataPartition(data$classe, p = 0.7, list=FALSE)
train = data[-testIndex,]
test = data[testIndex,]

# library(doParallel)
# registerDoParallel(makeCluster(7))
# predictor <- train(classe ~ ., data = train, method="rf"), gbm - Stochastic Gradient Boosting, svmRadial - Support Vector Machines
predictor <- randomForest(classe ~ ., data = train)

testPred <- predict(predictor, newdata = test)

accurancy = confusionMatrix(test$classe, testPred)$overall[1]

# ====== Answers part
if (!file.exists("pml-testing.csv")){
  download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile = "pml-testing.csv", method="curl")
}

ansData = cleanupData(read.csv("pml-testing.csv", na.strings = c("NA",""), stringsAsFactors = FALSE))
answers <- predict(predictor, newdata = ansData)

