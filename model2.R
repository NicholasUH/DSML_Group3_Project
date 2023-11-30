# Model - Decision Tree
# Sleep Efficiency ~ Bedtime + Caffeine + Alcohol + Smoking Status + Exercise

#LIBRARIES
library(readr)
library(lubridate)
library(tree)
library(boot)
library(dplyr)
library(randomForest)

# READ DATASET
dataset = read_csv("Repositories/DSML_Group3_Project/Sleep_Efficiency.csv")

# CLEAN DATASET OF NULL VALUES
dataset = na.omit(dataset)

# REMOVE ANY UNWANTED VARIABLES
columns_to_keep <- c(4,7,12,13,14,15)
dataset <- dataset[,columns_to_keep]

# FIX COLUMN NAMES
colnames(dataset) <- c('Bedtime', 'SleepEfficiency', 'CaffeineConsumption', 'AlcoholConsumption',
                       'SmokingStatus', 'ExerciseFrequency')

# FIXED BEDTIME VARIABLE TO ONLY BE TIME
dataset$Bedtime <- ymd_hms(dataset$Bedtime)
dataset$Bedtime <- format(dataset$Bedtime, format = "%H:%M:%S")
dataset$Bedtime <- ifelse(between(dataset$Bedtime, '21:00:00', '23:59:00'), 'Early', 'Late')

# REMOVE ANY UNWANTED VARIABLES
dataset = na.omit(dataset)

# FACTOR CATEGORICAL VARIABLES
dataset$Bedtime <- as.factor(dataset$Bedtime)
dataset$SmokingStatus <- as.factor(dataset$SmokingStatus)
dataset$ExerciseFrequency <- as.factor(dataset$ExerciseFrequency)


#-------------------------------------------------------------


# REGRESSION TREE MODEL

# TEST DECISION / REGRESSION TREE 10 TIMES
regressionTreeMSE <- rep(0, 10)
for (i in 1:10) {
  set.seed(i)
  
  # SPLIT INTO TRAINING AND TESTING SETS
  train <- sample(1:nrow(dataset), nrow(dataset) * 0.8)
  trainSet <- dataset[train,]
  testSet <- dataset[-train,]
  
  # BUILD REGRESSION TREE
  sleep.reg.tree <- tree(SleepEfficiency ~ Bedtime + CaffeineConsumption + 
                           AlcoholConsumption + SmokingStatus + ExerciseFrequency, 
                         data = trainSet)
  
  # CALCULATE MSE OF REGRESSION TREE
  yHat <- predict(sleep.reg.tree, newdata = testSet)
  regressionTreeMSE[i] <- mean((yHat - testSet$SleepEfficiency)^2)
}
regressionTreeMSE
mean(regressionTreeMSE)

# DISPLAY DECISION TREE
plot(sleep.reg.tree)
text(sleep.reg.tree, pretty = 0)

# FIND R^2 OF REGRESSION TREE
datasetMean <- mean(testSet$SleepEfficiency)
SSTotal <- sum((testSet$SleepEfficiency - datasetMean)^2)
unprunedSSResidual <- sum((testSet$SleepEfficiency - yHat)^2)
(prunedR2 <- 1 - (unprunedSSResidual / SSTotal))


#-------------------------------------------------------------

# PRUNED TREE

# TEST PRUNED TREE 10 TIMES
prunnedTreeMSE <- rep(0, 10)
for (i in 1:10) {
  set.seed(i)
  
  # SPLIT INTO TRAINING AND TESTING SETS
  train <- sample(1:nrow(dataset), nrow(dataset) * 0.8)
  trainSet <- dataset[train,]
  testSet <- dataset[-train,]
  
  # TREE PRUNING (PRUNING CROSS VALIDATION)
  treeCV <- cv.tree(sleep.reg.tree)
  
  # BUILD PRUNED TREE
  sleep.pruned.tree <- prune.tree(sleep.reg.tree, best = 5)
  
  # CALCULATE MSE OF PRUNED TREE
  yHat <- predict(sleep.pruned.tree, newdata = testSet)
  prunnedTreeMSE[i] <- mean((yHat - testSet$SleepEfficiency)^2)
}
prunnedTreeMSE
mean(prunnedTreeMSE)

# PLOT CV DEVIANCE
plot(treeCV$size, treeCV$dev, type = "b")

# DISPLAY PRUNED TREE
plot(sleep.pruned.tree)
text(sleep.pruned.tree, pretty = 0)

# FIND R^2 OF PRUNED TREE
prunedSSResidual <- sum((testSet$SleepEfficiency - yHat)^2)
(prunedR2 <- 1 - (prunedSSResidual / SSTotal))


#-------------------------------------------------------------

# RANDOM FOREST MODEL

# TEST RF 10 TIMES
randomForestMSE = rep(0, 10)
for (i in 1:10) {
  set.seed(i)
  
  # SPLIT INTO TRAINING AND TESTING SETS
  train <- sample(1:nrow(dataset), nrow(dataset) * 0.8)
  trainSet <- dataset[train,]
  testSet <- dataset[-train,]
  
  # SET VARIABLES FOR RANDOM FOREST
  p <- ncol(dataset)
  n <- nrow(dataset)
  B <- 1000
  included.vars = names(dataset[])
  
  # RANDOM FOREST MODEL
  sleep.rf <- randomForest(SleepEfficiency ~ Bedtime + CaffeineConsumption + 
                             AlcoholConsumption + SmokingStatus + ExerciseFrequency,
                           data = dataset, subset = train, 
                           mtry = p/3, ntree = B, 
                           importance = TRUE, keep.forest = TRUE)
  
  # CALCULATE MSE OF RANDOM FOREST
  yhat.rf = predict(sleep.rf, newdata = testSet)
  randomForestMSE[i] = mean((yhat.rf - testSet$SleepEfficiency)^2)
}
randomForestMSE
mean(randomForestMSE)

# IMPORTANCE PLOT
varImpPlot(sleep.rf)

# CREATE DECISION TREE BASED ON RANDOM FOREST
sleep.rf.tree <- tree(SleepEfficiency ~ Bedtime + CaffeineConsumption + 
                        AlcoholConsumption + SmokingStatus + ExerciseFrequency, 
                      data = trainSet, subset = sleep.rf$inbag)
plot(sleep.rf.tree)
text(sleep.rf.tree, pretty = 0)

