# Model - Decision Tree

# Sleep Efficiency ~ Bedtime + Caffeine + Alcohol + Smoking Status + Exercise

#LIBRARIES
library(readr)
library(lubridate)
library(tree)
library(boot)

# READ DATASET

# CHANGE BACK TO ORIGINAL PATH AFTER LOCAL TESTING
# dataset = read_csv("Repositories/DSML_Group3_Project/Sleep_Efficiency.csv")

# LOCAL TESTING
dataset = read_csv("repos/DSML_Group3_Project/Sleep_Efficiency.csv")

# CLEAN DATASET OF NULL VALUES
sum(is.na(dataset))
dataset = na.omit(dataset)
sum(is.na(dataset))

# REMOVE ANY UNWANTED VARIABLES
columns_to_keep <- c(4,7,12,13,14,15)
dataset <- dataset[,columns_to_keep]

# FIX COLUMN NAMES
colnames(dataset) <- c('Bedtime', 'SleepEfficiency', 'CaffeineConsumption', 'AlcoholConsumption',
                       'SmokingStatus', 'ExerciseFrequency')

# FIXED BEDTIME VARIABLE TO ONLY BE TIME
dataset$Bedtime <- ymd_hms(dataset$Bedtime)
dataset$Bedtime <- format(dataset$Bedtime, format = "%H:%M:%S")

# REMOVE ANY UNWANTED VARIABLES
sum(is.na(dataset))
dataset = na.omit(dataset)
sum(is.na(dataset))

# ---------------------------------------------------------------------------

# FACTOR CATEGORICAL VARIABLES
dataset$Bedtime <- as.factor(dataset$Bedtime)
dataset$SmokingStatus <- as.factor(dataset$SmokingStatus)
dataset$ExerciseFrequency <- as.factor(dataset$ExerciseFrequency)

# SPLIT INTO TRAINING AND TESTING SETS
set.seed(100)
dataSetIndices <- sample(1:nrow(dataset), nrow(dataset) * 0.8)
trainSet <- dataset[dataSetIndices,]
testSet <- dataset[-dataSetIndices,]

# BUILD REGRESSION TREE
sleepTreeModel <- tree(SleepEfficiency ~ Bedtime + CaffeineConsumption + AlcoholConsumption +
                      SmokingStatus + ExerciseFrequency, data = trainSet)

summary(sleepTreeModel)

# UNPRUNED TREE
plot(sleepTreeModel)
text(sleepTreeModel, pretty = 0)

# TREE PRUNING (PRUNING CROSS VALIDATION)
treeCV <- cv.tree(sleepTreeModel)
plot(treeCV$size, treeCV$dev, type = "b")

prunedTree <- prune.tree(sleepTreeModel, best = 3)
plot(prunedTree)
text(prunedTree)

# MAKE PREDICTIONS FOR MODEL
yHatUP <- predict(sleepTreeModel, newdata = testSet)
yHatP <- predict(prunedTree, newdata = testSet)

# FIND MSE OF MODELS
(unprunedMSE <- mean((yHatUP - testSet$SleepEfficiency)^2))
(prunedMSE <- mean((yHatP - testSet$SleepEfficiency)^2))

# FIND R^2 OF MODELS
datasetMean <- mean(testSet$SleepEfficiency)

unprunedSSResidual <- sum((testSet$SleepEfficiency - yHatUP)^2)
SSTotal <- sum((testSet$SleepEfficiency - datasetMean)^2)

(unprunedR2 <- 1 - (unprunedSSResidual / SSTotal))

prunedSSResidual <- sum((testSet$SleepEfficiency - yHatP)^2)
(prunedR2 <- 1 - (prunedSSResidual / SSTotal))