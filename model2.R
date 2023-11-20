# Model - Decision Tree

# Sleep Efficiency ~ Bedtime + Caffeine + Alcohol + Smoking Status + Exercise

#LIBRARIES
library(readr)
library(lubridate)
library(tree)
library(randomForest)

# READ DATASET
dataset = read_csv("Repositories/DSML_Group3_Project/Sleep_Efficiency.csv")

# CLEAN DATASET OF NULL VALUES
sum(is.na(dataset))
dataset = na.omit(dataset)
sum(is.na(dataset))

# REMOVE ANY UNWANTED VARIABLES
columns_to_keep <- c(4,7,12,13,14,15)
dataset <- dataset[,columns_to_keep]

# FIXED BEDTIME VARIABLE TO ONLY BE TIME
dataset$Bedtime <- ymd_hms(dataset$Bedtime)
dataset$Bedtime <- format(dataset$Bedtime, format = "%H:%M:%S")

# REMOVE ANY UNWANTED VARIABLES
sum(is.na(dataset))
dataset = na.omit(dataset)
sum(is.na(dataset))

View(dataset)