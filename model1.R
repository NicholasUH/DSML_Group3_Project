# Linear Regression

# Sleep Efficiency ~ Bedtime + Caffeine + Alcohol + Smoking Status + Exercise

# READ DATASET
library(readr)
dataset = read_csv("Repositories/DSML_Group3_Project/Sleep_Efficiency.csv")
View(dataset)

# CLEAN DATASET OF NULL VALUES
sum(is.na(dataset))
dataset = na.omit(dataset)
sum(is.na(dataset))

# REMOVE ANY UNWANTED VARIABLES
columns_to_keep <- c(4,7,12,13,14,15)
dataset <- dataset[,columns_to_keep]
View(dataset)

# FIXED BETIME VARIABLE TO ONLY BE TIME
library(lubridate)
dataset$Bedtime <- ymd_hms(dataset$Bedtime)
dataset$Time <- format(dataset$Bedtime, format = "%H:%M:%S")
View(dataset)

sum(is.na(dataset))
dataset = na.omit(dataset)
sum(is.na(dataset))

dataset <- dataset[,-1]
View(dataset)

# FACTOR ANY CATEGORICALS, NUMERIC ANY NUMBERS R DOESNT READ PROPERLY


# CREATE THE LINEAR MODEL