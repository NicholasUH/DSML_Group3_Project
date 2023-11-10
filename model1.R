# Model - Linear Regression

# Sleep Efficiency ~ Bedtime + Caffeine + Alcohol + Smoking Status + Exercise

# Load required libraries
library(readr)
library(lubridate)

# READ DATASET
dataset = read_csv("repos/DSML_Group3_Project/Sleep_Efficiency.csv")

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


# FACTOR THE CATEGORICAL VARIABLES
dataset$`Smoking status` = as.factor(dataset$`Smoking status`)

`Exercise frequency` = as.factor(`Exercise frequency`)

# FITTING THE MODEL
sleep_lm = lm(`Sleep efficiency` ~ Bedtime + `Caffeine consumption` + `Alcohol consumption` + `Smoking status` + `Exercise frequency`)
summary(sleep_lm)


# MISSING ONE LEVEL IN EACH OF THE CATEGORICAL PREDICTORS
