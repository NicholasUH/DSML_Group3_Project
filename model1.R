# Model - Linear Regression

# Sleep Efficiency ~ Bedtime + Caffeine + Alcohol + Smoking Status + Exercise

# Load required libraries
library(readr)
library(lubridate)

# Read Data set
dataset = read_csv("repos/DSML_Group3_Project/Sleep_Efficiency.csv")

# Clean data set of null values
sum(is.na(dataset))
dataset = na.omit(dataset)
sum(is.na(dataset))

# Remove unnecessary variables
columns_to_keep <- c(4,7,12,13,14,15)
dataset <- dataset[,columns_to_keep]

# Removed Year/Month/Day from Bedtime Variable
dataset$Bedtime <- ymd_hms(dataset$Bedtime)
dataset$Bedtime <- format(dataset$Bedtime, format = "%H:%M:%S")

# Clean data set of null values
sum(is.na(dataset))
dataset = na.omit(dataset)
sum(is.na(dataset))

# Factor categorical variables
dataset$`Smoking status` = as.factor(dataset$`Smoking status`)

`Exercise frequency` = as.factor(`Exercise frequency`)

# Model Fitting
sleep_lm = lm(`Sleep efficiency` ~ Bedtime + `Caffeine consumption` + `Alcohol consumption` + `Smoking status` + `Exercise frequency`)
summary(sleep_lm)

# Issues to fix
# Missing one level on each of the categorical variables
