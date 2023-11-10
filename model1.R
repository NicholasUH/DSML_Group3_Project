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

# FACTOR ANY CATEGORICALS, NUMERIC ANY NUMBERS R DOESNT READ PROPERLY


# CREATE THE LINEAR MODEL