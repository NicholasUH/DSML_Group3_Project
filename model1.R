# Model - Linear Regression

# Sleep Efficiency ~ Bedtime + Caffeine + Alcohol + Smoking Status + Exercise

# Load required libraries
library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)

# Read Data set
dataset <- read_csv("Repositories/DSML_Group3_Project/Sleep_Efficiency.csv")

# Clean data set of null values
dataset <- na.omit(dataset)

# Remove unnecessary variables
columns_to_keep <- c(4, 7, 12, 13, 14, 15)
dataset <- dataset[, columns_to_keep]

# Removed Year/Month/Day from Bedtime Variable
dataset$Bedtime <- ymd_hms(dataset$Bedtime)
dataset$Bedtime <- format(dataset$Bedtime, format = "%H:%M:%S")
dataset$Bedtime <- ifelse(between(dataset$Bedtime, '21:00:00', '23:59:00'), 'Early', 'Late')

# Clean data set of null values
dataset <- na.omit(dataset)

# Factor categorical variables
dataset$Bedtime <- as.factor(dataset$Bedtime)
dataset$`Smoking status` <- as.factor(dataset$`Smoking status`)
dataset$`Exercise frequency` <- as.factor(dataset$`Exercise frequency`)

# Model Fitting
sleep_lm <- lm(`Sleep efficiency` ~ Bedtime + `Caffeine consumption` + `Alcohol consumption` + `Smoking status` + `Exercise frequency`, data = dataset)
summary(sleep_lm)

par(mfrow=c(2,2))
plot(sleep_lm)

#Manually taking out Insignificant Predictors
manual_sleep_lm = lm(dataset$`Sleep efficiency` ~ dataset$`Alcohol consumption` + dataset$`Smoking status` + dataset$`Exercise frequency`, data = dataset)
summary(manual_sleep_lm)

# Find best subset of predictors - all predictors were used after step-wise 
step_model <- step(sleep_lm, direction = "backward")
summary(step_model)

AIC(sleep_lm)
AIC(manual_sleep_lm)
AIC(step_model)


# Cross Validation
MSE = rep(0,10)

for(i in 1:10){
  set.seed(i)
  train = sample(1:nrow(dataset), 0.8*nrow(dataset))
  test = dataset[-train,]
  cv_sleep_lm = lm(`Sleep efficiency` ~ Bedtime + `Caffeine consumption` + 
                     `Alcohol consumption` + `Smoking status` + `Exercise frequency`
                   ,data=dataset, subset=train)
  yhat = predict(cv_sleep_lm, newdata=test)
  MSE[i] = mean((yhat - test$`Sleep efficiency`)^2)
}

MSE
mean(MSE)
range(MSE)










