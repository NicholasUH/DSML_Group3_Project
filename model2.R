# TBD

# READ DATASET
library(readr)
dataset = read_csv("Repositories/DSML_Group3_Project/Sleep_Efficiency.csv")
View(dataset)

# CLEAN DATASET OF NULL VALUES
sum(is.na(dataset))
new_dataset = na.omit(dataset)
sum(is.na(new_dataset))

