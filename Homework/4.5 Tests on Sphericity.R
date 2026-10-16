# 4.5 - Tests on Sphericity

source("functions.R")

mydata = read.csv("FlowerGrowth_9.25.csv")
X = mydata[, c("Height_2Weeks_cm", "Height_1Month_cm", "Height_2Months_cm", "Height_4Months_cm")]
mymauchly (X)
