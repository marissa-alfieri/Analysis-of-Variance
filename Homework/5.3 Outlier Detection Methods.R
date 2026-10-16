## 5.3 - Outlier Detection Methods

HalfMarathon <- read_csv("HalfMarathon_9.25.csv")

###question 3
avg_time = mean(HalfMarathon$HalfMarathonTime_min)
avg_time

###question 4
data$PersonID = 1:nrow(data)
grubbs(data$HalfMarathonTime_min, data$PersonID, 0.05)
data[data$PersonID == 30, ]
data$Height_cm[data$PersonID == 30]
data$Weight_kg[data$PersonID == 30]
data$HalfMarathonTime_min[data$PersonID == 30]

###question 6
grubbs(data$HalfMarathonTime_min, data$PersonID, 0.07)

###question 7
x = data$HalfMarathonTime_min
skew = (avg_time - median(x)) / sd(x)
skew

### question 8
X = sort(HalfMarathon$HalfMarathonTime_min)
n = length(X)
n0 = 7   
xstar = X[(n - n0 + 1):n]  #Outliers
xor = X[1:(n - n0)] #Without Outliers

mean_outliers = mean(xstar)

sd_outliers = sd(xstar)

mean_no_outliers = mean(xor)

sd_no_outliers = sd(xor)


### question 9
TietjenMoore(data$HalfMarathonTime_min, 7)


