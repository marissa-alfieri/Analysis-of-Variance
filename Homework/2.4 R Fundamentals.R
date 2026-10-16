
##  M02.4 - R Fundamentals 


EnvStudyData <- read_csv("Desktop/MA416/EnvStudyData_9.25.csv")

mean(EnvStudyData$AverageTemperature_C)
median(EnvStudyData$AirQualityIndex)
geom_mean <- exp(mean(log(EnvStudyData$ForestAreaPercentage)))
geom_mean

sd(EnvStudyData$AverageTemperature_C)

cv <- sd(EnvStudyData$AirQualityIndex) / mean(EnvStudyData$AirQualityIndex)
cv

m <- median(EnvStudyData$ForestAreaPercentage)
value <- sqrt(sum((EnvStudyData$ForestAreaPercentage - m)^2))
value          

A = EnvStudyData$AverageTemperature_C[which(EnvStudyData$ClimateZone == 'Polar')]
mean(A)

B = EnvStudyData$AnnualRainfall[which(EnvStudyData$ClimateZone == 'Tropical')]
median(B)

C = EnvStudyData$AirQualityIndex[which(EnvStudyData$ConservationStatus == 'Vulnerable')]
sd(C)

#(Age > 20) & (GPA < 3.4) # this is an "and" operator; for this, both must be true
#(Age > 20) | (GPA < 3.4) # this is an "or" operator; for this, either (or both) must be true

mean(EnvStudyData$AverageTemperature_C[which(EnvStudyData$AirQualityIndex > 100 & EnvStudyData$ClimateZone == 'Continental' )])

mean(EnvStudyData$AverageTemperature_C[which(EnvStudyData$PollutionLevel == 'Severe' | EnvStudyData$SpeciesCount > 1000 )])
