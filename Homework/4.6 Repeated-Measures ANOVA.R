# 4.6 - Repeated-Measures ANOVA

source("functions.R")

data = read.csv("NoiseInfluence_9.25.csv")

Xs = split(data$test_score, data$noise_level)

matrix = sapply(Xs, identity)

####### Question 2 ####### 
mymauchly(matrix)

####### Question 3 ####### 
KS_stat(test_wide$test_score.quiet)
KS_stat(test_wide$test_score.moderate)
KS_stat(test_wide$test_score.loud)

####### Question 4 ####### 
rm.ANOVA(Xs$quiet, Xs$moderate, Xs$loud)


####### Question 6 ####### 
nonspher.rmANOVA(matrix)





