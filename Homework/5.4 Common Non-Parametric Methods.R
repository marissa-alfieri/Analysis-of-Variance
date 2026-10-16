# 5.4 - Common Non-Parametric Methods

source("functions.R")
data = read.csv("CorporateAnalysis_9.25.csv")

x = data$Work.Efficiency

# question 1
quantile(x, probs = 0.04)
quantile(x, probs = 0.81) # upper 19th percentile

# question 2
sign_percentiles(x, 6, 0.35)

# question 4
r = split(x, factor(data$Secondary.Job))
two_rank_based(r$yes, r$no)

# question 7 
# (s1 = U_small, s2 = n1*n2 - U_small)

#Suppose you have sample sizes of 24 and 17, and the smaller test statistic was equal to 123.

# What is the larger test statistic equal to?
nj = c(s1 = 24, s2 = 17)
U_small = 123
n1=24
n2=17
MW_stat = n1*n2 - U_small

# For an alpha of 5%, what would be the one-tailed lower-tail critical value?
MW_crit = qwilcox(0.05, n1, n2, lower.tail = T)
Z = (U_small - (n1 * n2) / 2) / sqrt((n1 * n2 * ((n1 + n2) + 1)) / 12)

MW_pval = pnorm(abs(Z), lower.tail = F)  












