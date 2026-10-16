## 5.5 - Common Non-Parametric Methods (Pt. 2)


source("functions.R")

data = read.csv("CorporateAnalysis_9.25.csv")

Xs = split(data$Average.Hours.Worked, data$Work.Category)

multi_rank_based(commercial = Xs$commercial, industry = Xs$industry, residential = Xs$residential, alpha=0.06)



multi_rank_based(commercial = abs(Xs$commercial - median(Xs$commercial)), industry = abs(Xs$industry - median(Xs$industry)), residential = abs(Xs$residential - median(Xs$residential)))


# 
idx = rep(names(Xs), times = sapply(Xs, length))
Sjc = sapply(Xs, function(x) abs(x - median(x)))

Sjcall = unlist(Sjc)
Rall = rank(Sjcall)
Rj = split(Rall, idx)

nj = sapply(Rj, length)
n = sum(nj)
g = length(Rj)
Rbar_j = sapply(Rj, mean)
Rbar = mean(Rall)

num = sum(nj * (Rbar_j - Rbar)^2)
den = sum(sapply(Rj, function(x) sum((x - mean(x))^2)))

Co_stat = ((n - g) / (g - 1)) * (num / den)
p_val = 1 - pf(Co_stat, g - 1, n - g)

Co_stat
p_val

# ~

ASR = (1/n) * sum(SRj)
ASR
V = (1/(n - 1)) * (sum(Rc^4) - n*(ASR^2))
Co_sr_stat = (1/V) * (sum((SRj^2)/nj) - n*(ASR^2))
Co_sr_stat

