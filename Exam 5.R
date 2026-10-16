# Exam 5

source("functions.R")
data = read.csv("CorporateAnalysis_9.25.csv")

## question 7
A = rbind(c(1, 5, -3), c(2, 9, 8), c(1, 3, 2))
sum(diag(A))

## question 8
set.seed(123)
X1 = rnorm(21, 1, 1.1)
X2 = rexp(21, 1/3.4)
X3 = runif(21, 0, 5)
X4 = rwilcox(21, 18, 17)
X = cbind(X1, X2, X3, X4)

cov_matrix = cov(X)
det(cov_matrix)

## question 9
A = rbind(c(1, 5, -3), c(2, 9, 8), c(1, 3, 2))
det(A)

## question 10
A = rbind(c(1, 5, -3), c(2, 9, 8), c(1, 3, 2))
eig = eigen(A)$values
B = abs(eig)
B

## question 11
two.an = twofac.ANOVA (data$Work.Efficiency, data$Work.Category, data$Work.Experience)
p_cat = 1 - pf(two.an$Fstat.A, two.an$SSA.dof, two.an$SSE.dof)
p_exp = 1 - pf(two.an$Fstat.B, two.an$SSB.dof, two.an$SSE.dof)
p_int = 1 - pf(two.an$Fstat.AB, two.an$SSAB.dof, two.an$SSE.dof)
p_cat + p_exp + p_int

## question 12
one_an = onefac.ANOVA(data$Work.Efficiency, data$Work.Category)
sqrt(one_an$MSM)

## question 13
w_an = welch.ANOVA(data$Work.Efficiency, data$Work.Category)
g = length(unique(data$Work.Category))
n = length(data$Work.Efficiency)
1 - pf(w_an$Fstat, g-1, n-g)

## question 14
set.seed(123)
E1 = data$Work.Efficiency
E2 = 1.3*E1 + rnorm(length(E1), 0, 0.8)
E3 = 2.5*E2 + rnorm(length(E2), 0, 0.4)
E4 = 3.8*E3 + rnorm(length(E3), 0, 0.2)

n = length(E1)
id = 1:n

data_long = data.frame(
  id = rep(id, each = 4),
  condition = rep(c("E1", "E2", "E3", "E4"), times = n),
  efficiency = c(E1, E2, E3, E4)
)

rm.ANOVA(data_long, "id", "condition", "efficiency")

## question 15
set.seed(123)
E1 = data$Work.Efficiency
E2 = 1.3*E1 + rnorm(length(E1), 0, 0.8)
E3 = 2.5*E2 + rnorm(length(E2), 0, 0.4)
E4 = 3.8*E3 + rnorm(length(E3), 0, 0.2)
X = cbind(E1, E2, E3, E4)

mymauch = mymauchly(X)
sqrt(mymauch$Mstat.m)

## question 16
set.seed(1234)
B = 15000
x = data$Satisfaction.with.Work      
y = data$Work.Efficiency   
n = length(x)

b1_boot = numeric(B)

idx = seq_len(n)
for (b in 1:B) {
  s = sample(idx, n)
  xb = x[s]
  yb = y[s]
  
  b1 = cov(xb, yb) / var(xb)
  b1_boot[b] = b1
}

alpha = 0.124
probs = c(alpha/2, 1 - alpha/2)  

ci_slope = round(quantile(b1_boot, probs, names = FALSE), 4)
ci_slope



