# 5.2 - Bootstrap Sampling

data = read.csv("NoiseInfluence_9.25.csv")

# question 1
X = mean(data$awareness)

# question 2

set.seed(14141)
B <- 15000
x <- data$awareness  # your numeric vector

boot_means <- replicate(B, mean(sample(x, length(x), replace = TRUE)))
ci <- quantile(boot_means, c(0.036, 0.964), names = FALSE)
ci


# question 3
x <- data$awareness
y <- data$test_score

b1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
b0 <- mean(y) - b1 * mean(x)

b1
b0

# question 4

set.seed(72727)
B <- 15000
x <- data$awareness        # numeric vector
y <- data$test_score     # numeric vector
n <- length(x)

r2_boot  <- numeric(B)
rmse_boot <- numeric(B)

idx <- seq_len(n)
for (b in 1:B) {
  s <- sample(idx, n, replace = TRUE)
  xb <- x[s]; yb <- y[s]
  
  # manual OLS
  b1 <- cov(xb, yb) / var(xb)
  b0 <- mean(yb) - b1 * mean(xb)
  
  yhat <- b0 + b1 * xb
  res  <- yb - yhat
  
  # R^2 and RMSE
  r2_boot[b]  <- 1 - sum(res^2) / sum( (yb - mean(yb))^2 )
  rmse_boot[b] <- sqrt(mean(res^2))
}

alpha <- 0.104
probs <- c(alpha/2, 1 - alpha/2)  # c(0.052, 0.948)

ci_r2   <- round(quantile(r2_boot,  probs, names = FALSE), 4)
ci_rmse <- round(quantile(rmse_boot, probs, names = FALSE), 4)

ci_r2; ci_rmse






