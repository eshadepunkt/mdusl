# Aufgabe 1
# a)

alpha <- 25
beta <- 0.5
variance <- 0.1

# vector length 100

x <- seq(0, 10, length.out = 100)

# b) independent normal distributed random variables

u <- rnorm(mean = 0, sd = sqrt(variance), n = length(x))


# c) y = alpha + beta * x + u

y <- alpha + beta * x + u

# d) plot

plot(x, y, xlab = "x", ylab = "y", main = "x vs y")

# e) linear model

print("=============== Linear Model ==========================")
print(summary(t <- lm(y ~ x)))

# add linear model to plot
abline(t, col = "red")

# f) generate 20 times new data for x, u and y and add each iteration to the plot
# generate an array like the rainbow


color_array <- c("red", "blue", "green", "yellow", "black")

for (i in 1:20) {
    x <- seq(0, 10, length.out = 100)
    u <- rnorm(mean = 0, sd = sqrt(variance), n = length(x))
    y <- alpha + beta * x + u
    # points(x, y, col = color_array[i %% length(color_array)])
    points(x, y, col = "yellow")
}

# g)

# confidence stripe and prognosed stripe of niveau 0.9

# confidence stripe
# confidence interval for beta

print("=============== Confidence Interval ==========================")

print(conf <- predict(t, interval = "confidence", level = 0.9))

# prognosed stripe
print("=============== Prognosed Stripe ==========================")

print(pred <- predict(t, interval = "prediction", level = 0.9))

# plot  confidence stripe and prognosed stripe

lines(x, pred[, 2], col = "blue")
lines(x, pred[, 3], col = "blue")
lines(x, conf[, 2], col = "green")
lines(x, conf[, 3], col = "green")

# h) predict interval

print("=============== Predict Interval ==========================")

print(x_0 <- x[runif(1, 1, length(x))])

print(x_0_prediction <- predict(
    t,
    newdata = data.frame(x = x_0),
    interval = "prediction",
    level = 0.9
))

count_is_in_interval <- 0

plot(x, y, xlab = "x", ylab = "y", main = "x vs y")
abline(t, col = "red")
lines(x, pred[, 2], col = "blue")
lines(x, pred[, 3], col = "blue")


for (i in 1:100000) {
    u <- rnorm(mean = 0, sd = sqrt(variance), n = length(x))
    y <- alpha + beta * x + u
    if (y > x_0_prediction[2] && y < x_0_prediction[3]) {
        count_is_in_interval <- count_is_in_interval + 1
    }
}

print(count_is_in_interval / 10000)


# i) confidence interval

print("=============== Confidence Interval ==========================")

print(x_0 <- x[runif(1, 1, length(x))])

print(x_0_confidence <- predict(
    t,
    newdata = data.frame(x = x_0),
    interval = "confidence",
    level = 0.9
))

count_is_in_interval <- 0

abline(t, col = "red")
lines(x, conf[, 2], col = "green")
lines(x, conf[, 3], col = "green")

for (i in 1:100000) {
    u <- rnorm(mean = 0, sd = sqrt(variance), n = length(x))
    y <- alpha + beta * x + u
    if (y > x_0_confidence[2] && y < x_0_confidence[3]) {
        count_is_in_interval <- count_is_in_interval + 1
    }
}

print(count_is_in_interval / 10000)


# j) null hypothesis, significance level 0.05, beta from a)

print("=============== Null Hypothesis ==========================")

print("H0: beta = 0.5")

# determine p value

print("=============== P Value ==========================")

print(p_value <- summary(t)$coefficients[2, 4])

if (p_value < 0.05) {
    print("H0 is rejected")
} else {
    print("H0 is not rejected")
}

# k) repeat 200 times

print("=============== P Value ==========================")
epsilon <- 0.05

# t distribution
degree_of_freedom <- length(x) - 2

quantile <- qt(1 - epsilon / 2, df = degree_of_freedom)

p_value_array <- c()
h0_rejected <- 0
h0_not_rejected <- 0
total <- 0

for (i in 1:2000) {
    x <- seq(0, 10, length.out = 100)
    u <- rnorm(mean = 0, sd = sqrt(variance), n = length(x))
    y <- alpha + beta * x + u
    t <- lm(y ~ x)

    beta_estimator <- cov(x, y) / var(x)
    beta_0 <- beta
    beta_estimator_standard_deviation <- sqrt(variance / sum((x - mean(x))^2))
    test_statistic <- (beta_estimator - beta_0) / beta_estimator_standard_deviation


    if (test_statistic < -quantile || test_statistic > quantile) {
        h0_rejected <- h0_rejected + 1
    } else {
        h0_not_rejected <- h0_not_rejected + 1
    }
    total <- total + 1
}

print("=============== H0 Rejected ==========================")

print(h0_rejected)
print(h0_rejected / total)

print("=============== H0 Not Rejected ==========================")

print(h0_not_rejected)
print(h0_not_rejected / total)
# Aufgabe 2

# design matrix X

print(X <- cbind(
    c(1, 1, 1, 1, 1, 1, 1, 1),
    c(0, 1, 2, 3, 4, 0, 3, 1),
    c(0, 4, 3, 1, 0, 0, 2, 1),
    c(1, 1, 1, 0, 0, 0, 0, 0),
    c(0, 0, 0, 1, 1, 1, 1, 1)
))

y <- c(19, 17, 31, 30, 16, 16, 22, 21)

# a) X^T * X not invertible
# TODO: reconsider this
# print("=============== X^T * X ==========================")

# if (rank(t(X) %*% X) < length(X)) {
#     print("X^T * X is not invertible")
# } else {
#     print("X^T * X is invertible")
# }
# invert matrix

print("=============== Invert Matrix ==========================")

#print(inverse <- solve(t(X) %*% X))

# b) 
library(MASS)

print(inverseMASS <- ginv(t(X) %*% X))

library(matlib)

print(inverse_matlib <- Ginv(t(X) %*% X))


# c) normal equation

print("=============== Normal Equation ==========================")

print(beta <- inverseMASS %*% t(X) %*% y)
print(theta <- inverse_matlib %*% t(X) %*% y)

print(sse <- sum((y - X %*% beta)^2))

# d) prediction

print("=============== Prediction ==========================")

print(x_0 <- c(1, 2, 1, 1, 0))

print(y_0 <- x_0 %*% beta)

# e) regression

print(lm <- lm(y ~ X - 1))

# Aufgabe 3

# a) design matrix X

print(X <- X[,-5])

# KQ-Estimator

print("=============== KQ-Estimator ==========================")

print(beta_hat <- ginv(t(X) %*% X) %*% t(X) %*% y)

# b) prediction

print("=============== Prediction ==========================")

print(x_0 <- c(1, 2, 1, 1))

print(y_0 <- x_0 %*% beta_hat)

# c) regression

print(lm <- lm(y ~ X - 1))
