# Aufgabe 1
# a)
var.1 <- -20 * 15 + 5 * 74
print(var.1)

# c)
print(sqrt(var.1))

# d)
root <- 5
var.2 <- var.1^(1 / root)
print(var.2)

# e
print(var.2 < pi)

# f
var.3 <- (exp(5) - pi^2) / log(10)
print(var.3)

# g
print(ceiling(var.3))

# Aufgabe 2
# a)

x <- c(2, -3, 5, 7, 8, 6, 5, 6, 9, 12)
y <- c(4, 7, 6, 9, 14, 17, 23, 20, 17, 30)

# b)
avg_x <- sum(x) / length(x)
avg_y <- sum(y) / length(y)

print(avg_x)

variance_x <- sum((x - avg_x)^2) / (length(x) - 1)
print(variance_x)

variance_y <- sum((y - avg_y)^2) / (length(y) - 1)
# c)
covariance_xy <- sum((x - avg_x) * (y - avg_y)) / (length(x) - 1)
print(covariance_xy)

# d)
# sd() - standard deviation
correlation_xy <- covariance_xy / (sqrt(variance_x) * sqrt(variance_y))
print(correlation_xy)


# e)
var_x <- var(x)
var_y <- var(y)

mean_x <- mean(x)
mean_y <- mean(y)

cov_xy <- cov(x, y)

cor_xy <- cor(x, y)

# Aufgabe 3
# a)

x1 <- c(-2, 4, 0.5, -4, 1, 2, -0.1, 5, 0.2, 5, -0.4)

# b)

arr <- 1:length(x1)
print(x2 <- x1^(-1) + arr)

# c)

print(x3 <- sort(x2, TRUE))

# d)

print(x4 <- seq(-2, 15, length.out = 11))

# e)

print(x5 <- pmin(x1, x2, x3, x4))

# Aufgabe 4

# a)

seed <- set.seed(321)

print(x1 <- rnorm(100, 4, 4))

# b)

print(summary(x1))

# c)

print(table(x1 < 0))

# d)

hist(x1, freq = FALSE, col = FALSE)

curve(dnorm(x, mean = 4, sd = 4), add = TRUE, col = "red", lwd = 2)
