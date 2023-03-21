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

correlation_xy <- covariance_xy / (sqrt(variance_x) * sqrt(variance_y))
print(correlation_xy)


# e)
var_x <- var(x)
var_y <- var(y)

mean_x <- mean(x)
mean_y <- mean(y)

cov_xy <- cov(x, y)

cor_xy <- cor(x, y)
