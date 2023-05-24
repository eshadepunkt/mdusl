# Aufgabe 1
# a)

# View(cars)

# b)

x <- cars$speed
y <- cars$dist

print(var_x <- var(x))
print(var_y <- var(y))
print(mean_x <- mean(x))
print(mean_y <- mean(y))
print(cov(x, y))
print(r <- cor(x, y))

# c)
# (i)
print("=========================================")
print(b <- r * (sqrt(var_y / var_x)))
print(a <- mean_y - b * mean_x)

plot(x, y, xlab = "speed", ylab = "dist", main = "cars")
# abline(a, b)

# (ii)
print("================ Design Matrix =========================")
## Design matrix
library("expm")
X <- cbind(1, x)
linear <- solve((t(X) %*% X)) %*% t(X) %*% y
print(linear)

abline(linear[1], linear[2], col = "blue")
# (iii)
# linear model
print("=============== Linear Model ==========================")
print(summary(t <- lm(y ~ x)))
# abline(t, col = "red")

# d)
# variance of errors

print("=============== Variance of Errors ==========================")

print(var_e <- summary(t)$sigma^2)

# e) confidence interval for a and b

print("=============== Confidence Interval ==========================")

print(confint(t, level = 0.95, digits = 3))

# f)

print("=============== SSE, SST, SSR ==========================")

print(SSE <- summary(t)$sigma^2 * (nrow(X) - 2)) # sum of squared errors
print(SST <- sum(y^2)) # sum of squared total
print(SSR <- SST - SSE) # sum of squared regression

# g) normal distribution
print("=============== Normal Distribution ==========================")

# (i)

# standardized residues

resid <- rstandard(t)
# (ii)
plot(x, resid, xlab = "speed", ylab = "resid", main = "cars")
# (iii)
hist(resid, main = "Histogram of standardized residues")
# (iv) q-q-plot
qqplot(resid, rnorm(length(resid)), main = "Q-Q-Plot of standardized residues")
# (v) Kolmogorov-Smirnov-Test
# unique
# print(ks.test(resid, "pnorm", mean(resid), sd(resid)))

unique_resid <- unique(resid)
print(ks.test(unique_resid, "pnorm", mean(unique_resid), sd(unique_resid)))

# h) significance test
print("=============== Significance Test ==========================")
# Niveau 0.05 b != 0

# (i) t-test
# print(summary(t))

# Ermitteln der t- und p-Werte für das Absolutglied (Intercept)
p_value_intercept <- summary(t)$coefficients["(Intercept)", "Pr(>|t|)"]

cat("p-Wert für das Absolutglied (Intercept):", p_value_intercept, "\n")

# TODO: p-Wert für die Steigung (Slope)

# (i)

speed_value <- 21

print(s <- sqrt(var_e * (1 + 1 / nrow(X) + (speed_value - mean_x)^2 / ((nrow(X) - 1) * var_x))))

t_quantile <- qt(0.95, df = nrow(X) - 2)

print(D <- t_quantile * s * sqrt(1 + (1 / nrow(X) + (speed_value - mean_x)^2 / ((nrow(X) - 1) * var_x))))

prog_upper <- summary(t)$coefficients["(Intercept)", "Estimate"] + summary(t)$coefficients["x", "Estimate"] * speed_value + D
prog_lower <- summary(t)$coefficients["(Intercept)", "Estimate"] + summary(t)$coefficients["x", "Estimate"] * speed_value - D

print(prog_lower)
print(prog_upper)

# j) streudiagramm mit regressionsgerade, konfidenzintervall und prognoseintervall

plot(x, y, xlab = "speed", ylab = "dist", main = "cars")
abline(linear[1], linear[2], col = "black")
abline(prog_upper, linear[2], col = "red")
abline(prog_lower, linear[2], col = "red")


# TODO: Grafik überprüfen
