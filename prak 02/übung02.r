# Aufgabe 1
# a)

print(A <- matrix(c(2, -3, 4, -3, 0.5, 1, 4, 1, -2),
  nrow = 3, ncol = 3, byrow = TRUE
))
print(B <- matrix(c(8, 2, -2, 3, 1, -3, 4, -1, 12),
  nrow = 3, ncol = 3, byrow = TRUE
))
print(C <- matrix(c(5, -3, 4, 3, -8, 3, -5, 1, 4, 3, 2, -3),
  nrow = 3, ncol = 4, byrow = TRUE
))

# A transponiert
print(t(A))
# A + B transponiert

print(D <- A + t(B))
#  ( A + B transponiert ) * C
print(D %*% C)

# A^7

library("expm") # used for %^% operator

print(A %^% 7)

# b)

result_a <- t(C) %*% C
result_b <- C %*% t(C)

print("Dimensionen der Matrizen:")
print(dim(result_a))
print("Rang der Matrizen:")
print(rankMatrix(result_a))
print("Determinanten der Matrizen:") # Determinante sollte 0 sein, da Rang < Dimension(quadratischer Matrizen)
print(det(result_a))
print("=====================================")
print("Dimensionen der Matrizen:")
print(dim(result_b))
print("Rang der Matrizen:")
print(rankMatrix(result_b))
print("Determinanten der Matrizen:")
print(det(result_b))

# c)
# check for inverse of matrix B
if (det(B) != 0) {
  print("Inverse of B:")
  print(inverseB <- solve(B))
} else {
  print("B is not invertible")
}
# check if result is correct
print("B * B^-1:")
print(B %*% solve(B))

# d)
# resolve linear equation system
print("Solve linear equation system:")
y <- c(100, 200, 300)

print("x = ")
tempMatrix <- C %*% (t(C) %*% A - 3 * t(C) %*% B)
print(round(solve(tempMatrix) %*% y, digits = 2))

# e)
# find another solution for linear equation system

print("--------------------")
print(round(solve(tempMatrix, y), digits = 2))

# f) find eigenvalues and eigenvectors of matrix A, is A positive definite?

print("Eigenvalues of A:")
print(eigenvalues <- eigen(A)$values)
print("Eigenvectors of A:")
print(eigenvectors <- eigen(A)$vectors)

# check if A is positive definite
if (all(eigenvalues > 0)) {
  print("A is positive definite")
} else {
  print("A is not positive definite")
}

# Aufgabe 2
# a)

# write a function that takes a start value and a natural number as parameters.
# The function should return 4 times the squareroot of the startValue plus the startValue

dörte <- function(startValue, iterations) {
  if (iterations == 0) {
    return(startValue)
  } else {
    return(dörte(4 * sqrt(startValue) + startValue, iterations - 1))
  }
}

print(paste("dörte(6.5, 8)", dörte(6.5, 8), sep = " "))

# b)
# Write a function that takes a natural number as parameter that represents the member of the fibonacci sequence.
# The function should return all the fibonacci numbers till that given parameter.
# e.g. fibonacci(6) should return 0,1,1,2,3,5


fibonacci <- function(n) {
  a <- 1
  b <- 1
  fib <- numeric(n)

  if (n >= 1) {
    fib[1] <- a
  }

  if (n >= 2) {
    fib[2] <- b
  }

  for (i in 3:n) {
    fib[i] <- a + b
    a <- b
    b <- fib[i]
  }

  return(fib)
}

print(paste("fibonacci(6)", toString((fibonacci(6))), sep = " "))


# c)

find_largest_element <- function(M, k) {
  # Berechnen Sie Mk
  Mk <- M %^% k

  # Finden Sie den Index des größten Elements in Mk
  max_index <- which(Mk == max(abs(Mk)), arr.ind = TRUE)

  # Extrahieren Sie den Wert des größten Elements und dessen Zeilen-/Spaltenindizes
  max_value <- Mk[max_index]
  row_index <- max_index[1]
  col_index <- max_index[2]

  # Gib das Ergebnis zurück
  result <- list("Maximaler Wert" = max_value, "Zeilenindex" = row_index, "Spaltenindex" = col_index)
  return(result)
}

## Aufgabe 3

# a)

wine <- read.csv("data/whitewines.csv", header = TRUE, sep = ",")

# b)

print(head(wine))
print(tail(wine))
print(wine[c(1, 4, 5), 1:4])
print(wine[c(1, 4, 5), -c(3, 4, 7, 8)])

# c)

print(nrow(wine_alcohol_gt_10 <- subset(wine, alcohol > 10)))

# d)

print(nrow(wine_alcohol_gt_10_and_quality_lt_5 <- subset(wine, alcohol > 10 & quality < 5)))

# e)
# find the wines with the highest alcohol content

print(head(wine_alcohol_gt_10_and_quality_lt_5[order(wine_alcohol_gt_10_and_quality_lt_5$pH, decreasing = TRUE), c(1, 2, 5, 6, 9)], n = 1))

# f)
# mean, variance, standard deviation for alcohol content

print(paste("Mean alcohol content:", mean(wine$alcohol), sep = " "))
print(paste("Variance alcohol content:", var(wine$alcohol), sep = " "))
print(paste("Standard deviation alcohol content:", sd(wine$alcohol), sep = " "))

# g)

print(table_quality <- table(wine$quality))

# h)

wine$strong <- ifelse(wine$alcohol > 10, TRUE, FALSE)
wine$tasty <- ifelse(wine$quality > 5, TRUE, FALSE)

# Kreuztabelle erstellen
print(table(wine$strong, wine$tasty))

# Chancenverhältnis berechnen
library(epitools)
print(oddsratio(wine$strong, wine$tasty))

# Aufgabe 4
# a)
