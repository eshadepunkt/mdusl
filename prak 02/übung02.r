# Aufgabe 1
# a)

print(A <- matrix(c(  2, -3, 4, -3, 0.5, 1, 4, 1, -2),
 nrow = 3, ncol = 3, byrow = TRUE))
print(B <- matrix(c(  8,2,-2,3,1,-3,4,-1,12),
 nrow = 3, ncol = 3, byrow = TRUE))
print(C <- matrix(c( 5, -3, 4, 3, -8,3,-5,1,4,3,2,-3), 
 nrow = 3, ncol = 4, byrow = TRUE))

# A transponiert
print(t(A))
# A + B transponiert

print(D <- A + t(B))
#  ( A + B transponiert ) * C
print(D %*% C)

# A^7

library("expm") # used for %^% operator

print(A%^%7)

# b)

result_a <- t(C) %*% C
result_b <- C %*% t(C)

print(dim(result_a))
print(rankMatrix(result_a))
print(det(result_a))
print(dim(result_b))
print(rankMatrix(result_b))
print(det(result_b))
