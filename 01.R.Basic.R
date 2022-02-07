# 1.1
print("Hello, World!")

# 1.2
3 + 4
3 + 4 == 7

# 1.3
head(iris)
plot(iris)
View(iris)

# 1.4
sum <- 0
for (i in 1:10) {
    sum <- sum + i
}
print(sum)

sum(1:10)

# 1.5
v1 <- vector(length=2)
v1
v2 <- 1:10
v2
v3 <- c(1, 2, 3, 5, 7)
v3
v4 <- seq(from=1, to=10, by=2)
v4
v5 <- rep(1:3, each=2, times=2)
v5

#1.6
v1 <- c(T, T, F, F, T)
v1
v2 <- c(T, F, 3, 3.14)
v2
v3 <- c(3, 3.14, "PI=3.14")
v3
v4 <- c(T, F, 3, "3.14")
v4

#1.7
v <- c(10, 20, 30, 40, 50, 60, 70)
v[1]
v[7]
v[1:3]
v[c(1, 3, 5, 7)]
v[c(T, T, F, F, F, F, T)]
v < 30
v[v < 30]
v < 30 | v > 50
v[v < 30 | v > 50]
v[v %% 3 == 0 & v %% 4 == 0]

# 1.8
v1 <- c(1, 2, 3, 4, 5)
v2 <- c(1, 2)
v3 <- v1 + v2
v3
v4 <- 2 * v1
v4
v5 <- v1 * v2
v5

# 1.9
f1 <- factor(c('A', 'B', 'AB', 'B', 'O', 'A'))
f1
levels(f1)
table(f1)
f2 <- factor(c(1, 2, 2, 1, 2), labels=c('M', 'F'))
f2
levels(f2)
table(f2)

# 1.10
lst <- list(v1=1:3, v2=vector(length=2), v3=c('A', 'B', 'C'))
lst
length(lst)
lst$v1
lst[3]
lst[[3]]
lst$v3[2]
lst[[3]][2]
lst[c(1,3)][2]
lst[c("v1", "v3")]

# 1.11
head(iris)
str(iris)
summary(iris)

iris$Sepal.Length
iris[, 1:4]
iris[, -5]
iris[1:50, ]
iris[1:50, c(1, 2, 5)]
iris[iris$Species=='versicolor', c(1, 2, 5)]
