# 2.1
x <- 6
A <- x * x
A

# 2.2
v <- 10:15
v
v[1]
v[6]
length(v)
sum(v)
mean(v)

v <- c(10, 20, 40, 70, 90)
v
v[1]
v[length(v)]
var(v)
sd(v)

# 2.3
v <- c(1)
for (i in 2:10) {
    v[i] <- v[i - 1] + 4 * (i - 1)
}
print(v)
print(sum(v))

# 2.4
n <- 10
factorial <- 1
for (i in 1:n) {
    factorial <- factorial * i
}
factorial

# 2.5
fact <- function (n) {
    factorial <- 1
    for (i in 1:n) {
        factorial <- factorial * i
    }
    return (factorial)
}

fact(10)

# 2.6
fact2 <- function (n) {
    if (n == 1)
        return (1)
    else
        return (5 * fact(n - 1))
}

fact2(10)


n <- 10
s <- (n*(n+1)*(2*n+1)/3) - n*(n+1) + n
s

n <- 10
s <- (2*(n^3) + n) / 3
s

# 2.7
perm1 <- function (n, r) {
    perm <- 1
    for (i in (n-r+1):n) {
        perm <- perm * i
    }
    return (perm)
}

perm1(10, 2)

# 2.8
perm2 <- function (n, r) {
    return (fact(n) / fact(n - r))
}

perm2(10, 2)

# 2.9
comb1 <- function (n, r) {
    return (perm1(n, r) / fact(r))
}

comb1(10, 2)

comb2 <- function (n, r) {
    return (fact(n) / fact(r) / fact(n - r))
}

comb2(10, 2)

# 2.10
comb3 <- function (n, r) {
    if (r == 0 || r == n)
        return (1)
    else
        return (comb3(n - 1, r) + comb3(n - 1, r - 1))
}

comb3(10, 2)

# 2.11
comb1(45, 6)
comb2(45, 6)
comb3(45, 6)

# 2.12
v <- c(1, 2, 3, 4, 5)
count <- 0
for (i in 1:length(v)) {
    x <- v[v != i]
    for (j in 1:length(x)) {
        print(c(i, x[j]))
        count <- count + 1
    }
}
print(count)

# 2.13
v <- c(1, 2, 3, 4, 5)
count <- 0
for (i in 1:(length(v) - 1)) {
    for (j in (i + 1):length(v)) {
        print(c(i, j))
        count <- count + 1
    }
}
print(count)

# 2.14
install.packages('gtools')
library(gtools)

v <- c(1, 2, 3, 4, 5)
p <- permutations(n=5, r=2, v=v, repeats.allowed=F)
p
nrow(p)

v <- c(1, 2, 3, 4, 5)
c <- combinations(n=5, r=2, v=v, repeats.allowed=F)
c
nrow(c)
