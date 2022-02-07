# 5.1

?choose

choose(45, 6)
choose(45-6, 6)
choose(39,5) * choose(6, 1)
choose(39,4) * choose(6, 2)
choose(39,3) * choose(6, 3)
choose(39,2) * choose(6, 4)
choose(39,1) * choose(6, 5)
choose(39,0) * choose(6, 6)

# 5.2

lotto <- function (x) {
    return (choose(39, 6-x) * choose(6, x))
}

for (x in 0:6) {
    cat(x, lotto(x), '\n')
}

# 5.3

lotto2 <- function (x) {
    return (lotto(x) / choose(45, 6))
}

for (x in 0:6) {
    cat(x, lotto2(x), '\n')
}

print(1 / choose(45, 6))

# 5.4

win <- 0:6
win
prob <- sapply(win, lotto2)
prob
barplot(prob, names=win)

sum(prob)

# 5.5

?dnorm
x <- seq(-3, 3, length=200)
plot(x, dnorm(x, mean=0, sd=1), 
     type='l', 
     main="Normal Distribution")

x <- seq(0, 100, length=200)
plot(x, dnorm(x, mean=50, sd=10), 
     type = 'l',
     main="Normal Distribution(m=50, s=10)")

x <- seq(0, 100, length=200)
plot(x, dnorm(x, mean=50, sd=20), 
     type = 'l',
     main="Normal Distribution(m=50, s=20")

# 5.4

?pnorm

pnorm(35000, 30000, 10000) - pnorm(25000, 30000, 10000)

x <- seq(10000, 50000, length=200)
y <- dnorm(x, mean = 30000, sd = 10000)
plot(x, y, type='l',
     main="pnorm(35000, 30000, 10000)")
xlim <- x[x <= 35000]
ylim <- y[x <= 35000]
xlim <- c(xlim[1], xlim, tail(xlim, 1))
ylim <- c(0, ylim, 0) 
polygon(xlim, ylim, col="grey")

x <- seq(10000, 50000, length=200)
y <- dnorm(x, mean = 30000, sd = 10000)
plot(x, y, type='l',
     main="pnorm(25000, 30000, 10000)")
xlim <- x[x <= 25000]
ylim <- y[x <= 25000]
xlim <- c(xlim[1], xlim, tail(xlim, 1))
ylim <- c(0, ylim, 0) 
polygon(xlim, ylim, col="grey")

x <- seq(10000, 50000, length=200)
y <- dnorm(x, mean = 30000, sd = 10000)
plot(x, y, type='l',
     main="$25000 ~ $35000 사이에 있을 확률")
xlim <- x[25000<=x & x<=35000]
ylim <- y[25000<=x & x<=35000]
xlim <- c(xlim[1], xlim, tail(xlim, 1))
ylim <- c(0, ylim, 0) 
polygon(xlim, ylim, col="grey")

pnorm(25000, 30000, 10000)
1-pnorm(35000, 30000, 10000)

x <- seq(10000, 50000, length=200)
y <- dnorm(x, mean = 30000, sd = 10000)
plot(x, y, type='l')
xlim <- x[x<=25000]
ylim <- y[x<=25000]
xlim <- c(xlim[1], xlim, tail(xlim, 1))
ylim <- c(0, ylim, 0) 
polygon(xlim, ylim, col="grey")

x <- seq(10000, 50000, length=200)
y <- dnorm(x, mean = 30000, sd = 10000)
plot(x, y, type='l')
xlim <- x[x>=35000]
ylim <- y[x>=35000]
xlim <- c(xlim[1], xlim, tail(xlim, 1))
ylim <- c(0, ylim, 0) 
polygon(xlim, ylim, col="grey")


# 5.5

1 - pnorm(70, 60, 10)
1 - pnorm(80, 70, 20)

z1 <- (70 - 60) / 10
z2 <- (80 - 70) / 20
z1
z2

1 - pnorm(z1)
1 - pnorm(z2)

# 5.6

x <- seq(-3, 3, length=200)
curve(dnorm(x), -3, 3, 
      xlab = 'X', ylab = 'Y',
      main="Probability Density Function")

curve(pnorm(x), -3, 3, 
      xlab = 'X', ylab = 'Y',
      main="Cumulative Distribution Function")

# 5.7

n_sim <- 10000
y <- rbinom(n_sim, 100, 0.5)
hist(y, xlab='X', ylab='mass', 
     main = 'B(100, 0.5)', 
     prob = T, 
     breaks = 30)
curve(dnorm(x, 50, 5), 25, 75, 
      lty=2, lwd=2, col='red',
      add=T)

# 5.8

?curve
x <- seq(120, 220, length=1000)
curve(dnorm(x, 160, 5) + dnorm(x, 175, 5), 
      from=120, to=220)

n_sim <- 10000
n <- 30 # 추출할 인원수
means <- vector(length = n_sim)

for (i in 1:n_sim) {
    y <- vector(length = n)
    for (j in 1:30) {
        gender <- rbinom(1, 1, 0.5) # 성별 선택
        if (gender == 0) { # 여성이 선택된 경우
            y[j] <- rnorm(1, 160, 5)
        } else { # 남성이 선택된 경우
            y[j] <- rnorm(1, 175, 5)
        }
    }
    means[i] <- mean(y)
}

hist(means, main = 'Distribution of means', 
     xlab = 'mean_height', ylab = 'prob', prob = T, breaks=50)

curve(dnorm(x, mean(means), sd(means)), from=120, to=220, 
      lty=2, lwd= 2, col='red', add=T)

# 7.1

head(iris)
str(iris)
summary(iris)

y <- subset(iris, Species=='setosa')$Petal.Length
y
mean(y)
sd(y)

n <- length(y)
ci_lower <- mean(y) - 2*sd(y) / sqrt(n)
ci_upper <- mean(y) + 2*sd(y) / sqrt(n)
c(ci_lower, ci_upper)

# 7.2

n_sim <- 10000
means <- c()
for (i in 1:n_sim) {
    bs_sample <- sample(y, length(y), replace=T)
    sample_mean <- mean(bs_sample)
    means <- c(means, sample_mean)
}

c(quantile(means, .025), quantile(means, .975))

c(mean(y) + qt(.025, df = n-1)*sd(y) / sqrt(n), 
  mean(y) + qt(.975, df=n-1)*sd(y) / sqrt(n))

# 7.3

n_sim <- 10000
sds <- c()
for (i in 1:n_sim) {
    bs_sample <- sample(y, length(y), replace=T)
    sample_sd <- sd(bs_sample)
    sds <- c(sds, sample_sd)
}

c(quantile(sds, .025), quantile(sds, .975))

sqrt(var(y)*(n-1) / qchisq(.975, n-1))
sqrt(var(y)*(n-1) / qchisq(.025, n-1))

# 7.4

x <- subset(iris, Species == 'virginica')$Petal.Length
y <- subset(iris, Species == 'versicolor')$Petal.Length

n_sim <- 10000
difs <- c()
for (i in 1:n_sim) {
    bs_virginica <- sample(x, length(x), replace=T)
    bs_setosa <- sample(y, length(y), replace=T)
    mean_dif <- mean(bs_virginica) - mean(bs_setosa)
    difs <- c(difs, mean_dif)
}

c(quantile(difs, .025), quantile(difs, .975))
c(quantile(difs, .005), quantile(difs, .995))

