# 3.1

?rbinom

x <- rbinom(10, 1, 0.5)
x
sum(x)/10
mean(x)

x <- rbinom(100, 1, 0.5)
mean(x)

x <- rbinom(10000, 1, 0.5)
mean(x)

# 3.2

?sample

n_simulation <- 1000000
n_success <- 0
for (i in 1:n_simulation) {
    x <- sample(1:5, 3, replace=F)
    if (x[1] >= 4) {
        n_success <- n_success + 1
    } else if (x[1] == 3 & (x[2] >= 2)) {
        n_success <- n_success + 1
    }
}

n_success
n_success / n_simulation

sample(1:10, 10, replace=T)
sample(1:10, 10, replace=F)

# 3.3

?runif

n_sim <- 1000
x <- vector(length = n_sim)
y <- vector(length = n_sim)
res <- 0
for (i in 1:n_sim) {
    x[i] <- runif(1)
    y[i] <- runif(1)
    if (x[i]^2 + y[i]^2 < 1) {
        res <- res + 1
    }
}
4 * res / n_sim

circle <- function (x) sqrt(1 - x^2)
plot(x, y, xlab='X', ylab='Y', col='blue')
curve(circle, from = 0, to = 1, add=T, col='red', lwd=2)

# 3.4

n_sim <- 100000
doors <- 1:3
success <- 0

for (i in 1:n_sim) {
    car <- sample(doors, 1) # 자동차의 위치
    if (car == 1) goat <- c(2, 3)
    else if (car == 2) goat <- c(1, 3)
    else if (car == 3) goat <- c(1, 2)
    
    pick = sample(doors, 1) # 참가자가 고른 문
    goat_not_picked = goat[goat != pick]
    
    # 고르지 않은 문 중에서 염소의 문 하나를 열어줌
    if (length(goat_not_picked) > 1) {
        open <- sample(goat_not_picked, 1)
    } else open <- goat_not_picked
    
    if (pick == car) success <- success + 1
}

success / n_sim


# 3.5

n_sim <- 100000
doors <- 1:3
success <- 0

for (i in 1:n_sim) {
    car <- sample(doors, 1) # 자동차의 위치
    if (car == 1) goat <- c(2, 3)
    else if (car == 2) goat <- c(1, 3)
    else if (car == 3) goat <- c(1, 2)
    
    pick = sample(doors, 1) # 참가자가 고른 문
    goat_not_picked = goat[goat != pick]
    
    # 고르지 않은 문 중에서 염소의 문 하나를 열어줌
    if (length(goat_not_picked) > 1) {
        open <- sample(goat_not_picked, 1)
    } else open <- goat_not_picked
    
    # 참가자가 고른 문을 바꾼다
    pick = doors[(doors != pick) & (doors != open)]
    
    if (pick == car) success <- success + 1
}

success / n_sim

# 3.6
table <- apply(UCBAdmissions, c(1, 2), sum)
mosaicplot(table, 
           main = "Admission at UC Berkeley, 1973", 
           col = c("red", "blue"))

par(mfrow = c(2, 3))
for (i in 1:6) {
    table <- UCBAdmissions[,,i]
    mosaicplot(table, 
               main = paste("Department", LETTERS[i]), 
               col = c("red", "blue"))
}
par(mfrow = c(1, 1))

# Ex.3.1

n_sim <- 10000
trial <- sample(1:6, n_sim, replace=T)
success <- trial[trial %in% c(1, 3, 5)]
length(success) / n_sim

# Ex.3.2

n_sim <- 1000000
success <- 0
for (i in 1:n_sim) {
    trial <- sample(1:6, 1)
    if (trial %in% c(2, 3, 5)) {
        if (trial %% 2 == 0) {
            success <- success + 1
        }
    }
}

success / n_sim
