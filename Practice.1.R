div_count <- function (n) sum(n %% 1:n == 0)
div_count(2)
div_count(9)
div_count(10)

prime_count <- function (n) sum(sapply(1:n, div_count) == 2)
prime_count(10)
prime_count(100)

mtcars
str(mtcars)
summary(mtcars)
mtcars[mtcars$gear == 4, ]
min.mpg <- min(mtcars$mpg)
mtcars[mtcars == min.mpg, ]
mtcars[mtcars == min.mpg, c("mpg", "cyl", "hp", "wt")]
mean(mtcars$mpg)
var(mtcars$mpg)
sd(mtcars$mpg)
mean(mtcars[mtcars$mpg > median(mtcars$mpg), c("mpg")])
sd(mtcars[mtcars$mpg > median(mtcars$mpg), c("mpg")])
hist(mtcars$mpg)
barplot(table(mtcars$gear))

st <- as.data.frame(state.x77)
str(st)
head(st)
cor(st)

?state.x77

plot(st)

