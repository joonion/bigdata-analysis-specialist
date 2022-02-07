#######################
## CHAPTER 3. t Test ##
#######################

################
## 3.1 t Test ##
################

x <- seq(-3, 3, length=200)
y <- dt(x, df=4)
plot(x, y, 
     main="PDF of t-distribution",
     type='l', lty=3, lwd=2, col="darkgreen")
lines(x, dt(x, df=2), 
      lty=2, lwd=2, col="blue")
lines(x, dt(x, df=1), 
      lty=1, lwd=2, col="red")
legend("topright", 
       c("df=4", "df=2", "df=1"),
       col=c("darkgreen", "blue", "red"),
       lty=c(3, 2, 1))

t <- (135-115)/(25/sqrt(20))
t

pt(3.58, df=19, lower.tail=FALSE)*2

qt(0.025, df=19, lower.tail=FALSE)

qt(0.005, df=19, lower.tail=FALSE)


x <- seq(-4, 4, length=300)
y <- dt(x, df=19)
plot(x, y, type='l',
     main="t-distribution (df=19)")

xlim <- x[-4<=x & x<=-2.09]
ylim <- y[-4<=x & x<=-2.09]
xlim <- c(xlim[1], xlim, tail(xlim, 1))
ylim <- c(0, ylim, 0) 
polygon(xlim, ylim, col="grey")

xlim <- x[2.09<=x & x<=4]
ylim <- y[2.09<=x & x<=4]
xlim <- c(xlim[1], xlim, tail(xlim, 1))
ylim <- c(0, ylim, 0) 
polygon(xlim, ylim, col="grey")

###########################
## 3.2 One-Sample t Test ##
###########################

library(MASS)
str(cats)

mean(cats$Bwt)

t.test(x=cats$Bwt, mu=2.6)

t.test(cats$Bwt, mu=2.7)

t.test(cats$Bwt, mu=2.6, alternative="greater")

cats.t <- t.test(cats$Bwt, mu=2.6)
str(cats.t)

cats.t$p.value
cats.t$conf.int

t.test(cats$Bwt, mu=2.6, conf.level=0.99)

prop.test(x=18, n=30, p=0.5, alternative="greater")

########################################
## 3.3 Two-Independent Samples t Test ##
########################################

result <- t.test(formula=Bwt ~ Sex, data=cats)

# [Figure 3-6]
?tapply
bars <- tapply(cats$Bwt, cats$Sex, mean)
bars
result$conf.int[1]

lower <- tapply(cats$Bwt, cats$Sex, function(x) t.test(x)$conf.int[1])
lower
upper <- tapply(cats$Bwt, cats$Sex, function(x) t.test(x)$conf.int[2])
upper

#install.packages("gplots")
library(gplots)
windows(width=4.0, height=5.5)
barplot2(bars, space=0.4, ylim=c(0, 3.0),
         plot.ci=TRUE, ci.l=lower, ci.u=upper, ci.color="maroon", ci.lwd=4, 
         names.arg=c("Female", "Male"), col=c("coral", "darkkhaki"),
         xlab="Cats", ylab="Body Weight (kg)", 
         main="Body Weight by Sex\nwith Confidence Interval")

Bwt.f <- cats$Bwt[cats$Sex=="F"]
Bwt.m <- cats$Bwt[cats$Sex=="M"]
t.test(Bwt.f, Bwt.m)

smokers  <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)

prop.test(x=smokers, n=patients)

###############################
## 3.4 Paired-Samples t Test ##
###############################
?sleep
str(sleep)
sleep[seq(1, 20, 2), ]

t.test(extra ~ group, data=sleep, paired=TRUE)

#install.packages("tidyr")
library(tidyr)
sleep.wide <- spread(sleep, key=group, value=extra)
sleep.wide

#install.packages("reshape2")
library(reshape2)
sleep.wide <- dcast(sleep, ID ~ group, value.var="extra")
sleep.wide

t.test(sleep.wide$'1', sleep.wide$'2', paired=TRUE)

sink()
