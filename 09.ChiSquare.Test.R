######################################################
## CHAPTER 5. Independence and Goodness of Fit Test ##
######################################################

#########################
## 5.1 Chi-square Test ##
#########################

sink("Chap.05.txt")

survivors <- matrix(c(1443, 151, 47, 1781, 312, 135), ncol=2)
dimnames(survivors) <- list("Status"=c("minor injury", "serious injury", "dead"),
                            "Seatbelt"=c("With Seatbelt", "Without Seatbelt"))
survivors

addmargins(survivors)
addmargins(prop.table(addmargins(survivors, 2), 2), 1)


x <- seq(0, 20, length=200)
y <- dchisq(x, df=2)
plot(x, y, 
     main="PDF of chi-square distribution",
     type='l', lty=1, lwd=2, col="red")
lines(x, dchisq(x, df=5), 
      lty=2, lwd=2, col="blue")
lines(x, dchisq(x, df=10), 
      lty=3, lwd=2, col="forestgreen")
legend("topright", 
       c("df=2", "df=5", "df=10"),
       col=c("red", "blue", "forestgreen"),
       lty=c(1, 2, 3))

expected <- matrix(c(1367, 1855.9, 196.9, 267.4, 77.1, 104.7), ncol=2, byrow = T)
dimnames(expected) <- list("Status"=c("minor injury", "serious injury", "dead"),
                           "Seatbelt"=c("With Seatbelt", "Without Seatbelt"))
expected

chisqr <- sum((survivors - expected)^2 / expected)
chisqr

# [Figure 5-1]
windows(width=9.0, height=5.0)
par(mfrow=c(1, 2))
barplot(survivors, ylim=c(0, 2500), las=1, 
        col=c("yellowgreen", "lightsalmon", "orangered"),
        ylab="Frequency", main="Frequency of Survivors")
legend(0.2, 2500, rownames(survivors), 
       fill=c("yellowgreen", "lightsalmon", "orangered"))
survivors.prop <- prop.table(survivors, 2)
barplot(survivors.prop*100, las=1, col=c("yellowgreen", "lightsalmon", "orangered"),
        ylab="Percent", main="Percent of Survivors")
par(mfrow=c(1, 1))

pchisq(45.91, df=2, lower.tail=FALSE)

qchisq(0.05, df=2, lower.tail=FALSE)

x <- seq(0, 8, length=500)
y <- dchisq(x, df=2)
plot(x, y, type='l',
     main="chi-square distribution (df=2)")

xlim <- x[5.99<=x & x<=8]
ylim <- y[5.99<=x & x<=8]
xlim <- c(xlim[1], xlim, tail(xlim, 1))
ylim <- c(0, ylim, 0) 
polygon(xlim, ylim, col="grey")



###########################
## 5.2 Independence Test ##
###########################

str(Titanic)
Titanic

Titanic.margin <- margin.table(Titanic, margin=c(4, 1))
Titanic.margin

addmargins(Titanic.margin)
addmargins(prop.table(addmargins(Titanic.margin, 2), 2), 1)

chisq.test(Titanic.margin)

library(vcd)
assocstats(Titanic.margin)

# [Figure 5-4]
library(vcd)
windows(width=7.0, height=5.5)
mosaic(Titanic.margin, shade=TRUE, legend=TRUE)
mosaic(~ Survived + Class, data=Titanic.margin, shade=TRUE, legend=TRUE)

library(MASS)
str(survey)

with(survey, chisq.test(Fold, Sex))

crosstab <- with(survey, table(Fold, Sex))
crosstab
chisq.test(crosstab)

##############################
## 5.3 Goodness of Fit Test ##
##############################

chisq.test(c(60, 55, 35))

oc <- c(60, 55, 35)
null.p <- c(0.45, 0.30, 0.25)
chisq.test(oc, p=null.p)

chisq.test(oc, p=c(45, 25, 15)/85)

str(HairEyeColor)

hairs <- margin.table(HairEyeColor, margin=1)
hairs

chisq.test(hairs, p=c(0.25, 0.50, 0.10, 0.15))

library(MASS)
smokers <- table(survey$Smoke)
smokers

chisq.test(smokers, p=c(0.1, 0.7, 0.10, 0.10))

sink()
