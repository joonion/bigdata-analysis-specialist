####################################
## CHAPTER 7. Regression Analysis ##
####################################

###########################################
## 7.1 Simple Linear Regression Analysis ##
###########################################

#install.packages("HistData")
library(HistData)
data(GaltonFamilies)
df <- GaltonFamilies[, c("midparentHeight", "childHeight")]
str(df)
head(df)

cor(df)
plot(df)

model <- lm(childHeight ~ midparentHeight, data=df)
model
plot(df)
abline(model, col="red", lty=2, lwd=2)

install.packages("car")
library(car)
str(Prestige)

Prestige.lm <- lm(income ~ education, data=Prestige)
class(Prestige.lm)
Prestige.lm

# [Figure 7-2]
windows(width=7.0, height=5.5)
plot(Prestige$income ~ Prestige$education,
     col="cornflowerblue", pch=19,
     xlab="Education (years)", ylab="Income (dollars)",
     main="Education and Income")
abline(Prestige.lm, col="salmon", lwd=2)

summary(Prestige.lm)

Prestige.lm.summary <- summary(Prestige.lm)
coef(Prestige.lm.summary)

anova(Prestige.lm)

rownames(anova(Prestige.lm)); colnames(anova(Prestige.lm))
anova(Prestige.lm)["education", "Pr(>F)"]
anova(Prestige.lm)[1, 5]

coef(Prestige.lm)

confint(Prestige.lm)
confint(Prestige.lm, level=0.99)

fitted(Prestige.lm)[1:3]
resid(Prestige.lm)[1:3]

Prestige.new <- data.frame(education=c(5, 10, 15))
predict(Prestige.lm, newdata=Prestige.new)

predict(Prestige.lm, newdata=Prestige.new, interval="confidence")

mean(Prestige$education)
lm(income ~ education, data=Prestige, subset=(education > mean(education)))
lm(income ~ education, data=Prestige, subset=(education <= mean(education)))

########################################
## 7.2 Polynomial Regression Analysis ##
########################################

# [Figure 7-4]
library(car)
windows(width=7.0, height=5.5)
scatterplot(income ~ education, data=Prestige, pch=19, col="orangered", cex=1.2,
            regLine=list(method=lm, lty=2, lwd=3, col="royalblue"),
            smooth=list(smoother=loessLine, spread=FALSE, 
                        lty.smooth=1, lwd.smooth=3, col.smooth="green3"),
            xlab="Education (years)", ylab="Income (dollars)",
            main="Education and Income")

Prestige.poly <- lm(income ~ education + I(education^2), data=Prestige)
summary(Prestige.poly)

# [Figure 7-6]
windows(width=7.0, height=5.5)
plot(Prestige$income ~ Prestige$education, pch=19, col="darkorange",
     xlab="Education (years)", ylab="Income (dollars)",
     main="Education and Income")

library(dplyr)
lines(arrange(data.frame(Prestige$education, fitted(Prestige.poly)), 
              Prestige$education), col="cornflowerblue", lwd=2)

# [Figure 7-7]
windows(width=7.0, height=5.5)
scatterplot(eruptions ~ waiting, data=faithful, pch=19, col="deepskyblue", cex=1.2,
            regLine=list(method=lm, lty=2, lwd=3, col="blueviolet"),
            smooth=list(smoother=loessLine, spread=FALSE, 
                        lty.smooth=1, lwd.smooth=3, col.smooth="coral"),
            xlab="Waiting (minutes)", ylab="Eruptions (minutes)",
            main="Waiting Time Between Eruptions and the Duration of the Eruption")

faithful.poly <- lm(eruptions ~ waiting + I(waiting^2) + I(waiting^3), 
                    data=faithful)
summary(faithful.poly)

faithful.lm <- lm(eruptions ~ waiting, data=faithful)
summary(faithful.lm)

#############################################
## 7.3 Multiple Linear Regression Analysis ##
#############################################

data(mtcars)
str(mtcars)
mtcars <- mtcars[c("mpg", "hp", "wt", "disp", "drat")]
summary(mtcars)
cor(mtcars)

# [Figure 7-8]
library(car)
windows(width=7.0, height=5.5)
scatterplotMatrix(mtcars, pch=19, col="royalblue", cex=1.2,
                  regLine=list(method=lm, lty=1, lwd=3, col="salmon"),
                  smooth=list(smoother=loessLine, spread=FALSE, 
                              lty.smooth=1, lwd.smooth=3, col.smooth="forestgreen"),
                  main="Car Performance")

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
summary(mtcars.lm)

#install.packages("stargazer")
library(stargazer)
stargazer(mtcars.lm, type="text", no.space=TRUE)

mtcars.lm <- lm(scale(mpg) ~ scale(hp) + scale(wt) + scale(disp) + scale(drat), data=mtcars)
summary(mtcars.lm)

install.packages("QuantPsyc")
library(QuantPsyc)
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
lm.beta(mtcars.lm)

# [Figure 7-9]
mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
windows(width=7.0, height=7.0)
old.par <- par(mfrow=c(2, 2))
plot(mtcars.lm)
old.par

library(car)
vif(mtcars.lm)
vif(mtcars.lm) > 4
vif(mtcars.lm) > 10

library(car)
summary(powerTransform(mtcars$mpg))

library(car)
boxTidwell(mpg ~ hp + wt, data=mtcars)

library(car)
windows(width=7.0, height=5.5)
spreadLevelPlot(lm(mpg ~ hp + wt, data=mtcars))

mtcars.lm1 <- lm(mpg ~ hp + wt, data=mtcars)
mtcars.lm2 <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
anova(mtcars.lm1, mtcars.lm2)

AIC(mtcars.lm1, mtcars.lm2)

mtcars.lm <- lm(mpg ~ hp + wt + disp + drat, data=mtcars)
step(mtcars.lm, direction="backward")

install.packages("leaps")
library(leaps)
mtcars.regsubsets <- regsubsets(x=mpg ~ hp + wt + disp + drat, data=mtcars, nbest=4)

# [Figure 7-11]
windows(width=7.0, height=5.5)
library(RColorBrewer)
plot(mtcars.regsubsets, scale="adjr2", col=brewer.pal(9, "Pastel1"),
     main="All Subsets Regression")

summary(mtcars.regsubsets)

names(summary(mtcars.regsubsets))

summary(mtcars.regsubsets)$adjr2

which.max(summary(mtcars.regsubsets)$adjr2)
coef(mtcars.regsubsets, 9)

str(InsectSprays)
levels(InsectSprays$spray)

tapply(InsectSprays$count, InsectSprays$spray, mean)

sprays.lm <- lm(count ~ spray, data=InsectSprays)
summary(sprays.lm)

contrasts(InsectSprays$spray)

sprays.aov <- aov(count ~ spray, data=InsectSprays)
summary(sprays.aov)
TukeyHSD(sprays.aov)

respray <- relevel(InsectSprays$spray, ref=6)
sprays.lm <- lm(count ~ respray, data=InsectSprays)
summary(sprays.lm)

contrasts(relevel(InsectSprays$spray, ref=6))
