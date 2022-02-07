######################
## CHAPTER 4. ANOVA ##
######################

################
## 4.2 F Test ##
################

adhd <- data.frame(score=c(95,105,98,103,107,110,125,105,113,120),
                   therapy=c(rep("A", 5), rep("B", 5)))

mean(adhd$score[1:5])
sd(adhd$score[1:5])

mean(adhd$score[6:10])
sd(adhd$score[6:10])

mstr <- ((101.6-108.1)^2*5+(114.6-108.1)^2*5)/(2-1)
mstr

g <- 2
ng <- c(5, 5)
mg <- c(mean(adhd$score[1:5]), mean(adhd$score[6:10]))
m <- mean(adhd$score)
mstr <- sum(((mg-m)^2*ng) / (g-1))
mstr

Xg1 <- adhd$score[1:5]
Xg2 <- adhd$score[6:10]
mse <- (sum((Xg1-mg[1])^2) + sum((Xg2-mg[2])^2)) / sum(ng - 1)
mse

mse <- (4.98^2*4+7.96^2*4)/(4+4)
mse <- ((95-101.6)^2+(105-101.6)^2+(98-101.6)^2+(103-101.6)^2+(107-101.6)^2+(110-114.6)^2+(125-114.6)^2+(105-114.6)^2+(113-114.6)^2+(120-114.6)^2)/(4+4)
mse

F <- mstr/mse
F

pf(F, df1=1, df=8, lower.tail=FALSE)

pf(9.59, df1=1, df=8, lower.tail=FALSE)
qf(0.05, df1=1, df=8, lower.tail=FALSE)

adhd
str(adhd)
adhd.aov <- aov(score ~ therapy, data=adhd)
summary(adhd.aov)

tapply(adhd$score, adhd$therapy, mean)
tapply(adhd$score, adhd$therapy, sd)
mean(adhd$score)
sd(adhd$score)

?df

x <- seq(0, 4, length=200)
y <- df(x, df1=1, df2=30)
plot(x, y, 
     ylim = c(0, 1.2),
     main="PDF of F distribution",
     type='l', lty=1, lwd=2, col="red")
lines(x, df(x, df1=5, df2=25), 
      lty=2, lwd=2, col="blue")
lines(x, df(x, df1=25, df2=5), 
      lty=3, lwd=2, col="forestgreen")
legend("topright", 
       c("df=1,30", "df=5,25", "df=25,5"),
       col=c("red", "blue", "forestgreen"),
       lty=c(1, 2, 3))

x <- seq(0, 6.5, length=200)
y <- df(x, df1=4, df2=4)
plot(x, y, type='l',
     main="F distribution (df1=4, df2=4)")

xlim <- x[5.32<=x]
ylim <- y[5.32<=x]
xlim <- c(xlim[1], xlim, tail(xlim, 1))
ylim <- c(0, ylim, 0) 
polygon(xlim, ylim, col="grey")

#######################
## 4.3 One-Way ANOVA ##
#######################

str(InsectSprays)

tapply(InsectSprays$count, InsectSprays$spray, mean)
tapply(InsectSprays$count, InsectSprays$spray, sd)
tapply(InsectSprays$count, InsectSprays$spray, length)

# [Figure 4-8]
windows(width=7.0, height=5.5)
library(gplots)
plotmeans(count ~ spray, data=InsectSprays,
          main="Performance of Insect Sprays\nwith 95% CI of Mean",
          xlab="Type of Sprays", 
          ylab="Insect Count", 
          barcol="tomato", 
          barwidth=3, 
          col="cornflowerblue",
          lwd=2)

# [Figure 4-9]
windows(width=7.0, height=5.5)
boxplot(count ~ spray, data=InsectSprays, col="tomato",
        xlab="Type of Sprays", ylab="Insect Count",
        main="Performance of Insect Sprays")

sprays.aov <- aov(count ~ spray, data=InsectSprays)
sprays.aov

summary(sprays.aov)

model.tables(sprays.aov, type="mean")
model.tables(sprays.aov, type="effects")
model.tables(sprays.aov)

sprays.compare <- TukeyHSD(sprays.aov)		
sprays.compare

sprays.compare$spray['D-C',]

# [Figure 4-10]
windows(width=7.0, height=5.5)
plot(TukeyHSD(sprays.aov), col="blue", las=1)

# [Figure 4-11]
#install.packages("multcomp")
library(multcomp)
tuk.hsd <- glht(model=sprays.aov, linfct=mcp(spray="Tukey"))
tuk.hsd

windows(width=7.0, height=5.5)
plot(cld(tuk.hsd, level=0.05), col="orange")

# [Figure 4-12]
library(car)
windows(width=7.0, height=5.5)
qqPlot(InsectSprays$count, pch=20, col="deepskyblue", id=FALSE,
       main="Q-Q Plot", xlab="Theoretical Quantiles", ylab="Sample Quantiles")

shapiro.test(InsectSprays$count)

outlierTest(sprays.aov)

leveneTest(count ~ spray, data=InsectSprays)
bartlett.test(count ~ spray, data=InsectSprays)

oneway.test(count ~ spray, data=InsectSprays)

oneway.test(count ~ spray, data=InsectSprays, var.equal=TRUE)

#######################
## 4.4 Two-Way ANOVA ##
#######################

str(ToothGrowth)

ToothGrowth$dose <- factor(ToothGrowth$dose, 
                           levels=c(0.5, 1.0, 2.0), labels=c("low", "med", "high"))
str(ToothGrowth)
ToothGrowth[seq(1, 60, 5),]

with(ToothGrowth, tapply(len, list(supp, dose), length))
with(ToothGrowth, tapply(len, list(supp, dose), mean))
with(ToothGrowth, tapply(len, list(supp, dose), sd))

ToothGrowth.aov <- aov(len ~ supp * dose, data=ToothGrowth)
ToothGrowth.aov <- aov(len ~ supp + dose + supp:dose, data=ToothGrowth)

summary(ToothGrowth.aov)

model.tables(ToothGrowth.aov, type="means")

# [Figure 4-13]
windows(width=7.0, height=5.5)
boxplot(len ~ supp * dose, data=ToothGrowth,
        col=c("deeppink", "yellowgreen"), las=1,
        xlab="Vitamin C Type", ylab="Tooth Growth",
        main="Effects of Vitamin C on Tooth Growth of Guinea Pigs")

# [Figure 4-14]
windows(width=7.0, height=5.5)
interaction.plot(x.factor=ToothGrowth$dose, trace.factor=ToothGrowth$supp, 
                 response=ToothGrowth$len, las=1, type="b", 
                 pch=c(1, 19), col=c("blue", "red"), trace.label="Supplement",
                 xlab="Dose Level", ylab="Tooth Length",
                 main="Interaction Plot for Tooth Growth of Guinea Pigs")

# [Figure 4-15]
windows(width=7.0, height=5.5)
#install.packages("gplots")
library(gplots)
plotmeans(len ~ interaction(supp, dose, sep=" "), data=ToothGrowth,
          connect=list(c(1,3,5), c(2,4,6)),
          col=c("red", "green3"),
          xlab="Supplement and Dose Combination", ylab="Tooth Length",
          main="Means Plot for Tooth Growth of Guinea Pigs\nwith 95% CI of Mean")

# [Figure 4-16]
windows(width=7.0, height=5.5)
coplot(len ~ dose | supp, data=ToothGrowth, 
       col="steelblue", pch=19, 
       panel=panel.smooth, lwd=2, col.smooth="darkorange",
       xlab="Dose Level", ylab="Tooth Length")

?coplot

# [Figure 4-17]
#install.packages("HH")
library(HH)
windows(width=7.0, height=5.5)
interaction2wt(len ~ supp * dose,  data=ToothGrowth)

TukeyHSD(ToothGrowth.aov)

TukeyHSD(ToothGrowth.aov, which=c("dose"), conf.level=0.99)

################
## 4.5 ANCOVA ##
################

#install.packages("faraway")
library(faraway)
str(sexab)

tapply(sexab$ptsd, sexab$csa, mean)
tapply(sexab$ptsd, sexab$csa, sd)
tapply(sexab$ptsd, sexab$csa, length)

sexab.aov <- aov(ptsd ~ cpa + csa, data=sexab)
summary(sexab.aov)

#install.packages("effects")
library(effects)
effect("csa", sexab.aov)

# [Figure 4-18]
library(HH)
windows(width=7.0, height=5.5)
ancova(ptsd ~ cpa + csa, data=sexab)

#################################
## 4.6 Repeated Measures ANOVA ##
#################################

head(CO2, 3); tail(CO2, 3)
CO2sub <- subset(CO2, Treatment=="chilled")
CO2sub$conc <- factor(CO2sub$conc)

CO2sub.aov <- aov(uptake ~ Type * conc + Error(Plant/conc), data=CO2sub)
summary(CO2sub.aov)

# [Figure 4-19]
windows(width=7.0, height=5.5)
par(mar=c(6,4,4,2))
boxplot(uptake ~ Type * conc, data=CO2sub,
        col=c("deepskyblue", "violet"), las=2, cex.axis=0.75,
        ylab="Carbon dioxide uptake rate",
        main="Effects of Plant Type and CO2 on Carbon Dioxide Uptake")
legend("topleft", inset=0.02, 
       legend=c("Quebec", "Mississippi"), fill=c("deepskyblue", "violet"))

# [Figure 4-20]
library(HH)
windows(width=7.0, height=5.5)
interaction2wt(uptake ~ conc * Type, data=CO2sub)

################
## 4.7 MANOVA ##
################

#install.packages("heplots")
library(heplots)
str(Skulls)
library(dplyr)
sample_n(Skulls, 10)

attach(Skulls)
y <- cbind(mb, bh, bl, nh)
aggregate(y, by=list(epoch), FUN=mean)

Skulls.manova <- manova(y ~ epoch)
summary(Skulls.manova)

summary.aov(Skulls.manova)
detach(Skulls)

