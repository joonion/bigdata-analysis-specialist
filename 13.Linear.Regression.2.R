##################################################
## 7.4 Mediation and Moderation Effect Analysis ##
##################################################

## Mediation effect analysis

data(mtcars)
model.total <- lm(mpg ~ disp, data=mtcars)
summary(model.total)

model.M <- lm(wt ~ disp, data=mtcars)
summary(model.M)

model.Y <- lm(mpg ~ disp + wt, data=mtcars)
summary(model.Y)

(0.0070103)*(-3.35082)

#install.packages("multilevel")
library(multilevel)
model.sob <- sobel(pred=mtcars$disp, med=mtcars$wt, out=mtcars$mpg)
model.sob

model.sob$Indirect.Effect
model.sob$SE
model.sob$z.value
model.sob$N

pnorm(abs(model.sob$z.value), lower.tail=FALSE)*2

#install.packages("bda")
library(bda)
mediation.test(mv=mtcars$wt, iv=mtcars$disp, dv=mtcars$mpg)

#install.packages("mediation")
library(mediation)
set.seed(123)
model.M <- lm(wt ~ disp, data=mtcars)
model.Y <- lm(mpg ~ disp + wt, data=mtcars)
model.mediation <- mediate(model.m=model.M, model.y=model.Y, 
                           treat="disp", mediator="wt", boot=TRUE, sims=500)
summary(model.mediation)

# [Figure 7-16]
windows(width=7.0, height=5.5)
plot(model.mediation, cex=1.2, col="royalblue", lwd=2,
     main="Mediation Effect Analysis")

## Moderation effect analysis

mtcars.lm <- lm(mpg ~ hp + wt + hp:wt, data=mtcars)
summary(mtcars.lm)

# [Figure 7-19]
#install.packages("effects")
library(effects)
windows(width=7.0, height=5.5)
m <- round(mean(mtcars$wt), 1); m
s <- round(sd(mtcars$wt), 1); s
plot(effect(term="hp:wt", mod=mtcars.lm, xlevels=list(wt=c(m-s, m, m+s))), 
     lines=list(multiline=TRUE, lwd=2, lty=c(3, 2, 1), 
                col=c("royalblue", "violet", "maroon")),
     main="Interaction Plot for Horsepower and Weight")

# [Figure 7-20]
#install.packages("rockchalk")
library(rockchalk)
windows(width=7.0, height=5.5)
plotSlopes(model=mtcars.lm, plotx="hp", modx="wt", modxVals="std.dev.", 
           pch=21, col=rainbow(3), cex=1, bg="dimgray",
           main="Interaction Plot for Horsepower and Weight")

## Moderated mediation effect analysis

data(mtcars)

model.M <- lm(wt ~ disp*am, data=mtcars)
model.Y <- lm(mpg ~ disp*am + wt*am, data=mtcars)

library(mediation)
set.seed(12)
model.med1 <- mediate(model.m=model.M, model.y=model.Y, covariates=list(am=0),
                      treat="disp", mediator="wt", boot=TRUE, sims=500)
summary(model.med1)
set.seed(12)
model.med2 <- mediate(model.m=model.M, model.y=model.Y, covariates=list(am=1),
                      treat="disp", mediator="wt", boot=TRUE, sims=500)
summary(model.med2)

set.seed(12)
model.med <- mediate(model.m=model.M, model.y=model.Y,
                     treat="disp", mediator="wt", sims=500)
set.seed(12)
test.modmed(object=model.med, 
            covariates.1=list(am=0), covariates.2=list(am=1), sims=500)

#######################################
## 7.5 Penalized Regression Analysis ##
#######################################

library(MASS)
str(Boston)

library(caret)
set.seed(123)
train <- createDataPartition(y=Boston$medv, p=0.7, list=FALSE)
Boston.train <- Boston[train,]
Boston.test <- Boston[-train,]

x <- model.matrix(medv ~ ., Boston.train)[,-1]
y <- Boston.train$medv

library(glmnet)
glmnet(x, y, alpha=1, lambda=NULL)

## Ridge

set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=0)

# [Figure 7-23]
windows(width=7.0, height=5.5)
plot(Boston.cv)

Boston.cv$lambda.min
log(Boston.cv$lambda.min)

Boston.gnet <- glmnet(x, y, family="gaussian", alpha=0, lambda=Boston.cv$lambda.min)
coef(Boston.gnet)

Boston.test.x <- model.matrix(medv ~ ., Boston.test)[,-1]
Boston.pred <- predict(Boston.gnet, newx=Boston.test.x)
postResample(pred=Boston.pred, obs=Boston.test$medv)

## Lasso

set.seed(123)
Boston.cv <- cv.glmnet(x=x, y=y, family="gaussian", alpha=1)

# [Figure 7-24]
windows(width=7.0, height=5.5)
plot(Boston.cv)

Boston.cv$lambda.min
log(Boston.cv$lambda.min)

Boston.cv$lambda.1se
log(Boston.cv$lambda.1se)

coef(Boston.cv, Boston.cv$lambda.min)

coef(Boston.cv, Boston.cv$lambda.1se)

Boston.gnet1 <- glmnet(x, y, family="gaussian", 
                       alpha=1, lambda=Boston.cv$lambda.min)
Boston.pred1 <- predict(Boston.gnet1, newx=Boston.test.x)
postResample(pred=Boston.pred1, obs=Boston.test$medv)

Boston.gnet2 <- glmnet(x, y, family="gaussian", 
                       alpha=1, lambda=Boston.cv$lambda.1se)
Boston.pred2 <- predict(Boston.gnet2, newx=Boston.test.x)
postResample(pred=Boston.pred2, obs=Boston.test$medv)

## Elastic net

library(caret)
set.seed(123)
Boston.cv <- train(form=medv ~ ., data=Boston.train, method="glmnet",
                   trControl=trainControl(method="cv", number=10),
                   tuneLength=10)

Boston.cv$bestTune

Boston.gnet <- glmnet(x, y, family="gaussian", 
                      alpha=Boston.cv$bestTune$alpha, 
                      lambda=Boston.cv$bestTune$lambda)
coef(Boston.gnet)

Boston.pred <- predict(Boston.gnet, newx=Boston.test.x)
postResample(pred=Boston.pred, obs=Boston.test$medv)

## Comparing different models

library(caret)
lambda <- 10^seq(-5, 5, length=100)
lambda

set.seed(123)
ridge <- train(medv ~ ., data=Boston.train, method="glmnet",
               trControl=trainControl(method="cv", number=10),
               tuneGrid=expand.grid(alpha=0, lambda=lambda))
coef(ridge$finalModel, ridge$bestTune$lambda)
ridge.pred <- predict(ridge, Boston.test)
postResample(pred=ridge.pred, obs=Boston.test$medv)

set.seed(123)
lasso <- train(medv ~ ., data=Boston.train, method="glmnet",
               trControl=trainControl(method="cv", number=10),
               tuneGrid=expand.grid(alpha=1, lambda=lambda))
coef(lasso$finalModel, lasso$bestTune$lambda)
lasso.pred <- predict(lasso, Boston.test)
postResample(pred=lasso.pred, obs=Boston.test$medv)

set.seed(123)
elastic <- train(form=medv ~ ., data=Boston.train, method="glmnet",
                 trControl=trainControl(method="cv", number=10),
                 tuneLength=10)
coef(elastic$finalModel, elastic$bestTune$lambda)
elastic.pred <- predict(elastic, Boston.test)
postResample(pred=elastic.pred, obs=Boston.test$medv)

models <- list(ridge=ridge, lasso=lasso, elastic=elastic)
summary(resamples(models), metric="RMSE")

summary(diff(resamples(models), metric="RMSE"))
