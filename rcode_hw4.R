library(MASS)
data("Boston")

summary(Boston)
?Boston
str(Boston)

set.seed(13437586)
training_index <- sample(1:506, 355, replace = FALSE)
boston_training <- Boston[training_index,]
boston_testing <- Boston[-training_index,]
summary(boston_training)

## For repeating steps(set of other random data)

set.seed(7046)
training_index_1 <- sample(1:506, 355, replace = FALSE)
boston_training_1 <- Boston[training_index_1,]
boston_testing_1 <- Boston[-training_index_1,]


par(mfrow = c(1,1))

hist(boston_training$crim, main = "Fig 2.1 Histogram of Per Capita Crime Rate", xlab = "Per Capita Crime Rate")
boxplot(boston_training$crim, data= boston_training, main = "Fig 2.2 Boxplot of Per Capita Crime Rate")

hist(boston_training$zn, main = "Fig 2.3 Histogram of zn", xlab = "zn")
boxplot(boston_training$zn, data= boston_training, main = "Fig 2.4 Boxplot of zn")

hist(boston_training$indus, main = "Fig 2.5 Histogram of indus", xlab = "indus")
boxplot(boston_training$indus, data= boston_training, main = "Fig 2.6 Boxplot of indus")

hist(boston_training$nox, main = "Fig 2.7 Histogram of nox", xlab = "nox")
boxplot(boston_training$nox, data= boston_training, main = "Fig 2.8 Boxplot of nox")

hist(boston_training$rm, main = "Fig 2.9 Histogram of rm", xlab = "rm")
boxplot(boston_training$rm, data= boston_training, main = "Fig 2.10 Boxplot of rm")

hist(boston_training$age, main = "Fig 2.11 Histogram of age", xlab = "age")
boxplot(boston_training$age, data= boston_training, main = "Fig 2.12 Boxplot of age")

hist(boston_training$dis, main = "Fig 2.13 Histogram of dis", xlab = "dis")
boxplot(boston_training$dis, data= boston_training, main = "Fig 2.14 Boxplot of dis")

hist(boston_training$rad, main = "Fig 2.15 Histogram of rad", xlab = "rad")
boxplot(boston_training$rad, data= boston_training, main = "Fig 2.16 Boxplot of rad")

hist(boston_training$tax, main = "Fig 2.17 Histogram of tax", xlab = "tax")
boxplot(boston_training$tax, data= boston_training, main = "Fig 2.18 Boxplot of tax")

hist(boston_training$ptratio, main = "Fig 2.19 Histogram of ptratio", xlab = "ptratio")
boxplot(boston_training$ptratio, data= boston_training, main = "Fig 2.20 Boxplot of ptratio")

hist(boston_training$black, main = "Fig 2.21 Histogram of black", xlab = "black")
boxplot(boston_training$black, data= boston_training, main = "Fig 2.22 Boxplot of black")

hist(boston_training$lstat, main = "Fig 2.23 Histogram of lstat", xlab = "lstat")
boxplot(boston_training$lstat, data= boston_training, main = "Fig 2.24 Boxplot of lstat")

hist(boston_training$medv, main = "Fig 2.25 Histogram of medv", xlab = "medv")
boxplot(boston_training$medv, data= boston_training, main = "Fig 2.26 Boxplot of medv")

summary(boston_training)

##Correlation Matric

corr_matrix <- cor(boston_training)
round(corr_matrix,2)

library(xlsx)
write.xlsx(corr_matrix, "~/desktop/mydata.xlsx")
getwd()


##Fititng a Regression Model in the data


full_model <-lm(medv~.,data = Boston_train)
full_model_summary<-summary(model_full)
0
AIC(full_model)
BIC(full_model)
full_model_summary

## best Subset 

library(leaps)
subset_reg<-regsubsets(medv~.,
                       data = boston_training,
                       nbest = 2,
                       nvmax = 14)
model_summaries <- summary(subset_reg)
plot(subset_reg)

data.frame(
  Adj.R2 = which.max(model_summaries$adjr2),
  CP = which.min(model_summaries$cp),
  BIC = which.min(model_summaries$bic)
)


####Stepwise 

nullmodel=lm(medv~1, data=boston_training)
fullmodel=lm(medv~., data=boston_training)
model_step_f<-step(nullmodel,
                   scope = list(lower=nullmodel,upper=fullmodel),
                   direction = 'forward')
model_step_b<-step(fullmodel,direction = 'backward')                                     

model_step_both<-step(nullmodel,
                      scope = list(lower=nullmodel,upper=fullmodel),
                      direction = 'both')

####LASSO 

library(glmnet)
lasso_fit = glmnet(x = as.matrix(boston_training[, -c(which(colnames(boston_training)=='medv'))]), 
                   y = boston_training$medv, 
                   alpha = 1)
lasso_fit$lambda

plot(lasso_fit,xvar = 'lambda')
cv_lasso_fit = cv.glmnet(x = as.matrix(boston_training[, -c(which(colnames(boston_training)=='medv'))]), 
                         y = boston_training$medv, 
                         alpha = 1, 
                         nfolds = 5)

plot(cv_lasso_fit)
cv_lasso_fit$lambda.1se



coef(cv_lasso_fit,s=cv_lasso_fit$lambda.1se)


## Linear Regression -  Hw4 continued 

model_1 <- lm(medv ~ crim + chas + rm + ptratio + black + lstat, data = boston_training_1)

plot(model_1$residuals)

summary_model_1 <- summary(model_1)


####in sample MSE

yfitted_insample <- fitted(object = model_1)

sum(((yfitted_insample - boston_training_1$medv)^2))/348

sum(summary_model_1$residuals^2)/348

### Out of Sample MSE

yfitted_out_sample <- predict(object = model_1, newdata = boston_testing_1)

mean((yfitted_out_sample - boston_testing_1$medv)^2)


### Cross Validation 

library(boot)

model_2 <- glm(medv ~ crim + chas + rm + ptratio + black + lstat, data = Boston)

cv.glm(data = Boston, glmfit = model_2, K = 3)$delta[2]

library(rpart)
library(rpart.plot)

####CART

boston.rpart <- rpart(formula = medv ~ crim + chas + rm + ptratio + black + lstat, 
                      data = boston_training)
prp(boston.rpart,digits = 4, extra = 1)

##In sample prediction using CART
boston.train.pred.tree = predict(boston.rpart)

sum(((boston.train.pred.tree - boston_training_1$medv)^2))/348

## Out of sample prediction using the CART 

boston.test.pred.tree = predict(boston.rpart,boston_testing)

sum(((boston.test.pred.tree - boston_testing$medv)^2))/144

length(boston.test.pred.tree_1)






