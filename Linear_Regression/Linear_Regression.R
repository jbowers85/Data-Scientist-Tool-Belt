# LINEAR REGRESSION #

# Press Alt + O to collapse all sections in order to choose the section of interest
# To expand all sections, press Shift + Alt + O

## 1.) Simple Linear Regression ####

### read in Old Faithful data
data <- faithful

### scatterplot and correlation to examine association between eruption time and wait time until next eruption
### ensure the 2 variables have a linear association
plot(data$eruptions,data$waiting, col = 'blue', main = 'Plot: Old Faitful Eruption Time vs Wait Time',
     xlab = 'Eruption Time', ylab = 'Time Until Next Eruption')
cor(data$eruptions,data$waiting)

### count the number of NULL values in each column 
colSums(is.na(data))

### boxplot - outlier check
### become aware of any outliers that may throw off the linear model
par(mfrow=c(1,2))
boxplot(data$eruptions, main="Eruption Time", col = 'light blue', sub=paste("Outlier rows: ", length(boxplot.stats(data$eruptions)$out)))
boxplot(data$waiting, main="Time Until Next Eruption", col = 'light green', sub=paste("Outlier rows: ", length(boxplot.stats(data$waiting)$out)))


### simple linear regression 
m <- lm(waiting ~ eruptions, data) # do not use data$waiting / data$eruptions
m
summary(m)


### residual plot
par(mfrow=c(1,1))
plot(m, pch=16, which=1) # see http://data.library.virginia.edu/diagnostic-plots/ for info on different plots (which=)
abline(h=0)


#### residual plot assumptions that need to be met:
####    true relationship is linear (points should be scattered)
####    observations are independent (only one "observation" per "unit")
####    variation of the response variable around the regression line is constant (ours may violate this)
####    residuals are normally distributed

### ensure residuals are normally distributed
hist(resid(m), xlab = 'residual', main = 'Residual Histogram', col='orange')


### add linear model to the plot
plot(data$eruptions,data$waiting, col = 'blue', main = 'Plot: Old Faitful Eruption Time vs Wait Time',
     xlab = 'Eruption Time', ylab = 'Time Until Next Eruption')
abline(m, col="red", lwd=2)

### predict the waiting time until next eruption
#### first define data frame that contains the data that needs predicting
#### note that the test data frame column needs to be identical to the training data column name
tst <- data.frame(eruptions=5) # an eruption time of 5 minutes

predict(m, tst) # predicted wait time = 87.12 minutes 






## 2.) Multiple Linear Regression ####
library(dplyr)

### read in car data
data <- mtcars

str(data)

### Count the number of NULL values in each column 
colSums(is.na(data))

### boxplot - outlier check
### become aware of any outliers that may throw off the linear model
par(mfrow=c(3,4))
boxplot(data$mpg, main="Miles per Gallon", col = 'light blue', sub=paste("Outliers: ", length(boxplot.stats(data$mpg)$out)))
boxplot(data$wt, main="Weight", col = 'light blue', sub=paste("Outliers: ", length(boxplot.stats(data$wt)$out)))
boxplot(data$cyl, main="Cylinders", col = 'light blue', sub=paste("Outliers: ", length(boxplot.stats(data$cyl)$out)))
boxplot(data$disp, main="Displacement", col = 'light blue', sub=paste("Outliers: ", length(boxplot.stats(data$disp)$out)))
boxplot(data$hp, main="Horsepower", col = 'light blue', sub=paste("Outliers: ", length(boxplot.stats(data$hp)$out)))
boxplot(data$drat, main="Rear Axle Ratio", col = 'light blue', sub=paste("Outliers: ", length(boxplot.stats(data$drat)$out)))
boxplot(data$vs, main="V/S", col = 'light blue', sub=paste("Outliers: ", length(boxplot.stats(data$vs)$out)))
boxplot(data$am, main="Transmission", col = 'light blue', sub=paste("Outliers: ", length(boxplot.stats(data$am)$out)))
boxplot(data$carb, main="# of Carbs", col = 'light blue', sub=paste("Outliers: ", length(boxplot.stats(data$carb)$out)))
boxplot(data$gear, main="# of Gears", col = 'light blue', sub=paste("Outliers: ", length(boxplot.stats(data$gear)$out)))
boxplot(data$qsec, main="1/4 Mile Time", col = 'light blue', sub=paste("Outliers: ", length(boxplot.stats(data$qsec)$out)))


### scatterplot matrix and correlation table to examine association between mpg and other features
plot(data) 
mpg.correl <- as.data.frame(abs(cor(data)))
mpg.correl <- mpg.correl[order(-mpg.correl$mpg), ]
mpg.correl <- dplyr::select(mpg.correl, mpg)
mpg.correl # (looks like all of them are medium to high correlation)

### look for features that correlate with one another, this is called multicollinearity 
### to do this, create the multiple linear regression model, then check the variance inflation factor (VIF)

#### first convert categorical variables to factors
data$vs <- as.factor(data$vs)
data$am <- as.factor(data$am)

library(car)

m <- lm(mpg ~ cyl+disp+hp+drat+wt+qsec+vs+am+gear+carb, data = mtcars) # create regression model using all variables
vifdf <- data.frame(vif(m)) # compute VIF
vifdf  # VIF > 10 is large; VIF >5 & <10 is medium 


### stepwise Regression to determine most valuable attributes in explaining mpg
library(MASS)
step <- stepAIC(m, direction="both", trace=FALSE)
summary(step)$coeff # weight, acceleration, and transmission have the highest relation to explaining mpg
summary(step)$adj.r.squared # this model explains 83% of the variation in mpg

#### "step" is now our best performing model according to the stepwise regression

### verify that the stepwise regression model chose the best model
### We can use this method because each model is nested without lots of parameters differentiating them

fit1 <- lm(mpg ~ am, data = data)
fit2 <- lm(mpg ~ am + wt, data = data)
fit3 <- lm(mpg ~ am + wt + qsec, data = data)
fit4 <- lm(mpg ~ am + wt + qsec + hp, data = data)
fit5 <- lm(mpg ~ am + wt + qsec + hp + drat, data = data)

anova(fit1, fit2, fit3, fit4, fit5) # anova tests each model with the preceding model (hence the empty first row)

### residual plot
par(mfrow=c(1,1))
plot(step, pch=16, which=1:5)
abline(h=0)

### visualize the model
termplot(step)

#### since this model has 3 predictors and 1 outcome variable, we cant visualize in a simple plot. 
#### consider a 3d plot if you have only 2 predictors 1 outcome variable


### predict the mpg, given the transmission, weight, and acceleration
#### first define data frame that contains the data that needs predicting
#### note that the data frame column needs to be identical to the training data column name
tst <- data.frame(am=0,wt=2.95,qsec=15.3) 

predict(step, tst) # predicted mpg = 16.82 


