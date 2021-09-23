# Linear Regression

install.packages("MASS")
library(MASS)

# Importing Dataset ###

getwd()

startup <- read.csv(choose.files())
View(startup)
startup1 <- startup
View(startup1)
View(startup)

dim(startup)
summary(startup)
str(startup)
head(startup)

# find missing value in the dataset

colSums(is.na(startup))

colSums(is.na(startup))/nrow(startup)

# check outlier
boxplot(startup$R.D.Spend) #no outlier#

boxplot(startup$Administration) #no outlier#

boxplot(startup$Marketing.Spend) #no outlier#


# Pre-processing - part3 - encoding concept

startup$State <- as.factor(startup$State)
startup$State <- as.numeric(startup$State)

#1 - california
#2 - florida
#3 - Newyork

#Creating dummy categorical data

startup$Newyork <- ifelse(startup$State == 3,1,0)
startup$Florida <- ifelse(startup$State == 2,1,0)

View(startup)

# Pre-processing part 4 - Feature Scaling
# Trick - If you have so many attribution (independent variable)
# and if wanted to handle outlier, pls go by this method 
# this is just an example

# we do not have any outlier in this dataset Startup

# pre-processing part completed

# splitting the data into training and test

install.packages("caTools") # splitting the data into training and test
library(caTools)

set.seed(123) # this number will ensure reproducing the same data everytime

# we can use any number while using set.seed(ANY_NUMBER) and please don't use char inside set.seed
# fixing the random number
# 70%-30% , 75%-25%,80%-20%

split <- sample.split(startup$Profit,SplitRatio = 0.80)
split

table(split)

#Training dataset

train_startup <-subset(startup,split==T)
train_startup
View(train_startup)
nrow(train_startup)

# Test dataset

test_startup <-subset(startup,split==F)
test_startup
View(test_startup)
nrow(test_startup)

#Visualization
# pls use heatmap, plotting # uni-variate
hist(startup$R.D.Spend)
hist(startup$Administration)
hist(startup$Marketing.Spend)

# bivariate testing
plot(startup$R.D.Spend, startup$Profit, col="blue") # Bi-variate, good linear relationship
plot(startup$Administration, startup$Profit, col="blue")
plot(startup$Marketing.Spend, startup$Profit, col="red")

library(corrgram)
library(corrplot)

#cor works only with numerical variables  # multivariate

startup2 <- subset(startup, select=c(R.D.Spend,Administration,Marketing.Spend,Profit))
View(startup2)

corr <- cor(startup2)
corrgram(startup2)

corrplot(corr,method = "circle", order = "alphabet")

heatmap(cor(startup2))

#########################################################
# Let us build our Linear Regression Model

#Backward elimination

names(startup)
regressor <- lm(Profit~. -State, data = train_startup)
summary(regressor)

library(car)
library(faraway)

vif(regressor) # there is no multicollinearity

# only R.D.Spend is significant
# Multiple R-squared:  0.9499,	Adjusted R-squared:  0.9425

regressor1 <- lm(Profit~. -State-Florida, data = train_startup)
summary(regressor1)
vif(regressor1)
# only R.D.Spend is significant
#Multiple R-squared:  0.9499,	Adjusted R-squared:  0.9442

regressor2 <- lm(Profit~. -State-Florida-Newyork, data = train_startup)
summary(regressor2)
vif(regressor2)
# only R.D.Spend is significant
#Multiple R-squared:  0.9499,	Adjusted R-squared:  0.9457 

regressor3 <- lm(Profit~. -State-Florida-Newyork-Administration, data = train_startup)
summary(regressor3)
vif(regressor3)
# only R.D.Spend is significant
#Multiple R-squared:  0.9495,	Adjusted R-squared:  0.9468

regressor4 <- lm(Profit~. -State-Florida-Newyork-Administration-Marketing.Spend, data = train_startup)
summary(regressor4)
#Multiple R-squared:  0.9448,	Adjusted R-squared:  0.9434
#Final model

# Let us try to use the build model for prediction
# we have test dataset for prediction and post prediction, we have to validate the model

reg_pred <- predict(regressor4, newdata = test_startup)
reg_pred

# cbind the test and predicted value for validation

reg_pred_cbind <- cbind(test_startup$Profit,reg_pred)
View(reg_pred_cbind)

# Actual and Predicted values of test dataset

plot(test_startup$Profit, col="green", type = "l", lty = 1.8)
lines(reg_pred, col="red", type = "l", lty=1.5)

# Measurement 
# MAE - Mean Absolute Error
# MAPE - Mean Absolute Percent Error
# MSE - Mean Square Error
# RMSE - Root Mean Square Error

# Assumption 1 - satisfied
#1) There should not be any autocorrelation
# we have to check durwin watson test

library(lmtest)
dwtest(regressor4)

#DW = 1.287 - no auto-correlation

# assumption 2 : data should be a linear line - satisfied

# assumption 3 : check multicollinearity - satisfied
# VIF 

# assumption 4 - Hetroscadicity - satisfied
# assumption 5 - endogenity - satisfied

# Sending full dataset to the stakeholder with predicted values

Predicted_Profit <- predict(regressor4, newdata = startup1)
Predicted_Profit

Startup_fin = cbind(startup1,Predicted_Profit)
View(Startup_fin)

# Export final file

getwd()
write.csv(Startup_fin, file = "Startup_Final.csv")
