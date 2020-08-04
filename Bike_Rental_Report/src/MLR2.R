########## Background ##########
# Use data frame bike.rental.data.reg
# make a duplicate
bike.rental.data.reg3 = bike.rental.data.reg

#Check the size of the 
dim(bike.rental.data.reg)

# Store n and p
(n = nrow(bike.rental.data.reg))
(p = ncol(bike.rental.data.reg) - 1)

# print the first few rows
head(bike.rental.data.reg)
# We can see that the response variable (rental.count) is stored in column 1 and 
# the explanatory variables appear in columns 2-6. The explanatory variables are 
# in their raw form and so we will need to be transformed. 

# Extract response variable
y = bike.rental.data.reg[,1]

# Extract explanatory variables
X_raw = bike.rental.data.reg[,2:11] # CHANGED FROM [,3:6]
# Convert to matrix
X_raw = as.matrix(X_raw)
# Scale the explanatory variables
X = scale(X_raw)
# print the first few rows
head(X)

# Create single data frame containing response and explanatory variables:
bike.rental.data.reg = data.frame(y, X)

########## Exploratory Data Analysis ##########
# Before proceeding with our regression analysis, we will perform some explanatory analysis
# to get a feel for the relationships between the resopnse and explanatry variables, and the 
# relationships between the explanatory 

pairs(bike.rental.data.reg)
cor(bike.rental.data.reg)

########## Least Squares ##########
# Fit the model using least squares
lsq_fit = lm(rental.count ~ ., data = bike.rental.data.reg)
# Summarise fitted model
(lsq_summary = summary(lsq_fit))

yhat = predict(lsq_fit, bike.rental.data.reg)
# print first few elements
head(yhat)
head(bike.rental.data.reg$rental.count)

########## Best Subset Selection ##########

# Both our exploratory analysis and the results from the least squares model fit suggest 
# that we do not need to include all the explanatory variables in our fitted model. If we 
# can eliminate some of the explanatory variables we might be able to estimate the effects 
# of those that remain more precisely, thereby improving the predictice performance of the 
# model. To this end we will consider best subset selection. 

library(leaps)
# apply the best subset selection algorithm
bss_fit = regsubsets(rental.count ~ ., data = bike.rental.data.reg, method = "exhaustive", nvmax = p)
# summarise the results
(bss_summary = summary(bss_fit))
str(bss_summary)

# To compare the fits of the models we can use adjusted-R^2, Mallow's C statistic or the BIC:
# adjusted-R^2
bss_summary$adjr2
# Mallow's C statistic
bss_summary$cp
# BIC
bss_summary$bic

# The best models according to each criteria are therefore:
# adjusted-R^2 = model 10
(best_adjr2 = which.max(bss_summary$adjr2))
# Mallow's C statistic = model 1
(best_cp = which.min(bss_summary$cp))
# BIC = model 1
(best_bic = which.min(bss_summary$bic))

# To reconcile the differences implied by the different model choice criteria, it is helpful to plot
# how each criterion varies with the number of predictor
# Create mulit-panel plotting device
par(mfrow=c(2,2))
# produce the plots, highlighting the optimal value of k
plot(1:p, bss_summary$adjr2, xlab="Number of predictors", ylab="Adjusted Rsq", type="b") 
points(best_adjr2, bss_summary$adjr2[best_adjr2], col="red", pch=16)
plot(1:p, bss_summary$cp, xlab="Number of predictors", ylab="Cp", type="b") 
points(best_cp, bss_summary$cp[best_cp], col="red", pch=16)
plot(1:p, bss_summary$bic, xlab="Number of predictors", ylab="BIC", type="b") 
points(best_bic, bss_summary$bic[best_bic], col="red", pch=16)

# As both the adjusted-R^2 and Mallow's C statistic show that the optimal point is 1 we will 
# select model 1. The associated regression coefficients are:
coef(bss_fit, 1)

########## Build Linear Model ##########
# Build linear model 
linearMod <- lm(rental.count ~ temperature, data=bike.rental.data.reg)  # build linear regression model on full data
# View coefficients 
print(linearMod)
summary(linearMod)
#The p value is less than 0.001 and so we can consider a linear model to be statistically significant

# Create Training and Test data sets
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex = sample(1:nrow(bike.rental.data.reg), 0.8*nrow(bike.rental.data.reg))  # row indices for training data
bikerental_train = bike.rental.data.reg[trainingRowIndex, ]  # model training data
bikerental_test  = bike.rental.data.reg[-trainingRowIndex, ]   # test data

# Develop the model on the training data and use it to predict the distance on test data
lmMod = lm(rental.count ~ temperature, data=bikerental_train)  # build the model
rentalPred = predict(lmMod, bikerental_test)  # predict distance

# Review diagnostic measures
summary (lmMod)
# From the model summary, the model p value and predictor’s p value are less than 
# the significance level, so we know we have a statistically significant model. 
# Also, the R-Sq and Adj R-Sq are comparable to the original model built on full data.

# Calculate prediction accuracy and error rates

actuals_preds = data.frame(cbind(actuals=bikerental_test$rental.count, predicteds=rentalPred))  # make actuals_predicteds dataframe.
correlation_accuracy = cor(actuals_preds)  # 82.7%
head(actuals_preds)

min_max_accuracy = mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# 75.06%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# 35.57%, mean absolute percentage deviation


