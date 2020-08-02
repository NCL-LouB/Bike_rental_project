# load the glmnet package
library(glmnet)

dim(bike.rental.data.reg)
# 731 observations, 1 response variable (rental count: n) 4 explanatory variables 
# (weathersit, temp, hum, windspeed)

#Column means
colMeans(bike.rental.data.reg[,-2])
#rental_count         temp          hum    windspeed 
#4504.34884     15.28181     62.78941     12.76258 

# print the first few rows
head(bike.rental.data)

# Extract response variable
y = bike.rental.data2[,1]

# Extract explanatory variables
X_raw = bike.rental.data2[,2:8]
class(X_raw)

# Convert to matrix
X_raw = as.matrix(X_raw)
class(X_raw)
X = scale(X_raw)

head(X)

# Create single data frame containing response and explanatory variables:
rental_data = data.frame(y, X)

#Use the lm function to find the least squares estimate of the vector of regression coefficients
#Fit linear model
lsq_fit = lm(rental_count ~ ., data=rental_data)
# Summarise model fit:
summary(lsq_fit)

# The least squares estimates are:
#(Intercept)  4504.35
#temp         1213.68
#hum          -442.24
#windspeed    -372.57 

#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4504.35      49.08  91.773  < 2e-16 ***
#  season        529.12      53.92   9.813  < 2e-16 ***
#  weathersit   -235.78      64.38  -3.662 0.000268 ***
#  temp         1382.34      55.82  24.766  < 2e-16 ***
#  hum          -369.97      66.94  -5.527 4.56e-08 ***
#  windspeed    -311.57      52.39  -5.947 4.25e-09 ***

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4504.35      48.95  92.014  < 2e-16 ***
#  season        528.78      53.79   9.831  < 2e-16 ***
#  weathersit   -244.97      64.41  -3.804 0.000155 ***
#  temp         1375.68      55.80  24.654  < 2e-16 ***
#  hum          -365.97      66.81  -5.477 5.96e-08 ***
#  windspeed    -310.00      52.26  -5.931 4.65e-09 ***
#  holiday       101.60      50.66   2.006 0.045272 *  
#  workingday     40.44      50.82   0.796 0.426455  


# split into training and test. split into odds and evens
#bikerental_train = bike.rental.data.reg[1:365,]
#bikerental_test = bike.rental.data.reg[365:731,]

bikerental_train = bike.rental.data.reg %>% 
  dplyr::filter(row_number() %% 2 == 0) ## Select even rows
bikerental_test = bike.rental.data.reg %>% 
  dplyr::filter(row_number() %% 2 == 1) ## Select odd rows

# Compute the test error for the baseline model
## Fit model to the training data
baseline_train = lm(rental_count ~ ., data=bikerental_train)
## Predict values for the test data
yhat = predict(baseline_train, newdata=bikerental_test)
## Compute the test error
(err = mean((yhat - bikerental_test$rental_count)^2))
