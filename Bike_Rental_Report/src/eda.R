library('ProjectTemplate')
load.project()
#install.packages('patchwork')
library(patchwork)
#install.packages('hrbrthemes')
library(hrbrthemes)

# inspect the first 5 rows
head(bike.rental.data, 5)

# locate any missing data fields. 
table(is.na(bike.rental.data))
# All data fields have an entry

table(bike.rental.data$season)
#FALL SPRING SUMMER WINTER 
#188    181    184    178 

table(bike.rental.data$yr)
#2011 2012 
#365  366 
# 2012 was a leap year

table(bike.rental.data$mnth)
#APR AUG DEZ FEB JAN JUL JUN MAR MAY NOV OKT SEP 
#60  62  62  57  62  62  60  62  62  60  62  60 

table(bike.rental.data$holiday)
#HOLIDAY NO HOLIDAY 
#21        710 

table(bike.rental.data$weekday)
#FRI MON SAT SUN THU TUE WED 
#104 105 105 105 104 104 104 

table(bike.rental.data$workingday)
#NO WORKING DAY    WORKING DAY 
#    231            500 

table(bike.rental.data$weathersit)
#GOOD           MISTY    RAIN/SNOW/STORM 
#463             247              21 
# From this summary we can see that there were more good weather 
# days than misty or rain/snow/storm days

# Temperature
table(bike.rental.data$temp)
ggplot(data = bike.rental.data) +
  geom_histogram(mapping = aes(x=temp))
# There are two peaks the first around 8 degrees and the other 26 degrees
summary(bike.rental.data$temp)
# Minimum temp is -5 degrees 
# Median temp is 15 degrees
# Maximum temp is 32 degrees
ggplot(data = bike.rental.data) +
  geom_point(mapping = aes(x=date, y=temp))
# This plot clearly shows two temperature peaks in July 2011 and July 2012
# It is coldest in December and January 

# Temperature against Rentals 1

# 1 Side by side
# Create two plots
p1 = ggplot(bike.rental.data, aes(x=date, y=temp)) +
  geom_point()

p2 = ggplot(bike.rental.data, aes(x=date, y=rental_count)) +
  geom_point()

# Use patchwork to show them side by side
p1 + p2

# Temperature against Rentals 2
# 2 dual Y axis with ggplot2

# value used to transform the data
coeff = 100
# Colours
temperatureColour = "#69b3a2"
rentalColour = rgb(0.2, 0.6, 0.9, 1)

ggplot(bike.rental.data, aes(x=date)) +
  
  geom_point(aes(y=temp), color = temperatureColour) +
  geom_point(aes(y=rental_count/ coeff), color = rentalColour) +

scale_y_continuous(
  # Features of the first axis
  name = "Temperature",
  #Add a second axis and specify its features
  sec.axis = sec_axis(~.*coeff, name = "Rentals")
) +
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColour, size=13),
    axis.title.y.right = element_text(color = rentalColour, size=13)
  ) +
  ggtitle("Number of rentals per day and daily temperature")


coeff = 100
# Colours
temperatureColour = "#69b3a2"
rentalColour = rgb(0.2, 0.6, 0.9, 1)

ggplot(bike.rental.data, aes(x=date)) +
  
  geom_line(aes(y=temp), color = temperatureColour) +
  geom_line(aes(y=rental_count/ coeff), color = rentalColour) +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature",
    #Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name = "Rentals")
  ) +
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = temperatureColour, size=13),
    axis.title.y.right = element_text(color = rentalColour, size=13)
  ) +
  ggtitle("Number of rentals per day and daily temperature")

# Count the number of rentals on all days that have the same temperature
# Calculate the median number of rentals when the temp is a given temperature
rentalsBYtemp = bike.rental.data %>% 
  group_by(temp) %>%
  summarise(totalrentals = sum(rental_count),
            medianrentals = median(rental_count))

# Plot the visual
ggplot(data = rentalsBYtemp, aes(x=temp)) +
  geom_line(aes(y=totalrentals)) +
  geom_point(aes(y=totalrentals))
# Plot the visual
ggplot(data = rentalsBYtemp, aes(x=temp)) +
  geom_line(aes(y=medianrentals)) +
  geom_line(aes(y=medianrentals))


ggplot(data = bike.rental.data, aes(x=temp, y=rental_count, color = weathersit)) +
  geom_point(shape = 20, position = "jitter")
  


# Humidity
table(bike.rental.data$hum)
ggplot(data = bike.rental.data) +
  geom_histogram(mapping = aes(x=hum))
# There are two peaks the first around 8 degrees and the other 26 degrees
summary(bike.rental.data$hum)
# Min humidity is 0 
# Median humidity is 62.67
# Max humidity is 97.25
ggplot(data = bike.rental.data) +
  geom_point(mapping = aes(x=date, y=hum))
# There is a less distinct seasonal patter to humidity levels
# The minimum humidity reading of 0 appears to be an anomoly. This could be attributed
# a recording error but we cannot know for certain with further investigation.

coeff = 100
# Colours
humidityColour = "#69b3a2"
renatalColour = rgb(0.2, 0.6, 0.9, 1)

ggplot(bike.rental.data, aes(x=date)) +
  
  geom_line(aes(y=hum), color = humidityColour) +
  geom_line(aes(y=rental_count/ coeff), color = rentalColour) +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Humidity",
    #Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name = "Rentals")
  ) +
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = humidityColour, size=13),
    axis.title.y.right = element_text(color = rentalColour, size=13)
  ) +
  ggtitle("Number of rentals per day and humidity")

# Count the number of rentals on all days that have the same temperature
# Calculate the median number of rentals when the temp is a given temperature
rentalsBYhum = bike.rental.data %>% 
  group_by(hum) %>%
  summarise(totalrentals = sum(rental_count),
            medianrentals = median(rental_count))

# Plot the visual
ggplot(data = rentalsBYhum, aes(x=hum)) +
  geom_col(aes(y=totalrentals)) 

# Plot the visual
ggplot(data = rentalsBYhum, aes(x=hum)) +
  geom_col(aes(y=medianrentals)) 

# Wind Speed
table(bike.rental.data$windspeed)
ggplot(data = bike.rental.data) +
  geom_histogram(mapping = aes(x=windspeed))
# This peaks at around 11
summary(bike.rental.data$windspeed)
# Min wind speed is 1.5
# Median wind speed is 12.125
# Max wind speed is 34
ggplot(data = bike.rental.data) +
  geom_point(mapping = aes(x=date, y=windspeed))
# There doesnt appear to be a distinct seasonal pattern to wind speed levels

coeff = 100
# Colours
windspeedColour = "#69b3a2"
renatalColour = rgb(0.2, 0.6, 0.9, 1)

ggplot(bike.rental.data, aes(x=date)) +
  
  geom_line(aes(y=windspeed), color = windspeedColour) +
  geom_line(aes(y=rental_count/ coeff), color = rentalColour) +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Wind Speed",
    #Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name = "Rentals")
  ) +
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text(color = windspeedColour, size=13),
    axis.title.y.right = element_text(color = rentalColour, size=13)
  ) +
  ggtitle("Number of rentals per day and wind speed")

# Count the number of rentals on all days that have the same temperature
# Calculate the median number of rentals when the temp is a given temperature
rentalsBYwindspeed = bike.rental.data %>% 
  group_by(windspeed) %>%
  summarise(totalrentals = sum(rental_count),
            medianrentals = median(rental_count))

# Plot the visual
ggplot(data = rentalsBYwindspeed, aes(x=windspeed)) +
  geom_col(aes(y=totalrentals)) 

# Plot the visual
ggplot(data = rentalsBYwindspeed, aes(x=windspeed)) +
  geom_col(aes(y=medianrentals)) 


# Bike rental count per day
ggplot(data = bike.rental.data) +
  geom_histogram(mapping = aes(x=rental_count))
summary(bike.rental.data$rental_count)
# Min rental count is 22
# Median rental count is 4548
# Max rental count is 8714
ggplot(data = bike.rental.data) +
  geom_point(mapping = aes(x=date, y=rental_count))
# There does appear to be a seasonal pattern to bike rental counts

