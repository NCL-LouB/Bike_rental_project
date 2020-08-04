library('ProjectTemplate')
load.project()
#install.packages('patchwork')
#library(patchwork)
#install.packages('hrbrthemes')
#library(hrbrthemes)

# inspect the first 5 rows 
head(bike.rental.data, 5)

# Are there any missing data fields? No. 
table(is.na(bike.rental.data))


################## Inspect the categorical variables ##################
##### SEASON #####
table(bike.rental.data$season)
#FALL SPRING SUMMER WINTER 
#188    181    184    178 

# SEASON summary table
bike.rental.data %>% 
  group_by(season) %>%
  summarise(total.rentals = sum(rental.count),
            median.rentals = median(rental.count),
            mean.rentals = mean(rental.count),
            sd.rentals = sd(rental.count),
            percentage = round((total.rentals/3292679)*100) # 3,292,679 = sum(bike.rental.data$rental.count)
)

# Plot showing the median rentals by season
ggplot(data = bike.rental.data %>% 
         group_by(season) %>%
         summarise(total.rentals = sum(rental.count),
                   median.rentals = median(rental.count),
                   mean.rentals = mean(rental.count),
                   sd.rentals = sd(rental.count),
                   percentage = round((total.rentals/3292679)*100) # 3,292,679 = sum(bike.rental.data$rental.count)
         )
         ) +
  geom_col(aes(x=season, y=median.rentals))

# The median rental number is significantly lower in spring than the other months
# Let's see if the lower rental rate in spring could be attributed to a lower 
# median temperature in this season

# Plot showing the median temperature by season
ggplot(data = bike.rental.data %>% 
         group_by(season) %>%
         summarise(total.temperature = sum(temperature),
                   median.temperature = median(temperature),
                   mean.temperature = mean(temperature),
                   sd.temperature = sd(temperature)) # 11171 = sum(bike.rental.data$temperature)
         ) + 
  geom_col(aes(x=season, y = median.temperature))

bike.rental.data %>% 
  group_by(season) %>%
  summarise(total.temperature = sum(temperature),
            median.temperature = median(temperature),
            mean.temperature = mean(temperature),
            sd.temperature = sd(temperature))

# Yes the lower median number of rentals (2209 rentals) in spring reflects the low median temperature in spring (5 degrees)

##### WEATHER.CATEGORY #####
table(bike.rental.data$weather.category)
#GOOD           MISTY    RAIN/SNOW/STORM 
#463             247              21 
# From this summary we can see that there were more good weather 
# days than misty or rain/snow/storm days

# WEATHER.CATEGORY summary table
bike.rental.data %>% 
  group_by(weather.category) %>%
  summarise(total.rentals = sum(rental.count),
            median.rentals = median(rental.count),
            mean.rentals = mean(rental.count),
            sd.rentals = sd(rental.count),
            percentage = round((total.rentals/3292679)*100)
  )

# Plot showing the percentage of total rentals by weather.category
ggplot(data = bike.rental.data %>% 
         group_by(weather.category) %>%
         summarise(total.rentals = sum(rental.count),
                   median.rentals = median(rental.count),
                   mean.rentals = mean(rental.count),
                   sd.rentals = sd(rental.count),
                   percentage = round((total.rentals/3292679)*100) # 3,292,679 = sum(bike.rental.data$rental.count)
         )
) +
  geom_col(aes(x=weather.category, y=percentage))



##### HOLIDAY #####
table(bike.rental.data$holiday)
#HOLIDAY NO HOLIDAY 
#21        710 

# HOLIDAY summary table
bike.rental.data %>% 
  group_by(holiday) %>%
  summarise(total.rentals = sum(rental.count),
            median.rentals = median(rental.count),
            mean.rentals = mean(rental.count),
            sd.rentals = sd(rental.count),
            percentage = round((total.rentals/3292679)*100)
  )

# Plot showing the percentage of total rentals by holiday
ggplot(data = bike.rental.data %>% 
         group_by(holiday) %>%
         summarise(total.rentals = sum(rental.count),
                   median.rentals = median(rental.count),
                   mean.rentals = mean(rental.count),
                   sd.rentals = sd(rental.count),
                   percentage = round((total.rentals/3292679)*100) # 3,292,679 = sum(bike.rental.data$rental.count)
         )
) +
  geom_col(aes(x=holiday, y=percentage))

##### WORKING.DAY #####
table(bike.rental.data$working.day)
#NO WORKING DAY    WORKING DAY 
#    231            500 

# WORKING.DAY summary table
bike.rental.data %>% 
  group_by(working.day) %>%
  summarise(total.rentals = sum(rental.count),
            median.rentals = median(rental.count),
            mean.rentals = mean(rental.count),
            sd.rentals = sd(rental.count),
            percentage = round((total.rentals/3292679)*100)
  )

# Plot showing the percentage of total rentals by working.day
ggplot(data = bike.rental.data %>% 
         group_by(working.day) %>%
         summarise(total.rentals = sum(rental.count),
                   median.rentals = median(rental.count),
                   mean.rentals = mean(rental.count),
                   sd.rentals = sd(rental.count),
                   percentage = round((total.rentals/3292679)*100) # 3,292,679 = sum(bike.rental.data$rental.count)
         )
) +
  geom_col(aes(x=working.day, y=percentage))

##### YEAR #####
table(bike.rental.data$year)
#2011 2012 
#365  366 
# 2012 was a leap year

# YEAR summary table
bike.rental.data %>% 
  group_by(year) %>%
  summarise(total.rentals = sum(rental.count),
            median.rentals = median(rental.count),
            mean.rentals = mean(rental.count),
            sd.rentals = sd(rental.count),
            percentage = round((total.rentals/3292679)*100)
  )

# Plot showing the percentage of total rentals by year
ggplot(data = bike.rental.data %>% 
         group_by(year) %>%
         summarise(total.rentals = sum(rental.count),
                   median.rentals = median(rental.count),
                   mean.rentals = mean(rental.count),
                   sd.rentals = sd(rental.count),
                   percentage = round((total.rentals/3292679)*100) # 3,292,679 = sum(bike.rental.data$rental.count)
         )
) +
  geom_col(aes(x=year, y=percentage))

##### JUST.MONTH2 #####
table(bike.rental.data$just.month2)
# Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec 
# 62  57  62  60  62  60  62  62  60  62  60  62 

# JUST.MONTH2 summary table
bike.rental.data %>% 
  group_by(just.month2) %>%
  summarise(total.rentals = sum(rental.count),
            median.rentals = median(rental.count),
            mean.rentals = mean(rental.count),
            sd.rentals = sd(rental.count),
            percentage = round((total.rentals/3292679)*100)
  )

# Plot showing the percentage of total rentals by just.month2
ggplot(data = bike.rental.data %>% 
         group_by(just.month2) %>%
         summarise(total.rentals = sum(rental.count),
                   median.rentals = median(rental.count),
                   mean.rentals = mean(rental.count),
                   sd.rentals = sd(rental.count),
                   percentage = round((total.rentals/3292679)*100) # 3,292,679 = sum(bike.rental.data$rental.count)
         )
) +
  geom_col(aes(x=just.month2, y=percentage))

##### JUST.DAYOFWEEK2 #####
table(bike.rental.data$just.dayofweek2)
# Sun Mon Tue Wed Thu Fri Sat 
# 105 105 104 104 104 104 105 

# JUST.DAYOFWEEK2 summary table
bike.rental.data %>% 
  group_by(just.dayofweek2) %>%
  summarise(total.rentals = sum(rental.count),
            median.rentals = median(rental.count),
            mean.rentals = mean(rental.count),
            sd.rentals = sd(rental.count),
            percentage = round((total.rentals/3292679)*100)
  )

# Plot showing the percentage of total rentals by just.dayofweek2
ggplot(data = bike.rental.data %>% 
         group_by(just.dayofweek2) %>%
         summarise(total.rentals = sum(rental.count),
                   median.rentals = median(rental.count),
                   mean.rentals = mean(rental.count),
                   sd.rentals = sd(rental.count),
                   percentage = round((total.rentals/3292679)*100) # 3,292,679 = sum(bike.rental.data$rental.count)
         )
) +
  geom_col(aes(x=just.dayofweek2, y=percentage))


################## Inspect the continuous variables ##################

##### Temperature
table(bike.rental.data$temperature)
ggplot(data = bike.rental.data) +
  geom_histogram(mapping = aes(x=temperature), bins = 25)
# The data is roughly bimodal. The first peak is at around 8 degrees and the other at around 26 degrees
summary(bike.rental.data$temperature)
# Minimum temp is -5 degrees 
# Median temp is 15 degrees
# Maximum temp is 32 degrees
ggplot(data = bike.rental.data) +
  geom_point(mapping = aes(x=date, y=temperature))
# This plot clearly shows two temperature peaks in July 2011 and July 2012
# It is coldest in December and January 


######## Plot the visual
ggplot(data = rentalsBYtemp, aes(x=temperature)) +
  geom_col(aes(y=totalrentals)) +
  geom_col(data = top.rental.daysBYtemp, aes(x=temperature, y=total.rentals.of.upper.quantile), fill = "blue")
#blue shows the proporation of the total rentals that had days 
#where the total rentals exceeded 5956, the upper 75% quantile

######## Plot the visual
ggplot(data = rentalsBYtemp, aes(x=temperature)) +
  geom_col(aes(y=mean.daily.rentals))
ggplot(data = rentalsBYtemp, aes(x=temperature)) +
  geom_col(aes(y=mean.daily.rentals)) +
  geom_line(aes(y=median.daily.rentals), color = "red")

# Plot the visual
ggplot(data = rentalsBYtemp, aes(x=temperature)) +
  geom_line(aes(y=medianrentals)) +
  geom_line(aes(y=medianrentals))


ggplot(data = bike.rental.data, aes(x=temperature, y=rental.count)) +
  geom_point(shape = 20, position = "jitter") + 
  geom_smooth()

  


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


# Bar chart of temperature and count
tempertaure.obs = data.frame(table(bike.rental.data$temperature))
colnames(tempertaure.obs) = c("temperature", "count")
tempertaure.obs$temperature = as.numeric(tempertaure.obs$temperature)
head(arrange(tempertaure.obs, desc(count)), 5)
# Bar chart of temperature and count
ggplot(data = tempertaure.obs) +
  geom_col(mapping = aes(x=temperature, y = count)) +
  scale_x_continuous(breaks = round(seq(min(bike.rental.data$temperature), max(bike.rental.data$temperature), by = 5),1)) +
  theme_classic()


# Count the number of rentals on all days that have the same temperature
# Calculate the median number of rentals when the temp is a given temperature
rentalsBYhumidity = bike.rental.data %>% 
  group_by(humidity) %>%
  summarise(totalrentals = sum(rental.count),
            medianrentals = median(rental.count))

# Plot the visual
ggplot(data = rentalsBYhumidity, aes(x=humidity)) +
  geom_col(aes(y=totalrentals)) 

# Plot the visual
ggplot(data = rentalsBYhumidity, aes(x=humidity)) +
  geom_col(aes(y=medianrentals)) 

# Wind Speed
table(bike.rental.data$wind.speed)
ggplot(data = bike.rental.data) +
  geom_histogram(mapping = aes(x=wind.speed))
# This peaks at around 11
summary(bike.rental.data$windspeed)
# Min wind speed is 1.5
# Median wind speed is 12.125
# Max wind speed is 34
ggplot(data = bike.rental.data) +
  geom_point(mapping = aes(x=date, y=wind.speed))
# There doesnt appear to be a distinct seasonal pattern to wind speed levels

coeff = 100
# Colours
windspeedColour = "#69b3a2"
renatalColour = rgb(0.2, 0.6, 0.9, 1)

ggplot(bike.rental.data, aes(x=date)) +
  
  geom_line(aes(y=wind.speed), color = windspeedColour) +
  geom_line(aes(y=rental.count/ coeff), color = rentalColour) +
  
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

# Wind speed against rental count
ggplot(bike.rental.data) +
  geom_point(aes(x=wind.speed, y=rental.count))

# Count the number of rentals on all days that have the same temperature
# Calculate the median number of rentals when the temp is a given temperature
rentalsBYwindspeed = bike.rental.data %>% 
  group_by(wind.speed) %>%
  summarise(totalrentals = sum(rental.count),
            medianrentals = median(rental.count))

# Plot the visual
ggplot(data = rentalsBYwindspeed, aes(x=wind.speed)) +
  geom_col(aes(y=totalrentals)) 

# Plot the visual
ggplot(data = rentalsBYwindspeed, aes(x=windspeed)) +
  geom_col(aes(y=medianrentals)) 


# Bike rental count per day
ggplot(data = bike.rental.data) +
  geom_histogram(mapping = aes(x=rental.count))
summary(bike.rental.data$rental_count)
# Min rental count is 22
# Median rental count is 4548
# Max rental count is 8714
quantile(bike.rental.data$rental.count) 
# 0%  25%  50%  75% 100% 
# 22 3152 4548 5956 8714
ggplot(data = bike.rental.data) +
  geom_point(mapping = aes(x=date, y=rental.count))
# There does appear to be a seasonal pattern to bike rental counts

# Upper 75% quantile is 5956 so let's filter by this and see if there are 
# any similar variables
top.rental.days = bike.rental.data %>%
  filter(rental.count >= 5956)

# Count the number of rentals on all days that have the same temperature
# Calculate the median number of rentals when the temp is a given temperature
top.rental.daysBYtemp = top.rental.days %>% 
  group_by(temperature) %>%
  summarise(total.rentals.of.upper.quantile = sum(rental.count),
            median.daily.rentals = median(rental.count),
            mean.daily.rentals = mean(rental.count),
            sd.daily.rentals = sd(rental.count),
            percentage = round((total.rentals.of.upper.quantile/1288768)*100), # 1288768 = sum(top.rental.days$rental.count)
            mean.humidity = mean(humidity),
            sd.humidity = sd(humidity),
            mean.wind.speed = mean(wind.speed),
            sd.wind.speed = sd(wind.speed)) 



######## Plot the visual
ggplot(data = top.rental.daysBYtemp, aes(x=temperature)) +
  geom_col(aes(y=total.rentals.of.upper.quantile))
######## Plot the visual
ggplot(data = top.rental.daysBYtemp, aes(x=temperature)) +
  geom_col(aes(y=mean.daily.rentals))
ggplot(data = top.rental.daysBYtemp, aes(x=temperature)) +
  geom_col(aes(y=mean.daily.rentals)) +
  geom_line(aes(y=median.daily.rentals), color = "red")


# Rental Count
ggplot(data = bike.rental.data) +
  geom_histogram(mapping = aes(x=rental_count))
summary(bike.rental.data$rental_count)
# Min rental count is 22
# Median rental count is 4548
# Mean rental count is 4504
# Max rental count is 8714
sd(bike.rental.data$rental_count)
# 1937.211 
# There is a lot of spread around the mean daily rental, so it is understandable that the company wants 
# to understand why this is so

ggplot(data = bike.rental.data) +
  geom_point(mapping = aes(x=date, y=rental_count, color = season))
# There does appear to be a seasonal pattern to bike rental counts. 
# Less rentals take place in Spring

rentalsBYseason = bike.rental.data %>% 
  group_by(season) %>%
  summarise(totalrentals = sum(rental_count),
            medianrentals = median(rental_count),
            median.temp = median(temp),
            mean.temp = mean(temp),
            median.hum = median(hum),
            mean.hum = mean(hum),
            median.windspeed = median(windspeed),
            mean.windspeed = mean(windspeed),
            good.weather.days = sum(weathersit == "GOOD"),
            misty.days = sum(weathersit == "MISTY"),
            bad.weather.days = sum(weathersit == "RAIN/SNOW/STORM"))
