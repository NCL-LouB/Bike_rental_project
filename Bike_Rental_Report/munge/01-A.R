# Example preprocessing script.

# Duplicate the dataframe
rental.data = bike.rental.data
# Create date column
bike.rental.data$date = seq(as.Date("2011/1/1"), as.Date("2012/12/31"), "days")
# Introduce lubridate
bike.rental.data$date = ymd(bike.rental.data$date)

# Create lubridate date elements
bike.rental.data$just.day = day(bike.rental.data$date)
bike.rental.data$just.dayofweek = wday(bike.rental.data$date) # numbered 1,2,3
bike.rental.data$just.dayofweek2 = wday(bike.rental.data$date, label=TRUE) #Monday, Tuesday etc
bike.rental.data$just.month = month(bike.rental.data$date) # numbered 1,2,3
bike.rental.data$just.month2 = month(bike.rental.data$date, label=TRUE) # January, February etc
bike.rental.data$year=year(bike.rental.data$date)

# Remove the columns no longer needed
bike.rental.data = select(bike.rental.data, -yr, -mnth, -weekday)

# Convert season, holiday, workingday and weathersit columns to factors
bike.rental.data[,1:4] = lapply(bike.rental.data[,1:4], factor)

# rename columns 
# cnt to rental_count
bike.rental.data = bike.rental.data %>% 
  rename(rental.count = cnt)
# weathersit to weather.category
bike.rental.data = bike.rental.data %>% 
  rename(weather.category = weathersit)
# temp to temperature
bike.rental.data = bike.rental.data %>% 
  rename(temperature = temp)
# rename hum to humidity
bike.rental.data = bike.rental.data %>% 
  rename(humidity = hum)
# rename windspeed to wind.speed
bike.rental.data = bike.rental.data %>% 
  rename(wind.speed = windspeed)
# rename workingday to working.day
bike.rental.data = bike.rental.data %>% 
  rename(working.day = workingday)
# rename days_since_2011 to days.since.2011
bike.rental.data = bike.rental.data %>% 
  rename(days.since.2011 = days_since_2011)

# Reorder columns 
bike.rental.data = select(bike.rental.data, rental.count, season, weather.category, 
                          temperature, humidity, wind.speed, holiday, working.day,
                          days.since.2011, date, just.day, just.dayofweek,
                          just.dayofweek2, just.month, just.month2, year)


# round the temp, wind speed and humidity  columns to remove decimal place
bike.rental.data[,4] = round(bike.rental.data[,4])
bike.rental.data[,5] = round(bike.rental.data[,5])
bike.rental.data[,6] = round(bike.rental.data[,6])


############## Create duplicate data frame for multiple linear regression analysis ##############
bike.rental.data.reg = bike.rental.data
# Check the levels for season
levels(bike.rental.data.reg$season)
# 1       2        3        4
#"FALL"   "SPRING" "SUMMER" "WINTER"
# change to numbers
bike.rental.data.reg$season = as.numeric(bike.rental.data.reg$season)


# Check the levels for weather.category
levels(bike.rental.data.reg$weather.category)
# 1       2        3        
# "GOOD"  "MISTY" "RAIN/SNOW/STORM"
# change to numbers
bike.rental.data.reg$weather.category = as.numeric(bike.rental.data.reg$weather.category)

# Check the levels for holiday
levels(bike.rental.data.reg$holiday)
# 1              2               
# "HOLIDAY"    "NO HOLIDAY"
# change to numbers
bike.rental.data.reg$holiday = as.numeric(bike.rental.data.reg$holiday)

# Check the levels for working.day
levels(bike.rental.data.reg$working.day)
# 1                 2               
# "NO WORKING DAY" "WORKING DAY" 
# change to numbers
bike.rental.data.reg$working.day = as.numeric(bike.rental.data.reg$working.day)

