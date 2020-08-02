# Example preprocessing script.

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

# Reorder columns 
bike.rental.data = select(bike.rental.data, cnt, season, weathersit, 
                          temp, hum, windspeed, holiday, workingday,
                          days_since_2011, date, just.day, just.dayofweek,
                          just.dayofweek2, just.month, just.month2, year)

# rename cnt to rental_count
bike.rental.data = bike.rental.data %>% 
  rename(rental_count = cnt)

# round the temp, wind speed and humidity  columns to remove decimal place
bike.rental.data[,4] = round(bike.rental.data[,4])
bike.rental.data[,5] = round(bike.rental.data[,5])
bike.rental.data[,6] = round(bike.rental.data[,6])

# Create separate dataframe for regression 
bike.rental.data.reg = select(bike.rental.data, rental_count, weathersit, 
                              temp, hum, windspeed)

#create duplicate data frame
bike.rental.data2 = bike.rental.data
# Check the levels for season
levels(bike.rental.data2$season)
# 1       2        3        4
#"FALL"   "SPRING" "SUMMER" "WINTER"
# change to numbers
bike.rental.data2$season = as.numeric(bike.rental.data2$season)


# Check the levels for weathersit
levels(bike.rental.data2$weathersit)
# 1       2        3        
# "GOOD"  "MISTY" "RAIN/SNOW/STORM"
# change to numbers
bike.rental.data2$weathersit = as.numeric(bike.rental.data2$weathersit)

# Check the levels for holiday
levels(bike.rental.data2$holiday)
# 1              2               
# "HOLIDAY"    "NO HOLIDAY"
# change to numbers
bike.rental.data2$holiday = as.numeric(bike.rental.data2$holiday)

# Check the levels for workingday
levels(bike.rental.data2$workingday)
# 1                 2               
# "NO WORKING DAY" "WORKING DAY" 
# change to numbers
bike.rental.data2$workingday = as.numeric(bike.rental.data2$workingday)

