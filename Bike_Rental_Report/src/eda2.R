# EDA get summary statistics

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

# Median and mean are very similar so go with the mean


