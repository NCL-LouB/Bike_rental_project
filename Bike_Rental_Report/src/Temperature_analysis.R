##### Temperature

# Review
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


##### Figure 1 - plot temperature and rental count over time with a dual Y axis

# Dual Y axis with ggplot2
# value used to transform the data
coeff = 100
# Select colours
temperatureColour = "#FFA500"
rentalColour = "#02075d"

# plot
ggplot(bike.rental.data, aes(x=date)) +
  
  geom_point(aes(y=temperature), color = temperatureColour) +
  geom_point(aes(y=rental.count/ coeff), color = rentalColour) +
  
  scale_y_continuous(
    # Features of the first axis
    name = "Temperature (°C)",
    breaks = round(seq(min(bike.rental.data$temperature), max(bike.rental.data$temperature), by = 5),1),
    #Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name = "Number of rentals per day")
  ) +
  
  theme_classic() +
  theme(axis.title.y = element_text(color = temperatureColour, size=12, vjust = 1),
        axis.title.y.right = element_text(color = rentalColour, size=12, vjust = 1),
        axis.title.x = element_text(size=12),
        plot.subtitle = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 12),
        plot.caption = element_text(size = 12)) +
  ggtitle(label = "Figure 1",
          subtitle = "Rental count and temperature per day") +
  labs(caption = "Data source: bike.rental.data") + 
  xlab("Date")


# FIGURE 2 - plot median daily rentals by temperature - seperate 2011 and 2012

# Derive informaiton about the number of rentals by temperature and separate by year
rentalsBYtemp = bike.rental.data %>% 
  group_by(temperature, year) %>%
  summarise(totalrentals = sum(rental.count),
            median.daily.rentals = median(rental.count),
            mean.daily.rentals = mean(rental.count),
            sd.daily.rentals = sd(rental.count),
            percentage = round((totalrentals/3292679)*100)) # 3,292,679 = sum(bike.rental.data$rental.count))

# Change year from numeric to character for plot functionality
rentalsBYtemp$year = as.character(rentalsBYtemp$year)

# Extract the 2011 and 2012 observation which has the highest median daily rental number
highlight_df <- rentalsBYtemp %>% 
  group_by(year) %>%
  filter(median.daily.rentals == max(median.daily.rentals))

# Plot
ggplot(rentalsBYtemp) +
  
  geom_point(aes(x=temperature, y=median.daily.rentals, color = year)) +
  geom_line(data = filter(rentalsBYtemp, year == 2012), aes(x=temperature, y=median.daily.rentals), color = "#02075d") +
  geom_line(data = filter(rentalsBYtemp, year == 2011),  aes(x=temperature, y=median.daily.rentals), color = "#FFA500") +
  geom_point(data=highlight_df, aes(x=temperature,y=median.daily.rentals), color='red') +
  geom_text(data=highlight_df, aes(x=temperature,y=median.daily.rentals, label= as.character(temperature)),
            hjust=1,vjust=0, nudge_x = 2, colour = "red", fontface = "bold") +
  
  theme_classic() +
  theme(plot.subtitle = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 12)) +
  
  scale_x_continuous(breaks = round(seq(min(rentalsBYtemp$temperature), max(rentalsBYtemp$temperature), by = 5),1)) +
  scale_colour_manual(values = c("2011" = "#FFA500", "2012" = "#02075d")) +
  
  ggtitle(label = "Figure 2",
          subtitle = "Median number of rentals per day by temperature and year") +
  labs(caption = "Data source: bike.rental.data") +
  ylab("Median number of daily rentals") + xlab("Temperature (°C)")



