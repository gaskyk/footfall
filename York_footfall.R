#############################################
# Analyse York City Centre footfall         #
#                                           #
# Date: December 2018                       #
# Author: Gaskyk                            #
#############################################

library(tidyverse)
library(leaflet)

# Read in data about footfall in York and convert date to date format
raw_data <- read.csv("~/Footfall/footfallhourly.csv",
                          stringsAsFactors = FALSE)
raw_data$Date <- as.POSIXct(raw_data$Date, format="%d/%m/%Y %H:%M:%S")

# Create fields for year, month and hour
raw_data$year <- as.numeric(format(raw_data$Date,'%Y'))
raw_data$year_month <- paste(raw_data$year, as.character(format(raw_data$Date,'%m')), sep = '-')
raw_data$month <- month.abb[(as.numeric(format(raw_data$Date,'%m')))] # month.abb[] gets Jan, Feb from numeric
raw_data$hour <- as.numeric(format(raw_data$Date,'%H'))

## Map: Average Saturday footfall by hour and location in 2018
# Calculate data for map input
sat_2018 <- raw_data %>%
              filter(year==2018) %>%
              filter(WeekDay=='Saturday') %>%
              group_by(LocationName, hour) %>%
              summarise(av_footfall = mean(TotalCount))

# Add latitude and longitude locations of footfall cameras
footfall_lat_long <- data.frame("Footfall_locations" = c("Parliament Street", "Coney Street",
                                                         "Micklegate", "Stonegate"),
                                "Long" = c(-1.0807997711, -1.0839766714, -1.0887644223,
                                           -1.0836852375),
                                "Lat" = c(53.9589757478, 53.958667443, 53.957166915,
                                          53.960840365),
                                stringsAsFactors = FALSE)
sat_2018 <- merge(sat_2018, footfall_lat_long, by.x="LocationName",
                             by.y="Footfall_locations")

# Select by hour
this_hour = 0
full_time = paste(this_hour, ":00", sep="")
by_hour <- sat_2018 %>%
            filter(hour==this_hour)

# Map
m <- leaflet(by_hour) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  setView(lng=-1.0835, lat=53.9592, zoom = 16) %>%
  addCircles(lng = ~Long, lat = ~Lat, weight = 1,
             radius = ~sqrt(av_footfall)*2, popup = ~LocationName) %>%
  addLabelOnlyMarkers(~-1.0785, ~53.957344, label =  ~full_time, 
                      labelOptions = labelOptions(noHide = T, textsize = "20px"))
m


## Graph: Average hourly footfall over an average week in 2018
# Get data for graph
weekly_2018 <- raw_data %>%
  filter(year==2018) %>%
  group_by(WeekDay, hour) %>%
  summarise(av_footfall = mean(TotalCount, na.rm=TRUE))
weekly_2018$WeekDay <- factor(weekly_2018$WeekDay, levels= c("Monday", "Tuesday",
                            "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Graph
ggplot(data = weekly_2018, aes(x = hour, y = av_footfall)) + geom_line() +
  facet_wrap(~WeekDay) + labs(x='Hour', y='Average footfall',
                              title='Average weekly footfall in York in 2018') +
  theme(plot.title = element_text(hjust = 0.5))


## Graph: Monthly footfall, 2009 to 2018
# Get data
monthly <- raw_data %>%
  group_by(month) %>%
  summarise(av_footfall = mean(TotalCount, na.rm=TRUE)) %>%
  na.omit
monthly$month = factor(monthly$month, levels = month.abb)

# Graph
ggplot(data = monthly, aes(x = month, y = av_footfall, group=1)) + geom_line() +
  labs(x='Month', y='Average footfall', title='Average monthly footfall, 2009 to 2018') +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0,1200)


## Graph: Month and yearly footfall, 2009 to 2018
# Get data
year_monthly <- raw_data %>%
  group_by(year_month) %>%
  summarise(av_footfall = mean(TotalCount, na.rm=TRUE)) %>%
  filter(year_month != 'NA-NA')

# Graph
ggplot(data = year_monthly, aes(x = year_month, y = av_footfall, group=1)) + geom_line() +
  labs(y='Average footfall', title='Average monthly footfall, 2009 to 2018') +
  theme(plot.title = element_text(hjust = 0.5)) + ylim(0, 1500) +
  scale_x_discrete('Year',
                   c('2010-01', '2011-01', '2012-01', '2013-01', '2014-01', '2015-01',
                     '2016-01', '2017-01', '2018-01'),
                   c('2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018'))


# Average footfall drops from 2013, why is that?
# Graph locations over time
# Get data
year_location <- raw_data %>%
  group_by(year, LocationName) %>%
  summarise(av_footfall = mean(TotalCount, na.rm=TRUE)) %>%
  filter(year != 'NA')

# Graph
ggplot(data = year_location, aes(x = year, y = av_footfall, colour=LocationName)) + geom_line() +
  labs(y='Average footfall', title='Average yearly footfall by location, 2009 to 2018') +
  theme(plot.title = element_text(hjust = 0.5))




