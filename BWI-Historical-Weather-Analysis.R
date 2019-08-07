#######################
#### Load Packages ####
#######################

# For debugging rm(list=ls())

rm(list=ls())

install.packages("tidyverse")
install.packages("stringr")
install.packages("lubridate")
install.packages("readxl")
install.packages("janitor")
install.packages("weathermetrics")
install.packages("ggplot2")
install.packages("reshape")
install.packages("writexl")
install.packages("purrr")

library(tidyverse)
library(stringr)
library(lubridate)
library(readxl)
library(janitor)
library(weathermetrics)
library(ggplot2)
library(reshape)
library(writexl)

rm(list=ls())

##############################
##### LOAD DATA ##############
##############################

# Load Baltimore Inner Harbor Temperature Data
bwi_data <- read_csv("/Users/tdiff/Documents/Github/bwi_dmh_temps/old-data/baltimore-bwi-temperature/BWI.csv")

# Load Baltimore Inner Harbor Temperature Data
inner_harbor_data <- read_excel("/Users/tdiff/Documents/Github/bwi_dmh_temps/old-data/baltimore-inner-harbor-temperature/DMH.xlsx")

#Load new inner harbor data for July 4-30
new_inner_harbor_data <- read_csv("/Users/tdiff/Documents/Github/bwi_dmh_temps/old-data/DMH.txt")

##############################
##### CLEAN DATA #############
##############################

# Clean BWI data 
clean_bwi_data <- bwi_data %>%
  filter(tmpf != 'M', dwpf != 'M') %>%
  mutate(tmpf = as.numeric(tmpf),
         dwpf = as.numeric(dwpf)) %>%
  mutate(date = as.Date(valid, format="%Y-%m-%d")) %>%
  mutate(year=year(valid)) %>%
  mutate(month=month(valid)) %>%
  mutate(hour=hour(valid)) %>%
  mutate(day=day(valid)) %>%
  mutate(hour=hour(valid)) %>%
  distinct(valid, .keep_all = TRUE) %>%
  mutate(heat_index = heat.index(t=tmpf, dp=dwpf, temperature.metric = "fahrenheit", round=0)) %>%
  mutate(relative_humidity = dewpoint.to.humidity(dp = dwpf, t = tmpf, temperature.metric = "fahrenheit")) %>%
  select(date, year, month, day, hour, tmpf, dwpf, relative_humidity, heat_index) %>%
  group_by(date, year, month, day, hour) %>%
  summarise(avg_hourly_temperature_bwi = mean(tmpf),
            avg_hourly_dewpoint_bwi = mean(dwpf),
            avg_hourly_relative_humidity_bwi = mean(relative_humidity),
            avg_hourly_heat_index_bwi = mean(heat_index)
  ) %>%
  filter(!is.na(avg_hourly_relative_humidity_bwi)) %>%
  distinct()

###Save newly clean BWI data and read it in
write.csv(clean_bwi_data, "/Users/tdiff/Documents/Github/bwi_dmh_temps/clean_bwi_data.csv")
clean_bwi_data <- read.csv("/Users/tdiff/Documents/Github/bwi_dmh_temps/clean_bwi_data.csv") %>%
  mutate(date = as.Date(date, format="%Y-%m-%d"))

#######
############Clean Inner Harbor data 
#######

#Rename new inner harbor columns and change inner harbor columns to numerics so can append new inner harbor data to old
renamed_new_inner_harbor_data <- new_inner_harbor_data %>%
  select(-feel) %>%
  dplyr::rename(LOCATION = station,
         DATETIME = valid,
         TEMPERATURE = tmpf,
         DEW_POINT = dwpf,
         RELATIVE_HUMIDITY = relh)

numeric_inner_harbor_data <- inner_harbor_data %>%
  mutate(TEMPERATURE = as.numeric(TEMPERATURE),
         DEW_POINT = as.numeric(DEW_POINT),
         RELATIVE_HUMIDITY = as.numeric(RELATIVE_HUMIDITY))
  
bind_inner_harbor_data <- 
  dplyr::bind_rows(numeric_inner_harbor_data, renamed_new_inner_harbor_data) 

clean_inner_harbor_data <- bind_inner_harbor_data %>%
  filter(TEMPERATURE != 'M', DEW_POINT != 'M') %>%
  mutate(TEMPERATURE = as.numeric(TEMPERATURE),
         DEW_POINT = as.numeric(DEW_POINT)) %>%
  mutate(date = as.Date(DATETIME, format="%Y-%m-%d")) %>%
  mutate(year=year(DATETIME)) %>%
  mutate(month=month(DATETIME)) %>%
  mutate(hour=hour(DATETIME)) %>%
  mutate(day=day(DATETIME)) %>%
  mutate(hour=hour(DATETIME)) %>%
  distinct(DATETIME, .keep_all = TRUE) %>%
  mutate(heat_index = heat.index(t=TEMPERATURE, dp=DEW_POINT, temperature.metric = "fahrenheit", round=0)) %>%
  mutate(relative_humidity = dewpoint.to.humidity(dp = DEW_POINT, t = TEMPERATURE, temperature.metric = "fahrenheit")) %>%
  select(date, year, month, day, hour, TEMPERATURE, DEW_POINT, relative_humidity, heat_index) %>%
  group_by(date, year, month, day, hour) %>%
  summarise(avg_hourly_temperature_dmh = mean(TEMPERATURE),
            avg_hourly_dewpoint_dmh = mean(DEW_POINT),
            avg_hourly_relative_humidity_dmh = mean(relative_humidity),
            avg_hourly_heat_index_dmh = mean(heat_index)
  ) %>%
  filter(!is.na(avg_hourly_relative_humidity_dmh)) %>%
  distinct()

####Save and load clean inner harbor data
write.csv(clean_inner_harbor_data, "/Users/tdiff/Documents/Github/bwi_dmh_temps/clean_inner_harbor_data.csv")
clean_inner_harbor_data <- read.csv("/Users/tdiff/Documents/Github/bwi_dmh_temps/clean_inner_harbor_data.csv") %>%
  mutate(date = as.Date(date, format="%Y-%m-%d"))

###########################################
###### Adjust Inner Harbor Temperatures####
###########################################

# We have 75 years of dew point and temperature data for BWI, which allows us to calculate heat index
# We have 20 years of dew point and temperature data for DMH (Inner Harbor), which allows us to calculate heat index. 
# I talked with Dan Li, a climate researcher at Boston University who has studied variation in urban heat island temperatures between suburbs in different seasons and different hours of the day, who said a pretty good way to calculate the difference would be this:
# 1. Join the 20 years of DMH data to the 20 years of BWI data.  Join on date and hour, so we end up with a data frame of values for both DMH and BWI for each date and hour in our data set.  Do an inner join, so that if for some reason DMH is missing an hour from its data set (or vice versa), it doesn't mess up our averages. Calculate new columns with difference in temperature, dew point, relative humidity and heat index between BWI and DMH.  Since in almost all cases BWI will be colder than DMH, make it so the difference is positive if DMH is greater than BWI.
dmhbwi <- 
  inner_join(clean_bwi_data, clean_inner_harbor_data, by = c("date", "hour")) %>%
  dplyr::rename(year = year.x, month = month.x, day = day.x) %>%
  select(-year.y, -month.y, -day.y) %>%
  mutate(tempDiff = avg_hourly_temperature_dmh - avg_hourly_temperature_bwi,
         dewDiff = avg_hourly_dewpoint_dmh - avg_hourly_dewpoint_bwi,
         rhDiff = avg_hourly_relative_humidity_dmh - avg_hourly_relative_humidity_bwi,
         hiDiff = avg_hourly_heat_index_dmh - avg_hourly_heat_index_bwi) 

# 4. Group by month and hour and calculate the AVERAGE difference in temperature, dew point, relative humidity and heat index. 
summarized_join <- dmhbwi %>%
  group_by(month, hour) %>%
  summarize(avgTempDiff = mean(tempDiff),
         avgDewDiff = mean(dewDiff),
         avgRhDiff = mean(rhDiff),
         avgHiDiff = mean(hiDiff))
  
# 5. Go back to the original 75 years of BWI data (after it's been cleaned, not on raw import).  Write case when function adjusting the temperature and dew point values based on table created in step 4. Create two new columns to recalculate heat index and relative humidity (see cleaning function above, weathermetrics package has a nice way of doing this). Call them adjusted_heat_index, adjusted_relative_humidity 
join_bwi_data <-
 inner_join(summarized_join, clean_bwi_data, by = c("month", "hour")) %>%
  mutate(adjusted_temp = avg_hourly_temperature_bwi + avgTempDiff,
         adjusted_dew = avg_hourly_dewpoint_bwi + avgDewDiff,
         adjusted_heat_index = heat.index(t=adjusted_temp, dp=adjusted_dew, temperature.metric = "fahrenheit", round=0),
         adjusted_relative_humidity = dewpoint.to.humidity(dp = adjusted_dew, t = adjusted_temp, temperature.metric = "fahrenheit"))

#Take new adjusted columns and put into a new table as estimates for inner harbor data
estimated_inner_harbor_data <- join_bwi_data %>%
  select(-avgTempDiff, -avgDewDiff, -avgRhDiff, -avgHiDiff, -avg_hourly_temperature_bwi, 
         -avg_hourly_dewpoint_bwi, -avg_hourly_relative_humidity_bwi, -avg_hourly_heat_index_bwi) 
# %>% select(-X)

#Check estimated data against cleaned inner harbor data
compare_inner_harbor_data <- estimated_inner_harbor_data %>%
  right_join(clean_inner_harbor_data, by = c("date", "hour")) %>%
  mutate(difference_temp = adjusted_temp - avg_hourly_temperature_dmh,
         difference_dew = adjusted_dew - avg_hourly_dewpoint_dmh,
         difference_rh = adjusted_relative_humidity - avg_hourly_relative_humidity_dmh,
         difference_hi = adjusted_heat_index - avg_hourly_heat_index_dmh)

summarized_compare <- compare_inner_harbor_data %>%
  group_by(year.x, month.x) %>%
  filter(adjusted_heat_index != "NA") %>%
  summarize(avg_difference_temp = mean(difference_temp),
            avg_difference_dew = mean(difference_dew),
            avg_difference_rh = mean(difference_rh),
            avg_difference_ri = mean(difference_hi))

##join estimated inner harbor data with inner harbor data updated through 7-30-19
updated_estimated_inner_harbor_data <- estimated_inner_harbor_data %>%
  dplyr::rename(avg_hourly_temperature_dmh = adjusted_temp,
         avg_hourly_dewpoint_dmh = adjusted_dew,
         avg_hourly_relative_humidity_dmh = adjusted_relative_humidity,
         avg_hourly_heat_index_dmh = adjusted_heat_index) 
        # %>%  select(-X)

updated_estimated_inner_harbor_data <- 
  full_join(updated_estimated_inner_harbor_data, clean_inner_harbor_data) %>%
  arrange(date, hour)

#Save inner harbor data updated through 7-30-2019
write.csv(updated_estimated_inner_harbor_data, "/Users/tdiff/Documents/Github/bwi_dmh_temps/updated_estimated_inner_harbor_data.csv")

updated_estimated_inner_harbor_data <- read.csv("/Users/tdiff/Documents/Github/bwi_dmh_temps/updated_estimated_inner_harbor_data.csv") %>%
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>%
  select(-X)

###########################################
###### QUESTIONS ##########################
###########################################

#FOR INNER HARBOR: What have the patterns been with 90+, 103+ heat index days? What years have had the most of these days? What have the longest stretches been? Are there more recently that have been intense?
hi_over_ninety_dmh <- updated_estimated_inner_harbor_data %>%
  filter(avg_hourly_heat_index_dmh >= 90) %>%
  distinct(date, .keep_all=TRUE) %>%
  group_by(year) %>%
  summarise(count=n()) #%>%
  #summarise(average_days = mean(count))
  
ggplot(data = hi_over_ninety_dmh, aes(x=year, y=count)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "#Days When Heat Index > 90 in Inner Harbor",
       x = "Year",
       y = "#Days")
ggsave(filename = "dmh_days_heat_index_over_ninety.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

###similar to above but for hours instead of days
hi_over_ninety_dmh <- updated_estimated_inner_harbor_data %>%
  filter(avg_hourly_heat_index_dmh >= 90) %>%
  group_by(year) %>%
  summarise(count=n())

ggplot(data = hi_over_ninety_dmh, aes(x=year, y=count)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "#Hours When Heat Index > 90 in Inner Harbor",
       x = "Year",
       y = "#Hours")
ggsave(filename = "dmh_hours_heat_index_over_ninety.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#Inner harbor heat index >= 100
hi_over_100_dmh <- updated_estimated_inner_harbor_data %>%
  filter(avg_hourly_heat_index_dmh >= 100) %>%
  distinct(date, .keep_all=TRUE) %>%
  group_by(year) %>%
  filter(year > 1980,
         year < 2013) %>%
  summarise(count=n()) #%>% 
  #summarise(avg_days_per_year = mean(count))

#write_xlsx(hi_over_100_dmh, "/Users/tdiff/Documents/Github/bwi_dmh_temps/data-for-others/hi_over_100_dmh.xlsx")

ggplot(data = hi_over_100_dmh, aes(x=year, y=count)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "#Days When Heat Index > 100 in Inner Harbor",
       x = "Year",
       y = "Days")
ggsave(filename = "dmh_days_heat_index_over_100.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#Inner harbor heat index >= 105
hi_over_105_dmh <- updated_estimated_inner_harbor_data %>%
  filter(avg_hourly_heat_index_dmh >= 105) %>%
  distinct(date, .keep_all=TRUE) %>%
  group_by(year) %>%
  summarise(count=n()) #%>%
  #summarise(average_days = mean(count))

ggplot(data = hi_over_105_dmh, aes(x=year, y=count)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "#Days When Heat Index > 105 in Inner Harbor",
       x = "Year",
       y = "Days")
ggsave(filename = "dmh_days_heat_index_over_103.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#same as above but for hours, not days
hi_over_103_dmh <- updated_estimated_inner_harbor_data %>%
  filter(avg_hourly_heat_index_dmh >= 103) %>%
  group_by(year, month) %>%
  summarise(count=n())

ggplot(data = hi_over_103_dmh, aes(x=year, y=count)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "#Hours When Heat Index > 103 in Inner Harbor",
       x = "Year",
       y = "#Hours")
ggsave(filename = "dmh_hours_heat_index_over_103.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#Looks like 1980s and 1990s had spikes in the amount of days with heat indexes greater than 90 or 103, with current years trending downward. The only months with hi > 103 are June-September. Months with hi>90 range from May-September, with a couple Octobers every down and then, but no more in recent years than in the past

####BWI DATA####
#BWI over 90 data
hi_over_ninety_bwi <- clean_bwi_data %>%
  #  select(-X) %>%
  filter(avg_hourly_heat_index_bwi >= 90) %>%
  distinct(date, .keep_all=TRUE) %>%
  group_by(year) %>%
  summarise(count=n()) #%>%
  #summarise(average_days = mean(count))

ggplot(data = hi_over_ninety_bwi, aes(x=year, y=count)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "#Days When Heat Index > 90 in BWI",
       x = "Year",
       y = "Days")
ggsave(filename = "bwi_days_heat_index_over_ninety.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#same as above but #hours instead of days
hi_over_ninety_bwi <- clean_bwi_data %>%
  select(-X) %>%
  filter(avg_hourly_heat_index_bwi >= 90) %>%
  group_by(year) %>%
  summarise(count=n())

ggplot(data = hi_over_ninety_bwi, aes(x=year, y=count)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "#Hours When Heat Index > 90 in BWI",
       x = "Year",
       y = "#Hours")
ggsave(filename = "bwi_hours_heat_index_over_ninety.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#BWI over 100 data
hi_over_100_bwi <- clean_bwi_data %>%
  select(-X) %>%
  filter(avg_hourly_heat_index_bwi >= 100) %>%
  distinct(date, .keep_all=TRUE) %>%
  group_by(year) %>%
  summarise(count=n()) #%>%
  #summarise(average_days = mean(count))

ggplot(data = hi_over_100_bwi, aes(x=year, y=count)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "#Days When Heat Index > 100 in BWI",
       x = "Year",
       y = "Days")
ggsave(filename = "bwi_days_heat_index_over_100.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#BWI over 105 data
hi_over_105_bwi <- clean_bwi_data %>%
  select(-X) %>%
  filter(avg_hourly_heat_index_bwi >= 105) %>%
  distinct(date, .keep_all=TRUE) %>%
  group_by(year) %>%
  summarise(count=n()) #%>%
  #summarise(average_days = mean(count))

ggplot(data = hi_over_105_bwi, aes(x=year, y=count)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "#Days When Heat Index > 105 in BWI",
       x = "Year",
       y = "Days")
ggsave(filename = "bwi_days_heat_index_over_105.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#same as above but #hours instead of days
hi_over_103_bwi <- clean_bwi_data %>%
  select(-X) %>%
  filter(avg_hourly_heat_index_bwi >= 103) %>%
  group_by(year, month) %>%
  summarise(count=n())

ggplot(data = hi_over_103_bwi, aes(x=year, y=count)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "#Hours When Heat Index > 103 in BWI",
       x = "Year",
       y = "#Hours")
ggsave(filename = "bwi_hours_heat_index_over_103.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#Inner harbor data for July/August: Have the hottest months of the year (July, August) gotten more hot on average?
#done by building a multi-line graph by making months into separate columns, joining, and then plotting; geom_smooth lines are easier to read than geom_line
july_dmh <- updated_estimated_inner_harbor_data %>%
  filter(month == 7,
         avg_hourly_heat_index_dmh != "NA") %>%
  group_by(year, month) %>%
  summarise(monthly_average_heat_index_july = mean(avg_hourly_heat_index_dmh))

august_dmh <- updated_estimated_inner_harbor_data %>%
  filter(month ==8,
         avg_hourly_heat_index_dmh != "NA") %>%
  group_by(year, month) %>%
  summarise(monthly_average_heat_index_august = mean(avg_hourly_heat_index_dmh))

july_august_dmh <-
  inner_join(july_dmh, august_dmh, by="year")

ggplot(data = july_august_dmh, aes(x=year)) + 
  geom_smooth(aes(y=monthly_average_heat_index_july, color = "July")) +
  geom_smooth(aes(y=monthly_average_heat_index_august, color = "August")) + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "Inner Harbor July and August Average Heat Indexes",
       x = "Year",
       y = "Average Heat Index")

ggsave(filename = "dmh_july_august_heat_indexes_smoothed.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

##BWI July/August Average Heat Index Data: Have the hottest months of the year (July, August) gotten more hot on average?
july_bwi <- clean_bwi_data %>%
  filter(month == 7) %>%
  group_by(year, month) %>%
  summarise(monthly_average_heat_index_july = mean(avg_hourly_heat_index_bwi))

august_bwi <- clean_bwi_data %>%
  filter(month ==8) %>%
  group_by(year, month) %>%
  summarise(monthly_average_heat_index_august = mean(avg_hourly_heat_index_bwi))

july_august_bwi <-
  inner_join(july_bwi, august_bwi, by="year")

ggplot(data = july_august_bwi, aes(x=year)) + 
  geom_smooth(aes(y=monthly_average_heat_index_july, color = "July")) +
  geom_smooth(aes(y=monthly_average_heat_index_august, color = "August")) + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "BWI July and August Average Heat Indexes",
       x = "Year",
       y = "Average Heat Index")

ggsave(filename = "bwi_july_august_heat_indexes_smoothed.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#It does seem that the average heat index in July and August are rising, but they're rising back to a level reached about 70 years ago

# How many days have not had a heat index BELOW 80 degrees, even at night? Are there more frequent recent stretches. Use the minimum heat index value for each day using summarise
#Data for BWI
bwi_days_above_80 <- clean_bwi_data %>%
  group_by(year, date) %>%
  summarise(min_heat_index = min(avg_hourly_heat_index_bwi)) %>%
  filter(min_heat_index >= 80) %>%
  summarise(bwi_days=n())
#count is 52

ggplot(data=bwi_days_above_80, aes(x=year, y=bwi_days)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "BWI #Days With Minimum Heat Index Above 80 Degrees",
       x = "Year",
       y = "Number of Days")

ggsave(filename = "bwi_days_above_80_col.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#Data for dmh
dmh_days_above_80 <- updated_estimated_inner_harbor_data %>%
  group_by(year, date) %>%
  summarise(min_heat_index = min(avg_hourly_heat_index_dmh)) %>%
  filter(min_heat_index >= 80) %>%
  summarise(dmh_days=n())
#count is 322

ggplot(data=dmh_days_above_80, aes(x=year, y=dmh_days)) + 
  geom_col() + 
  geom_smooth() + 
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "Inner Harbor Number of Days Where Heat Index Stayed Above 80 Degrees",
       x = "Year",
       y = "Number of Days")

ggsave(filename = "dmh_days_above_80_col.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

days_above_80 <- full_join(dmh_days_above_80, bwi_days_above_80, by=c("year"))
days_above_80[is.na(days_above_80)] <- 0

ggplot(data=days_above_80, aes(x=year)) + 
  geom_smooth(aes(y=dmh_days, fill = "Inner Harbor")) +
  geom_smooth(aes(y=bwi_days, fill = "BWI")) +
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "Number of Days Where Heat Index Stayed Above 80 Degrees",
       x = "Year",
       y = "Number of Days")

ggsave(filename = "days_above_80_smoothed.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

#tried using geom_col, but data wouldn't change when position set to dodge. Tried to melt but didn't work

#melt_days_above_80 <- melt(days_above_80, id = c("year"), measure=c("dmh_days", "bwi_days"))


##day to night temp differences between BWI and inner harbor on July 2, 2019, hottest day we have on data for 2019 so far
daily_temp_differences <-
  left_join(clean_inner_harbor_data, clean_bwi_data, by=c("date", "hour")) %>%
  dplyr::rename(year = year.x, month = month.x, day = day.x) %>%
  select(-year.y, -month.y, -day.y) %>%
  filter(date == "2019-07-02")

  daily_temp_differences[is.na(daily_temp_differences)] <- 0 

ggplot(data=daily_temp_differences, aes(x=hour)) + 
  geom_line(aes(y=avg_hourly_heat_index_dmh, color = "Inner Harbor")) +
  geom_line(aes(y=avg_hourly_heat_index_bwi, color = "BWI")) +
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "Heat Index Differences 2019-07-02",
       x = "Hour",
       y = "Heat Index")

ggsave(filename = "heat_index_differences_july_second.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

twenty_eighteen_temp_differences <-
  left_join(clean_inner_harbor_data, clean_bwi_data, by=c("date", "hour")) %>%
  dplyr::rename(year = year.x, month = month.x, day = day.x) %>%
  select(-X.x, -year.y, -month.y, -day.y) %>%
  filter(year == "2018",
         month == "7") %>%
  mutate(HiDiff = avg_hourly_heat_index_dmh - avg_hourly_heat_index_bwi) %>%
  group_by(hour) %>%
  summarise(meanDifference = mean(HiDiff))

ggplot(data=twenty_eighteen_temp_differences, aes(x=hour, y=meanDifference)) + 
  geom_line() +
  theme(plot.title = element_text(size = 25),
        axis.title = element_text(size=20),
        axis.text = element_text(size=15)) +
  labs(title = "Average Heat Index Differences in July 2018",
       subtitle = "Inner Harbor - BWI",
       x = "Hour",
       y = "Heat Index")

ggsave(filename = "heat_index_differences_july_twenty_eighteen.png",
       device = "png", path = "/Users/tdiff/Documents/Github/bwi_dmh_temps/Graphs",
       width = 20, height = 15, units = "in")

##################
#### RANDOM CODE THAT MIGHT BE HELPFUL#
##################

##Daily summary stats for bwi(original data) and dmh (using calculated/estimated data)
daily_summary_stats_bwi <- clean_bwi_data %>%
  mutate(date = as.Date(date, format="%Y-%m-%d")) %>%
  group_by(date) %>%
  summarise(avg_daily_temperature = mean(avg_hourly_temperature_bwi),
            avg_daily_dewpoint = mean(avg_hourly_dewpoint_bwi),
            avg_daily_relative_humidity = mean(avg_hourly_relative_humidity_bwi),
            avg_daily_heat_index = mean(avg_hourly_heat_index_bwi),
            max_daily_temperature = max(avg_hourly_temperature_bwi),
            max_daily_dewpoint = max(avg_hourly_dewpoint_bwi),
            max_daily_relative_humidity = max(avg_hourly_relative_humidity_bwi),
            max_daily_heat_index = max(avg_hourly_heat_index_bwi)) 

daily_summary_stats_dmh <- estimated_inner_harbor_data
group_by(date) %>%
  summarise(avg_daily_temperature = mean(avg_hourly_temperature),
            avg_daily_dewpoint = mean(avg_hourly_dewpoint),
            avg_daily_relative_humidity = mean(avg_hourly_relative_humidity),
            avg_daily_heat_index = mean(avg_hourly_heat_index),
            max_daily_temperature = max(avg_hourly_temperature),
            max_daily_dewpoint = max(avg_hourly_dewpoint),
            max_daily_relative_humidity = max(avg_hourly_relative_humidity),
            max_daily_heat_index = max(avg_hourly_heat_index)) 

days_index_90_plus <- daily_summary_stats %>%
  filter(max_daily_heat_index >= 90)

year_days_index_90_plus <- days_index_90_plus %>%
  group_by(year(date)) %>%
  summarise(count=n())

barplot(year_days_index_90_plus$count)


daily_summary_stats <- temp_data %>%
  group_by(date) %>%
  summarise(avg_daily_temperature = mean(avg_hourly_temperature),
            avg_daily_dewpoint = mean(avg_hourly_dewpoint),
            avg_daily_relative_humidity = mean(avg_hourly_relative_humidity),
            avg_daily_heat_index = mean(avg_hourly_heat_index),
            max_daily_temperature = max(avg_hourly_temperature),
            max_daily_dewpoint = max(avg_hourly_dewpoint),
            max_daily_relative_humidity = max(avg_hourly_relative_humidity),
            max_daily_heat_index = max(avg_hourly_heat_index)) 


test <- daily_summary_stats %>%
  filter(is.na(avg_daily_relative_humidity))

monthly_summary_stats <- temp_data %>%
  group_by(month) %>%
  summarise(avg_monthly_temperature = mean(avg_hourly_temperature),
            avg_monthly_dewpoint = mean(avg_hourly_dewpoint),
            avg_monthly_relative_humidity = mean(avg_hourly_relative_humidity),
            avg_monthly_heat_index = mean(avg_hourly_heat_index),
            max_monthly_temperature = max(avg_hourly_temperature),
            max_monthly_dewpoint = max(avg_hourly_dewpoint),
            max_monthly_relative_humidity = max(avg_hourly_relative_humidity),
            max_monthly_heat_index = max(avg_hourly_heat_index)
  ) 

year_month_summary_stats <- temp_data %>%
  group_by(year, month) %>%
  summarise(avg_daily_temperature = mean(avg_hourly_temperature),
            avg_daily_dewpoint = mean(avg_hourly_dewpoint),
            avg_daily_relative_humidity = mean(avg_hourly_relative_humidity),
            avg_daily_heat_index = mean(avg_hourly_heat_index),
            max_daily_temperature = max(avg_hourly_temperature),
            max_daily_dewpoint = max(avg_hourly_dewpoint),
            max_daily_relative_humidity = max(avg_hourly_relative_humidity),
            max_daily_heat_index = max(avg_hourly_heat_index)
  ) 

temp_data <- temp_data %>%
  group_by(year, month) %>%
  summarise(count = n()) %>%
  arrange(year, month)


###Data for amina/maris/adam
amina_clean_inner_harbor_data <- clean_inner_harbor_data %>%
  select(date, year, month, avg_hourly_temperature_dmh, avg_hourly_heat_index_dmh) %>%
  group_by(year, month) %>%
  summarise(mean_monthly_temperature = mean(avg_hourly_temperature_dmh),
            mean_monthly_heat_index = mean(avg_hourly_heat_index_dmh))

#hourly heat index from noon on June 27-end day July 3
adam_clean_inner_harbor_data <- clean_inner_harbor_data %>%
  select(date, hour, avg_hourly_heat_index_dmh) %>%
  filter(date >= as.Date("2019-06-27")) %>%
  filter(row_number() != 1:12)
adam_clean_inner_harbor_data <- write_xlsx(adam_clean_inner_harbor_data, "/Users/tdiff/Documents/Github/bwi_dmh_temps/data-for-others/adam_clean_inner_harbor_data.xlsx")
 
#Adam inner harbor data for July 16-22
adam_inner_harbor_data_july_sixteen <- new_inner_harbor_data  %>%
  select(-avg_hourly_dewpoint_dmh, -avg_hourly_relative_humidity_dmh) %>%
  filter(date > "2019-07-15", 
         date < "2019-07-23")
write_xlsx(adam_inner_harbor_data_july_sixteen, "/Users/tdiff/Documents/Github/bwi_dmh_temps/data-for-others/adam_inner_harbor_data_july_sixteen.xlsx")

#Sean data june and july 2019
sean_june_july_inner_harbor_data <- clean_inner_harbor_data %>%
  filter(date > "2019-05-31")
write_xlsx(sean_june_july_inner_harbor_data, "/Users/tdiff/Documents/Github/bwi_dmh_temps/data-for-others/sean_june_july_inner_harbor_data.xlsx")

#read in and clean updated BWI data
clean_updated_bwi_data <- 
  read_csv("/Users/tdiff/Documents/Github/bwi_dmh_temps/BWI.txt") %>%
  filter(tmpf != 'M', dwpf != 'M') %>%
  mutate(tmpf = as.numeric(tmpf),
         dwpf = as.numeric(dwpf)) %>%
  mutate(date = as.Date(valid, format="%Y-%m-%d")) %>%
  mutate(year=year(valid)) %>%
  mutate(month=month(valid)) %>%
  mutate(hour=hour(valid)) %>%
  mutate(day=day(valid)) %>%
  mutate(hour=hour(valid)) %>%
  distinct(valid, .keep_all = TRUE) %>%
  mutate(heat_index = heat.index(t=tmpf, dp=dwpf, temperature.metric = "fahrenheit", round=0)) %>%
  mutate(relative_humidity = dewpoint.to.humidity(dp = dwpf, t = tmpf, temperature.metric = "fahrenheit")) %>%
  select(date, year, month, day, hour, tmpf, dwpf, relative_humidity, heat_index) %>%
  group_by(date, year, month, day, hour) %>%
  summarise(avg_hourly_temperature_bwi = mean(tmpf),
            avg_hourly_dewpoint_bwi = mean(dwpf),
            avg_hourly_relative_humidity_bwi = mean(relative_humidity),
            avg_hourly_heat_index_bwi = mean(heat_index)
  ) %>%
  filter(!is.na(avg_hourly_relative_humidity_bwi)) %>%
  distinct()

write_xlsx(clean_updated_bwi_data, "/Users/tdiff/Documents/Github/bwi_dmh_temps/clean_updated_bwi_data.xlsx")