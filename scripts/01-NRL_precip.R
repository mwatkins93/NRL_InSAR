## ORGANISATION ---------
## Name: MGW
## Date: 2024-07-18
## Project: NRL_precip_sf
## Objective: (1) Use 'weathercan to find 2016-2024 precip data for Lansdowne House and Ogoki Post stations and plot daily and monthly summaries' (2) Potentially find streamflow data for the same period - ideally the river where the proposed holes are running along
## Inputs: raw weathercan data
## Outputs: precipitation plots
## ----------------------

## 0. NOTES ----

### 0.1 Change preferences so you can see all the columns of a dataframe in the viewer ----

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 100L)

### 0.2 A vector for changing the display of the x-axis on the following plots to these ----

month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")

## 1. PREPARE ----

### Remove the comments (#) and run lines 29 and 31 if you've never ran R before. These install packages needed for this analysis
### Descriptions:
### tidyverse - core/meta R package used for everything
### readxl - functions for importing excel data
### lubridate - makes working with date/times easier
### weathercan - ECCC climate data
### lutz and sf - for timezones. weathercan pkg requires them to work

# install.packages(c("tidyverse", "readxl", "lubridate", "lutz", "sf"))

# install.packages("weathercan", 
                 # repos = c("https://ropensci.r-universe.dev", 
                 #        "https://cloud.r-project.org"))

### I just run these at the beginning of every R session - rm(list = ls()) clears the "Environment" tab on the right; options sets behaviour of strings to non-factor, scipen removes scientific notation and encoding sets encoding to UTf-8
rm(list = ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

### Loads packages so their functions can be used
library(tidyverse)
library(readxl)
library(weathercan)
library(lubridate)

## 2. IMPORT ----

## Nothing

## 3. TIDY // PROCESS ----

### 3.01 This downloads the latest version of weathercan's station database ----

stations_dl()

## 3.03 Search for Lansdowne House and Ogoki Post A to acquire their station ID used in 3.04 ----

stations_search(name = "Lansdowne") # id: 10244
stations_search(name = "Ogoki Post A") # id: 52898

## 3.04 Pull all daily data from the start of 2016 to most recent for both stations ----

lansdowne <- weather_dl(station_ids = 10244, start = "2016-01-01", end = "2024-07-17", interval = "day")

ogoki <- weather_dl(station_ids = 52898, start = "2015-08-01", end = "2024-07-17", interval = "day") # Ogoki is problematic - had to pull from 2015 and exclude up until 2016-01-01. Perhaps because the first half of 2016 is filled with NAs...

### 3.05 We only need certain columns, so this extracts the necessary columns ----

lansdowne_clean <- lansdowne[, c(11:14, 32)]

ogoki_clean <- ogoki[, c(11:14, 32)]

ogoki_2016 <- ogoki[-c(1:153), ]

ogoki_2016[, c(11:14, 32)]

## 3.06 Grouping by year and month and summarising the 'monthly_precip' column to create monthly summaries for each station. Note: na.rm removes the NAs so the totals don't become NA ----

## Lansdowne
lansdowne_monthly <- lansdowne_clean %>% 
  group_by(year, month) %>% 
  summarise(monthly_precip = sum(total_precip, na.rm = TRUE))

## Ogoki  
ogoki_monthly <- ogoki_clean %>% 
  group_by(year, month) %>% 
  summarise(monthly_precip = sum(total_precip, na.rm = TRUE))

## 4. PLOTTING ----

### 4.01 Daily and monthly plots for Lansdowne with ggplot2 ----

### Daily
lansdowne_clean %>% 
  ggplot(aes(x = date, y = total_precip)) +
  
  # Bar plot
  geom_bar(stat = "identity", fill = "#6aadfb") +
  
  # Axes - scale_y sets y-axis to display values 0-100; scale_x shows every year and make the labels show year with century (i.e., type 'strptime' into search bar inside the Help tab and see the formats to see why this works); labs changes the axis titles
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "", y = "Daily precipitation (mm)") +
  
  # Theme - changes theme to light, adds a title and centers it
  theme_light() +
  ggtitle("Lansdowne House (AUT)") +
  theme(plot.title = element_text(hjust = 0.5))

### Monthly
lansdowne_monthly %>% 
  ggplot(aes(x = month, y = monthly.precip)) +
  facet_wrap(~ year) + # makes multiple plots separated by year
  
  # Bar plot
  geom_bar(stat = "identity", fill = "#6aadfb") +
  
  # Axes
  labs(x = "", y = "Monthly precipitation (mm)") +
  scale_y_continuous(limits = c(0, 200)) +

  # Theme
  theme_light() +
  theme(strip.background = element_rect(fill = "white"), # strip.background/strip.text changes the facet labels
        strip.text = element_text(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 2.5)) +
  scale_x_discrete(labels = month_list) + # using that month list vector here for labels
  ggtitle("Lansdowne House (AUT)")

### 4.02 Daily and monthly plots for Ogoki ----

### Daily
ogoki_clean %>% 
  ggplot(aes(x = date, y = total_precip)) +
    
  # Bar plot
  geom_bar(stat = "identity", fill = "#6aadfb") +
    
  # Axes
  scale_y_continuous(limits = c(0, 80)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "", y = "Daily precipitation (mm)") +
  
  # Theme
  theme_light() +
  ggtitle("Ogoki Post") +
  theme(plot.title = element_text(hjust = 0.5))

### Monthly
ogoki_monthly %>% 
  ggplot(aes(x = month, y = monthly.precip)) +
  facet_wrap(~ year) +
  
  # Bar plot
  geom_bar(stat = "identity", fill = "#6aadfb") +
  
  # Axes
  labs(x = "", y = "Monthly precipitation (mm)") +
  scale_y_continuous(limits = c(0, 150)) +
  
  # Theme
  theme_light() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black")) +
  scale_x_discrete(labels = month_list) +
  ggtitle("Ogoki Post A") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 2.5))

## 5. SAVING // EXPORTING ----

### 5.01 Saving the raw precipitation variables to csv files using the write_csv function ----

write_csv(lansdowne, file = "X:\\PROJECTS\\CURRENT\\2021-004 SNC_Northern Road Link\\NRL_precip_sf\\data\\lansdowne_precip.csv")

write_csv(ogoki_2016, file = "X:\\PROJECTS\\CURRENT\\2021-004 SNC_Northern Road Link\\NRL_precip_sf\\data\\ogoki_precip.csv")

## 6. TRIAL // JUNK CODE ----

### 6.01 Use this call to see all of the flags associated with the precip data to get more context about collection ----

weathercan::flags




