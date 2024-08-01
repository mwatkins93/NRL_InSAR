## ORGANISATION ---------
## Name: MGW
## Date: 2024-07-18
## Project: NRL_precip_sf
## Objective: (1) Use 'weathercan to find 2016-2024 precip data for Lansdowne House and Ogoki Post stations and plot daily and monthly summaries' (2) Potentially find streamflow data for the same period - ideally the river where the proposed holes are running along
## Inputs: raw weathercan data
## Outputs: precipitation plots
## ----------------------

## 0. NOTES ----

## See max columns of a dataframe

rstudioapi::writeRStudioPreference("data_viewer_max_columns", 100L)

## Set month list for time plot x-axis

month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")

## 1. PREPARE ----

install.packages(c("tidyverse", "readxl"))

install.packages("weathercan", 
                 repos = c("https://ropensci.r-universe.dev", 
                           "https://cloud.r-project.org"))

install.packages("lubridate")

install.packages(c('lutz', 'sf')) # for stations() timezone updates

rm(list = ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(weathercan)
library(lubridate)

## 2. IMPORT ----

## Nothing

## 3. TIDY // PROCESS ----

## Update stations dataframe

stations_dl()

## Search for Lansdowne House (ID = 10244) and Ogoki Post (ID = 52898)

lansdowne <- stations_search("Ogoki Post")

## Pull data from 01-01-2016 - recent for both stations

lansdowne <- weather_dl(station_ids = 10244, start = "2016-01-01", end = "2024-07-17", interval = "day")

ogoki <- weather_dl(station_ids = 52898, start = "2015-08-01", end = "2024-07-17", interval = "day") # Ogoki is problematic - had to pull from 2015 and exclude up until 2016-01-01. Perhaps because the first half of 2016 is filled with NAs...

## Clean data and extract necessary columns

lansdowne_clean <- lansdowne[, c(11:14, 32)]

ogoki_2016 <- ogoki[-c(1:153), ]

ogoki_clean <- ogoki_2016[, c(11:14, 32)]

## Create monthly summaries for each site

## Lansdowne
lansdowne_monthly <- lansdowne_clean %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(monthly_precip = sum(total_precip))

## Ogoki  
ogoki_monthly <- ogoki_clean %>% 
  group_by(year, month) %>% 
  na.omit() %>% 
  summarise(monthly_precip = sum(total_precip))

## 4. PLOTTING ----

## Daily amounts - Lansdowne
lansdowne_clean %>% 
  ggplot(aes(x = date, y = total_precip)) +
  
  # Bar plot
  geom_bar(stat = "identity", fill = "#6aadfb") +
  
  # Axes
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "", y = "Daily precipitation (mm)") +
  
  # Theme
  theme_light() +
  ggtitle("Lansdowne House (AUT)") +
  theme(plot.title = element_text(hjust = 0.5))

## Monthly amounts - Lansdowne
lansdowne_monthly %>% 
  ggplot(aes(x = month, y = monthly_precip)) +
  facet_wrap(~ year) +
  
  # Bar plot
  geom_bar(stat = "identity", fill = "#6aadfb") +
  
  # Axes
  labs(x = "", y = "Monthly precipitation (mm)") +
  scale_y_continuous(limits = c(0, 200)) +

  # Theme
  theme_light() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black")) +
  scale_x_discrete(labels = month_list) +
  ggtitle("Lansdowne House (AUT)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 2.5))

## Daily amounts - Ogoki
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

## Monthly amounts - Ogoki
ogoki_monthly %>% 
  ggplot(aes(x = month, y = monthly_precip)) +
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

## Save raw precipitation data to csv files

write_csv(lansdowne, file = "X:\\PROJECTS\\CURRENT\\2021-004 SNC_Northern Road Link\\NRL_precip_sf\\data\\lansdowne_precip.csv")

write_csv(ogoki_2016, file = "X:\\PROJECTS\\CURRENT\\2021-004 SNC_Northern Road Link\\NRL_precip_sf\\data\\ogoki_precip.csv")

## 6. TRIAL // JUNK CODE ----

weathercan::flags # List of flags for precipitation
