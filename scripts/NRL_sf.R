## ORGANISATION ---------
## Name: MGW
## Date: 2024-07-17
## Project: NRL_precip_sf
## Objective: (1) Extract and plot streamflow data for the Attawapiskat River between 2016 and 2022; (2) Combine streamflow and precipitation plots and overlay InSAR dates; (3) Add InSAR profiles to combined plots
## Inputs: raw tidyhydat data
## Outputs: plots
## ----------------------

## 0. NOTES ----

### 0.1 Set month list for time plot x-axis ----

month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")

## 1. PREPARE ----
rm(list = ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(tidyhydat)
library(lubridate)
library(patchwork)

## 2. IMPORT ----

insar_points <- read_excel(path = "./data/insar_point_dates.xlsx")

insar_profiles <- read_excel(path = "./data/raw_insar_profiles.xlsx")

## 3. TIDY // PROCESS ----

### 3.01 Pull out Attawapiskat River from 2016 to the present ----
attawapiskat <- hy_daily_flows(station_number = "04FB001", start_date = "2016-01-01")

### Expand date into three separate columns so facetting by year works
attawapiskat_exp <- attawapiskat %>% 
  separate_wider_delim(Date, delim = "-", names = c("year", "month", "day"), cols_remove = FALSE) %>% 
  mutate(false_date = paste("2100", month, day))

### Unite the InSar ymd columns into one date class

insar_tidy <- unite(insar_points, col = date, 2:4, sep = "-", remove = TRUE)

insar_tidy$date <- as.Date(insar_tidy$date)

### Set new range for dates to match precip and sf

date_range <- c(as.Date("2016-01-01"), as.Date("2022-12-31"))

### 3.02 Subset Lansdowne and Ogoki precip and Attawapiskat streamflow to prep for joint hydro-hyeto plots ----

lansdowne_clean <- lansdowne[, c(11:14, 32)]

### Only need code below for plotting section 4.01 and 4.02!

### Run line 56 in NRL_precip.R first
lansdowne_2016 <- lansdowne_clean %>%
  filter(year %in% "2016") %>% 
  mutate(total_precip = coalesce(total_precip, 0)) # fill NAs with zeroes for graph purposes

### Ogoki here if needed ...
ogoki_2022 <- ogoki_clean %>% 
  filter(year %in% "2022") %>% 
  mutate(total_precip = coalesce(total_precip, 0))

### Attawapiskat annual discharge
attawa_2016 <- attawapiskat_exp %>% 
  filter(year %in% "2016")

attawa_2017 <- attawapiskat_exp %>% 
  filter(year %in% "2017")

attawa_2018 <- attawapiskat_exp %>% 
  filter(year %in% "2018")

attawa_2019 <- attawapiskat_exp %>% 
  filter(year %in% "2019")

attawa_2020 <- attawapiskat_exp %>% 
  filter(year %in% "2020")

attawa_2021 <- attawapiskat_exp %>% 
  filter(year %in% "2021")

attawa_2022 <- attawapiskat_exp %>% 
  filter(year %in% "2022")

### 3.03 InSAR profiles - wrangle data into a tidy format ----

insar_sub <- insar_profiles[, c(3, 11:140)] # take only needed columns

insar_long <- insar_sub %>% 
  pivot_longer(cols = 2:131, names_to = "date", values_to = "displacement") # pivot data to long format

insar_long$date <- str_sub(insar_long$date, end = -3) # remove unnecessary characters from date column

insar_long$date <- ymd(insar_long$date) # parse dates

## 4. PLOTTING ----

### 4.01 Basic hydrographs (2016-2022) ----
attawapiskat %>%
  ggplot(aes(x = Date, y = Value)) +
  
  # Line plot
  geom_line(colour = "#6aadfb") +
  
  # Axes
  scale_y_continuous(limits = c(0, 1500)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "", y = "Discharge") +
  
  # Theme
  theme_light() +
  ggtitle("Attawapiskat River") +
  theme(plot.title = element_text(hjust = 0.5))

### 4.02 Yearly hyeto-hydrographs (Station: Lansdowne) ----

ggplot(data=df, mapping=aes(x=MonthDay, y=Y, shape=Year, color=Year)) + geom_point() +geom_line(aes(group = 1))

attawapiskat_exp %>% 
  ggplot(aes(x = false_date, y = Value)) +
  geom_line(colour = "#6aadfb" ) + 
  facet_wrap(~year) +
  
  # Axes
  scale_y_continuous(limits = c(0, 1500)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(x = "", y = "Discharge") +
  
  # Theme
  theme_light() +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = "black")) +
  ggtitle("Attawapiskat River") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 2.5))

## 2016 hyeto-hydrograph
precip_2016 <- lansdowne_2016 %>% 
  ggplot(aes(x = date, y = total_precip)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  
  # Axes
  labs(x = "", y = "Rainfall (mm/day)") +
  scale_y_reverse(limits = c(30, 0)) +

  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle("2016 Attawapiskat River | Weather Station: Lansdowne House (AUT)") +

  # Annotations
  annotate("text", x = as.Date("2016-11-28"), y = 30, label = "Annual precipitation: 545.1 mm")


sf_2016 <- attawa_2016 %>% 
  ggplot(aes(x = Date, y = Value)) +
  
  # Freshet and storm fill
  geom_area(data = attawa_2016 |> filter(Date >= "2016-04-16" & Date <= "2016-08-17"),
            mapping = aes(x = Date, y = Value), 
            fill = '#6aadfb', alpha = .25) +
  geom_area(data = attawa_2016 |> filter(Date >= "2016-09-04" & Date <= "2016-12-31"),
            mapping = aes(x = Date, y = Value), 
            fill = '#FFCC59', alpha = .25) +
  
  geom_line(colour = "#6aadfb") +
  labs(x = "") +
  ylab(expression(paste("Discharge ", (m^3/s)))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 600)) +
  theme_light() +

  # Annotations
  annotate("text", x = as.Date("2016-06-13"), y = 250, label = "Freshet") +
  annotate("text", x = as.Date("2016-11-01"), y = 250, label = "Stormflow")

precip_2016 / sf_2016

## 2017 hyeto-hydrograph; annual = 472.4
sum(lansdowne_2017$total_precip, na.rm = TRUE)

precip_2017 <- lansdowne_2017 %>% 
  ggplot(aes(x = date, y = total_precip)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  
  # Axes
  labs(x = "", y = "Rainfall (mm/day)") +
  scale_y_reverse(limits = c(30, 0)) +
  
  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle("2017 Attawapiskat River | Weather Station: Lansdowne House (AUT)") +
  
  # Annotations
  annotate("text", x = as.Date("2017-11-28"), y = 30, label = "Annual precipitation: 472.4 mm")

sf_2017 <- attawa_2017 %>% 
  ggplot(aes(x = Date, y = Value)) +
  
  # Freshet and storm fill
  geom_area(data = attawa_2017 |> filter(Date >= "2017-04-01" & Date <= "2017-08-17"),
            mapping = aes(x = Date, y = Value), 
            fill = '#6aadfb', alpha = .25) +
  geom_area(data = attawa_2017 |> filter(Date >= "2017-09-23" & Date <= "2017-12-31"),
            mapping = aes(x = Date, y = Value), 
            fill = '#FFCC59', alpha = .25) +
  
  geom_line(colour = "#6aadfb") +
  labs(x = "") +
  ylab(expression(paste("Discharge ", (m^3/s)))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 1000)) +
  theme_light() +
  
  # Annotations
  annotate("text", x = as.Date("2017-06-01"), y = 250, label = "Freshet") +
  annotate("text", x = as.Date("2017-11-14"), y = 37.5, label = "Stormflow")

precip_2017 / sf_2017

## 2018 hyeto-hydrograph; annual = 695.6
sum(lansdowne_2018$total_precip, na.rm = TRUE)

precip_2018 <- lansdowne_2018 %>% 
  ggplot(aes(x = date, y = total_precip)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  
  # Axes
  labs(x = "", y = "Rainfall (mm/day)") +
  scale_y_reverse(limits = c(100, 0)) +
  
  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle("2018 Attawapiskat River | Weather Station: Lansdowne House (AUT)") +
  
  # Annotations
  annotate("text", x = as.Date("2018-11-28"), y = 100, label = "Annual precipitation: 695.6 mm")

sf_2018 <- attawa_2018 %>% 
  ggplot(aes(x = Date, y = Value)) +
  
  # Freshet and storm fill
  geom_area(data = attawa_2018 |> filter(Date >= "2018-04-21" & Date <= "2018-08-17"),
            mapping = aes(x = Date, y = Value), 
            fill = '#6aadfb', alpha = .25) +
  geom_area(data = attawa_2018 |> filter(Date >= "2018-09-16" & Date <= "2018-12-31"),
            mapping = aes(x = Date, y = Value), 
            fill = '#FFCC59', alpha = .25) +
  
  geom_line(colour = "#6aadfb") +
  labs(x = "") +
  ylab(expression(paste("Discharge ", (m^3/s)))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 800)) +
  theme_light() +

  # Annotations
  annotate("text", x = as.Date("2018-06-06"), y = 250, label = "Freshet") +
  annotate("text", x = as.Date("2018-10-23"), y = 250, label = "Stormflow")

precip_2018 / sf_2018

## 2019 hyeto-hydrograph; annual = 465.8
sum(lansdowne_2019$total_precip, na.rm = TRUE)

precip_2019 <- lansdowne_2019 %>% 
  ggplot(aes(x = date, y = total_precip)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  
  # Axes
  labs(x = "", y = "Rainfall (mm/day)") +
  scale_y_reverse(limits = c(30, 0)) +
  
  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle("2019 Attawapiskat River | Weather Station: Lansdowne House (AUT)") +
  
  # Annotations
  annotate("text", x = as.Date("2019-10-23"), y = 30, label = "Annual precipitation: 465.8 mm")

sf_2019 <- attawa_2019 %>% 
  ggplot(aes(x = Date, y = Value)) +
  
  # Freshet and storm fill
  geom_area(data = attawa_2019 |> filter(Date >= "2019-04-21" & Date <= "2019-07-15"),
            mapping = aes(x = Date, y = Value), 
            fill = '#6aadfb', alpha = .25) +
  geom_area(data = attawa_2019 |> filter(Date >= "2019-07-15" & Date <= "2019-12-31"),
            mapping = aes(x = Date, y = Value), 
            fill = '#FFCC59', alpha = .25) +
  
  geom_line(colour = "#6aadfb") +
  labs(x = "") +
  ylab(expression(paste("Discharge ", (m^3/s)))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 800)) +
  theme_light() +
  
  # Annotations
  annotate("text", x = as.Date("2019-06-01"), y = 250, label = "Freshet") +
  annotate("text", x = as.Date("2019-11-03"), y = 200, label = "Stormflow")

precip_2019 / sf_2019

## 2020 hyeto-hydrograph; annual = 84.2
sum(lansdowne_2020$total_precip, na.rm = TRUE)

precip_2020 <- lansdowne_2020 %>% 
  ggplot(aes(x = date, y = total_precip)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  
  # Axes
  labs(x = "", y = "Rainfall (mm/day)") +
  scale_y_reverse(limits = c(30, 0)) +
  
  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle("2020 Attawapiskat River | Weather Station: Lansdowne House (AUT)") +
  
  # Annotations
  annotate("text", x = as.Date("2020-11-28"), y = 30, label = "Annual precipitation: 84.2 mm")

sf_2020 <- attawa_2020 %>% 
  ggplot(aes(x = Date, y = Value)) +
  
  # Freshet and storm fill
  geom_area(data = attawa_2020 |> filter(Date >= "2020-05-13" & Date <= "2020-08-23"),
            mapping = aes(x = Date, y = Value), 
            fill = '#6aadfb', alpha = .25) +
  geom_area(data = attawa_2020 |> filter(Date >= "2020-08-23" & Date <= "2020-12-31"),
            mapping = aes(x = Date, y = Value), 
            fill = '#FFCC59', alpha = .25) +
  
  geom_line(colour = "#6aadfb") +
  labs(x = "") +
  ylab(expression(paste("Discharge ", (m^3/s)))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 1000)) +
  theme_light() +
  
  # Annotations
  annotate("text", x = as.Date("2020-06-25"), y = 250, label = "Freshet") +
  annotate("text", x = as.Date("2020-10-18"), y = 175, label = "Stormflow")

precip_2020 / sf_2020

## 2021 hyeto-hydrograph; annual = 584.6
sum(lansdowne_2021$total_precip, na.rm = TRUE)

precip_2021 <- lansdowne_2021 %>% 
  ggplot(aes(x = date, y = total_precip)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  
  # Axes
  labs(x = "", y = "Rainfall (mm/day)") +
  scale_y_reverse(limits = c(50, 0)) +
  
  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle("2021 Attawapiskat River | Weather Station: Lansdowne House (AUT)") +
  
  # Annotations
  annotate("text", x = as.Date("2021-11-28"), y = 50, label = "Annual precipitation: 584.6 mm")

sf_2021 <- attawa_2021 %>% 
  ggplot(aes(x = Date, y = Value)) +
  
  # Freshet and storm fill
  geom_area(data = attawa_2021 |> filter(Date >= "2021-04-16" & Date <= "2021-08-25"),
            mapping = aes(x = Date, y = Value), 
            fill = '#6aadfb', alpha = .25) +
  geom_area(data = attawa_2021 |> filter(Date >= "2021-08-25" & Date <= "2021-12-31"),
            mapping = aes(x = Date, y = Value), 
            fill = '#FFCC59', alpha = .25) +
  
  geom_line(colour = "#6aadfb") +
  labs(x = "") +
  ylab(expression(paste("Discharge ", (m^3/s)))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 600)) +
  theme_light() +
  
  # Annotations
  annotate("text", x = as.Date("2021-06-12"), y = 250, label = "Freshet") +
  annotate("text", x = as.Date("2021-10-07"), y = 175, label = "Stormflow")

precip_2021 / sf_2021

## 2022 hyeto-hydrograph; annual = 650 mm
sum(lansdowne_2022$total_precip)

precip_2022 <- lansdowne_2022 %>% 
  ggplot(aes(x = date, y = total_precip)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  
  # Axes
  labs(x = "", y = "Rainfall (mm/day)") +
  scale_y_reverse(limits = c(40, 0)) +
  
  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ggtitle("2022 Attawapiskat River | Weather Station: Lansdowne House (AUT)") +
  
  # Annotations
  annotate("text", x = as.Date("2022-11-28"), y = 40, label = "Annual precipitation: 650 mm")

sf_2022 <- attawa_2022 %>% 
  ggplot(aes(x = Date, y = Value)) +
  
  # Freshet and storm fill
  geom_area(data = attawa_2022 |> filter(Date >= "2022-04-16" & Date <= "2022-07-31"),
            mapping = aes(x = Date, y = Value), 
            fill = '#6aadfb', alpha = .25) +
  geom_area(data = attawa_2022 |> filter(Date >= "2022-07-31" & Date <= "2022-12-31"),
            mapping = aes(x = Date, y = Value), 
            fill = '#FFCC59', alpha = .25) +
  
  geom_line(colour = "#6aadfb") +
  labs(x = "") +
  ylab(expression(paste("Discharge ", (m^3/s)))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_y_continuous(limits = c(0, 1600)) +
  theme_light() +
  
  # Annotations
  annotate("text", x = as.Date("2022-06-12"), y = 250, label = "Freshet") +
  annotate("text", x = as.Date("2022-10-01"), y = 67.5, label = "Stormflow")

precip_2022 / sf_2022

### 4.03 Plot annual rainfall for Ogoki station ----

## 2016 - 321.4 mm
sum(ogoki_2016$total_precip)
ogoki_2016 %>% 
  ggplot(aes(x = date, y = total_precip)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  
  # Axes
  labs(x = "", y = "Rainfall (mm/day)") +
  scale_y_reverse(limits = c(40, 0)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  
  # Theme
  theme_light() +
  theme(axis.title.y = element_text(vjust = 2.5)) +
  ggtitle("2016 Rainfall | Weather Station: Ogoki") +
  
  # Annotations
  annotate("text", x = as.Date("2016-11-28"), y = 40, label = "Annual precipitation: 321.4 mm")

### 4.04 Insar dates on combined hyeto-hydrographs ----

### InSAR station subset
insar_4685 <- insar_tidy %>%
  filter(id %in% "4944685")

### Combined plot
lansdowne_hist <- lansdowne_clean %>% 
  filter(date >= "2016-01-01", date <= "2022-12-31") %>% 
  ggplot(aes(x = date, y = total_precip)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  
  # Axes
  ylab("Rainfall (mm/day)") +
  scale_y_reverse(limits = c(100, 0)) +
  
  # Annotations
  geom_vline(data = insar_4685, aes(xintercept = date, colour = "salmon"), linetype = "dashed") +
  
  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  ggtitle("Historical Attawapiskat River discharge | Weather station: Lansdowne House (AUT) | InSAR ID: 4944685")


attawa_hist_q <- attawapiskat_exp %>%
  ggplot(aes(x = Date, y = Value)) +
  
  # Line plot
  geom_line(colour = "#6aadfb") +
  
  # Axes
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 1600)) +
  labs(x = "") +
  ylab(expression(paste("Discharge ", (m^3/s)))) +
  
  # Annotations
  geom_vline(data = insar_4685, aes(xintercept = date, colour = "salmon"), linetype = "dashed") +
  geom_text(data = insar_4685,
            mapping = aes(x = date - 20, y = 1600, label = date,),
            inherit.aes = FALSE,
            size = 3,
            hjust = 1,
            angle = 90) + 
  
  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 3),
        legend.position = "none")

lansdowne_hist / attawa_hist_q

### 4.05 Raw Insar profile combined with corresponding sf and precip

### InSAR 4685
insar4685_plot <- insar_long %>% 
  filter(point_id %in% "4944685") %>% 
  ggplot(aes(x = date, y = displacement)) +
  
  # Point plot
  geom_point() +
  
  # Axes
  labs(x = "", y = "Displacement (cm)") +
  scale_x_date(limits = date_range, date_labels = "%Y", date_breaks = "1 year") +
  
  # Annotations
  geom_vline(data = insar_4685, aes(xintercept = date, colour = "salmon"), linetype = "dashed") +
  
  # Theme
  theme_light() +
  theme(legend.position = "none")

lansdowne_hist / attawa_hist_q / insar4685_plot + plot_layout(guides = "collect")

## 5. SAVING // EXPORTING ----

## Export raw streamflow data to csv
write_csv(attawapiskat, file = "X:\\PROJECTS\\CURRENT\\2021-004 SNC_Northern Road Link\\NRL_precip_sf\\data\\attawapiskat_sf.csv")

## 6. TRIAL // JUNK CODE ----

## Sum of 2016 precip at Lansdowne = 545.1

sum(lansdowne_2016$total_precip, na.rm = TRUE)

### 6.01 Plot stage of attawapiskat with InSAR dates ----

attawapiskat_stage <- hy_daily_levels(station_number = "04FB001", start_date = "2016-01-01")

attawapiskat_stage %>%
  ggplot(aes(x = Date, y = Value)) +
  # Line plot
  geom_line(colour = "#6aadfb") +
  
  # Axes
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(x = "", y = "Stage (m)") +
  
  # Theme
  theme_light() +
  ggtitle("Attawapiskat River") +
  theme(plot.title = element_text(hjust = 0.5)) +
  
  geom_vline(xintercept = as.numeric(as.Date(("2016-09-28")), linetype=4)) +
  geom_vline(xintercept = as.numeric(as.Date(("2017-08-23")), linetype=4)) +
  geom_vline(xintercept = as.numeric(as.Date(("2018-08-24")), linetype=4)) +
  geom_vline(xintercept = as.numeric(as.Date(("2019-09-18")), linetype=4)) +
  geom_vline(xintercept = as.numeric(as.Date(("2021-08-09")), linetype=4))



  
  


