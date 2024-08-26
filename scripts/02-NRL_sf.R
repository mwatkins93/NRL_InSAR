## ORGANISATION ---------
## Name: MGW
## Date: 2024-07-17
## Project: NRL_precip_sf
## Objective: (1) Extract and plot streamflow data for the Attawapiskat River between 2016 and 2022; (2) Combine streamflow and precipitation plots and overlay InSAR dates; (3) Add InSAR profiles to combined plots
## Inputs: raw tidyhydat data
## Outputs: plots
## ----------------------

## 0. NOTES ----

### 0.1 Month vectory for x-axis plotting (same as 01_NRL_precip) ----

month_list <- c("Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")

## 1. PREPARE ----

rm(list = ls())
options(stringsASfactors = FALSE, scipen = 999, encoding = "UTF-8")

library(tidyverse)
library(readxl)
library(tidyhydat) # pkg for Canadian hydrometric data
library(lubridate)
library(patchwork) # pkg for combining multiple plots
library(RColorBrewer) # pkg for nice looking/colourblind palettes

## 2. IMPORT ----

### 2.01 Bring in excel sheets for (1) dates of inflection points; and (2) raw insar data ----

insar_points <- read_excel(path = "./data/insar_point_dates.xlsx")

insar_profiles <- read_excel(path = "./data/raw_insar_profiles.xlsx")

## 3. TIDY // PROCESS ----

### 3.01 Download the latest tidyhydat dataset ----

download_hydat()

### 3.02 Search for stations on the Attawapiskat River, we went 04FB001

search_stn_name("Attawapiskat")

### 3.03 Pull out Attawapiskat River from 2016 to the present ----

attawapiskat <- hy_daily_flows(station_number = "04FB001", start_date = "2016-01-01")

### 3.04 (1) Expand date into three separate columns so we can facet by year in the plotting section; (2) keep the old columns with cols_remove; (3) create a new false date column with mutate so x-axis can play nicely when plotting ----

attawapiskat_exp <- attawapiskat %>% 
  separate_wider_delim(Date, delim = "-", names = c("year", "month", "day"), cols_remove = FALSE) %>% 
  mutate(false_date = paste("2100", month, day))

### 3.05 Merge the InSar year, month, and day columns into one with unites; format the date column into a date class as it is currently in character format ----

insar_tidy <- unite(insar_points, col = date, 2:4, sep = "-", remove = TRUE)

insar_tidy$date <- as.Date(insar_tidy$date)

### 3.06 Set new range for dates to match precip and sf ranges (solves some plotting issues) ----

date_range <- c(as.Date("2016-01-01"), as.Date("2022-12-31"))

### 3.07 Subset Lansdowne and Ogoki precip to prep for joint hydro-hyeto plots ----

lansdowne_clean <- lansdowne[, c(11:14, 32)]

# ogoki_clean <- ogoki[, c(11:14, 32)] # Repeat for Ogoki if needed

### 3.08 Only need code below (i.e., lines 74-104) for plotting section 4.01 and 4.02! ----

### Run line 56 in NRL_precip.R first
###lansdowne_2016 <- lansdowne_clean %>%
###  filter(year %in% "2016") %>% 
###  mutate(total_precip = coalesce(total_precip, 0)) # fill NAs with zeroes for graph purposes

### Ogoki here if needed ...
###ogoki_2022 <- ogoki_clean %>% 
###  filter(year %in% "2022") %>% 
###  mutate(total_precip = coalesce(total_precip, 0))

### Attawapiskat annual discharge
###attawa_2016 <- attawapiskat_exp %>% 
###  filter(year %in% "2016")

###attawa_2017 <- attawapiskat_exp %>% 
###  filter(year %in% "2017")

###attawa_2018 <- attawapiskat_exp %>% 
###  filter(year %in% "2018")

###attawa_2019 <- attawapiskat_exp %>% 
###  filter(year %in% "2019")

###attawa_2020 <- attawapiskat_exp %>% 
###  filter(year %in% "2020")

###attawa_2021 <- attawapiskat_exp %>% 
###  filter(year %in% "2021")

###attawa_2022 <- attawapiskat_exp %>% 
###  filter(year %in% "2022")

### 3.03 InSAR profiles - taking the raw data and 'wrangling' it into a tidy format () ----

insar_sub <- insar_profiles[, c(3, 11:140)] # take only needed columns

insar_long <- insar_sub %>% 
  pivot_longer(cols = 2:131, names_to = "date", values_to = "displacement") # pivot data to long format

insar_long$date <- str_sub(insar_long$date, end = -3) # remove unnecessary characters from date column

insar_long$date <- ymd(insar_long$date) # parse dates

insar_long$point_id <- as.character(insar_long$point_id) # changing this column to character format for plotting purposes

### 3.04 Testing if the newly formatted InSAR data works for subsetting comparison groups ----

lake_insar <- insar_long %>% 
  filter(point_id %in% c("7537571", "7548218", "7558218", "7580186"))

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

### Compare annual discharge patterns for the river

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

### 4.02 Yearly hyeto-hydrographs from 2016-2022 using the Lansdowne station for precip ----

### 2016 hyeto-hydrograph
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

precip_2016 / sf_2016 # using patchwork pkg to put precip plot above streamflow plot

### 2017 hyeto-hydrograph

sum(lansdowne_2017$total_precip, na.rm = TRUE) # taking annual precip amount so we can annotate it on graph

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

### 2018 hyeto-hydrograph

sum(lansdowne_2018$total_precip, na.rm = TRUE) # annual = 695.6

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

### 2019 hyeto-hydrograph

sum(lansdowne_2019$total_precip, na.rm = TRUE) # annual = 465.8

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

### 2020 hyeto-hydrograph

sum(lansdowne_2020$total_precip, na.rm = TRUE) # annual = 84.2

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

### 2021 hyeto-hydrograph

sum(lansdowne_2021$total_precip, na.rm = TRUE) # annual = 584.6

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

### 2022 hyeto-hydrograph

sum(lansdowne_2022$total_precip) # annual = 650 mm

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

### Combined plot
lansdowne_hist <- lansdowne_clean %>% 
  filter(date >= "2016-01-01", date <= "2022-12-31") %>% 
  ggplot(aes(x = date, y = total_precip)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  
  # Axes
  ylab("Rainfall (mm/day)") +
  scale_y_reverse(limits = c(100, 0), expand = expansion(mult = c(0, 0))) +
  
  # Annotations
  #geom_vline(data = insar_8218, aes(xintercept = date), linetype = "dashed") +
  
  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  ggtitle("Historical Attawapiskat River discharge | Weather station: Lansdowne House (AUT)")

### 4.05 Daily rainfall combined plot workflow: (1) subset required InSAR sites; (2) change data in "annotations" for all plots to InSAR site needed; (3) change InSAR profile to match subset in step 1 - line 587 (4) change legend items in InSAR plot to match the subset in step 1 - line 597; (5) change third plot on line 604 to update to new InSAR plot ----

### InSAR station subset (combined InSAR sites)
your_group_name <- insar_tidy %>%
  filter(id %in% c("Station 1", "Station 2", "Station N"))

### Rainfall
lansdowne_hist <- lansdowne_clean %>% 
  filter(date >= "2016-01-01", date <= "2022-12-31") %>% 
  ggplot(aes(x = date, y = total_precip)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  
  # Axes
  ylab("Rainfall (mm/day)") +
  scale_y_reverse(limits = c(100, 0), expand = expansion(mult = c(0, 0))) +
  
  # Annotations
  geom_vline(data = insar_4685, aes(xintercept = date), linetype = "dashed") +
  
  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  ggtitle("Historical Attawapiskat River discharge | Weather station: Lansdowne House (AUT)")

### Hydrograph
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
  geom_vline(data = insar_4685, aes(xintercept = date), linetype = "dashed") +
  geom_text(data = insar_4685, mapping = aes(x = date - 21, y = 1600, label = date,),
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

### InSAR profiles
comp_2_plot <- comp_2 %>% 
  ggplot(aes(x = date, y = displacement, colour = point_id)) +
  
  # Point plot
  geom_point() +
  
  # Axes
  labs(x = "", y = "Displacement (cm)", colour = "InSAR ID") +
  scale_x_date(limits = date_range, date_labels = "%Y", date_breaks = "1 year") +
  scale_color_brewer(breaks = c("4937641", "4944683", "4944685"), palette = "Dark2") +
  
  # Annotations
  geom_vline(data = insar_4685, aes(xintercept = date), linetype = "dashed") +
  
  # Theme
  theme_light()

lansdowne_hist / attawa_hist_q / comp_2_plot + plot_layout(guides = "collect")

## 5. SAVING // EXPORTING ----

### 5.01 Export raw streamflow data to csv

write_csv(attawapiskat, file = "X:\\PROJECTS\\CURRENT\\2021-004 SNC_Northern Road Link\\NRL_precip_sf\\data\\attawapiskat_sf.csv")

## 6. TRIAL // JUNK CODE ----

### 6.01 Change hyeto plot to monthly bins instead of daily amounts if desired; however, they don't line up with sf and InSAR daily dates... ----

ld_monthly_plot <- lansdowne_monthly %>% 
  ggplot(aes(x = make_date(year, factor(month)), y = monthly.precip)) +
  geom_col(fill = "darkblue",
           alpha = 0.6,
           position = position_dodge(width = 0.5)) +
  
  # Axes
  ylab("Monthly rainfall (mm)") +
  scale_y_reverse(limits = c(200, 0), expand = expansion(mult = c(0, 0))) +
  
  # Annotations
  #geom_vline(data = insar_8218, aes(xintercept = date, colour = "salmon"), linetype = "dashed") +
  
  # Theme
  theme_light() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  ggtitle("Historical Attawapiskat River discharge | Weather station: Lansdowne House (AUT)")
  


