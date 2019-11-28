# adapted from https://gist.github.com/marcusvolz/84d69befef8b912a3781478836db9a75
# Plot activity calendar

# Required packages
# devtools::install_github("marcusvolz/strava")
# devtools::install_github("marcusvolz/ggart")
# devtools::install_github("marcusvolz/ggTimeSeries")

# Load packages
library(data.table)
library(here)
library(ggart)
library(ggthemes)
library(ggTimeSeries)
library(lubridate)
library(readr)
library(strava)
library(viridis)
source(here::here("code", "ggplot_calendar_heatmap_KLS.R"))

# Process the data
# data <- process_data(<gpx file path>)
data <- read_csv(here::here("data", "processed", "activities.csv")) %>%
    select(start_date, elapsed_time_hrs, year) %>%
    drop_na()

# Create plot
cal_plot <- ggplot_calendar_heatmap_KLS(data, "start_date", "elapsed_time_hrs", fill_label= 'hrs')
#print(cal_plot)

# Save plot
ggsave(here::here("figures", "calendar.png"), cal_plot, width = 30, height = 30, units = "cm", dpi = 300)
