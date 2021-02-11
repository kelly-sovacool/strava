library(dplyr)
library(forcats)
library(here)
library(lubridate)
library(magrittr)
library(readr)
library(tidyr)

if (exists("snakemake")) {
    filename_csv_raw <- snakemake@input[["csv"]]
    filename_csv_processed <- snakemake@output[["csv"]]
} else {
    filename_csv_raw <- here::here("data", "raw", "activities.csv")
    filename_csv_processed <- here::here("data", "processed", "activities.csv")
}
km_to_mi <- function(km) {
    miles_per_km <- 0.621371
    km * miles_per_km
}
act_data_raw <- readr::read_csv(filename_csv_raw)
act_data <- act_data_raw %>% 
    mutate(distance_km = distance) %>%
    mutate(distance_mi = km_to_mi(distance_km),
           start_date = start_date %>% lubridate::ymd_hms(),
           start_date_local = start_date_local %>% lubridate::ymd_hms(),
           elapsed_time_min = round((elapsed_time / 60), 1),
           elapsed_time_hrs = round((elapsed_time / 60 / 60), 1),
           moving_time_min = round((moving_time / 60), 1),
           moving_time_hrs = round((moving_time / 60 / 60), 1),
           type = recode(type, 'WeightTraining' = 'Workout'),
           type = fct_reorder(type, elapsed_time, .fun = sum, .desc=TRUE),
           wday = start_date_local %>% lubridate::ymd_hms() %>% lubridate::wday(week_start = 1) %>% as.integer(),
           mday = start_date_local %>% lubridate::ymd_hms() %>% lubridate::mday(),
           yday = start_date_local %>% lubridate::yday() %>% as.integer(),
           week = start_date_local %>% lubridate::ymd_hms() %>% 
               lubridate::floor_date('week', week_start = 1),
           year = start_date_local %>% lubridate::year(),
           month = start_date_local %>% lubridate::month() %>% as.integer()
           ) %>%
    arrange(start_date_local) %>%
    group_by(type) %>%
    mutate(elapsed_hrs_cum_type = cumsum(elapsed_time),
           elapsed_dist_cum_type = cumsum(distance_mi)
           )

readr::write_csv(act_data, filename_csv_processed)

table_sum <- act_data %>%
    group_by(year, type) %>%
    summarize(n=n(),
              sum_dist_mi=round(sum(distance_mi),1),
              med_dist_mi=round(median(distance_mi), 1),
              sum_time_hrs=sum(elapsed_time_hrs),
              med_time_hrs=median(elapsed_time_hrs))
readr::write_csv(table_sum, here::here("data" ,"processed", "summary.csv"))
