library(dplyr)
library(forcats)
library(here)
library(ggplot2)
library(lubridate)
library(magrittr)
library(RColorBrewer)
library(readr)
library(rStrava)
library(scales)
library(tidyr)

if (exists("snakemake")) {
    filename_csv_raw <- snakemake@input[["csv"]]
    filename_csv_processed <- snakemake@output[["csv"]]
} else {
    filename_csv_raw <- here::here("data", "raw", "activities.csv")
    filename_csv_processed <- here::here("data", "processed", "activities.csv")
}

act_data_raw <- readr::read_csv(filename_csv_raw)
act_data <- act_data_raw %>% 
    mutate(start_date = start_date %>% lubridate::ymd_hms(),
                                    start_date_local = start_date_local %>% lubridate::ymd_hms(),
                                    elapsed_time_min = round((elapsed_time / 60), 1),
                                    elapsed_time_hrs = round((elapsed_time / 60 / 60), 1),
                                    moving_time_min = round((moving_time / 60), 1),
                                    moving_time_hrs = round((moving_time / 60 / 60), 1),
                                    type = fct_reorder(type, elapsed_time, .fun = sum, .desc=TRUE),
                                    mday = start_date %>% lubridate::ymd_hms() %>% lubridate::mday(),
                                    week = start_date %>% lubridate::ymd_hms() %>% lubridate::floor_date('week'),
                                    year = start_date %>% lubridate::year(),
                                    month = start_date %>% lubridate::month() %>% as.integer()
                                    ) %>%
    arrange(start_date) %>%
    group_by(type) %>%
    mutate(elapsed_hrs_cum_type = cumsum(elapsed_time))

readr::write_csv(act_data, filename_csv_processed)
