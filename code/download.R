library(here)
library(httr)
library(readr)
library(rStrava)

if (exists("snakemake")) {
    filename_csv_raw <- snakemake@output[["csv"]]
} else {
    filename_csv_raw <- here::here("data", "raw", "activities.csv")
}
filename_token <- here::here('.httr-oauth')
stoken <- httr::config(token = readRDS(filename_token)[[1]])
act_list <- get_activity_list(stoken)
act_data_raw <- compile_activities(act_list)
readr::write_csv(act_data_raw, filename_csv_raw)
