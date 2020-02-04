library(here)
library(httr)
library(readr)
library(rStrava)

if (exists("snakemake")) {
    filename_csv_raw <- snakemake@output[["csv"]]
} else {
    filename_csv_raw <- here::here("data", "raw", "activities.csv")
}
stoken_path <- '.httr-oauth'
if (file.exists(stoken_path)) {
    filename_token <- here::here(stoken_path)
    stoken <- httr::config(token = readRDS(filename_token)[[1]])
} else {
    app_secret <-
        readChar(client_secret_path, file.info(client_secret_path)$size)
    stoken <-
        httr::config(token = strava_oauth(appname, client_id, app_secret, 
                                          app_scope = "activity:read_all",
                                          cache=TRUE))
}
act_list <- get_activity_list(stoken)
act_data_raw <- compile_activities(act_list)
readr::write_csv(act_data_raw, filename_csv_raw)
