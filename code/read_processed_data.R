library(dplyr)
library(forcats)
library(readr)
read_data <- function(filename_csv) {
    data <- readr::read_csv(filename_csv) %>%
        mutate(type = fct_reorder(type, elapsed_time, .fun = sum, .desc=TRUE),
               wday = fct_relevel(fct_recode(as.factor(wday), 
                                             "M"="1", "T"="2", "W"="3", "H"="4", "F"="5", "Sa"="6", "Su"="7"), 
                                  "M", "T", "W", "H", "F", "Sa", "Su") 
    )
}
