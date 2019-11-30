library(dplyr)
library(forcats)
library(readr)
read_data <- function(filename_csv) {
    data <- readr::read_csv(filename_csv) %>%
        mutate(type = fct_reorder(type, elapsed_time, .fun = sum, .desc=TRUE),
               wday = fct_relevel(fct_recode(as.factor(wday), 
                                             "M"="2", "T"="3", "W"="4", "H"="5", "F"="6", "Sa"="7", "Su"="1"), 
                                  "M", "T", "W", "H", "F", "Sa", "Su") 
    )
}
