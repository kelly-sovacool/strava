library(dplyr)
library(forcats)
library(here)
library(ggplot2)
library(lubridate)
library(magrittr)
library(RColorBrewer)
library(readr)
library(scales)
library(tidyr)

if (exists("snakemake")) {
    filename_csv <- snakemake@input[["csv"]]
    filename_line <- snakemake@output[["line_plot"]]
    filename_bar_all_week <- snakemake@output[["bar_plot_week"]]
    filename_bar_all_month <- snakemake@output[["bar_plot_month"]]
    years = snakemake@params[["years"]]
} else {
    filename_csv <- here::here("data", "processed", "activities.csv")
    filename_line <- here::here("figures", "line_all.png")
    filename_bar_all_week <- here::here("figures", "bar_all_week.png")
    filename_bar_all_month <- here::here("figures", "bar_all_month.png")
    years = 2017:2019
}

act_data <- readr::read_csv(filename_csv) %>%
    mutate(type = fct_reorder(type, elapsed_time, .fun = sum, .desc=TRUE))

set_colors <- function() {
    palette = RColorBrewer::brewer.pal(n = act_data$type %>% unique() %>% length(), name = "Paired")
    colors = list()
    colors[["Ride"]] <- palette[[4]]
    colors[["Run"]] <- palette[[8]]
    colors[["RockClimbing"]] <- palette[[3]]
    colors[["Rowing"]] <- palette[[1]]
    colors[["Swim"]] <- palette[[2]]
    colors[["Hike"]] <- palette[[6]]
    colors[["Walk"]] <- palette[[5]]
    colors[["Elliptical"]] <- palette[[7]]
    colors
}
plot_bar_week <- function(data) {
    colors <- set_colors()
    plot <- data %>% ggplot2::ggplot(aes(x=week, y=elapsed_time_hrs, fill=type)) +
        geom_col(position="stack") +
        scale_fill_manual("type", values=colors) +
        ylim(0, 25) +
        scale_x_datetime(date_breaks = "4 weeks", date_labels = "%b %d") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
filter_year <- function(data, date_col, year_str) {
    year_interval <- lubridate::interval(lubridate::ymd(paste0(year_str, "-01-01")),
                                         lubridate::ymd(paste0(year_str, "-12-31")))
    data %>% filter(UQ(as.symbol(date_col)) %within% year_interval)
}
aspect_ratio <- 4/3
height <- 5
width <- height * aspect_ratio

plot_bar_all_week <- plot_bar_week(act_data) +
    scale_x_datetime(date_breaks = "4 weeks", date_labels = "%d %b %Y") +
    ggtitle("All Strava Activities")
ggsave(plot_bar_all_week, filename = filename_bar_all_week,
       width = width, height = height)

for (year in years) {
    year <- as.character(year)
    plot_bar_year <- plot_bar_week(filter_year(act_data, "start_date", year)) +
        ggtitle(paste0(year, " Activites"))
    ggsave(plot_bar_year, filename = here::here("figures", paste0("bar_", year,".png")),
           width = width, height = height)
}

colors <- set_colors()
plot_bar_month <- act_data %>% 
    ggplot2::ggplot(aes(x=month, y=elapsed_time_hrs, fill=type)) +
    geom_col(position="stack") +
    scale_fill_manual("type", values=colors) +
    facet_wrap(~year, nrow = length(act_data$year %>% unique()), scale="free") +
    ylim(0, 85) +
    scale_x_continuous(breaks=1:12, labels = month.abb)+
    theme_classic()
ggsave(plot_bar_month, filename=filename_bar_all_month, height=7/aspect_ratio, width=7)

plot_line <- act_data %>% ggplot(aes(x=start_date, y=elapsed_hrs_cum_type, color=type)) +
    geom_line() +
    scale_color_brewer(palette = "Dark2") +
    #scale_color_manual("type", colors) +  # bug in ggplot?
    scale_x_datetime(date_breaks = "4 weeks", date_labels = "%b %Y") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Cumulative Activity Time")
ggsave(plot_line, filename=filename_line, width = width, height = height)
