library(cowplot)
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
    filename_bar_all_week <- snakemake@output[["bar_plot_week"]]
    filename_bar_all_month <- snakemake@output[["bar_plot_month"]]
    filename_bar_all_day <- snakemake@output[["bar_plot_day"]]
    years = snakemake@params[["years"]]
} else {
    filename_csv <- here::here("data", "processed", "activities.csv")
    filename_bar_all_week <- here::here("figures", "bar_all_week.png")
    filename_bar_all_month <- here::here("figures", "bar_all_month.png")
    filename_bar_all_day <- here::here("figures", "bar_all_day.png")
    years = 2017:2019
}

set_colors <- function(data) {
    palette = RColorBrewer::brewer.pal(n = 8, name = "Paired")
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

filter_year <- function(data, date_col, year_str) {
    year_interval <- lubridate::interval(lubridate::ymd(paste0(year_str, "-01-01")),
                                         lubridate::ymd(paste0(year_str, "-12-31")))
    data %>% filter(UQ(as.symbol(date_col)) %within% year_interval)
}

#default_aspect_ratio <- 4/3
default_height <- 6
get_width <- function(height=6, aspect_ratio=3/2) {
    height * aspect_ratio
}
default_width <- get_width()
get_height <- function(width=8, aspect_ratio=3/2) {
    width / aspect_ratio
}
default_alpha <- 0.9

act_data <- readr::read_csv(filename_csv) %>%
    mutate(type = fct_reorder(type, elapsed_time, .fun = sum, .desc=TRUE),
           wday = fct_relevel(fct_recode(as.factor(wday), "M"="2", "T"="3", "W"="4", "H"="5", "F"="6", "Sa"="7", "Su"="1"), "M", "T", "W", "H", "F", "Sa", "Su") 
           )

colors <- set_colors(act_data)

### Make the plots ###

# activities binned by week 
plot_bar_week <- function(data) {
    colors <- set_colors(data)
    plot <- data %>% ggplot2::ggplot(aes(x=week, y=elapsed_time_hrs, fill=type)) +
        geom_col(position="stack") +
        scale_fill_manual("type", values=colors) +
        ylim(0, 25) +
        scale_x_datetime(date_breaks = "4 weeks", date_labels = "%b %d") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

bar_plot_all_week <- plot_bar_week(act_data) +
    scale_x_datetime(date_breaks = "4 weeks", date_labels = "%d %b %Y") +
    ggtitle("All Strava Activities")
ggsave(bar_plot_all_week, filename = filename_bar_all_week,
       width = default_width, height = default_height)

for (year in years) {
    year <- as.character(year)
    bar_plot_year <- plot_bar_week(filter_year(act_data, "start_date", year)) +
        ggtitle(paste0(year, " Activites"))
    ggsave(bar_plot_year, filename = here::here("figures", paste0("bar_", year,".png")),
           width = default_height, height = default_height)
}

# binned by month
plot_bar_facet <- function(data, x_col_str, scale = "fixed") {
    ggplot2::ggplot(data, aes_string(x=x_col_str, y="elapsed_time_hrs", fill="type")) +
        geom_col(position="stack") +
        scale_fill_manual("type", values=colors) +
        facet_wrap(~year, nrow = length(data$year %>% unique()), scale=scale) +
        theme_classic()
}

bar_plot_month <- plot_bar_facet(act_data, "month") + 
    ylim(0, 85) + 
    xlim(0, 12) +
    scale_x_continuous(breaks=1:12, labels = month.abb)
ggsave(bar_plot_month, filename=filename_bar_all_month, height=get_height(7), width=7)

# binned by day
bar_plot_day <- plot_bar_facet(act_data, "yday") + 
    ylim(0, 7) +
    xlim(0, 366) +
    scale_x_continuous(breaks = c(1,31,59,90,120,151,181,212,243,273,304,334,365), labels=c("", "31 Jan", "28 Feb", "31 Mar", "30 Apr", "31 May", "30 Jun", "31 Jul", "31 Aug", "30 Sep", "31 Oct", "30 Nov", "31 Dec"))
ggsave(bar_plot_day, filename=filename_bar_all_day, height=get_height(7), width=7)

# jitterplot type x time
jitter_plot_time <- act_data %>% filter(!(type %in% c("Hike", "Walk", "Elliptical"))) %>% 
    ggplot(aes(type, elapsed_time_hrs)) +
    geom_jitter(aes(color=type)) +#, alpha=default_alpha)) +
    scale_color_manual("type", values=colors) +
    scale_y_continuous(breaks=1:7) +
    theme_classic()
ggsave(jitter_plot_time, filename = here::here("figures", "jitter_type_time.png"), height = 7, width = get_width(7))

# boxplot weekday x time
box_plot_weekday_time <- act_data %>% filter(!(type %in% c("Hike", "Walk", "Elliptical"))) %>% 
    ggplot(aes(wday, elapsed_time_hrs, fill=type)) +
    geom_boxplot(aes(fill=type)) +
    scale_fill_manual("type", values=colors) +
    scale_y_continuous(breaks=1:7) +
    facet_wrap(~type, nrow = 1) +
    theme_classic()
ggsave(box_plot_weekday_time, filename = here::here('figures', "box_weekday_time.png"), width=10, height=get_height(10))

# jitter weekday x distance
jitter_plot_weekday_dist_grid <- act_data %>% filter(!(type %in% c("Hike", "Walk", "Elliptical", "RockClimbing"))) %>% ggplot(aes(wday, distance)) +
    geom_jitter(aes(color=type)) +#, alpha=default_alpha)) +
    scale_color_manual("type", values=colors) +
    facet_grid(type~wday, scale="free") +
    ylab("Distance (km)") +
    theme_classic()
ggsave(jitter_plot_weekday_dist_grid, filename = here::here('figures', "jitter_weekday_dist_grid.png"), width=10, height=get_height(10))

# boxplot weekday x distance
# TODO: use cowplot for better control
filter_type <- function(data, type_str) {
    filter(data, as.character(type) == type_str)
}
plot_box <- function(data, x_str, y_str, fill_str) {
    ggplot(data, aes_string(x=x_str, y=y_str, fill=fill_str)) +
        geom_boxplot() + 
        scale_fill_manual(fill_str, values=colors) +
        scale_x_discrete(as.factor(c("M", "T", "W", "H", "F", "Sa", "Su"))) +
        theme_classic() +
        ylab("") + xlab("wday") +
        theme(legend.position = "none")
}

plots = list()
plots[["Ride"]] <- plot_box(filter_type(act_data, "Ride"), "wday", "distance", "type")
plots[["Run"]]  <- plot_box(filter_type(act_data, "Run"), "wday", "distance", "type") + ylim(0, 25)
plots[["Rowing"]] <- plot_box(filter_type(act_data, "Rowing"), "wday", "distance", "type") + ylim(0,6)
plots[["Swim"]] <- plot_box(filter_type(act_data, "Swim"), "wday", "distance", "type") + ylim(0, 1)
box_cow_weekday_dist <- cowplot::plot_grid(plotlist = plots)
#####
box_plot_weekday_dist_wrap <- act_data %>% filter(!(type %in% c("Hike", "Walk", "Elliptical", "RockClimbing"))) %>% ggplot(aes(wday, distance)) +
    geom_boxplot(aes(fill=type)) +    
    scale_fill_manual("type", values=colors) +
    facet_wrap(~type, scale="free_y", ncol = 1) +
    ylab("Distance (km)") +
    theme_classic()
ggsave(box_plot_weekday_dist_wrap, filename = here::here('figures', "box_weekday_dist_wrap.png"), width=10, height=get_height(10))
######
# cumulative activity time
line_plot_time <- act_data %>% ggplot(aes(x=start_date, y=elapsed_hrs_cum_type, color=type)) +
    geom_line() +
    scale_color_brewer(palette = "Dark2") +
    #scale_color_manual("type", colors) +  # bug in ggplot?
    scale_x_datetime(date_breaks = "4 weeks", date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks()) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Cumulative Activity Time (hrs)")
ggsave(line_plot_time, filename=here::here("figures", "line_time.png"), width = default_width, height = default_height)
# cumulative activity distance
line_plot_dist <- act_data %>% ggplot(aes(x=start_date, y=elapsed_dist_cum_type, color=type)) +
    geom_line() +
    scale_color_brewer(palette = "Dark2") +
    #scale_color_manual("type", colors) +  # bug in ggplot?
    scale_x_datetime(date_breaks = "4 weeks", date_labels = "%b %Y") +
    scale_y_continuous(breaks=pretty_breaks()) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Cumulative Activity Distance (km)")
ggsave(line_plot_dist, filename=here::here("figures", "line_dist.png"), width = default_width, height = default_height)