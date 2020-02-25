library(cowplot)
library(dplyr)
library(forcats)
library(here)
library(ggplot2)
library(glue)
library(lubridate)
library(magrittr)
library(RColorBrewer)
library(readr)
library(scales)
library(tidyr)
source(here::here("code", "read_processed_data.R"))
# TODO: label plot with all data over time with lines for each year
# TODO: DRY & modularize the code 
# TODO: use rmarkdown chunk fig.path option instead of ggsave
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
theme_set(theme_classic())
set_colors <- function(data) {
    palette = RColorBrewer::brewer.pal(n = 10, name = "Paired")
    colors = list()
    colors[["Ride"]] <- palette[[4]]
    colors[["Run"]] <- palette[[8]]
    colors[["RockClimbing"]] <- palette[[3]]
    colors[["Rowing"]] <- palette[[1]]
    colors[["Swim"]] <- palette[[2]]
    colors[["Hike"]] <- palette[[6]]
    colors[["Walk"]] <- palette[[5]]
    colors[["Elliptical"]] <- palette[[7]]
    colors[["NordicSki"]] <- palette[[10]]
    colors[["Yoga"]] <- palette[[9]]
    return(colors)
}

filter_year <- function(data, date_col, year_str) {
    year_interval <- lubridate::interval(lubridate::ymd(paste0(year_str, "-01-01")),
                                         lubridate::ymd(paste0(year_str, "-12-31")))
    return(data %>% 
               filter(UQ(as.symbol(date_col)) %within% year_interval)
    )
}

filter_last_n_weeks <- function(data, 
                                date_col = start_date_local,
                                num_weeks_ago=4) {
    week_date_thresh <- lubridate::floor_date(lubridate::today() - lubridate::dweeks(num_weeks_ago), 
                                              unit='week', week_start = 1)
    return(
        data %>% 
            filter(lubridate::floor_date({{ date_col }}, 
                                         unit='week', week_start = 1
                                         ) > week_date_thresh)
    )
}

filter_dist <- function(data) {
    types_to_keep <- data %>% 
        group_by(type) %>% 
        summarise(total_dist_mi=sum(distance_mi),
                  n=n()) %>% 
        filter(total_dist_mi > 0) %>% 
        pull(type)
    return(
        data %>%
            filter(type %in% types_to_keep)
    )
}

filter_count <- function(data, min = 5) {
    types_to_keep <- data %>%
        group_by(type) %>%
        summarise(n=n()) %>%
        filter(n > min) %>%
        pull(type)
    return(data %>%
               filter(type %in% types_to_keep))
}

filter_type <- function(data, type_str) {
    filter(data, as.character(type) == type_str)
}

plot_bar_facet <- function(data, x_col_str, scale = "fixed") {
    ggplot2::ggplot(data, aes_string(x=x_col_str, y="moving_time_hrs", fill="type")) +
        geom_col(position="stack") +
        scale_fill_manual("type", values=colors) +
        facet_wrap(~year, nrow = length(data$year %>% unique()), scale=scale) +
        theme_classic()
}

# activities binned by week 
plot_bar_week <- function(data, ymax) {
    colors <- set_colors(data)
    plot <- data %>% ggplot2::ggplot(aes(x=week, y=moving_time_hrs, fill=type)) +
        geom_col(position="stack") +
        scale_fill_manual("type", values=colors) +
        ylim(0, ymax) +
        scale_x_datetime(date_breaks = "4 weeks", date_labels = "%b %d") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# default_aspect_ratio <- 4/3
default_height <- 6
get_width <- function(height=6, aspect_ratio=4/3) {
    height * aspect_ratio
}
default_width <- get_width()
get_height <- function(width=8, aspect_ratio=4/3) {
    width / aspect_ratio
}
default_alpha <- 0.7

### Load the data ###

act_data <- read_data(filename_csv)

colors <- set_colors(act_data)

### Make the plots ###


## summarize last 4 weeks ##
act_data_last_4_weeks_sum <- act_data %>% 
    filter_last_n_weeks(num_weeks_ago = 4) %>% 
    mutate(type = fct_rev(type)) %>%
    group_by(type) %>%
    summarize(total_dist=round(sum(distance_mi),1),
              total_time=round(sum(moving_time_hrs),1))

bar_dist_last_4_weeks <- act_data_last_4_weeks_sum %>%
    filter(total_dist > 0) %>%
    ggplot(aes(x=type, y=total_dist, fill=type)) + 
    geom_col() + 
    scale_fill_manual("type", values=colors) + 
    coord_flip() + 
    ylab("Distance (mi)") + xlab("") + ggtitle("Activities during the last 4 weeks") +
    theme(legend.position = "none")
ggsave(bar_dist_last_4_weeks, filename=here::here("figures", "bar_dist_last_4_weeks.png"), height=get_height(6), width=6)

bar_time_last_4_weeks <- act_data_last_4_weeks_sum %>% 
    mutate(name=as.character(type)) %>% 
    mutate(name = case_when(total_dist > 0 ~ glue("{type} ({total_dist} mi) "),
                            TRUE ~ name)
           ) %>%
    mutate(name = fct_reorder(name, total_time, .fun = sum, .desc=FALSE)) %>%
    ggplot(aes(x=name, y=total_time)) + 
    geom_col(aes(fill=type)) + 
    geom_text(aes(label=total_time), nudge_y = 1) +
    #annot_dist(act_data_last_4_weeks, "Ride") +
    #annot_dist(act_data_last_4_weeks, "Run") +
    #annot_dist(act_data_last_4_weeks, "Swim") +
    scale_fill_manual("type", values=colors) + 
    ylab("Time (hrs)") + xlab("") +
    ylim(0, max(ceiling(act_data_last_4_weeks_sum$total_time))+5) +
    coord_flip() +
    theme(legend.position = "none")#, axis.text.y = ggtext::element_markdown())
ggsave(bar_time_last_4_weeks, filename=here::here("figures", "bar_time_last_4_weeks.png"), height=3, width=4)

# like Strava profile bar plot
bar_time_stacked_4_weeks <- act_data %>%
    filter_last_n_weeks(num_weeks_ago = 4) %>% 
    group_by(week, type) %>%
    summarize(total_time=sum(moving_time_hrs)) %>%
    mutate(week_int = as.integer(week),
           week_str = glue("{mday(week)} {as.character(month(week, label=TRUE, abbr = TRUE))}")) %>%
    ggplot(aes(x=week_int, y=total_time, fill=type)) +
    geom_bar(position = "stack", stat = "identity") + 
    scale_fill_manual("type", values=colors) + 
    scale_x_reverse(labels=c())+#labels = unique(act_data_last_4_wks$week_str), 
                    #breaks = unique(act_data_last_4_wks$week_int)) +
    scale_y_continuous(breaks = pretty_breaks()) +
    coord_flip() +
    labs(x = '', y = 'Time (hrs)') +
    theme(legend.position = "bottom", 
          legend.title = element_blank(),
          axis.ticks = element_blank())

act_data_last_4_wks <- act_data %>% 
    filter_last_n_weeks(num_weeks_ago = 4) %>% 
    mutate(start_day = lubridate::floor_date(start_date_local, unit="day")) %>%
    group_by(start_day) %>% 
    summarize(total_time=sum(moving_time_hrs)) %>% 
    mutate(week = floor_date(start_day, unit='week', week_start = 1),
           week_int = as.integer(week),
           week_str = glue("{mday(week)} {as.character(month(week, label=TRUE, abbr = TRUE))}"),
           wday = wday(start_day, week_start = 1, label = TRUE, abbr = TRUE))
# github contribution-style calendar heatmap
heatmap_calendar <- act_data_last_4_wks %>% 
    ggplot(aes(x=wday, y=week_int, fill=total_time)) + 
    geom_tile(colour="white", size=1) +
    scale_fill_distiller(type="seq", 
                         na.value = "white",
                         direction = 1,
                         limits = c(0, max(act_data_last_4_wks$total_time)),
                         name = "Time (hrs)",
                         palette = "Greys") +
    scale_y_reverse(labels = unique(act_data_last_4_wks$week_str), 
                    breaks = unique(act_data_last_4_wks$week_int)) +
    scale_x_discrete(position = "top") +
    labs(x = '', y = '') + 
    theme(panel.background = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_rect("grey92"),
          legend.position = "bottom",
          axis.line = element_blank()
    )
ggsave(heatmap_calendar, filename=here::here('figures', 'heatmap_calendar.png'), height=3, width=4)
title <- ggdraw() + 
    draw_label(
        "Activity Summary",
        fontface = 'bold',
        x = 0,
        hjust = 0
    ) +
    theme(
        # add margin on the left of the drawing canvas,
        # so title is aligned with left edge of first plot
        plot.margin = margin(10, 10, 0, 240)
    )
plots_last_4_wks <- cowplot::plot_grid(heatmap_calendar, bar_time_stacked_4_weeks, align = "h")
plot_summary_4_wks <- cowplot::plot_grid(title, plots_last_4_wks, rel_heights = c(0.1, 1), ncol=1)
ggsave(plot_summary_4_wks, filename=here::here('figures', 'plot_summary_4_weeks.png'), height=4, width=8)
# TODO: annotate with personal events (bought commuter bike, bought road bike, etc)

ymax_week <- act_data %>%
    group_by(week) %>%
    summarise(total_hrs = sum(moving_time_hrs)) %>% 
    pull(total_hrs) %>% 
    max()

bar_plot_all_week <- plot_bar_week(act_data, ymax_week) +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
    ggtitle("All Strava Activities")
ggsave(bar_plot_all_week, filename = filename_bar_all_week,
       width = default_width, height = default_height)

for (year in years) {
    year <- as.character(year)
    bar_plot_year <- plot_bar_week(filter_year(act_data, "start_date", year), 
                                   ymax_week) +
        ggtitle(paste0(year, " Activites"))
    ggsave(bar_plot_year, filename = here::here("figures", paste0("bar_", year,".png")),
           width = 6, height = get_height(6))
}

line_plot_month_time <- act_data %>% 
    #filter_count(min=3) %>%
    mutate(month_year = lubridate::floor_date(start_date_local, 
                                              unit = "month")) %>% 
    group_by(month_year, type) %>%
    summarize(moving_time_hrs = sum(moving_time_hrs)) %>%
    ggplot2::ggplot(aes(x=month_year, y=moving_time_hrs, colour=type)) +
    geom_line() +
    scale_colour_manual(values=unlist(colors, use.names=FALSE)) +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
    #scale_y_continuous(trans='log2') +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(line_plot_month_time, filename = here::here('figures', 'line_plot_month_time.png'),
       width = default_width, height = default_height)

line_plot_month_dist <- act_data %>% 
    #filter_dist() %>%
    mutate(month_year = lubridate::floor_date(start_date_local, 
                                              unit = "month")) %>% 
    group_by(month_year, type) %>%
    summarize(distance_mi = sum(distance_mi)) %>%
    ggplot2::ggplot(aes(x=month_year, y=distance_mi, colour=type)) +
    geom_line() +
    scale_colour_manual(values=unlist(colors, use.names=FALSE)) +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_continuous(trans='log2', 
                       #limits = c(1, 800), 
                       breaks =  c(1, 6.2, 13.1, 24.8, 40, 62.1, 100, 250, 500, 750, 1000)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(line_plot_month_dist, filename = here::here('figures', 'line_plot_month_dist.png'),
       width = default_width, height = default_height)

# binned by month
ymax_month <- act_data %>% 
    group_by(month) %>% 
    summarise(total_hrs = sum(moving_time_hrs)) %>% 
    pull(total_hrs) %>% 
    max()
bar_plot_month <- plot_bar_facet(act_data, "month") + 
    ylim(0, ymax_month) + 
    xlim(0, 12) +
    scale_x_continuous(breaks=1:12, labels = month.abb)
ggsave(bar_plot_month, filename=filename_bar_all_month, height=get_height(7), width=7)

# binned by day
ymax <- act_data %>% group_by(yday) %>% summarise(total_hrs = sum(moving_time_hrs)) %>% pull(total_hrs) %>% max()
bar_plot_day <- plot_bar_facet(act_data, "yday") + 
    ylim(0, ymax) +
    xlim(0, 366) +
    scale_x_continuous(breaks = c(1,31,59,90,120,151,181,212,243,273,304,334,365), labels=c("", "31 Jan", "28 Feb", "31 Mar", "30 Apr", "31 May", "30 Jun", "31 Jul", "31 Aug", "30 Sep", "31 Oct", "30 Nov", "31 Dec"))
ggsave(bar_plot_day, filename=filename_bar_all_day, height=get_height(7), width=7)

# jitter type x time
jitter_plot_time <- act_data %>% filter_count() %>%
    ggplot(aes(type, moving_time_hrs)) +
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.9, color="gray35") +
    geom_jitter(aes(color=type), alpha=default_alpha) +
    scale_color_manual("type", values=colors) +
    scale_y_continuous(breaks=0:7) +
    ggtitle("Strava Activities") +
    theme_classic() +
    theme(legend.position = "none")
ggsave(jitter_plot_time, filename = here::here("figures", "jitter_type_time.png"), height = 6, width = get_width(6))

# jitter time x year, facet by type
jitter_plot_time_year <- act_data %>% filter(type %in% c("Ride", "Run")) %>% 
    ggplot(aes(year, moving_time_hrs)) +
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.9, color="gray35") +
    geom_jitter(aes(color=type), alpha=default_alpha) +
    facet_wrap(~type, nrow=1, scale="free") +
    scale_color_manual("type", values=colors) +
    scale_y_continuous(breaks=0:7) +
    theme_classic() +
    theme(legend.position = "none")
ggsave(jitter_plot_time_year, filename = here::here("figures", "jitter_time_year.png"), height = 5, width = get_width(5))

# jitter dist x year, facet by type
jitter_plot_dist_year <- act_data %>% filter(type %in% c("Ride", "Run")) %>% 
    ggplot(aes(year, distance_mi)) +
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.9, color="gray35") +
    geom_jitter(aes(color=type), alpha=default_alpha) +
    facet_wrap(~type, nrow=1, scale="free") +
    scale_color_manual("type", values=colors) +
    scale_y_continuous(breaks = c(0, 5, seq(10, 100, by = 10))) +
    theme_classic() +
    theme(legend.position = "none")
ggsave(jitter_plot_dist_year, filename = here::here("figures", "jitter_dist_year.png"), height = 5, width = get_width(5))


# jitter type x dist
jitter_plot_dist <- act_data %>% 
    filter_dist() %>% 
    filter_count() %>%
    ggplot(aes(type, distance_mi)) +
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.9, color="gray35") +
    geom_jitter(aes(color=type), alpha=default_alpha) +
    scale_color_manual("type", values=colors) +
    theme_classic()
ggsave(jitter_plot_dist, filename = here::here("figures", "jitter_type_dist.png"), height = 6, width = get_width(6))

# log2-transform for better distance comparison
jitter_plot_dist_log2 <- act_data %>% 
    filter_dist() %>% 
    filter_count() %>%
    filter(distance_mi > 0) %>% 
    ggplot(aes(type, distance_mi)) +
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.9, color="gray35") +
    geom_jitter(aes(color=type), alpha=default_alpha) +
    scale_color_manual("type", values=colors) +
    scale_y_continuous(trans="log2", limits=c(0.25, 105), breaks = c(0.3, 0.5, 1, 3.1, 6.2, 13.1, 24.8, 40, 62.1, 100)) +
    ggtitle("Strava Activities") +
    theme_classic()
ggsave(jitter_plot_dist_log2, filename = here::here("figures", "jitter_type_dist_log2.png"), height = 6, width = get_width(6))

# jitter weekday x distance
jitter_plot_weekday_dist_grid <- act_data %>% 
    filter_dist() %>%
    filter_count() %>%
    ggplot(aes(wday, distance_mi)) +
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.9, color="gray35") +
    geom_jitter(aes(color=type), alpha=default_alpha) +
    scale_color_manual("type", values=colors) +
    facet_grid(type~wday, scale="free") +
    ylab("Distance (mi)") +
    theme_classic()  +
    theme(legend.position = "none")
ggsave(jitter_plot_weekday_dist_grid, filename = here::here('figures', "jitter_weekday_dist_grid.png"), width=10, height=get_height(10))

# jitter weekday x time
jitter_plot_weekday_time_grid <- act_data %>% 
    filter_count() %>%
    ggplot(aes(wday, moving_time_hrs)) +
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = 0.9, color="gray35") +
    geom_jitter(aes(color=type), alpha=default_alpha) +
    scale_color_manual("type", values=colors) +
    facet_grid(type~wday, scale="free") +
    ylab("Moving time (hrs)") +
    theme_classic() +
    theme(legend.position = "none")
ggsave(jitter_plot_weekday_time_grid, filename = here::here('figures', "jitter_weekday_time_grid.png"), width=10, height=get_height(10))

# boxplot weekday x distance
# TODO: use cowplot for better control
plot_box <- function(data, x_str, y_str, fill_str) {
    ggplot(data, aes_string(x=x_str, y=y_str, fill=fill_str)) +
        geom_boxplot() + 
        scale_fill_manual(fill_str, values=colors) +
        scale_x_discrete(as.factor(c("M", "T", "W", "H", "F", "Sa", "Su"))) +
        theme_classic() +
        ylab("") + xlab("wday") +
        theme(legend.position = "none")
}

# boxplot weekday x time
box_plot_weekday_time <- act_data %>% 
    filter_count() %>%
    ggplot(aes(wday, moving_time_hrs, fill=type)) +
    geom_boxplot(aes(fill=type)) +
    scale_fill_manual("type", values=colors) +
    scale_y_continuous(breaks=1:7) +
    facet_wrap(~type, nrow = 1) +
    theme_classic() +
    theme(legend.position = "none")
ggsave(box_plot_weekday_time, filename = here::here('figures', "box_weekday_time.png"), width=8, height=get_height(8))

#### try cowplot
plots = list()
plots[["Ride"]] <- plot_box(filter_type(act_data, "Ride"), "wday", "distance_mi", "type")
plots[["Run"]]  <- plot_box(filter_type(act_data, "Run"), "wday", "distance_mi", "type") + ylim(0, 25)
plots[["Rowing"]] <- plot_box(filter_type(act_data, "Rowing"), "wday", "distance_mi", "type") + ylim(0,6)
plots[["Swim"]] <- plot_box(filter_type(act_data, "Swim"), "wday", "distance_mi", "type") + ylim(0, 1)
box_cow_weekday_dist <- cowplot::plot_grid(plotlist = plots)
#####
box_plot_weekday_dist_wrap <- act_data %>% filter_dist() %>% filter_count() %>%
    ggplot(aes(wday, distance_mi)) +
    geom_boxplot(aes(fill=type)) +    
    scale_fill_manual("type", values=colors) +
    facet_wrap(~type, scale="free_y") +
    ylab("Distance (mi)") +
    theme_classic() +
    theme(legend.position = "none")
ggsave(box_plot_weekday_dist_wrap, 
       filename = here::here('figures', "box_weekday_dist_wrap.png"), 
       width=8, height=get_height(8))
######

# cumulative activity time
line_plot_time <- act_data %>% 
    filter_count() %>%
    ggplot(aes(x=start_date, y=elapsed_hrs_cum_type, color=type)) +
    geom_line() +
    scale_colour_manual(values=unlist(colors, use.names=FALSE)) +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_continuous(labels=label_comma()) +
    ylab("elapsed hrs (cumulative)") +
    xlab("") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Cumulative Activity Time (hrs)")
ggsave(line_plot_time, filename=here::here("figures", "line_time.png"), width = default_width, height = default_height)

# cumulative activity distance
line_plot_dist <- act_data %>% 
    filter_count() %>%
    filter_dist() %>%
    ggplot(aes(x=start_date, y=elapsed_dist_cum_type, color=type)) +
    geom_line() +
    scale_colour_manual(values=unlist(colors, use.names=FALSE)) +
    scale_x_datetime(date_breaks = "1 month", date_labels = "%b %Y") +
    scale_y_continuous(breaks=seq(0, max(act_data$elapsed_dist_cum_type), 500),
                       labels=label_comma()) +
    ylab("distance mi (cumulative)") +
    xlab("") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle("Cumulative Activity Distance (mi)")
ggsave(line_plot_dist, filename=here::here("figures", "line_dist.png"), width = default_width, height = default_height)
