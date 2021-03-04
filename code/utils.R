set_colors <- function(data) {
    palette = RColorBrewer::brewer.pal(n = 11, name = "Paired")
    colors = list()
    colors[["Ride"]] <- palette[[4]]
    colors[["Run"]] <- palette[[8]]
    colors[["RockClimbing"]] <- palette[[3]]
    colors[["Rowing"]] <- palette[[1]]
    colors[['Kayaking']] <- colors[['Rowing']]
    colors[["Swim"]] <- palette[[2]]
    colors[["Hike"]] <- palette[[6]]
    colors[["Walk"]] <- palette[[5]]
    colors[["Elliptical"]] <- palette[[7]]
    colors[["NordicSki"]] <- palette[[10]]
    colors[["Yoga"]] <- palette[[9]]
    colors[['Workout']] <- palette[[11]]
    colors[['WeightTraining']] <- colors[['Workout']]
    return(colors)
}
get_width <- function(height=6, aspect_ratio=4/3) {
    height * aspect_ratio
}
get_height <- function(width=8, aspect_ratio=4/3) {
    width / aspect_ratio
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
plot_bar_week <- function(data, ymax = NA, yvar = moving_time_hrs) {
    colors <- set_colors(data)
    if (is.na(ymax)) {
        ymax <- data %>% 
            group_by(week) %>%
            summarise(total_yvar = sum({{ yvar }})) %>% 
            pull(total_yvar) %>% 
            max()
    }
    plot <- data %>% ggplot2::ggplot(aes(x=week, y={{ yvar }}, fill=type)) +
        geom_col(position="stack") +
        scale_fill_manual("type", values=colors) +
        ylim(0, ymax) +
        scale_x_datetime(date_breaks = "4 weeks", date_labels = "%b %d") +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
}
filter_dist_sum <- function(data) {
    types_to_keep <- data %>% 
        group_by(type) %>% 
        summarise(total_dist_mi=sum(sum_dist_mi),
                  count=n()) %>% 
        filter(count >= 2) %>%
        filter(total_dist_mi > 0) %>% 
        pull(type)
    return(
        data %>%
            filter(type %in% types_to_keep)
    )
}
filter_time_sum <- function(data) {
    types_to_keep <- data %>% 
        group_by(type) %>% 
        summarise(total_time_hrs=sum(sum_time_hrs),
                  count=n()) %>% 
        filter(count >= 2) %>%
        filter(total_time_hrs > 0) %>% 
        pull(type)
    return(
        data %>%
            filter(type %in% types_to_keep)
    )
}
