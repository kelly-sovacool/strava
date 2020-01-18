library(here)
library(forcats)
library(ggplot2)
library(dplyr)
library(scales)

get_width <- function(height=6, aspect_ratio=4/3) {
    height * aspect_ratio
}
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

# read the data

table_sum <- readr::read_csv(here::here("data" ,"processed", "summary.csv")) %>%
    mutate(type = fct_reorder(type, sum_time_hrs, .fun = sum, .desc=TRUE))
colors <- set_colors(table_sum)

# plots summary stats

bar_sum_dist <- table_sum %>%
    filter_dist_sum() %>%
    ggplot(aes(year, sum_dist_mi, fill=type, label=sum_dist_mi)) +
    geom_col(position="dodge") +
    geom_text(vjust=-0.3, position = position_dodge(width=1)) +
    scale_fill_manual("type", values=colors) +
    facet_wrap(~type, nrow=1) +
    scale_y_continuous(breaks=pretty_breaks()) +
    ggtitle("Total Distance (mi) by year") +
    theme_classic() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none")
ggsave(bar_sum_dist, filename = here::here("figures", "bar_sum_dist.png"), height = 5, width = get_width(5))

bar_sum_hrs <- table_sum %>%
    filter_time_sum() %>%
    ggplot(aes(year, sum_time_hrs, fill=type, label=sum_time_hrs)) +
    geom_col(position="dodge") +
    geom_text(vjust=-0.5,
              position = position_dodge(width=0.5)) +
    scale_fill_manual("type", values=colors) +
    facet_wrap(~type, nrow=1) +
    scale_y_continuous(breaks=pretty_breaks()) +
    ggtitle("Total Time (hrs) by year") +
    theme_classic() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none")
ggsave(bar_sum_hrs, filename = here::here("figures", "bar_sum_hrs.png"), height = 5, width = get_width(5))
