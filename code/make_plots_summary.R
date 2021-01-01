library(here)
library(forcats)
library(ggplot2)
library(dplyr)
library(scales)
source(here::here("code", "utils.R"))

# read the data
table_sum <- readr::read_csv(here::here("data" ,"processed", "summary.csv")) %>%
    mutate(type = fct_reorder(type, sum_time_hrs, .fun = sum, .desc=TRUE))
colors <- set_colors(table_sum)

# plots summary stats
bar_sum_dist <- table_sum %>%
    filter_dist_sum() %>%
    ggplot(aes(type, sum_dist_mi, fill=type, label=sum_dist_mi)) +
    geom_col(position="dodge") +
    geom_text(vjust=-0.3, 
              position = position_dodge(width=1)) +
    scale_fill_manual("type", values=colors) +
    facet_wrap(~year, nrow=1) +
    scale_y_continuous(breaks=pretty_breaks()) +
    ggtitle("Total Distance (mi) by year") +
    theme_classic() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none")
ggsave(bar_sum_dist, filename = here::here("figures", "bar_sum_dist.png"), height = 5, width = 1.5 * get_width(5))

bar_sum_hrs <- table_sum %>%
    filter_time_sum() %>%
    ggplot(aes(type, sum_time_hrs, fill=type, label=sum_time_hrs)) +
    geom_col(position="dodge") +
    geom_text(vjust=-0.2,
              position = position_dodge(width=0.5)) +
    scale_fill_manual("type", values=colors) +
    facet_wrap(~year, nrow=1) +
    scale_y_continuous(breaks=pretty_breaks()) +
    ggtitle("Total Time (hrs) by year") +
    theme_classic() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none")
ggsave(bar_sum_hrs, filename = here::here("figures", "bar_sum_hrs.png"), height = 5, width = 1.5 * get_width(5))


# one-offs
height <- 4
dist_nye_2020 <- table_sum %>%
    filter_dist_sum() %>%
    filter(year %in% c(2019, 2020), 
           type %in% c('Ride', 'Run', 'RockClimbing', 'Hike', 'Swim')) %>% 
    ggplot(aes(type, sum_dist_mi, fill=type, label=sum_dist_mi)) +
    geom_col(position="dodge") +
    geom_text(vjust=-0.2, hjust=0.5,
              position = position_dodge(width=1)) +
    scale_fill_manual("type", values=colors) +
    facet_wrap(~year, nrow=1) +
    scale_y_continuous(breaks=pretty_breaks()) +
    theme_classic() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none") +
    labs(y = 'Distance (mi)', x = '')
ggsave(dist_nye_2020, 
       filename = here::here("figures", "dist_nye_2020.png"), 
       height = height, width = get_width(height))

time_nye_2020 <- table_sum %>%
    filter_time_sum() %>%
    filter(year %in% c(2019, 2020), 
           type %in% c('Ride', 'Run', 'RockClimbing', 'Hike', 'Swim')) %>% 
    ggplot(aes(type, sum_time_hrs, fill=type, label=sum_time_hrs)) +
    geom_col(position="dodge") +
    geom_text(vjust=-0.1, hjust=0.5,
              position = position_dodge(width=0.5)) +
    scale_fill_manual("type", values=colors) +
    facet_wrap(~year, nrow=1) +
    scale_y_continuous(breaks=pretty_breaks()) +
    theme_classic() +
    theme(axis.text.x=element_text(angle=45, hjust=1),
          legend.position = "none") +
    labs(y = 'Moving Time (hrs)', x = '')
ggsave(time_nye_2020, 
       filename = here::here("figures", "time_nye_2020.png"), 
       height = height, width = get_width(height))
