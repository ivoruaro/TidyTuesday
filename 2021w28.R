# Independence Days

library('tidyverse')
library('tidytuesdayR')
library('ggplot2')
library('gganimate')

### Get Data
tuesdata <- tidytuesdayR::tt_load(2021, week = 28)
holidays <- tuesdata$holidays

### Clean
month_levels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

df <- holidays %>%
    mutate(date = ymd(format(date_parsed, "2001-%m-%d"))) %>%
    drop_na(date) %>%
    complete(date = seq.Date(ymd('2001-01-01'), ymd('2001-12-31'), by='day')) %>%
    group_by(date) %>%
    summarize(count = sum((if_else(is.na(date_parsed), 0, 1))))


df %>% ggplot(
    aes(
        x = date,
        y = count
        )
    ) +
#    geom_vline(aes(xintercept = date), color = 'green', alpha = 0.2) +
    geom_point(size = 1, color = 'red') +
    theme_dark() +
    coord_polar()

+
    transition_time(date) +
    ease_aes('quadratic-out') +
    shadow_wake(0.3, wrap = FALSE)
