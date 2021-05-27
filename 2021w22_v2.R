library("tidyverse")
library("lubridate")
library("dplyr")
library("ggthemes")
library("scales")
library("ggtext")

### Download data
records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')

### Add the field year
records$year <- year(records$date)

### Summarize data
race_type <- "Three Lap"
race_title <- "Three Laps"
y_max <- 400

# race_type <- "Single Lap"
# race_title <- "Single Lap"
# y_max <- 150


record_by_year <- records %>%
    filter(type == race_type) %>%
    group_by(year, track, shortcut) %>%
    summarise(record = min(time), records = n())

### Build the figure
record_by_year %>%
    ggplot(
        aes(
            x = year, 
            y = record, 
            group = shortcut, 
            color = shortcut,)) +
    geom_line() +
    geom_point() +
    facet_wrap(. ~ track) +
    # Limits
    scale_y_continuous(
        limits = c(0, y_max)) +
    scale_x_continuous(
        limits = c(1997, 2022)) +
    # Visuals
    labs(
        title = paste("Mario Kart 64 Track Records Over the Years for", race_title),
        subtitle = "Records are in seconds, <span style='color:#214249;'><strong>with shortcuts</strong></span> and <span style='color:#1e96c2;'><strong>without shortcuts</strong></span>. Each dot is an annual record.",
        caption = 'Ivo Ruaro - 2021-05-27 | #TinyTuesday 2021w22 | Mario Kart World Record') +
    # Theme
    theme_fivethirtyeight() +
    scale_colour_economist() +
    theme(legend.position = "none") +
    theme(plot.subtitle = element_markdown()) +
    theme(plot.caption = element_text( color = "gray60", size = 8))

### Save
ggsave(
    "TidyTuesday2020w22_2.jpg",
    dpi = 150,
    units = "cm",
    width = 20 * 1.9,
    height = 20
)
