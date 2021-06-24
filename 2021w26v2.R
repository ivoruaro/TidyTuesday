library('tidyverse')
library('tidytuesdayR')
library('ggplot2')
library('gganimate')
library('hrbrthemes')

# Get data
tuesdata <- tidytuesdayR::tt_load(2021, week = 26)
parks <- tuesdata$parks
parks$city <- gsub("Washington, DC", "Washington, D.C.", parks$city)

# Number of cities to analyze
filter_top <- 10

# Select the top expensive cities in the last available year
df_top_rank_last_year <- parks %>%
    filter(year == max(year)) %>%
    arrange(rank) %>%
    top_n(filter_top, -rank) %>%
    select(city, rank)

# Filter the data
df <- parks %>%
    filter(city %in% df_top_rank_last_year$city) %>%
    filter(rank <= 10) %>%
    select(city, year, rank) %>%
    arrange(year, city)

# Plot
p <- df %>%
    ggplot(
        aes(
            x = year,
            y = rank,
            group = city,
            color = city
        )
    ) +
    geom_line(size = 1) +
    geom_point(size = 3.5) +
    geom_text(
        aes(
            x = 2020.3,
            label = city
        ),
        hjust = 0,
        alpha = 0.7, 
        size = 6
    ) +
    geom_segment(
        aes(
            xend = max(year),
            yend = rank
        ), 
        linetype = 2,
        size = 0.7) +
    geom_text(
        df %>% filter(city == 'San Francisco'),
        mapping = aes(
            x = 2013,
            y = 8.8,
            label = as.character(year)
        ),
        size = 25,
        color = 'gray50'
    ) +
    # Theme
    theme_modern_rc() +
    theme(
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
    ) +
    # Scale
    scale_y_reverse() +
    xlim(2012, 2022) +
    # Titles
    labs(
        title = 'City Ranking Over Time',
        subtitle = 'Evolution of the 2020 top 10 cities starting from 2012',
        caption = 'Ivo Ruaro 2021-06-24 | Data Source: The Trust for Public Land | #TidyTuesday 2021w26'
    ) +
    # Animation
    transition_reveal(year) +
    enter_fade() +
    exit_fade() +
    ease_aes('cubic-in-out')

# Animation
animate(p, fps = 10, width = 800, height = 500, end_pause = 30, rewind = FALSE)

# Save
anim_save('2021w26v2.gif')
