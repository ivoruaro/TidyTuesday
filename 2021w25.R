library('tidytuesdayR')
library('tidyverse')
library('ggplot2')
library('dplyr')
library('maps')
library('plotly')


### Load and cleanup data
tidytuesday_data <- 
    tidytuesdayR::tt_load(2021, week = 25)

df_2021w25 <- 
    tidytuesday_data$tweets %>%
    drop_na()

### Prepare data
df <- df_2021w25 %>%
    group_by(username, lat, long) %>%
    summarize(count = n())

### World data map
world_map <- map_data('world')

### Build the plot
plot <- world_map %>%
    ggplot(
        aes(
            x = long,
            y = lat,
            group = group)) +
    # World map
    geom_polygon(
        fill = '#b39d88',
        colour = "grey50",
        aes(
            text = paste(
                region, 
                ifelse(
                    is.na(subregion),
                    '', 
                    subregion)))) +
    # Tweet Location
    geom_point(
        data = df,
        mapping = aes(
            x = long,
            y = lat,
            group = username,
            size = count,
            text = paste(
                username, 
                '\nTweets:', count, 
                '\nLong:', long, 
                '\nLat:', lat)
        ),
        color = '#bb263c',
        alpha = 0.7,
        position = position_dodge(width=0.3)
    ) +
    # Title
    annotate(
        'text',
        x = -150,
        y = -0,
        size = 6,
        hjust = 0,
        label = '#DuBoisChallenge\nTweets by location',
        color = 'gray40',
        family = 'mono',
        text = ':)'
    ) +
    # Tag line
    annotate(
        'text',
        x = -150,
        y = -15,
        size = 3,
        hjust = 0,
        label = 'Ivo Ruaro | #TinyTuesday 2021w25\n2021-06-16',
        color = 'gray50',
        family = 'mono',
        text = ':)'
    ) +
    # Theme
    theme_void() +
    theme(
        legend.position='none',
        panel.background = element_rect(
            fill = "#faf0e6",
            colour = "#faf0e6",
            size = 0.5, 
            linetype = "solid"),
        axis.line = element_blank())

### Plotly
ply <- ggplotly(plot, tooltip = 'text')

### Save html
htmlwidgets::saveWidget(as_widget(ply), "2021w25.html")


