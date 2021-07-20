library(tidyverse)
library(tidytuesdayR)
library(ggformula)
library(grid)
library(ggimage)
library(extrafont)
library(ggtext)

# Takes a while
# font_import()
# loadfonts(device="win")
# fonts()          

# Load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 30)
drought <- tuesdata$drought

# Wrangling
df <- drought %>% 
    select(state_abb, valid_start, drought_lvl, pop_pct) %>%
    filter(drought_lvl != 'None') %>%
    pivot_wider(names_from = c(drought_lvl), values_from = c(pop_pct))

# Colors
pal_drought <- c('#ffff00', '#fcd37f', '#ffaa00', '#e60000', '#730000')

# Plot
p <- df  %>%
    ggplot(
        aes(
            x = valid_start,
            group = state_abb
        )
    ) +
    # Geometries
    stat_smooth(
        aes(y = D0),
        geom = 'area',
        fill = pal_drought[1],
        alpha = 1) +
    stat_smooth(
        aes(y = D1),
        geom = 'area',
        fill = pal_drought[2],
        alpha = 1) +
    stat_smooth(
        aes(y = D2),
        geom = 'area',
        fill = pal_drought[3],
        alpha = 1) +
    stat_smooth(
        aes(y = D3),
        geom = 'area',
        fill = pal_drought[4],
        alpha = 1) +
    stat_smooth(
        aes(y = D4),
        geom = 'area',
        fill = pal_drought[4],
        alpha = 1) +
    facet_wrap( ~ state_abb) +
    # Theme
    theme_dark() +
    theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(colour = "gray90"),
        plot.title = element_text(colour = "#e60000", family = 'Verdana Pro Cond Black', size = 30),
        plot.subtitle = element_markdown(colour = "gray30", family = 'Verdana Pro Cond Black', size = 15)
    ) +
    # Scale
    scale_y_continuous(limits = c(0, 115)) +
    # Labs
    labs(
        title = 'Percent of Total State Population in Drought by Year',
        subtitle = "<span style='color:#ffff00;'>Abnormally Dry</span> - 
        <span style='color:#fcd37f;'>Moderate Drought</span> - 
        <span style='color:#ffaa00;'>Severe Drought</span> - 
        <span style='color:#e60000;'>Extreme Drought</span> - 
        <span style='color:#730000;'>Exceptional Drought</span>",
        caption = 'Ivo Ruaro 2021-07-20 | Data: U.S. Drought Monitor | #TidyTuesday 2021w30')

img = '2021w30-background-830x455.png'
ggbackground(p, img)

