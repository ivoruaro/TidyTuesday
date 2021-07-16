library('tidyverse')
library('tidytuesdayR')
library('lubridate')
library('ggplot2')
library('ggbump')
library('png')
library('grid')
library('ggpubr')

# Scooby Doo fonts: https://www.dafont.com/it/scoobydoo.font
windowsFonts(ScoobyFonts=windowsFont("Scooby Doo"))

# Load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
scoobydoo <- tuesdata$scoobydoo

# snack
df <- scoobydoo %>% 
    # fred
    mutate(snack_fred_v = if_else(snack_fred == 'TRUE', 1, 0, 0)) %>%
    # daphnie
    mutate(snack_daphnie_v = if_else(snack_daphnie == 'TRUE', 1, 0, 0)) %>%
    # velma
    mutate(snack_velma_v = if_else(snack_velma == 'TRUE', 1, 0, 0)) %>%
    # shaggy
    mutate(snack_shaggy_v = if_else(snack_shaggy == 'TRUE', 1, 0, 0)) %>%
    # scooby
    mutate(snack_scooby_v = if_else(snack_scooby == 'TRUE', 1, 0, 0)) %>%
    # summarize
    mutate(year = year(ymd(date_aired))) %>%
    group_by(year) %>%
    summarize(
        fred = sum(snack_fred_v),
        daphnie = sum(snack_daphnie_v),
        velma = sum(snack_velma_v),
        shaggy = sum(snack_shaggy_v),
        scooby = sum(snack_scooby_v)) %>%
    # cumulative sum
    mutate(
        fred = cumsum(fred),
        daphnie = cumsum(daphnie),
        velma = cumsum(velma),
        shaggy = cumsum(shaggy),
        scooby = cumsum(scooby)
    ) %>%
    select(year, fred, daphnie, velma, shaggy, scooby) %>%
    # pivot
    pivot_longer(!year) %>%
    group_by(year) %>%
    mutate(rank = rank(value, ties.method = "first")) %>%
    ungroup()
    

### Load images
i_fred <- rasterGrob(readPNG("2021w29-fred.png"))
i_daphnie <- rasterGrob(readPNG("2021w29-daphnie.png"))
i_velma <- rasterGrob(readPNG("2021w29-velma.png"))
i_shaggy <- rasterGrob(readPNG("2021w29-shaggy.png"))
i_scooby <- rasterGrob(readPNG("2021w29-scooby.png"))
i_background <- rasterGrob(readPNG("2021w29-background-v2.png"))

### coords
y_fred <- max(df %>% filter(name == 'fred') %>% select(value))
y_daphnie <- max(df %>% filter(name == 'daphnie') %>% select(value))
y_velma <- max(df %>% filter(name == 'velma') %>% select(value))
y_shaggy <- max(df %>% filter(name == 'shaggy') %>% select(value))
y_scooby <- max(df %>% filter(name == 'scooby') %>% select(value))
x_all <- max(df$year) - 1.5 #2019.5

size <- 5

df %>% 
    ggplot(
        aes(
            x = year,
            y = value,
            color = name
        )
    ) +
    annotation_custom(
        i_background,
        ymin = 15,
        ymax = 50,
        xmin = 1965, 
        xmax = 1987) +
    geom_bump(smooth = 20, size = 2) +
    annotation_custom(
        i_fred,
        ymin = y_fred - size / 2,
        ymax = y_fred + size / 2,
        xmin=x_all, 
        xmax=x_all + size) +
    annotation_custom(
        i_daphnie,
        ymin = y_daphnie - size / 2,
        ymax = y_daphnie + size / 2,
        xmin=x_all, 
        xmax=x_all + size) +
    annotation_custom(
        i_velma,
        ymin = y_velma - size / 2,
        ymax = y_velma + size / 2,
        xmin=x_all, 
        xmax=x_all + size) +
    annotation_custom(
        i_shaggy,
        ymin = y_shaggy - size / 2,
        ymax = y_shaggy + size / 2,
        xmin=x_all, 
        xmax=x_all + size) +
    annotation_custom(
        i_scooby,
        ymin = y_scooby - size / 2,
        ymax = y_scooby + size / 2,
        xmin=x_all, 
        xmax=x_all + size
    ) +
    theme_minimal() +
    theme(
        legend.position = 'none',
        plot.title = element_text(size = 60),
        plot.subtitle = element_text(size = 30),
        plot.caption = element_text(size = 12),
        text = element_text(family = 'ScoobyFonts'),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank()
    ) + 
    scale_color_manual(
        values = c(
            '#4039e8',
            '#feef7a70',
            '#ad722970',
            '#e9943a70',
            '#8b443e70'
        )
    ) +
    labs(
        title = 'Who eaten the snack?',
        subtitle = 'Cumulative eaten snacks over the years.',
        caption = '\nIvo Ruaro 2021-07-15 | Data: Kaggle| #TidyTuesday 2021w29'
    )
    