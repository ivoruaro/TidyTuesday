library('tidyverse')
library('tidytuesdayR')
library('lubridate')
library('ggplot2')
#library('gganimate')
#library('hrbrthemes')
library('ggbump')
library('png')
library('grid')
library('ggpubr')

tuesdata <- tidytuesdayR::tt_load(2021, week = 29)
scoobydoo <- tuesdata$scoobydoo


# # captured
# df <- scoobydoo %>% 
#     # fred
#     mutate(captured_fred_v = if_else(captured_fred == 'TRUE', 1, 0, 0)) %>%
#     # daphnie
#     mutate(captured_daphnie_v = if_else(captured_daphnie == 'TRUE', 1, 0, 0)) %>%
#     # velma
#     mutate(captured_velma_v = if_else(captured_velma == 'TRUE', 1, 0, 0)) %>%
#     # shaggy
#     mutate(captured_shaggy_v = if_else(captured_shaggy == 'TRUE', 1, 0, 0)) %>%
#     # scooby
#     mutate(captured_scooby_v = if_else(captured_scooby == 'TRUE', 1, 0, 0)) %>%
#     # summarize
#     mutate(year = year(ymd(date_aired))) %>%
#     group_by(year) %>%
#     summarize(
#         fred = sum(captured_fred_v),
#         daphnie = sum(captured_daphnie_v),
#         velma = sum(captured_velma_v),
#         shaggy = sum(captured_shaggy_v),
#         scooby = sum(captured_scooby_v)) %>%
#     # cumulative sum
#     mutate(
#         fred = cumsum(fred),
#         daphnie = cumsum(daphnie),
#         velma = cumsum(velma),
#         shaggy = cumsum(shaggy),
#         scooby = cumsum(scooby)
#     ) %>%
#     select(year, fred, daphnie, velma, shaggy, scooby) %>%
#     # pivot
#     pivot_longer(!year)
    
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
# fred
i_fred <- rasterGrob(readPNG("2021w29-fred.png"))
y_fred <- max(df %>% filter(name == 'fred') %>% select(value))

# daphnie
i_daphnie <- rasterGrob(readPNG("2021w29-daphnie.png"))
y_daphnie <- max(df %>% filter(name == 'daphnie') %>% select(value))

# velma
i_velma <- rasterGrob(readPNG("2021w29-velma.png"))
y_velma <- max(df %>% filter(name == 'velma') %>% select(value))

# shaggy
i_shaggy <- rasterGrob(readPNG("2021w29-shaggy.png"))
y_shaggy <- max(df %>% filter(name == 'shaggy') %>% select(value))

# scooby
i_scooby <- rasterGrob(readPNG("2021w29-scooby.png"))
y_scooby <- max(df %>% filter(name == 'scooby') %>% select(value))

# background
i_background <- readPNG("2021w29-background.png")

# Images coordinates
size <- 5
x_all <- 2019.5

df %>% 
    ggplot(
        aes(
            x = year,
            y = value,
            color = name
        )
    ) +
    geom_bump(smooth = 20, size = 2, alpha = 0.8) +
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
        plot.title = element_text(family = 'Scooby Doo'),
        text = element_text(family = 'Scooby Doo'),
        plot.background = element_rect(
            size = 15,
            fill = 'white',
            color = 'white'
        )
    ) + 
    scale_color_manual(
        values = c(
            '#aa9ec5',
            '#feef7a',
            '#ad7229',
            '#e9943a',
            '#8b443e'
        )
    ) +
    labs(
        title = 'Who eaten the snack?'
    )
