library("tidyverse")
library("dplyr")
library("GGally")
library("ggrepel")
library("survivoR")
library("tidyr")
library("directlabels")







colnames(season_summary)
data <- season_summary %>%
    mutate(
        group = ifelse(rank <= 3, '1', ifelse(rank == 32, '2', '3')),
        group2 = str_pad(rank, 2, pad = "0")) %>%
    arrange(desc(rank))

# Color scale
color_palette <- 
    append(
        c("#011f4b", "#03396c"),
        scales::seq_gradient_pal("gray60", "gray100", "Lab")(seq(0,1,length.out=32 - 2))
    )
    

ggparcoord(
    data = data,
    columns = c(15, 18, 16),
    showPoints = TRUE,
    groupColumn = 21,
    alphaLines = 1,
    scale="globalminmax") +
    #scale_color_manual(values=c("#072f5f", "blue", "grey85")) +
    scale_colour_manual(values=color_palette) +
    geom_line(size=0.6) +
    theme(
        legend.position="none") +
    geom_text(
        data = data %>%
            filter(group == '1') %>%
            mutate(
                x = 1,
                y = viewers_premier) %>%
            select(season_name, x, y),
        aes(
            x = x, 
            y = y, 
            label = season_name),
        hjust = 1,
        inherit.aes = FALSE) +
    labs(x = "pippo")


