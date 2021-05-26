library("tidyverse")
library("lubridate")
library("dplyr")
library("ggthemes")
library("gridExtra")
library("grid")

# Get the Data
records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

records$year <- year(records$date)

#############################
### Create the main graph ###
#############################
build_fig <- function(track_filtered) {
    
    # Some fixed settings
    color_yes_shortcut <- "#77ab43"
    color_no_shortcut <- "#008fd5"
    
    # Prepare the data (shortcut No)
    record_by_year_no_scut <- records %>%
        filter(track == track_filtered) %>%
        filter(type == "Three Lap") %>%
        filter(shortcut == "No") %>%
        group_by(year) %>%
        summarise(record = min(time), records = n())
    
    # Prepare the data (shortcut Yes)
    record_by_year_yes_scut <- records %>%
        filter(track == track_filtered) %>%
        filter(type == "Three Lap") %>%
        filter(shortcut == "Yes") %>%
        group_by(year) %>%
        summarise(record = min(time), records = n())
    
    # Build the figure
    fig <-
        # Main graph
        ggplot(
            data = record_by_year_yes_scut,
            aes(x = year, y = record)) +   
        geom_line(
            color = color_yes_shortcut) +
        geom_point(
            #aes(size = records)
            color = color_yes_shortcut) +
        geom_line(
            data = record_by_year_no_scut,
            color = color_no_shortcut) +
        geom_point(
            data = record_by_year_no_scut,
            #aes(size = records),
            color = color_no_shortcut) +
        # Visuals
        labs(
            x = "",
            y = "",
            subtitle = track_filtered) +
        # Limits
        scale_y_continuous(
            limits = c(0, 400)
        ) +
        scale_x_continuous(
            limits = c(1997, 2022)
        ) +
        # Theme
        theme_fivethirtyeight() +
        theme(legend.position = "none")
    
    return(fig)
}

remove_x <- function(figure) {
    return(
        figure + 
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
    )
}

remove_y <- function(figure) {
    return(
        figure + 
            theme(axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank())
    )
}

# unique(records$track)

# Build the single plot
f_luigi_raceway <- build_fig("Luigi Raceway")
f_moo_moo_farm <- build_fig("Moo Moo Farm")
f_koopa_troopa_beach <- build_fig("Koopa Troopa Beach")
f_kalimari_desert <- build_fig("Kalimari Desert")

f_toads_turnpike <- build_fig("Toad's Turnpike")
f_frappe_snowland <- build_fig("Frappe Snowland")
f_choco_mountain <- build_fig("Choco Mountain")
f_mario_raceway <- build_fig("Mario Raceway")

f_wario_stadium <- build_fig("Wario Stadium")
f_sherbet_land <- build_fig("Sherbet Land")
f_royal_raceway <- build_fig("Royal Raceway")
f_bowsers_castle <- build_fig("Bowser's Castle")

f_dks_jungle_parkway <- build_fig("D.K.'s Jungle Parkway")
f_yoshi_valley <- build_fig("Yoshi Valley")
f_banshee_boardwalk <- build_fig("Banshee Boardwalk")
f_rainbow_road <- build_fig("Rainbow Road")

# Rows
r1 <- remove_x(f_luigi_raceway) | 
    remove_y(remove_x(f_moo_moo_farm)) | 
    remove_y(remove_x(f_koopa_troopa_beach)) | 
    remove_y(remove_x(f_kalimari_desert))

r2 <- remove_x(f_toads_turnpike) | 
    remove_y(remove_x(f_frappe_snowland)) | 
    remove_y(remove_x(f_choco_mountain)) | 
    remove_y(remove_x(f_mario_raceway))

r3 <- remove_x(f_wario_stadium) | 
    remove_y(remove_x(f_sherbet_land)) | 
    remove_y(remove_x(f_royal_raceway)) | 
    remove_y(remove_x(f_bowsers_castle))

r4 <- f_dks_jungle_parkway | 
    remove_y(f_yoshi_valley) | 
    remove_y(f_banshee_boardwalk) | 
    remove_y(f_rainbow_road)


# Patchwork
pw <- (r1 / r2 / r3 / r4) &
    theme_fivethirtyeight() &
    plot_annotation(
        title = "Mario Kart 64 Track Records Over the Years for Three Laps",
        subtitle = "Records are in seconds, <span style='color:#77ab43;'><strong>with shortcuts</strong></span> and <span style='color:#008fd5;'><strong>without shortcuts</strong></span>. Each dot is an annual record.",
        caption = 'Ivo Ruaro - 2021-05-26 | #TinyTuesday 2021w22 | Mario Kart World Record',
        theme = theme(plot.subtitle = element_markdown()))
pw

ggsave(
    "TidyTuesday2020w22.jpg",
    dpi = 150,
    units = "cm",
    width = 20 * 1.9,
    height = 20
)
