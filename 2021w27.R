library('tidyverse')
library('tidytuesdayR')
library('ggplot2')
library('gganimate')
library('hrbrthemes')


tuesdata <- tidytuesdayR::tt_load(2021, week = 27)
animal_rescues <- tuesdata$animal_rescues

# Test

#animal <- ('cat')
state  <- c('1', '2', 3)
x  <- c(0, 10, 10)
y  <- c(0, 10, 0)


t <- tibble(
    state,
    x,
    y,
)

t %>% 
    ggplot(
        aes(
            x = x,
            y = y
        )
    ) +
    geom_point( shape = '\U1F98C', size = 5) +
    transition_states(states = state)
