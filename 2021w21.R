library("tidyverse")
library("ggthemes")
library("patchwork")

survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

# Copied and pasted from the survey
us_states <-
    c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
      "Connecticut", "Delaware", "District of Columbia", "Florida",
      "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas",
      "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts",
      "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana",
      "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico",
      "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma",
      "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
      "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
      "Washington", "West Virginia", "Wisconsin", "Wyoming")


industry_filter <- "Computing or Tech"
max_y <- 300000 # for fine tuning the graphs


# Filter data
survey_us <- survey %>% 
    filter(state %in% us_states & currency == "USD") %>%
    filter(industry == industry_filter)


# Annual salary by age
fig_salary_vs_age <- survey_us %>%
    ggplot(
        aes(
            x = how_old_are_you,
            y = annual_salary
        )
    ) +
    geom_boxplot(
        outlier.shape = NA
    ) +
    scale_y_continuous(
        breaks=c(100000, 200000, 300000, 400000),
        labels=c("100K", "200K", "300K", "400K"),
        limits = c(0, max_y)
    ) +
    scale_x_discrete(
        limits = c("18-24",
                   "25-34",
                   "35-44",
                   "45-54",
                   "55-64",
                   "65 or over"
        ),
        labels = c("18-24" = "18 - 24",
                   "25-34" = "25 - 34",
                   "35-44" = "35 - 44",
                   "45-54" = "45 - 54",
                   "55-64" = "55 - 64",
                   "65 or over" = "65 or over"
        )
    ) +
    labs(
        title = "",
        subtitle = "Age",
        x = "",
        y = ""
    ) +
    theme_economist() + 
    scale_colour_economist() +
    theme(
        axis.text.x = element_text(angle = 30, vjust = 0.6)
    )


# Annual salary by Education
fig_salary_vs_education <- 
    survey_us %>% 
    drop_na(highest_level_of_education_completed) %>%
    ggplot(
        aes(
            x = highest_level_of_education_completed,
            y = annual_salary
        )
    ) +
    geom_boxplot(
        outlier.shape = NA
    ) +
    scale_y_continuous(
        breaks=c(100000, 200000, 300000, 400000),
        labels=c("100K", "200K", "300K", "400K"),
        limits = c(0, max_y)
    ) +
    scale_x_discrete(
        limits = c("High School",
                   "Some college",
                   "College degree",
                   "Master's degree",
                   "PhD",
                   "Professional degree (MD, JD, etc.)"
        ),
        labels = c("High School" = "High School",
                   "Some college" = "Some college",
                   "College degree" = "College deg.",
                   "Master's degree" = "Master's deg.",
                   "PhD" = "PhD",
                   "Professional degree (MD, JD, etc.)" = "Professional deg."
        )
    )  +
    labs(
        subtitle = "Education",
        x = "",
        y = ""
    ) +
    theme_economist() + 
    scale_colour_economist() +
    theme(
        axis.text.y=element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 0.6)
    )


# Annual salary by in field experience
fig_salary_vs_experience <- survey_us %>%
    ggplot(
        aes(
            x = years_of_experience_in_field,
            y = annual_salary
        )
    ) +
    geom_boxplot(
        outlier.shape = NA
    ) +
    scale_y_continuous(
        breaks=c(100000, 200000, 300000, 400000),
        labels=c("100K", "200K", "300K", "400K"),
        limits = c(0, max_y)
    ) +
    scale_x_discrete(
        limits = c("1 year or less",
                   "2 - 4 years",
                   "5-7 years",
                   "8 - 10 years",
                   "11 - 20 years",
                   "21 - 30 years",
                   "31 - 40 years",
                   "41 years or more"
        ),
        labels = c("1 year or less" = "1 or less",
                   "2 - 4 years" = "2 - 4",
                   "5-7 years" = "5 - 7",
                   "8 - 10 years" = "8 - 10",
                   "11 - 20 years" = "11 - 20",
                   "21 - 30 years" = "21 - 30",
                   "31 - 40 years" = "31 - 40",
                   "41 years or more" = "41 or more"
        )
    )  +
    labs(
        subtitle = "Years of experience in field",
        x = "",
        y = ""
    ) +
    theme_economist() + 
    scale_colour_economist() +
    theme(
        axis.text.y=element_blank(),
        axis.text.x = element_text(angle = 30, vjust = 0.6)
    )

# Patchwork
pw <- (fig_salary_vs_age | fig_salary_vs_education | fig_salary_vs_experience) +
    plot_annotation(
        title = paste('US salary differences in', industry_filter, "industry by ..."),
        #        subtitle = "In USD",
        caption = 'Ivo Ruaro - 2021-05-25 | #TinyTuesday 2021w21 | Ask a manger survey',
        theme = theme_economist()
    )

pw

ggsave(
    "TidyTuesday2020w21.jpg",
    dpi = 150,
    units = "cm",
    width = 35,
    height = 20
)



