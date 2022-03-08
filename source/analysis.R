library("dplyr")
library("ggplot2")
library("tidyverse")
library(maps)
library("openintro")
library("mapproj")


setwd("~/Documents/_Code/a3-Qiuqing-Ge/source")

jail_jurisdiction <-
  read.csv("../data/incarceration_trends_jail_jurisdiction.csv")

incarceration_trends <- read.csv("../data/incarceration_trends.csv")

# Make a data frame to explore the rate of color of people in jail and the rate
# of white people in jail
time_trends_df <- incarceration_trends %>%
  group_by(year) %>%
  select(year, aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64,
         native_pop_15to64, white_pop_15to64, aapi_jail_pop, black_jail_pop,
         latinx_jail_pop, native_jail_pop, white_jail_pop) %>%
  summarize(total_aapi_pop_15to64 = sum(aapi_pop_15to64, na.rm = TRUE),
            total_black_pop_15to64 = sum(black_pop_15to64, na.rm = TRUE),
            total_latinx_pop_15to64 = sum(latinx_pop_15to64, na.rm = TRUE),
            total_native_pop_15to64 = sum(native_pop_15to64, na.rm = TRUE),
            total_white_pop_15to64 = sum(white_pop_15to64, na.rm = TRUE),
            total_aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE),
            total_black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
            total_latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE),
            total_native_jail_pop = sum(native_jail_pop, na.rm = TRUE),
            total_white_jail_pop = sum(white_jail_pop, na.rm = TRUE)) %>%
  mutate(total_color_pop_15to64 = total_aapi_pop_15to64 + total_black_pop_15to64
         + total_latinx_pop_15to64 + total_native_pop_15to64,
         total_color_jail_pop = total_aapi_jail_pop + total_black_jail_pop
         + total_latinx_jail_pop + total_native_jail_pop) %>%
  select(year, total_color_pop_15to64, total_white_pop_15to64,
         total_color_jail_pop, total_white_jail_pop) %>%
  mutate(color_rate_jail =
           round(total_color_jail_pop / total_color_pop_15to64, 4) * 100) %>%
  mutate(white_rate_jail =
           round(total_white_jail_pop / total_white_pop_15to64, 4) * 100) %>%
  filter_all(all_vars(is.finite(.)))

# select eight years to represents other data
time_trends_df_p <- time_trends_df %>%
  filter(year == "1990" | year == "1994" | year == "1998" | year == "2002"
         | year == "2006" | year == "2010" | year == "2014" | year == "2018")

#Draw the graph about the rate of people of color in jail changed by year
time_trends_color_rate <- ggplot(data = time_trends_df_p) +
  geom_col(mapping = aes(x = year, y = color_rate_jail, fill = year))  +
  scale_fill_continuous(low = "#B0C4DE", high = "#6E7B8B") +
  labs(
    title = "The Rate of Color of People in Jail",
    subtitle = "From 1990-2018(extract 8 years)",
    x = "year",
    y = "rate of color of people in jail",
  )

#Draw the graph about the rate of white people in jail changed by year
time_trends_white_rate <- ggplot(data = time_trends_df_p) +
  geom_col(mapping = aes(x = year, y = total_white_jail_pop, fill = year))  +
  scale_fill_continuous(low = "#87CEFA", high = "#607B8B") +
  labs(
    title = "The Rate of White People in Jail",
    subtitle = "From 1990-2018(extract 8 years)",
    x = "year",
    y = "rate of white people in jail",
  )

# This is graph to show two variable's relationships between "the rate of people
# of color in jail" and "the rate of white people in jail".
p_chart <- ggplot() +
  geom_line(data = time_trends_df, aes(x = year, y = color_rate_jail,
                    colour = "the rate of people of color in jail"), size = 1) +
  geom_line(data = time_trends_df, aes(x = year, y = white_rate_jail,
                    colour = "the rate of white people in jail"), size = 1) +
  scale_colour_manual("", values = c("the rate of people of color in jail" =
                                      "red",
                        "the rate of white people in jail" = "orange")) +
  xlab("Year") + ylab("rate") +
  theme(text = element_text(size = 13, family = "Comic Sans MS")) +
  ggtitle("The relationsip between color of people and white people in jail")

# This is the scatter plot to find the relationship between the rate of people
# of color in jail and the rate of white people in jail
p_relationship <- ggplot(time_trends_df, aes(x = color_rate_jail,
                                             y = white_rate_jail)) +
  geom_point(colour = 4, size = 3) +
  labs(x = "people of color rate in the jail",
       y = "white people in the jail",
       title = "The relationsip between two rates",
       subtitle = "people of color rate in jail and white poeple in jail")

# Subtract a new data frame about the relationship between state and
# "the rate of people of color"
state_df <- incarceration_trends %>%
  group_by(state) %>%
  select(state, aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64,
         native_pop_15to64, white_pop_15to64, aapi_jail_pop, black_jail_pop,
         latinx_jail_pop, native_jail_pop, white_jail_pop) %>%
  summarize(total_aapi_pop_15to64 = sum(aapi_pop_15to64, na.rm = TRUE),
            total_black_pop_15to64 = sum(black_pop_15to64, na.rm = TRUE),
            total_latinx_pop_15to64 = sum(latinx_pop_15to64, na.rm = TRUE),
            total_native_pop_15to64 = sum(native_pop_15to64, na.rm = TRUE),
            total_white_pop_15to64 = sum(white_pop_15to64, na.rm = TRUE),
            total_aapi_jail_pop = sum(aapi_jail_pop, na.rm = TRUE),
            total_black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
            total_latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE),
            total_native_jail_pop = sum(native_jail_pop, na.rm = TRUE),
            total_white_jail_pop = sum(white_jail_pop, na.rm = TRUE)) %>%
  mutate(total_color_pop_15to64 = total_aapi_pop_15to64 + total_black_pop_15to64
         + total_latinx_pop_15to64 + total_native_pop_15to64,
         total_color_jail_pop = total_aapi_jail_pop + total_black_jail_pop
         + total_latinx_jail_pop + total_native_jail_pop)

# Make the data frame for top 10 states in U.S.
state_df_plot_top_10 <- state_df %>%
  select(state, total_color_pop_15to64, total_white_pop_15to64,
         total_color_jail_pop, total_white_jail_pop) %>%
  mutate(color_rate_jail =
           round(total_color_jail_pop / total_color_pop_15to64, 4) * 100) %>%
  mutate(white_rate_jail =
           round(total_white_jail_pop / total_white_pop_15to64, 4) * 100) %>%
  mutate(state_full_name = abbr2state(state)) %>%
  select(state_full_name, color_rate_jail, white_rate_jail) %>%
  top_n(10, color_rate_jail)

# Draw a bar chart to show the relationship between the state and "the rate of
# people of color in jail"
state_chart_top_10 <- ggplot(data = state_df_plot_top_10) +
  geom_col(mapping = aes(x = reorder(state_full_name, color_rate_jail),
                         y = color_rate_jail), fill = "steelblue") +
  labs(
    title = "The Rate of people of color in Jail",
    x = "State Name",
    y = "rate of people of color in jail") +
  coord_flip()

# Make a new data frame about the rate of people of color in jail
# spererated by state.
# This map is for making a map about the rate of people of color in jail.
map_rate_df <- state_df %>%
  select(state, total_color_pop_15to64, total_color_jail_pop) %>%
  mutate(color_rate_jail =
           round(total_color_jail_pop / total_color_pop_15to64, 4))

# Find the data frame which the foundation to make the map
state_shape <- map_data("state") %>%
  mutate(state = state2abbr(region)) %>%
  left_join(map_rate_df, by = "state")

# Make the map be outputted nicely without any weird axes or other such
# "graph-things" in the way
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  )

# Make the map about the rate of people of color in jail(the people of color
# only including AAPI, Black, Latinx, Native American race)
p_map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = color_rate_jail),
    color = "white",
    size = .1
  ) +
  coord_map() +
  scale_fill_continuous(low = "Orange", high = "Red") +
  labs(fill = "Rate",
       titles = "The Rate of Color People in Jail",
       subtitles = "only including AAPI, Black, Latinx, Native American race"
  ) +
  blank_theme

# These values are written in the Rmd.xws
max_color_rate_year <- time_trends_df %>%
  filter(color_rate_jail == max(color_rate_jail, na.rm = TRUE)) %>%
  pull(year)

max_color_rate <- time_trends_df %>%
  filter(color_rate_jail == max(color_rate_jail, na.rm = TRUE)) %>%
  pull(color_rate_jail)

min_year_white_rate <- time_trends_df %>%
  filter(white_rate_jail == min(white_rate_jail, na.rm = TRUE)) %>%
  pull(year)

early_year <- time_trends_df %>%
  filter(year == min(year)) %>%
  pull(year)

recent_year <- time_trends_df %>%
  filter(year == max(year)) %>%
  pull(year)
