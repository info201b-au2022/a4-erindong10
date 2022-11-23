library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)
View(incarceration_trends)

# Question 1: How much has the average Latinx jail population changed from 2008 to 2018 across all counties?
latinx_pop_2008 <- incarceration_trends %>%
  filter(year == "2008", na.rm = TRUE) %>%
  select(latinx_jail_pop)
mean_latinx_2008 <- mean(latinx_pop_2008[,"latinx_jail_pop"], na.rm = TRUE)

latinx_pop_2018 <- incarceration_trends %>%
  filter(year == "2018", na.rm =  TRUE) %>%
  select(latinx_jail_pop)
mean_latinx_2018 <- mean(latinx_pop_2018[,"latinx_jail_pop"], na.rm = TRUE)

latinx_pop_diff <- mean_latinx_2018 - mean_latinx_2008

# Question 2: Which county has the highest proportion of Latinx inmates in 2008 and 2018?
incarceration_trends <- incarceration_trends %>% 
  mutate(latinx_ratio = latinx_jail_pop/total_pop)

highest_latinx_ratio_2008 <- incarceration_trends %>%
  filter(year == "2008", na.rm = TRUE) %>%
  filter(latinx_ratio == max(latinx_ratio, na.rm = TRUE)) %>%
  pull(county_name)

highest_latinx_ratio_2018 <- incarceration_trends %>%
  filter(year == "2018", na.rm = TRUE) %>%
  filter(latinx_ratio == max(latinx_ratio, na.rm = TRUE)) %>%
  pull(county_name)

# Question 3: How do these past values compare to total_jail_from_ice (using total_jail_from_ice/total_pop)? <- rephrase this mf 
jail_from_ice_2008 <- incarceration_trends %>%
  filter(year == "2008", na.rm = TRUE) %>%
  select(total_jail_from_ice)
mean_jail_from_ice_2008 <- mean(jail_from_ice_2008[,"total_jail_from_ice"], na.rm = TRUE)

jail_from_ice_2018 <- incarceration_trends %>%
  filter(year == "2018", na.rm = TRUE) %>%
  select(total_jail_from_ice)
mean_jail_from_ice_2018 <- mean(jail_from_ice_2018[,"total_jail_from_ice"], na.rm = TRUE)

incarceration_trends <- incarceration_trends %>% 
  mutate(ice_ratio = total_jail_from_ice/total_pop)

highest_ice_ratio_2008 <- incarceration_trends %>%
  filter(year == "2008", na.rm = TRUE) %>%
  filter(ice_ratio == max(ice_ratio, na.rm = TRUE)) %>%
  pull(county_name)

highest_ice_ratio_2018 <- incarceration_trends %>%
  filter(year == "2018", na.rm = TRUE) %>%
  filter(ice_ratio == max(ice_ratio, na.rm = TRUE)) %>%
  pull(county_name)
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
    test_df <- incarceration_trends %>%
      group_by(year) %>%
      summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(test_df)
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  plot_df <- get_year_jail_pop()
  final_plot <- ggplot(data = plot_df, aes(x = year, y = total_jail_pop)) + 
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Increase of Jail Population in U.S. (1970-2018)", 
         x = "Year", 
         y = "Total Jail Pop")
  return(final_plot)   
} 

plot_jail_pop_for_us()


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

get_jail_pop_by_states <- function(states) {
  state_df <- incarceration_trends %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE))
  return(state_df)
}

plot_jail_pop_by_states <- function(states) {
  plot_df_2 <- get_jail_pop_by_states(states)
  final_plot_2 <- ggplot(data = plot_df_2, aes(x = year, y = total_jail_pop, color = state)) +
    geom_line() +
    labs(title = "Jail Population by States",
         x = "Year",
         y = "Total Jail Pop")
  return(final_plot_2)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


