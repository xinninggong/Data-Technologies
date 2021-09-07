library(tidyverse)
step_count_raw <- read_csv("data/step-count/step-count.csv",
                           locale = locale(tz = "Australia/Melbourne"))
location <- read_csv("data/step-count/location.csv")
step_count_raw
location

##Q1 
step_count <- step_count_raw %>%
  rename( 
    date_time = `Date/Time`,
    date = Date, 
    count = `Step Count (count)`)

##Q2
step_count_loc <- step_count %>%
  full_join(location, by = c("date" = "date"))

##Q3 
step_count_full <- step_count_loc %>% 
  replace_na(list(location = "Melbourne"))

##Q4
step_count_daily <- step_count_full %>%
  group_by(date) %>%
  summarise(
    count = sum(count)) %>%
  rename(daily_count = count)

##Q5 
step_count_10000 <- step_count_daily %>%
  filter(daily_count >= 10000) 
              