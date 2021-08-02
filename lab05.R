library(lubridate)
library(tidyverse)
step_count_raw <- read_csv("data/step-count/step-count.csv",
                           locale = locale(tz = "Australia/Melbourne"))
location <- read_csv("data/step-count/location.csv")
step_count <- step_count_raw %>% 
  rename_with(~ c("date_time", "date", "count")) %>% 
  left_join(location) %>% 
  mutate(location = replace_na(location, "Melbourne"))
step_count

## Question 1 

date_location_steps <- step_count %>%
  group_by(location, date) %>%
  summarise(
    count = sum(count)
  )

city_avg_steps <- date_location_steps %>%
  group_by(location) %>%
  summarise(
    avg_count = mean(count)
  )

## Question 2 

p1 <- city_avg_steps %>%
  mutate(location = fct_reorder(location, avg_count)) %>%
  ggplot(aes(location, avg_count)) + 
  geom_segment(aes(xend = location, y = 0, yend = avg_count)) +
  geom_point(size = 4, colour = "#dd1c77") 



## Question 3 
step_count_time <- step_count %>%
  mutate(
    time = as_factor(hour(date_time)),
    country = case_when(
      location == "Melbourne" ~ "AU", 
      TRUE ~ "US"
    )
  )

## Question 4 
p2 <- step_count_time %>%
  ggplot(aes(time, count)) + 
  geom_boxplot(outlier.size = 1)


## Question 5 

p3 <- p2 +
  facet_grid(rows = vars(country))
