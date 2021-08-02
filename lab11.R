library(fs)
library(lubridate)
library(tidyverse)
csv_files <- dir_ls("data/aklbus2017", glob = "*.csv")
head(csv_files)

## Q1 
aklbus <- map_dfr(
  dir_ls("data/aklbus2017", glob = "*.csv"), 
  read_csv, 
  .id = "path")


## Q2 
aklbus_time <- aklbus %>% 
  mutate(route = str_sub(route, start = 1, end = 5),
         datetime = str_sub(path, start = 17, end = -5),
         datetime = ymd_hm(datetime, tz = "Pacific/Auckland")) %>% 
  select(datetime, delay, stop.id, stop.sequence, route)

## Q3 
aklbus_ontime <- aklbus_time %>% 
  mutate(ontime = case_when(delay <= 300 & delay >= -300 ~ 1, 
         TRUE ~ 0)) %>% 
  group_by(route) %>% 
  summarise(ontime_prop = mean(ontime)) %>% 
  arrange(ontime_prop)

## Q4 
exclude_routes <- aklbus_ontime %>%
  filter(!ontime_prop %in% c(0,1)) %>%
  select(route)

aklbus_lst <- aklbus_time %>% 
  filter(route %in% exclude_routes$route) %>% 
  group_by(route) %>% 
  group_split()

## Q5

aklbus_size <- map(aklbus_lst, function(.x) c(nrow(.x), ncol(.x)))

         