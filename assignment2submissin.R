# Assignment 2 

library(lubridate)
library(tidyverse)
nycbikes18 <- read_csv("data/2018-citibike-tripdata.csv",
                       locale = locale(tz = "America/New_York"))
nycbikes18


## Q1 

p1 <- nycbikes18 %>%
  ggplot(aes(start_station_longitude, 
             start_station_latitude)) + 
  geom_count(alpha = 0.5) + 
  geom_count(aes(end_station_longitude, end_station_latitude), alpha = 0.5)


## Q2 
order_bike_trips <- nycbikes18 %>%
  count(bikeid) %>%
  arrange(-n)

top_bike_trips <- nycbikes18 %>%
  filter(bikeid == 26288)
  
## Q3 
p2 <- ggplot(data = top_bike_trips, aes(x = start_station_longitude, y = start_station_latitude,
                                    xend = end_station_longitude, yend = end_station_latitude)) + 
  geom_segment(alpha = 0.5)

## Q4

age_list <- c(0, 14, 24, 44, 64, Inf)

nycbikes18_age <- nycbikes18 %>% 
  mutate(tripduration = tripduration/60,
         birth_year = replace(birth_year, birth_year < 1900, NA_real_),
         age = 2018 - birth_year,
         age_group = cut(age, breaks = age_list, include.lowest = TRUE),
         age_group = fct_recode(age_group, "65+" = "(64,Inf]")
         )

## Q5 
p3 <- nycbikes18_age %>%
  ggplot(aes(age_group, tripduration, color = usertype)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  labs(x = "Age Group", y = "Trip in minutes (on log10)")

## Q6 

p4_data <- nycbikes18 %>%
  mutate(month = month(starttime, label = TRUE),
         gender_fct = case_when(gender == 0 ~ "unknown", 
                            gender == 1 ~ "male", 
                            gender == 2 ~ "female"),
         gender_fct = factor(gender_fct)
  ) %>%
  group_by(month, gender_fct) %>%
  count(month, gender_fct, sort = FALSE) %>%
  ungroup() %>%
  rename(gender = gender_fct, ntrips = n)

p4 <- p4_data %>% 
  mutate(gender = fct_reorder(gender, ntrips)) %>% 
  ggplot(aes(fill = month,  x = gender, y = ntrips)) + 
  geom_bar(stat = "identity", position = position_dodge())

## Q7 

p5_data <- nycbikes18_age %>%
  mutate(month = month(starttime, label = TRUE),
         age_group = fct_shift(age_group)
         ) %>% 
  group_by(month, age_group) %>% 
  summarise(qtl_tripd = quantile(tripduration, probs = 0.75))
  
p5 <- p5_data %>% 
  mutate(age_group = fct_reorder(age_group, -qtl_tripd)) %>%
  ggplot(aes(x = month, y = qtl_tripd, group = age_group)) + 
  geom_line(aes(color = age_group)) 
  

## Q8 

trip_quantile <- nycbikes18_age %>% 
  group_by(age_group) %>% 
  mutate(tripd90 = quantile(tripduration, probs=0.9), longtrip = tripduration > tripd90) %>% 
  ungroup() %>% 
  group_by(age_group, usertype) %>% 
  count(longtrip)
 

user_behaviours <- trip_quantile %>% 
  subset(longtrip == TRUE) %>%
  pivot_wider(names_from = usertype, values_from = n) %>%
  select(age_group, Customer, Subscriber) %>% 
  rename(`Age Group` = age_group) %>% 
  ungroup()

## Q9 

get_hourly_ntrips <- nycbikes18 %>% 
  mutate(starttime = floor_date(starttime, unit = "hour"), 
         startdate = date(starttime),
         starthour = hour(starttime), 
         startwday = wday(starttime, label = TRUE, abbr = TRUE, week_start = 1 )) %>% 
  group_by(starttime, usertype, startdate, starthour, startwday) %>% 
  count() %>% 
  rename(ntrips = n) %>%
  ungroup() 

hourly_ntrips <- get_hourly_ntrips %>%
  select(starttime, usertype, ntrips, startdate, starthour, startwday)

## Q10 
p6 <- hourly_ntrips %>%
  ggplot(aes(starthour, ntrips)) + 
  geom_freqpoly(alpha = 0.5, stat = "identity",  colour = "#bdbdbd", aes(group = startdate)) + 
  facet_grid(rows = vars(usertype), cols = vars(startwday), scales = "free")+ 
  theme_bw() + 
  stat_summary(fun = mean, geom = "line", size = 1, aes(color = startwday))

