library(tidyverse)
library(lubridate)
cn <- c("Quarter", "Region", "Holiday", "Visiting", "Business", "Other")
states <- c("New South Wales", "Victoria", "Queensland", "South Australia",
            "Western Australia", "Tasmania", "Northern Territory", "ACT")

##Q1
domestic_trips <- read_csv("data/domestic-trips.csv", skip = 9, col_names = TRUE, 
                           skip_empty_rows = TRUE )
domestic_trips <- domestic_trips %>%
  rename("Quarter" = X1, "Region" = 'Stopover reason', 
         "Visiting" = "Visiting friends and relatives", "Other" = "Other reason") %>%
  select(cn)

domestic_trips <- domestic_trips[-1,] %>%
  slice(1:(n()-1))

##Q2
qtr_full <- domestic_trips %>%
  fill(Quarter, .direction = "down") %>%
  filter(Quarter != "Total")

##Q3 
states_trips <- qtr_full %>%
  mutate(State = case_when(Region %in% states ~ Region, TRUE ~ NA_character_)) %>%
  fill(State, .direction = "up")  %>% 
  filter(!Region %in% states)

##Q4 
tidy_trips <- states_trips %>% 
  pivot_longer(
    cols = Holiday:Other, 
    names_to = "Purpose",
    values_to = "Trips"
  )

##Q5 
qtr_vector <- parse_date(tidy_trips$Quarter, format = "%B quarter %Y")
qtr_trips <-tidy_trips %>% 
  mutate(Quarter = qtr_vector )
  
