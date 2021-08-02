library(scales)
library(tidyverse)
selected <- c("Australia", "Brazil", "Canada", "China", "France", "Germany", 
              "India", "Israel", "Italy", "Japan", "Korea, South", "New Zealand",
              "Spain", "Sweden", "United Kingdom", "US")
covid19 <- read_csv("data/covid19-daily-cases.csv") %>% 
  filter(country_region %in% selected)
covid19

## Q1 
covid19_cases <- covid19 %>%
  mutate(country_region = case_when(country_region == "Korea, South" ~ "South Korea", 
                                    TRUE ~ as.character(country_region))
         ) %>% 
  group_by(country_region) %>%
  mutate(new_cases = confirmed - lag(confirmed, n = 7)) %>% 
  ungroup()

## Q2
covid19_bg <- covid19_cases %>%
  rename(country_region_bg = country_region)

## Q3
p1 <- covid19_cases %>%
  ggplot(aes(x = confirmed, y = new_cases, group = country_region)) +
  geom_line(colour = "gray80", size = 0.4) + 
  scale_y_log10(label = label_number_si()) +
  scale_x_log10(label = label_number_si()) 


## Q4
p2 <- p1 +
  geom_line(data = covid19_bg, aes(group = country_region_bg), colour = "firebrick", size = 0.8) +
  facet_wrap(~(fct_reorder(country_region_bg, confirmed, .fun = last, .desc = TRUE)), ncol = 4)
  
## Q5 
p3 <- p2 + 
  labs(x = "Total Confirmed Cases", 
       y = "New Confirmed Cases (in the Past Week)",
       title = "Trajectory of World COVID-19 Confirmed Cases",
       subtitle = "Data as of 2020-05-31",
       caption = "Data: John Hopkins University, CSSE") + 
  theme_minimal() + 
  theme(plot.title = element_text(face = "bold"),
        axis.text = element_text(size = 8))
  

  