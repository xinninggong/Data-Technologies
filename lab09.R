library(rvest)
library(tidyverse)
link <- "https://www.imdb.com/search/title/?title_type=feature&num_votes=25000,&genres=horror&sort=user_rating,desc&view=simple&sort=user_rating"
horror <- read_html(link)
horror


## Q1 
film_poster <- horror %>% 
  html_elements(".loadlate") 

film_poster <-  film_poster %>%
  html_attr("loadlate")

## Q2 

movie <- horror %>% 
  html_elements(".lister-item-header")

movie <- movie %>% 
  html_elements("a") %>% 
  html_text2()

## Q3 
year <- horror %>% 
  html_elements(".lister-item-year") %>%
  html_text2()

year <- parse_number(year)

## Q4 
rating <- horror %>% 
  html_elements(".col-imdb-rating") %>% 
  html_text2() 

rating <- parse_number(rating)

## Q5 

rank <- horror %>% 
  html_elements(".lister-item-index") %>%
  html_text2()

rank <- parse_number(rank)

top50_horror <- tibble(rank, film_poster, movie, year, rating) %>%
  mutate(rank = as.integer(rank)) %>% 
  rename(Rank = rank, 
         Poster = film_poster, 
         Movie = movie, 
         Year = year,
         Rating = rating)
