library(tidyverse)

user_reviews <- read_tsv("data/animal-crossing/user_reviews.tsv")

user_grade <- user_reviews[["grade"]]

good_grade <- user_grade >= 7
  
user_good_grade <- user_reviews[user_grade >= 7, c(1,2,4)]

gapminder <- readRDS("data/gapminder.rds")

