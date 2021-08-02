library(ggrepel)
library(tidytext)
library(tidyverse)
rm_words <- c("animal", "crossing", "horizons", "game", "nintendo", 
              "switch", "series", "island")
critic <- read_tsv("data/animal-crossing/critic.tsv")
critic

## Q1 
critic_tokens <- critic %>% 
  mutate(text = str_remove(text, "Expand$")) %>%
  unnest_tokens(output = word, input = text)

## Q2 
stopwords_smart <- get_stopwords(source = "smart")
critic_smart <- critic_tokens %>% 
  anti_join(stopwords_smart)


## Q3 

p1 <- critic_smart %>% 
  filter(!(word %in% rm_words)) %>% 
  count(word) %>%
  slice_max(n, n = 20) %>% 
  ggplot(aes(x = n, y = fct_reorder(word, n))) + 
  geom_col() + 
  labs(x = "Frequency of words", 
       y = "")

## Q4 
sentiments_bing <- get_sentiments("bing")
critic_sentiments <- critic_smart %>%
  inner_join(sentiments_bing) %>% 
  count(sentiment, word, sort = TRUE) %>%
  arrange(n)

## Q5 
dist_sentiment = unique(critic_sentiments$sentiment)
p2 <- critic_sentiments %>% 
  mutate(count = n) %>%
  ggplot(aes(
         colour = sentiment, 
         x = 0, 
         y = 0, size = factor(n)))+ 
  geom_text_repel(aes(x = 0, y= 0,label = word),
                  force_pull = 0, 
                  max.overlaps = Inf, 
                  segment.color = NA, 
                  seed = 220) + 
  facet_wrap(~factor(sentiment, levels = rev(dist_sentiment))) +
  scale_colour_manual(values = c("#4d9221", "#c51b7d")) + 
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none") + 
  labs(x = "", y = "")
                  

  


