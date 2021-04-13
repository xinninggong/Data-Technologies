library(tidyverse)
villagers <- read_csv("data/animal-crossing/villagers.csv")
villagers

p1 <- ggplot(data = villagers) + 
  geom_bar(
    aes(x = personality)
  )
  
  
p2 <- ggplot(data = villagers) + 
  geom_bar(
    aes(x = personality, fill = gender)
  )

p3 <- ggplot(data = villagers) + 
  geom_bar(
    aes(x = personality, fill = gender), 
    colour = "black"
  )


p4 <- p3 + coord_polar()

p5 <- p4 + facet_wrap(vars(species), ncol  = 6)
