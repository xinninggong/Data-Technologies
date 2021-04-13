library(tidyverse)
#Q1
nycbikes18_raw <- read_csv("data/2018-citibike-tripdata.csv")

#Q2
p1 <- ggplot(data = nycbikes18_raw) +
  geom_bar(
    aes(x = birth_year, fill = usertype),
    colour = "white"
  )

             
#Q3 
nycbikes18 <- nycbikes18_raw[nycbikes18_raw$birth_year > 1900, c(1:15)]


#Q4 
trip_duration <- nycbikes18[1]
ttl_tripd <- sum(trip_duration)


#Q5 
bike_id <- nycbikes18$bikeid
n_bikes <- length(unique(bike_id))


#Q6 
p2 <- ggplot(data = nycbikes18) + 
  geom_bar(
    aes(x=usertype, fill = tripduration)
  )


#Q7 
p3 <- ggplot(data = nycbikes18) + 
  geom_bar(
    aes(x=gender, fill = usertype),
    position = "dodge"
  ) 


#Q8 

p4 <- ggplot(data = nycbikes18, aes(birth_year, tripduration)) + 
  geom_point(size = 0.5) + 
  facet_grid(rows = vars(usertype), cols = vars(gender))


#Q9
p5 <- ggplot(data = nycbikes18, aes(start_station_longitude, start_station_latitude)) +
  geom_point() + 
  geom_point(aes(end_station_longitude, end_station_latitude))


#Q10

p6 <- ggplot(data = nycbikes18, aes(x = start_station_longitude, y = start_station_latitude,
                                    xend = end_station_longitude, yend = end_station_latitude)) + 
  geom_segment(arrow = arrow(length = unit(0.01, "npc")),
               alpha = 0.3)


