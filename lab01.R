x <- seq(from = 200, to = 400, by = 2)

remainder <- x%%3

x2 <- x[which(remainder == 0)]

n_x2 <- length(x2)

sd_x2 <- sd(x2) * 2


mean_x2 <- mean(x2)
rng_x <- c(mean_x2 - sd_x2, 
           mean_x2 + sd_x2)
