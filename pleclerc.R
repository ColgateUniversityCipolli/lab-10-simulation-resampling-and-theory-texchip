
############################
##### Task 1 ###############
############################

library(ggplot2)
set.seed(7272)
p <- 0.39

n <- 1004
sim.study <- rbinom(10000, n, p)/n
ggplot(tibble(p.hat = sim.study), aes(p.hat)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black") +
  geom_density(color = "red") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlim(0.32,0.45) +
  ggtitle("Simulation Study, n = 1004")
ggsave("task1_1.png", width = 5, height = 4)
middle.95 <- quantile(sim.study, c(0.025, 0.975))
range.95 <- abs(middle.95[2] - middle.95[1])
(margin.error <- range.95/2)

n <- n*2
sim.study <- rbinom(10000, n, true)/n
ggplot(tibble(p.hat = sim.study), aes(p.hat)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black") +
  geom_density(color = "red") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlim(0.32,0.45) +
  ggtitle("Simulation Study, n = 2008")
ggsave("task1_2.png", width = 5, height = 4)
middle.95 <- quantile(sim.study, c(0.025, 0.975))
range.95 <- abs(middle.95[2] - middle.95[1])
(margin.error <- range.95/2)

############################
##### Task 2 ###############
############################

n <- 1004
df <- tibble(
  ID = 1:n,
  support = sample(c(1, 0, 0), size = n, replace = T, 
                   prob = c(0.39, 0.59, 0.02))
)

R <- 1000
resamples <- tibble(p.hat = numeric(R))

for(i in 1:R){
  curr.resample <- sample(x = df$support,
                          size = nrow(df),
                          replace = T)
  resamples$p.hat[i] <- mean(curr.resample)
}
resamples

ggplot(resamples, aes(p.hat)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, color = "black") +
  geom_density(color = "red") +
  theme_bw() +
  geom_hline(yintercept = 0) +
  xlim(0.32,0.45) +
  ggtitle("Resampling of Gallup Study, n = 1004")
ggsave("task2.png", width = 5, height = 4)
middle.95 <- quantile(resamples$p.hat, c(0.025, 0.975))
range.95 <- abs(middle.95[2] - middle.95[1])
(margin.error <- range.95/2)

############################
##### Task 3 ###############
############################

n.vals <- seq(100,3000,10)
p.vals <- seq(0.01,0.99,0.01)
ranges <- tibble(n = numeric(), p = numeric(), range = numeric())
for(i in 1:length(n.vals)){
  n <- n.vals[i]
  for(p in p.vals){
    sim <- rbinom(10000, n, p)/n
    middle.95 <- quantile(sim, c(0.025, 0.975))
    range.95 <- abs(middle.95[2] - middle.95[1])
    margin.error <- range.95/2
    ranges <- ranges |>
      add_row(n = n, p = p, range = margin.error)
  }
}

ggplot(ranges, aes(x = n, y = p, fill = range)) +
  geom_raster() +
  scale_fill_viridis_c() + 
  theme_bw() +
  ggtitle("Margin of Error as a function of n and p")
ggsave("task3.png", width = 5, height = 4)

############################
##### Task 4 ###############
############################

n.vals <- seq(100,2000,10)
p.vals <- seq(0.01,0.99,0.01)
z <- qnorm(0.975)
ranges <- tibble(n = numeric(), p = numeric(), range = numeric())
for(i in 1:length(n.vals)){
  n <- n.vals[i]
  for(p in p.vals){
    margin.error <- z*sqrt(n*p*(1-p)+(z^2/4))/(n+z^2)
    ranges <- ranges |>
      add_row(n = n, p = p, range = margin.error)
  }
}

ggplot(ranges, aes(x = n, y = p, fill = range)) +
  geom_raster() +
  scale_fill_viridis_c() + 
  theme_bw()+
  ggtitle("Wilson Margin of Error")
ggsave("task4.png", width = 5, height = 4)
