library(tidyverse)
library(patchwork)

set.seed(2009)

#DATA AND PARAMETERS TO CHANGE

data <- seq(0, 1, length.out = 1000)

DS_density <- dbeta(data, shape1 = 0.03611447, shape2 = 8.03275)
SS_density <- dbeta(data, shape1 = 18.02, shape2 = 31.78)

DS_random <- rbeta(9999, shape1 = 0.03611447, shape2 = 8.03275)
SS_random <- rbeta(1, shape1 = 18.02, shape2 = 31.78)

all_samples <- c(DS_random, SS_random)
all_samples_ordered <- sort(all_samples, decreasing = TRUE)
top_10 <- all_samples_ordered[1:10]

densities_data <- data.frame(
  x = data,
  ss = SS_density,
  ds = DS_density,
  top10 = top_10
)

densities_data[1,]$ds <- 0


densities_data %>% 
  ggplot(aes(x = x)) + 
  geom_polygon(aes(y = ss, fill="Same Source (SS)"), alpha = 0.8) + 
  geom_polygon(aes(y = ds, fill="Different Source (DS)"), alpha = 0.8) +
  geom_line(aes(x = top10, y = 0, color = "k = 10"), linewidth = 8, alpha = 0.5) +
  scale_fill_manual("Densities", values=c("steelblue", "green")) +
  scale_color_manual("Top K", values = c("black")) + 
  coord_cartesian(ylim = c(0, 10)) +
  xlab("Similarity Scores") +
  ylab("Density") + 
  labs(title = "Densities of Similarity Scores for a Single Comparison") +
  theme(panel.background = element_blank(), axis.line = element_line(), legend.position = "right")

