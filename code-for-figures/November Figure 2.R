library(tidyverse)
library(patchwork)

set.seed(2009)

data <- seq(0, 1, length.out = 1000)

DS_density <- dbeta(data, shape1 = 0.03611447, shape2 = 8.03275)
SS_density <- dbeta(data, shape1 = 18.02, shape2 = 31.78)

DS_prob <- pbeta(data, shape1 = 0.03611447, shape2 = 8.03275, lower.tail = TRUE)


max_n_density <- function(n, t, pdf, cdf) {
  (n-t)*cdf^((n-t) - 1)*pdf
}

plotbetas <- function(ss, ds, title, lim = NA, f, ...) {
  density1 <- f(500, ...)
  density2 <- f(5000, ...)
  density3 <- f(50000, ...)
  density4 <- f(500000, ...)
  
  densities_data <- data.frame(
    x = data,
    ss = ss,
    ds = ds,
    N500 = density1,
    N5000 = density2,
    N50000 = density3,
    N500000 = density4
  )
  
  if(!is.na(lim)){
    densities_data[densities_data > lim] <- 0
  }
  
  plot <-
    ggplot(data = densities_data, aes(x = x)) + 
    geom_polygon(aes(y = ds, fill="Different Source"), alpha = 0.8) +
    geom_polygon(aes(y = ss, fill="Same Source"), alpha = 1) + 
    geom_polygon(aes(y = N500, fill="N = 500"), alpha = 0.5) +
    geom_polygon(aes(y = N5000, fill="N = 5,000"), alpha = 0.5) +
    geom_polygon(aes(y = N50000, fill="N = 50,000"), alpha = 0.5) +
    geom_polygon(aes(y = N500000, fill="N = 500,000"), alpha = 0.5) +
    scale_fill_manual("Densities", values=c("Different Source" = "steelblue", "Same Source" = "green", "N = 500" = "yellow",
                                            "N = 5,000" = "orange", "N = 50,000" = "red", "N = 500,000" = "darkred"),
                      breaks = c("Different Source", "Same Source", "N = 500",
                                 "N = 5,000", "N = 50,000", "N = 500,000")) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) +  
    xlab("Similarity Scores") +
    ylab("Density") + 
    labs(title = title) +
    theme(panel.background = element_blank(), axis.line = element_line(), legend.position = "right")
  if(!is.na(lim)){
    plot <- plot + scale_y_continuous(expand = c(0,0), limits = c(NA, lim))
  }
  return(plot)
  
}

plot1 <- plotbetas(ss = SS_density, ds = DS_density, title = "Maximum DS Densities for Varying Search Sizes (t = 1)",
                  f = max_n_density, t = 1, lim = 12, cdf = DS_prob, pdf = DS_density)
plot2 <- plotbetas(ss = SS_density, ds = DS_density, title = "Maximum DS Densities for Varying Search Sizes (t = 2)",
                   f = max_n_density, t = 2, lim = 12, cdf = DS_prob, pdf = DS_density)
plot3 <- plotbetas(ss = SS_density, ds = DS_density, title = "Maximum DS Densities for Varying Search Sizes (t = 5)",
                   f = max_n_density, t = 5, lim = 12, cdf = DS_prob, pdf = DS_density)
plot4 <- plotbetas(ss = SS_density, ds = DS_density, title = "Maximum DS Densities for Varying Search Sizes (t = 10)",
                   f = max_n_density, t = 10, lim = 12, cdf = DS_prob, pdf = DS_density)

library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, nrow = 2)