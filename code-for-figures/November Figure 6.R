library(ggplot2)
library(doSNOW)

there.exists <- function(N, k, t){
  registerDoSNOW(makeCluster(4, type = "SOCK"))
  
  top_k <- rep(0, times = length(N))
  
  foreach(i = 1:length(N)) %:% foreach(j = 1:1000) %dopar% {
    DS_random <- rbeta(N[i] - t, shape1 = 0.03611, shape2 = 8.032)
    SS_random <- rbeta(t, shape1 = 18.02, shape2 = 31.78)
    DS_ordered <- sort(DS_random, decreasing = TRUE)
    
    if(DS_ordered[k] < max(SS_random)){
      top_k[i] <- top_k[i] + 1
    }
    
  }
  return(top_k/1000)
}

for.all <- function(N, k, t){
  registerDoSNOW(makeCluster(4, type = "SOCK"))
  
  top_k <- rep(0, times = length(N))
  
  foreach(i = 1:length(N)) %:% foreach(j = 1:1000) %dopar% {
    DS_random <- rbeta(N[i] - t, shape1 = 0.03611, shape2 = 8.032)
    SS_random <- rbeta(t, shape1 = 18.02, shape2 = 31.78)
    DS_ordered <- sort(DS_random, decreasing = TRUE)
    
    if(DS_ordered[k] < min(SS_random)){
      top_k[i] <- top_k[i] + 1
    }
    
  }
  return(top_k/1000)
}


plotprob <- function(title, f, ...){

  k1 <- f(k = 1, ...)
  k10 <- f(k = 10, ...)
  k20 <- f(k = 20, ...)
  k50 <- f(k = 50, ...)
  k100 <- f(k = 100, ...)

  densities_data <- data.frame(
    x = search_sizes,
    k1 = k1,
    k10 = k10,
    k20 = k20,
    k50 = k50,
    k100 = k100
  )

  plot <-
    ggplot(data = densities_data, aes(x = x)) +
    geom_line(aes(y = k1, color ="k = 1"), alpha = 1) +
    geom_line(aes(y = k10, color ="k = 10"), alpha = 1) +
    geom_line(aes(y = k20, color ="k = 20"), alpha = 1) +
    geom_line(aes(y = k50, color ="k = 50"), alpha = 1) +
    geom_line(aes(y = k100, color ="k = 100"), alpha = 1) +
    scale_color_manual("Returned Results", values=c("k = 1" = "blue", "k = 10" = "purple", "k = 20" = "orange", "k = 50" = "red", "k = 100" = "green"),
                       breaks = c("Different Source", "Same Source", "k = 1", "k = 10",
                                  "k = 20", "k = 50", "k = 100")) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    xlab("Search Size") +
    ylab("Probability SS in Top K") +
    labs(title = title) +
    theme(panel.background = element_blank(), axis.line = element_line(), legend.position = "right")
  
  return(plot)

}

search_sizes <- seq(500, 100000, length.out = 50)

plot1 <- plotprob(title = "Probability At Least One True Match in Top K (t = 2)", f = there.exists, N = search_sizes, t = 2)
plot2 <- plotprob(title = "Probability At Least One True Match in Top K (t = 5)", f = there.exists, N = search_sizes, t = 5)
plot3 <- plotprob(title = "Probability At Least One True Match in Top K (t = 10)", f = there.exists, N = search_sizes, t = 10)
plot4 <- plotprob(title = "Probability All True Matches in Top K (t = 2)", f = for.all, N = search_sizes, t = 2)
plot5 <- plotprob(title = "Probability All True Matches in Top K (t = 5)", f = for.all, N = search_sizes, t = 5)
plot6 <- plotprob(title = "Probability All True Matches in Top K (t = 10)", f = for.all, N = search_sizes, t = 10)


library(gridExtra)
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, nrow = 3, ncol = 2)