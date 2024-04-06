library(tidyverse)
library(ggplot2)

source(file = "sources.r")

p <- generate_plot(.year = "2022", .state = "IN", .office = "State Senator")
ggsave(paste("plots/2022_IN_State_Senator.png"), plot = p)

q <- generate_plot(.year = "2022", .state = "IN", .office = "State Representative")
ggsave(paste("plots/2022_IN_State_Representative.png"), plot = q)

r <- generate_plot(.year = "2022", .state = "IN", .office = "US Representative")
ggsave(paste("plots/2022_IN_US_Representative.png"), plot = r)

s <- generate_plot(.year = "2020", .state = "IN", .office = "State Representative")
ggsave(paste("plots/2022_IN_State_Representative.png"), plot = s)

