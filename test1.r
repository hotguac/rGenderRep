library(tidyverse)
library(ggplot2)

source(file = "~/LWV/sources.r")

p <- generate_plot("State Senator")
ggsave(paste("LWV/2022_IN_State_Senator.png"), plot = p)

q <- generate_plot("State Representative")
ggsave(paste("LWV/2022_IN_State_Representative.png"), plot = q)


