library(data.table)
library(ggplot2)
library(tidyr)
dt = readRDS("dtmmcenew.rds")

ggplot2::ggplot(dt, aes(x = algo, y = openbox, fill = bag)) + geom_boxplot() + facet_grid(cols = vars(lrn))

ggplot2::ggplot(dt, aes(x = algo, y = curator, fill = bag)) + geom_boxplot() + facet_grid(cols = vars(lrn))

ggplot2::ggplot(dt, aes(x = algo, y = lockbox, fill = bag)) + geom_boxplot() + facet_grid(cols = vars(lrn))
