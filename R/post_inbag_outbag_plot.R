library(data.table)
library(ggplot2)
library(tidyr)
dt = readRDS("dtmmcenew.rds")

fig_ob = ggplot2::ggplot(dt, aes(x = algo, y = openbox, fill = bag)) + geom_boxplot() + facet_grid(cols = vars(lrn)) + ggtitle("openbox")

ggsave("ob.pdf", plot = fig_ob)


fig_cu = ggplot2::ggplot(dt, aes(x = algo, y = curator, fill = bag)) + geom_boxplot() + facet_grid(cols = vars(lrn)) + ggtitle("curator")
ggsave("cu.pdf", plot = fig_cu)

fig_lb = ggplot2::ggplot(dt, aes(x = algo, y = lockbox, fill = bag)) + geom_boxplot() + facet_grid(cols = vars(lrn)) + ggtitle("lockbox")

ggsave("lb.pdf", plot = fig_lb)
