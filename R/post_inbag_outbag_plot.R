library(data.table)
library(ggplot2)
library(tidyr)
dt = readRDS("dt_res_oml_jan29.rds")

dtl = tidyr::gather(dt, key = box, value = mmce, openbox, lockbox, curator)

fig = ggplot2::ggplot(dtl, aes(x = algo, y = mmce, color = bag)) + geom_boxplot() + facet_grid(rows = vars(lrn), cols = vars(box)) + ggtitle("openbox")
ggsave("lb.pdf", plot = fig)
