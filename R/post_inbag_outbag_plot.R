library(data.table)
library(ggplot2)
library(tidyr)
dt = readRDS("dt_res_oml_jan29.rds")
dt = readRDS("dt_lambdaJan31.rds")

dtl = tidyr::gather(dt, key = box, value = mmce, openbox, lockbox, curator)

fig = ggplot2::ggplot(dtl, aes(x = algo, y = mmce, color = bag)) + geom_boxplot() + facet_grid(rows = vars(lrn), cols = vars(box)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("comparison of mmce across learner and data site on geo dataset")
ggsave("geo_box.pdf", plot = fig)
