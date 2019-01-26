library(data.table)
library(ggplot2)
library(tidyr)
dt = readRDS("dt_geo_mmce.rds")

dtoutbag = dt[bag == "outbag"]
dtinbag = dt[bag == "inbag"]


tran = function(dtg) {

dtg$openbox = apply(as.data.frame(dtg), 1, FUN = function(x) {
  as.numeric(x[x["openbox_name"]])})

dtg$lockbox = apply(as.data.frame(dtg), 1, FUN = function(x) {
  as.numeric(x[x["lockbox_name"]])})

dtg = dtg[, .(openbox_name, lockbox_name, openbox, lockbox, algo, lrn)]


}

dtoutbag = tran(dtoutbag)
dtinbag = tran(dtinbag)

ggplot2::ggplot(dtg, aes(x = algo, y = openbox)) + geom_boxplot() + facet_grid(cols = vars(lrn))
dev.new()
showp(dtinbag)

ggplot2::ggplot(dtg, aes(x = algo, y = openbox)) + geom_boxplot() + facet_grid(cols = vars(lrn))

which(dtinbag$openbox > 0.8)
loadResult(430)
