library(data.table)
library(ggplot2)
library(tidyr)
dt = readRDS("resultJan25.rds")

dtoutbag = dt[bag == "outbag"]
dtinbag = dt[bag == "inbag"]


showp = function(dtg) {

dtg$openbox = apply(as.data.frame(dtg), 1, FUN = function(x) {
  as.numeric(x[x["openbox_name"]])})

dtg$lockbox = apply(as.data.frame(dtg), 1, FUN = function(x) {
  as.numeric(x[x["lockbox_name"]])})

dtg = dtg[, .(openbox_name, lockbox_name, openbox, lockbox, algo, lrn)]


ggplot2::ggplot(dtg, aes(x = algo, y = openbox)) + geom_boxplot() + facet_grid(cols = vars(lrn))
}

showp(dtoutbag)
dev.new()
showp(dtinbag)
