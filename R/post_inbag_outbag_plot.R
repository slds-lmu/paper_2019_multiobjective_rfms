library(data.table)
dt = readRDS("geo_ladder.rds")

dtoutbag = dt[bag == "outbag"]
dtoutbag = dt[bag == "inbag"]
function(dtoutbag) {

dtoutbag$openbox = apply(as.data.frame(dtoutbag), 1, FUN = function(x) {
  x[x["openbox_name"]]})

dtoutbag$lockbox = apply(as.data.frame(dtoutbag), 1, FUN = function(x) {
  x[x["lockbox_name"]]})


library(tidyr)

dttemp = dtoutbag[, .(openbox_name, lockbox_name, openbox, lockbox, algo, lrn)]

library(ggplot2)
dttemp$openbox = as.numeric(dttemp$openbox)
dttemp$lockbox = as.numeric(dttemp$lockbox)

ggplot2::ggplot(dttemp, aes(x = algo, y = lockbox)) + geom_boxplot() + facet_grid(cols = vars(lrn))

ggplot2::ggplot(dttemp, aes(x = algo, y = openbox)) + geom_boxplot() + facet_grid(cols = vars(lrn))
}
