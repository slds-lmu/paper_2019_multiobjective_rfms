library(data.table)
library(ggplot2)

dat = as.data.table(readRDS(file = "dtmmcenew.rds"))
dat = dat[bag == "outbag", ]

ns = dat[, .N, by = c("algo", "openbox_name", "lockbox_name", "lrn", "dsna", "repl")]
table(ns$algo, ns$N)

hv = function(x) {
  pf = mco::paretoFilter(as.matrix(x))
  pf = unique(pf)
  mco::dominatedHypervolume(pf, rep(1, 3))
}

dat2 = dat[, list(dhv = hv(.SD)),
  by = c("algo", "openbox_name", "lockbox_name", "lrn", "dsna", "repl"),
  .SDcols = c("curator", "lockbox", "openbox")]

dat2s = split(dat2, dat2$lrn)

pdf(file = "hypervolumes.pdf", height = 7, width = 10)
lapply(dat2s, function(d) {
  ggplot(data = d, mapping = aes(y = dhv, x = algo, color = algo)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
    xlab("Algorithm") + ylab("Dominated hyper volume") +
    ggtitle(paste("Rows: openbox, columns: lockbox, learner:", unique(d$lrn))) +
    facet_grid(openbox_name ~ lockbox_name)
})
dev.off()

dat3 = dat2[, list(mdhv = mean(dhv)),
  by = c("algo", "openbox_name", "lockbox_name", "lrn", "dsna")]

pdf(file = "mean_hypervolumes.pdf", height = 7, width = 10)
ggplot(data = dat3, mapping = aes(y = mdhv, x = algo, color = algo)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  xlab("Algorithm") + ylab("Mean dominated hyper volume") +
  facet_wrap("lrn")
dev.off()


# Number of wins and losses
fr = function(x, algo) {
  design = expand.grid(i = seq_along(x), j = seq_along(x))
  design = design[design[, 1] != design[, 2], ]
  wins = apply(design, 1, function(a) {
    if (x[a[1]] > x[a[2]]) 1
    else if (x[a[1]] == x[a[2]]) 0.5
    else 0
  })
  data.table(algo1 = algo[design[, 1]], algo2 = algo[design[, 2]], wins = wins)
}
dat4 = dat2[, fr(dhv, algo), by = c("openbox_name", "lockbox_name", "lrn", "dsna", "repl")]

dat4a = dat4[, list(wins = sum(wins)), by = c("algo1", "algo2")]
n.exp = nrow(dat[, .N, c("openbox_name", "lockbox_name", "lrn", "dsna", "repl")])

pdf("wins_and_losses.pdf", width = 8, height = 6)
ggplot(data = dat4a, mapping = aes(x = algo1, y = algo2)) +
  geom_tile(aes(fill = wins)) +
  geom_text(aes(label = wins, color = abs(wins - n.exp / 2) >= 125), size = 5) +
  scale_color_manual(guide = FALSE, values = c("black", "white")) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", name = "Times",
    midpoint = n.exp / 2) +
  xlab("Winner") + ylab("Loser")
dev.off()

