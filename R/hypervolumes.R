library(data.table)
library(ggplot2)

dat = as.data.table(readRDS(file = "geo4flor.rds"))

hv = function(x) {
  pf = mco::paretoFilter(as.matrix(x))
  pf = unique(pf)
  mco::dominatedHypervolume(pf, rep(1, 3))
}

dat2 = dat[, list(dhv = hv(.SD)),
  by = c("algo", "openbox_name", "lockbox_name", "lrn", "repl"),
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
  by = c("algo", "openbox_name", "lockbox_name", "lrn")]

pdf(file = "mean_hypervolumes.pdf", height = 7, width = 10)
ggplot(data = dat3, mapping = aes(y = mdhv, x = algo, color = algo)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
  xlab("Algorithm") + ylab("Mean dominated hyper volume") +
  facet_wrap("lrn")
dev.off()


