bag2sel = "inbag"
filename = sprintf("tsc_%s.rds", bag2sel)
# Plots
# since cluster does not support plot, the following code will be execulated locally.
mkna4plot = function(prefix, strna) {
  paste0(prefix, strna)
}

library(data.table)
library(ggplot2)
tuple = readRDS(file = filename)
prefix = stringi::stri_replace(filename, replacement = "_", regex = ".rds")
res = tuple$res
res2 = tuple$res2
res3 = tuple$res3
# Plot the development over time: each job is on a separate page
pdf(file = mkna4plot(prefix, "mmce_over_time.pdf"), height = 7, width  = 10)
lapply(split(res, res$job.id), function(dat) {
  ggplot(data = dat, mapping = aes(x = iter, y = mmce, group = dataset, color = dataset)) +
    geom_line() + geom_point() +
    facet_wrap("algo") +
    ggtitle(paste0("lrn: ", dat$lrn[1], ", problem: ", dat$problem[1],
      ", openbox_name: ", dat$openbox_name[1], ", lockbox_name: ", dat$lockbox_name[1],
      ", repl: ", dat$repl[1], ", algorithm: ", dat$algorithm[1]))
})
dev.off()

pdf(file = paste0(prefix, "hypervolume_over_time.pdf"), height = 7, width  = 10)
lapply(split(res2, res2$job.id), function(dat) {
  ggplot(data = dat, mapping = aes(x = iter, y = cdhv, group = algo, color = algo)) +
    geom_line() + geom_point() +
    ylab("Currently dominated hypervolume (based on all datasets except openbox)") +
    ggtitle(paste0("lrn: ", dat$lrn[1], ", problem: ", dat$problem[1],
      ", openbox_name: ", dat$openbox_name[1], ", lockbox_name: ", dat$lockbox_name[1],
      ", repl: ", dat$repl[1], ", algorithm: ", dat$algorithm[1]))
})
dev.off()


# Aggregate over repls
pdf(mkna4plot(prefix, "hypervolume_over_time_aggr_quartiles.pdf"), height = 7, width = 10)
lapply(split(res2, paste(res2$lrn, res2$problem, res2$algorithm)), function(dat) {
  ggplot(data = dat, mapping = aes(x = iter, y = cdhv, group = algo, color = algo)) +
    stat_summary(geom = "ribbon",
      fun.ymin = function(x) quantile(x, 0.25, type = 2),
      fun.ymax = function(x) quantile(x, 0.75, type = 2),
      fun.y = function(x) median(x),
      aes(fill = algo), alpha = 0.3) +
    ylab("Quartiles of currently dominated hypervolume (based on all datasets except openbox)") +
    facet_grid(openbox_name ~ lockbox_name, scales = "free_y") +
    ggtitle(paste0("lrn: ", dat$lrn[1], ", problem: ", dat$problem[1],
      ", algorithm: ", dat$algorithm[1]))
})
dev.off()

res2m = res2[, list(
  mean.cdhv = mean(cdhv),
  median.cdhv = median(cdhv),
  lower = quantile(cdhv, 0.25, type = 2),
  upper = quantile(cdhv, 0.75, type = 2)), by = c("lrn", "problem", "algorithm", "openbox_name", "lockbox_name", "iter", "algo")]
res2m$lower[res2m$iter %% 5 != 0] = NA
res2m$upper[res2m$iter %% 5 != 0] = NA

res2m.part = res2m[iter >= 20, ]

pdf(mkna4plot(prefix, "hypervolume_over_time_aggr_mean.pdf"), height = 7, width = 10)
lapply(split(res2m.part, paste(res2m.part$lrn, res2m.part$problem, res2m.part$algorithm)), function(dat) {
  ggplot(data = dat, mapping = aes(x = iter, y = mean.cdhv, group = algo, color = algo)) +
    geom_line() +
    ylab("Mean currently dominated hypervolume (based on all datasets except openbox)") +
    facet_grid(openbox_name ~ lockbox_name, scales = "free_y") +
    ggtitle(paste0("lrn: ", dat$lrn[1], ", problem: ", dat$problem[1],
        ", algorithm: ", dat$algorithm[1]))
  })
dev.off()

pdf(mkna4plot(prefix, "hypervolume_over_time_aggr_median.pdf"), height = 7, width = 10)
lapply(split(res2m, paste(res2m$lrn, res2m$problem, res2m$algorithm)), function(dat) {
  ggplot(data = dat, mapping = aes(x = iter, y = median.cdhv, group = algo, color = algo)) +
    geom_line() +
    geom_errorbar(mapping = aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
    ylab("Median currently dominated hypervolume (based on all datasets except openbox)") +
    facet_grid(openbox_name ~ lockbox_name, scales = "free_y") +
    ggtitle(paste0("lrn: ", dat$lrn[1], ", problem: ", dat$problem[1],
        ", algorithm: ", dat$algorithm[1]))
  })
dev.off()

res3m = res3[, list(mean.lb = mean(lockbox.best)),
  by = c("lrn", "problem", "algorithm", "openbox_name", "lockbox_name", "iter", "algo")]
res3m.part = res3m[iter >= 20, ]

pdf(mkna4plot(prefix, "lockbox_over_time_aggr_mean.pdf"), height = 7, width = 10)
lapply(split(res3m.part, paste(res3m.part$lrn, res3m.part$problem, res3m.part$algorithm)), function(dat) {
  ggplot(data = dat, mapping = aes(x = iter, y = mean.lb, group = algo, color = algo)) +
    geom_line() +
    ylab("Mean current best mmce on lockbox") +
    facet_grid(openbox_name ~ lockbox_name, scales = "free_y") +
    ggtitle(paste0("lrn: ", dat$lrn[1], ", problem: ", dat$problem[1],
      ", algorithm: ", dat$algorithm[1]))
})
dev.off()


# Aggregate over repls, openbox_name and lockbox_name
pdf(mkna4plot(prefix, "hypervolume_over_time_aggr_quartiles2.pdf"), height = 7, width = 7)
ggplot(data = res2, mapping = aes(x = iter, y = cdhv, color = algo)) +
  stat_summary(geom = "ribbon",
    fun.ymin = function(x) quantile(x, 0.25, type = 2),
    fun.ymax = function(x) quantile(x, 0.75, type = 2),
    fun.y = function(x) median(x),
    aes(fill = algo), alpha = 0.3) +
  ylab("Quartiles of currently dominated hypervolume (based on all datasets except openbox)") +
  facet_grid(lrn ~ algo + problem + algorithm, scales = "free_y")
dev.off()

res2ma = res2[, list(
  mean.cdhv = mean(cdhv),
  median.cdhv = median(cdhv),
  lower = quantile(cdhv, 0.25, type = 2),
  upper = quantile(cdhv, 0.75, type = 2)
  ),
  by = c("lrn", "problem", "algorithm", "iter", "algo")]
res2ma$lower[res2ma$iter %% 5 != 0] = NA
res2ma$upper[res2ma$iter %% 5 != 0] = NA

res2ma.part = res2ma[iter >= 20, ]

pdf(mkna4plot(prefix, "hypervolume_over_time_aggr_mean2.pdf"), height = 7, width = 7)
ggplot(data = res2ma.part, mapping = aes(x = iter, y = mean.cdhv, group = algo, color = algo)) +
  geom_line() +
  ylab("Mean currently dominated hypervolume (based on all datasets except openbox)") +
  facet_grid(lrn ~ problem + algorithm, scales = "free_y")
dev.off()

pdf("hypervolume_over_time_aggr_median2.pdf", height = 7, width = 7)
ggplot(data = res2ma, mapping = aes(x = iter, y = median.cdhv, group = algo, color = algo)) +
  geom_line() +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  ylab("Median currently dominated hypervolume (based on all datasets except openbox)") +
  facet_grid(lrn ~ problem + algorithm, scales = "free_y")
dev.off()


res3ma = res3[, list(mean.lb = mean(lockbox.best)),
  by = c("lrn", "problem", "algorithm", "iter", "algo")]
res3ma.part = res3ma[iter >= 20, ]
browser()
# lockbox inbag and outbag is the same
pdf(mkna4plot(prefix, "lockbox_over_time_aggr_mean2.pdf"), height = 7, width = 10)
ggplot(data = res3ma.part, mapping = aes(x = iter, y = mean.lb, group = algo, color = algo)) +
  geom_line() +
  ylab("Mean current best mmce on lockbox") +
  facet_grid(lrn ~ problem + algorithm, scales = "free_y")
dev.off()

res4= tuple$res4
res3mb = res4[, list(mean.lb = mean(openbox.best)),
  by = c("lrn", "problem", "algorithm", "iter", "algo")]
res3mb.part = res3mb[iter >= 20, ]
pdf(mkna4plot(prefix, sprintf("openbox_over_time_aggr_mean2_%s.pdf", bag2sel)), height = 7, width = 10)
ggplot(data = res3mb.part, mapping = aes(x = iter, y = mean.lb, group = algo, color = algo)) +
  geom_line() +
  ylab(sprintf("Mean current best mmce on openbox %s", bag2sel)) +
  facet_grid(lrn ~ problem + algorithm, scales = "free_y")
dev.off()
