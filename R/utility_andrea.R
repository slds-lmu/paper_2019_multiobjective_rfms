# Reduce results of batchtools jobs
bag2sel = "inbag"
filename = sprintf("tsc_%s.rds", bag2sel)
redu = function() {
library(data.table)
# c("fmo", "fso_ladder", "fso_th", "fso2", "fso5", "fso8", "lso", "rand_mo")
algos = c("fmo", "fso2", "fso5", "fso8", "lso", "rand_mo")
reduce.fun = function(job, res) {
  algos = intersect(algos, ls(res$res$gperf_env))
  dat = lapply(algos, function(a) {
    d = lapply(1:length(res$res$gperf_env[[a]]), function(i) {
      l = res$res$gperf_env[[a]][[i]][[bag2sel]]
      tmp = BBmisc::convertListOfRowsToDataFrame(l)
      tmp = cbind(tmp, dataset = names(l), iter = i, stringsAsFactors = FALSE)
      rownames(tmp) = NULL
      return(tmp)
    })
    d = dplyr::bind_rows(d)
    d = cbind(d, algo = a, stringsAsFactors = FALSE)
    return(d)
  })
  dat = dplyr::bind_rows(dat)
  dat = as.data.table(dat)

  ret = cbind(
    dat,
    lrn = job$pars$algo.pars$lrn,
    openbox_name = res$res$instance$openbox_name,
    lockbox_name = res$res$instance$lockbox_name,
    repl = job$repl,
    algorithm = job$algo.name,
    problem = job$prob.name,
    job.id = job$job.id
  )

  return(ret)
}

res = reduceResultsList(findDone(), reduce.fun)
res = dplyr::bind_rows(res)

#################################################################
# Calculate currently dominated hypervolume

jobinfo.cols = c("lrn", "problem", "openbox_name", "lockbox_name", "repl", "algorithm", "job.id")

f1 = function(mmces, nam) {
  l = as.list(mmces)
  names(l) = paste0("mmce.", nam)
  return(l)
}
res1 = res[, f1(mmce, dataset), by = c("iter", "algo", jobinfo.cols)]

f2 = function(x) {
  iter = x$iter
  o.index = unique(x$openbox.index)
  nc = ncol(x)
  z = as.matrix(x[, -c(nc, nc - 1, o.index), with = FALSE])

  dhvs = lapply(1:length(iter), function(i) {
    pf = mco::paretoFilter(z[1:i, , drop = FALSE])
    pf = unique(pf)
    mco::dominatedHypervolume(pf, rep(1, 4))
  })
  return(data.table(iter = iter, cdhv = dhvs))
}

mmce.cols = colnames(res1)[grep("mmce.", colnames(res1))]
openbox.ind = sapply(res1$openbox_name, function(n) which(n == substring(mmce.cols, first = 6)))
res1 = cbind(res1, openbox.index = openbox.ind)

res2 = res1[, f2(.SD), .SDcols = c(mmce.cols, "iter", "openbox.index"), by = c("algo", jobinfo.cols)]
res2$cdhv = unlist(res2$cdhv)


#####################################################################
# current best performance on lockbox dataset

lockbox.ind = sapply(res1$lockbox_name, function(n) which(n == substring(mmce.cols, first = 6)))
tmp = subset(res1, select = mmce.cols)
lockbox.perf = unlist(sapply(1:length(lockbox.ind), function(i) tmp[i, lockbox.ind[i], with = FALSE]))
res1 = cbind(res1, mmce.lockbox = lockbox.perf)  # slow step

f3 = function(mmces, iter) {
  best = lapply(1:length(iter), function(i) {
    min(mmces[1:i])
  })
  return(data.table(iter = iter, lockbox.best = best))
}
res3 = res1[, f3(mmce.lockbox, iter), by = c("algo", jobinfo.cols)]
res3$lockbox.best = unlist(res3$lockbox.best)

res4 = res1[, f3(mmce.openbox, iter), by = c("algo", jobinfo.cols)]
res4$openbox.best = unlist(res4$openbox.best)

saveRDS(list(res = res, res2 = res2, res3 = res3, res4 = res4), file = filename)
###################################################################
}
# Plots
# since cluster does not support plot, the following code will be execulated locally.
mkna4plot = function(prefix, strna) {
  paste0(prefix, strna)
}
#' mkplot(filename)
mkplot = function(filename) {
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

pdf(mkna4plot(prefix, "lockbox_over_time_aggr_mean2.pdf"), height = 7, width = 10)
ggplot(data = res3ma.part, mapping = aes(x = iter, y = mean.lb, group = algo, color = algo)) +
  geom_line() +
  ylab("Mean current best mmce on lockbox") +
  facet_grid(lrn ~ problem + algorithm, scales = "free_y")
dev.off()

}
