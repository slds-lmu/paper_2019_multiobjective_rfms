# Reduce results of batchtools jobs

library(data.table)

reduce.fun = function(job, res) {
  algos = c("fmo", "fmo_nocv", "fso", "fso_th", "lso_openbox", "lso_openbox_nocv", "rso_curator")
  algos = intersect(algos, ls(res$res$gperf_env))

  dat = lapply(algos, function(a) {
    d = lapply(1:length(res$res$gperf_env[[a]]), function(i) {
      l = res$res$gperf_env[[a]][[i]]
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
    dataset_name = job$pars$prob.pars$dataset_name,
    openbox_name = res$res$instance$openbox_name,
    lockbox_name = res$res$instance$lockbox_name,
    repl = job$repl,
    algorithm = job$algo.name,
    job.id = job$job.id
  )

  return(ret)
}

res = reduceResultsList(findDone(), reduce.fun)
res = dplyr::bind_rows(res)

#################################################################
# Calculate currently dominated hypervolume

jobinfo.cols = c("lrn", "dataset_name", "openbox_name", "lockbox_name", "repl", "algorithm", "job.id")

f1 = function(mmces, nam) {
  l = as.list(mmces)
  names(l) = paste0("mmce.", nam)
  return(l)
}
res1 = res[, f1(mmce, dataset), by = c("iter", "algo", jobinfo.cols)]

f2 = function(x) {
  iter = x$iter
  z = x[, -"iter"]
  dhvs = lapply(1:length(iter), function(i) {
    pf = mco::paretoFilter(as.matrix(z[1:i, , drop = FALSE]))
    pf = unique(pf)
    mco::dominatedHypervolume(pf, rep(1, 5))
  })
  return(data.table(iter = iter, cdhv = dhvs))
}

mmce.cols = colnames(res1)[grep("mmce.", colnames(res1))]
res2 = res1[, f2(.SD), .SDcols = c("iter", mmce.cols),
  by = c("algo", jobinfo.cols)]
res2$cdhv = unlist(res2$cdhv)

###################################################################
# Plots

library(ggplot2)

# Plot the development over time: each job is on a separate page
pdf(file = "mmce_over_time.pdf", height = 7, width  = 10)
lapply(split(res, res$job.id), function(dat) {
  ggplot(data = dat, mapping = aes(x = iter, y = mmce, group = dataset, color = dataset)) +
    geom_line() + geom_point() +
    facet_wrap("algo") +
    ggtitle(paste0("lrn: ", dat$lrn[1], ", dataset_name: ", dat$dataset_name[1],
      ", openbox_name: ", dat$openbox_name[1], ", lockbox_name: ", dat$lockbox_name[1],
      ", repl: ", dat$repl[1], ", algorithm: ", dat$algorithm[1]))
})
dev.off()

pdf(file = "hypervolume_over_time.pdf", height = 7, width  = 10)
lapply(split(res2, res2$job.id), function(dat) {
  ggplot(data = dat, mapping = aes(x = iter, y = cdhv, group = algo, color = algo)) +
    geom_line() + geom_point() +
    ylab("Currently dominated hypervolume (based on all datasets)") +
    ggtitle(paste0("lrn: ", dat$lrn[1], ", dataset_name: ", dat$dataset_name[1],
      ", openbox_name: ", dat$openbox_name[1], ", lockbox_name: ", dat$lockbox_name[1],
      ", repl: ", dat$repl[1], ", algorithm: ", dat$algorithm[1]))
})
dev.off()


# Aggregate over repls
pdf("hypervolume_over_time_aggr_quartiles.pdf", height = 7, width = 10)
lapply(split(res2, paste(res2$lrn, res2$dataset_name, res2$algorithm)), function(dat) {
  ggplot(data = dat, mapping = aes(x = iter, y = cdhv, group = algo, color = algo)) +
    stat_summary(geom = "ribbon",
      fun.ymin = function(x) quantile(x, 0.25, type = 2),
      fun.ymax = function(x) quantile(x, 0.75, type = 2),
      fun.y = function(x) median(x),
      aes(fill = algo), alpha = 0.3) +
    ylab("Quartiles of currently dominated hypervolume (based on all datasets)") +
    facet_grid(openbox_name ~ lockbox_name, scales = "free_y") +
    ggtitle(paste0("lrn: ", dat$lrn[1], ", dataset_name: ", dat$dataset_name[1],
      ", algorithm: ", dat$algorithm[1]))
})
dev.off()

res2m = res2[, list(
  mean.cdhv = mean(cdhv),
  median.cdhv = median(cdhv),
  lower = quantile(cdhv, 0.25, type = 2),
  upper = quantile(cdhv, 0.75, type = 2)
),
  by = c("lrn", "dataset_name", "algorithm", "openbox_name", "lockbox_name", "iter", "algo")]
res2m$lower[res2m$iter %% 5 != 0] = NA
res2m$upper[res2m$iter %% 5 != 0] = NA

pdf("hypervolume_over_time_aggr_mean.pdf", height = 7, width = 10)
lapply(split(res2m, paste(res2m$lrn, res2m$dataset_name, res2m$algorithm)), function(dat) {
  ggplot(data = dat, mapping = aes(x = iter, y = mean.cdhv, group = algo, color = algo)) +
    geom_line() +
    ylab("Mean currently dominated hypervolume (based on all datasets)") +
    facet_grid(openbox_name ~ lockbox_name, scales = "free_y") +
    ggtitle(paste0("lrn: ", dat$lrn[1], ", dataset_name: ", dat$dataset_name[1],
      ", algorithm: ", dat$algorithm[1]))
})
dev.off()

pdf("hypervolume_over_time_aggr_median.pdf", height = 7, width = 10)
lapply(split(res2m, paste(res2m$lrn, res2m$dataset_name, res2m$algorithm)), function(dat) {
  ggplot(data = dat, mapping = aes(x = iter, y = median.cdhv, group = algo, color = algo)) +
    geom_line() +
    geom_errorbar(mapping = aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
    ylab("Median currently dominated hypervolume (based on all datasets)") +
    facet_grid(openbox_name ~ lockbox_name, scales = "free_y") +
    ggtitle(paste0("lrn: ", dat$lrn[1], ", dataset_name: ", dat$dataset_name[1],
      ", algorithm: ", dat$algorithm[1]))
})
dev.off()


# Aggregate over repls, openbox_name and lockbox_name
pdf("hypervolume_over_time_aggr_quartiles2.pdf", height = 7, width = 7)
ggplot(data = res2, mapping = aes(x = iter, y = cdhv, color = algo)) +
  stat_summary(geom = "ribbon",
    fun.ymin = function(x) quantile(x, 0.25, type = 2),
    fun.ymax = function(x) quantile(x, 0.75, type = 2),
    fun.y = function(x) median(x),
    aes(fill = algo), alpha = 0.3) +
  ylab("Quartiles of currently dominated hypervolume (based on all datasets)") +
  facet_grid(lrn ~ algo + dataset_name + algorithm, scales = "free_y")
dev.off()

res2ma = res2[, list(
  mean.cdhv = mean(cdhv),
  median.cdhv = median(cdhv),
  lower = quantile(cdhv, 0.25, type = 2),
  upper = quantile(cdhv, 0.75, type = 2)
  ),
  by = c("lrn", "dataset_name", "algorithm", "iter", "algo")]
res2ma$lower[res2ma$iter %% 5 != 0] = NA
res2ma$upper[res2ma$iter %% 5 != 0] = NA

pdf("hypervolume_over_time_aggr_mean2.pdf", height = 7, width = 7)
ggplot(data = res2ma, mapping = aes(x = iter, y = mean.cdhv, group = algo, color = algo)) +
  geom_line() +
  ylab("Mean currently dominated hypervolume (based on all datasets)") +
  facet_grid(lrn ~ dataset_name + algorithm, scales = "free_y")
dev.off()

pdf("hypervolume_over_time_aggr_median2.pdf", height = 7, width = 7)
ggplot(data = res2ma, mapping = aes(x = iter, y = median.cdhv, group = algo, color = algo)) +
  geom_line() +
  geom_errorbar(mapping = aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  ylab("Median currently dominated hypervolume (based on all datasets)") +
  facet_grid(lrn ~ dataset_name + algorithm, scales = "free_y")
dev.off()



