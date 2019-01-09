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
# Plot the development over time: each job is on a separate page

library(ggplot2)

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

