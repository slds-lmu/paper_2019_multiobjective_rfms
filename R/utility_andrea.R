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

# res1$curator_names = sapply()
func = function(x) {
  mmcecols = substring(mmce.cols, first = 6)
  cuna = setdiff(mmcecols, c(x$openbox_name[1], x$lockbox_name[1])) 
  as.list(cuna)
}
res1[, curator_names:= func(.SD), by = c("openbox_name", "lockbox_name"), .SDcols = c("openbox_name", "lockbox_name")]
mmce.cols = colnames(res1)[grep("mmce.", colnames(res1))]
openbox.ind = sapply(res1$openbox_name, function(n) which(n == substring(mmce.cols, first = 6)))
#curator.ind = sapply(res1$curator_name, function(n) which(n == substring(mmce.cols, first = 6)))
res1 = cbind(res1, openbox.index = openbox.ind)


res2 = res1[, f2(.SD), .SDcols = c(mmce.cols, "iter", "openbox.index"), by = c("algo", jobinfo.cols)]
res2$cdhv = unlist(res2$cdhv)


#####################################################################
# current best performance on lockbox dataset

lockbox.ind = sapply(res1$lockbox_name, function(n) which(n == substring(mmce.cols, first = 6)))
tmp = subset(res1, select = mmce.cols)
lockbox.perf = unlist(sapply(1:length(lockbox.ind), function(i) tmp[i, lockbox.ind[i], with = FALSE]))
openbox.perf = unlist(sapply(1:length(openbox.ind), function(i) tmp[i, openbox.ind[i], with = FALSE]))
res1 = cbind(res1, mmce.lockbox = lockbox.perf)  # slow step
res1 = cbind(res1, mmce.openbox = openbox.perf)  # slow step

f3 = function(mmces, iter) {
  best = lapply(1:length(iter), function(i) {
    min(mmces[1:i])
  })
  return(data.table(iter = iter, lockbox.best = best))
}
res3 = res1[, f3(mmce.lockbox, iter), by = c("algo", jobinfo.cols)]
res3$lockbox.best = unlist(res3$lockbox.best)
browser()
f4 = function(mmces, iter) {
  best = lapply(1:length(iter), function(i) {
    min(mmces[1:i])
  })
  return(data.table(iter = iter, openbox.best = best))
}

res4 = res1[, f4(mmce.openbox, iter), by = c("algo", jobinfo.cols)]
res4$openbox.best = unlist(res4$openbox.best)

f5 = function(mmces, iter) {
  best = lapply(1:length(iter), function(i) {
    min(mmces[1:i])
  })
  return(data.table(iter = iter, curator.best = best))
}

saveRDS(list(res = res, res2 = res2, res3 = res3, res4 = res4), file = filename)
###################################################################
}
# source("utility_tsc_plot.R")
