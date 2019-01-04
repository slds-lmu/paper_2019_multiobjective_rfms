#' res2[1, result][[1]]$fig_front
source("post_vis.R")

res = BBmisc::load2("output/DortmundClusterResVonAndrea.RData")

getTestLevelFromNa = function(ns, test_name, major_level) {
 which(ns[-major_level] == test_name)
}

function() {
  res = reduceResultsList(ids = findDone(), fun = function(job, res) {
    list(repl = job$repl, prob.pars = job$prob.pars, algo.pars = job$algo.pars, res = res)
    })
  res = reduceResultsList(ids = 1:10, fun = function(job, res) {
      job$repl  # the replication does not help us aggregate the pareto front!!
      # it only make sense to aggregate the baseline model
      ind = res$res$tune_res_pr$ind
      #ind
      res$res
      #names(res$res$gperf_side)
      pareto.list = res$res$gperf_side$pr[ind]
      ns = names(pareto.list[[1]])
      list.dt = data.table::rbindlist(pareto.list)
      #library(rgl)
      #library(magrittr)
      library(data.table)
      so = res$res$gperf_side$bs1
      bs_ind = res$res$tune_res_bs$mbo.result$best.ind
      sodt = data.table::rbindlist(so[bs_ind])
      #res$res$tune_res_bs2$mbo.result$y
      lrn.id = res$res$tune_res_bs$learner$id
      print(lrn.id)
      sodt$algo = "bs1"
      list.dt$algo = "pr"
      bspr = rbind(sodt, list.dt)
      bspr$major_name = ns[res$res$major_level]
      bspr$test_name = res$res$test_name
      bspr$lrn.id = lrn.id
      #bspr
      majorname = ns[res$res$major_level]
      testname = res$res$test_name
      msname = setdiff(ns, c(majorname, testname))
      side = apply(as.data.frame(bspr)[, msname], 1, FUN = mean)
      major = bspr[[majorname]]
      test = bspr[[testname]]
      bspr$algo = as.factor(bspr$algo)
      bspr
      listofrow = apply(bspr,1,as.list)
      rbindlist(listofrow)
    })
  res.table = rbindlist(res)
  res.table[,.(mean(arr_delay), mean(GSE32646)),by = .(major_name, test_name, lrn.id)]
}






f1_lookSingleResult = function() {
  library(BBmisc)
  source("bt_conf.R")
  library("batchtools")
  REG_FILE_DIR = "testReg"
  REG_FILE_DIR = "regOpenML"
  REG_FILE_DIR = "dortmund"
  REG_FILE_DIR = "openml_gina_kmeans_snicker"
  reg = loadRegistry(file.dir = REG_FILE_DIR, work.dir = getwd(), conf.file = NA)
  reg$writeable = TRUE  # never execute this line when the process is running.
  ids = findDone(reg = reg)
  source("playground.R")
  # check if result is right
  function() {
    res = loadResult(144)
    as.data.frame(res$res$tune_res_pr$opt.path)
    as.data.frame(res$res$tune_res_bs$opt.path)
    compare(res$res)$fig_front
    prob = funGenProb(data = list(), job = NULL, major_level = res$res$major_level, test_level = res$res$test_level)
    pvss = res$res$tune_res_pr$x
    lrn.id = res$res$tune_res_pr$learner$id
    pvs = pvss[[1L]]
    pvs = pvss[[2L]]
    sapply(pvss, function(pvs) { getTestSetPerf(instance = prob, lrn.id = lrn.id, pvs = pvs)})
  }
  function() {
    res2 = reduceResultsDataTable(ids = ids, fun = function(job, res) {
      list.res = compare(res$res)
      list.res })
    library(ggpubr)
    list_fig_front = lapply(1:nrow(res2), function(i) res2[i, result][[1L]]$fig_front)
    si = seq(from = 1, to = nrow(res2), by = 9)
    for (i in si) {
      fig = ggarrange(plotlist = list_fig_front[i: (i + 8L)], nrow = 3, ncol = 3)
      ggsave(filename = paste0("fig/dataset_accn_", as.character(i), "_asMajorDS_3lrn3repl.pdf"), plot = fig, scale = 1, pointsize = 0.1)
    }
  }
}




onerowresult = function(i) {
  major_level = df_res[i, "major_level"]
  test_level = df_res[i, "test_level"]
  test_ind = setdiff(1:5, major_level)[test_level]
  pr.major = paste0("result.", major_level)
  pr.test = paste0("result.", test_ind)
  bs.major = paste0("result.", major_level + 5)
  bs.test = paste0("result.", test_ind + 5)
  dfhh = df_res[i, c("lrn", pr.major, pr.test, bs.major, bs.test)]
  colnames(dfhh) =  c("lrn", "pr.major", "pr_test", "bs.major", "bs.test")
  dfhh = cbind(dfhh, df_res[i, c("major_level", "test_level")])
  #
  dfhm = df_res[i, c("lrn", pr.major, pr.test)]
  colnames(dfhm) = c("lrn", "major", "test")
  dfhm$algo = "pr"
  dfhm = cbind(dfhm, df_res[i, c("major_level", "test_level")])
  dfhb = df_res[i, c("lrn", bs.major, bs.test)]
  colnames(dfhb) = c("lrn", "major", "test")
  dfhb$algo = "bs"
  dfhb = cbind(dfhb, df_res[i, c("major_level", "test_level")])
  fig_front = ggplot(data = rbind(dfhb, dfhm), mapping = aes(x = major, y = test)) + geom_point(alpha = 1, mapping = aes(shape = algo), size = 3) # + ggtitle(paste0("pfront_", lrn, "_dataset", major_level, "_", test_level)) + theme(text = element_text(size=7), axis.text.x = element_text(angle=90, hjust=1))
  return(list(dfhm = dfhm, dfhb = dfhb, dfhh = dfhh, fig_front = fig_front))
}


#formTable = function(res) {
#  rbind(res$res$perf_side_pr, res$res$perf_side_bs)
#}

f2_optPlot = function() {
  jpar = getJobPars(reg = reg, ids = ids)
  res_ds = reduceResultsDataTable(ids = ids, fun = function(job, res) {
      #list.res = formTable(res)
      rbind(res$res$perf_side_pr, res$res$perf_side_bs)
      #list.res
  })
  res_par = merge(res_ds, jpar)
  dt_res = unwrap(res_par[, list(prob.pars, algo.pars, result)])
  df_res = as.data.frame(dt_res)
  list.result = lapply(1:nrow(df_res), FUN = function(i) {return(onerowresult(i = i)$dfhh) })
  list.result.m = lapply(1:180, FUN = function(i) {return(onerowresult(i)$dfhm) })
  list.result.b = lapply(1:180, FUN = function(i) {return(onerowresult(i)$dfhb) })
  list.fig = lapply(1:180, FUN = function(i) {return(onerowresult(i)$fig_front) })
  dtmb = rbindlist(c(list.result.m,list.result.b))
  dt.resulthh = data.table::rbindlist(list.result)
  dt.resulthh
  pr = as.data.frame(dt.resulthh[, list(pr.major, pr_test, major_level)])
  colnames(pr) = c("major", "test", "set")
  bs = as.data.frame(dt.resulthh[, list(bs.major, bs.test, major_level)])
  colnames(bs) = c("major", "test", "set")
  require(eaf)
  eafplot(pr, bs)
  eafplot(pr, bs, percentiles = c(50))
  pdf("empericalattainment.pdf")
  eafplot(pr, bs, percentiles = c(50))
  dev.off()
  dfmb = as.data.frame(dtmb)
  fig_front_all = ggplot(data = dfmb, mapping = aes(x = major, y = test, color = algo)) + geom_point(alpha = 1, mapping = aes(shape = algo), size = 3)
  fig_front_all
}



reduceTestPerf = function(res, instance) {
  major_level = res$res$major_level
  test_level = res$res$test_level
  testName = getTestName(instance$ns, major_level = instance$major_level, test_level = instance$test_level)
  pvs_bs = res$res$tune_res_bs$x
  pvs_bs2 = res$res$tune_res_bs2$x
  lrn.id = res$res$tune_res_pr$learner$id

  pr_test_list = lapply(res$res$tune_res_pr$x, function(pvs_pr) {
    #pvs_pr = res$res$tune_res_pr$x[[1L]]
    pr_model = getModelFromInstance(instance = instance, lrn.id = lrn.id, pvs = pvs_pr)
   pr_test = getTestSetPerf(instance = instance, lrn.id = res$res$tune_res_pr$learner$id, pvs = pvs_pr)
   pr_test
  })
  pr_test_list = unlist(pr_test_list)


  bs_model = getModelFromInstance(instance = instance, lrn.id = lrn.id, pvs = pvs_bs)
  bs_test = getTestSetPerf(instance = instance, lrn.id = res$res$tune_res_bs$learner$id, pvs = pvs_bs)
  bs_test2 = getTestSetPerf(instance = instance, lrn.id = res$res$tune_res_bs$learner$id, pvs = pvs_bs2)
  return(list(major_level = major_level, test_level = test_level, lrn.id = res$res$tune_res_pr$learner$id, pr_test = mean(pr_test_list), pr_test_list = pr_test_list, bs_test = bs_test, bs_test2 = bs_test2, pvs_bs = pvs_bs, pvs_pr = res$res$tune_res_pr$x, testName = testName))
}


f5_testPerf = function() {
  #instance = funGenProb(data = NULL, job = NULL, major_level = major_level, test_level = test_level)
  pars = unwrap(getJobPars()[, list(prob.pars)])
  ids = findDone()[1:20]
  res_test = reduceResultsList(ids = ids, fun = function(job, res) {
   major_level = res$res$major_level
   test_level = res$res$test_level
   instance = funGenProb(data = prob_inputs, job = NULL, major_level = major_level, test_level = test_level)
   reduceTestPerf(res, instance = instance)
  })
  save2(res_test, file = "testPerf.RData")
  coml = lapply(res_test, function(x) {
    list(major = x$major_level, test_level = x$test_level, testName = x$testName, bs_test = x$bs_test, bs_test2 = x$bs_test2, pr_test = x$pr_test, lrn = x$lrn.id)
  })
  dt2 = rbindlist(coml)
  library(tidyr)
  dtlong = gather(dt2, key = "method", value = "mmce", pr_test, bs_test, bs_test2)
  dtlong = dtlong[which(dtlong$lrn == "classif.glmnet"), ]
  dtlong = dtlong[which(dtlong$lrn == "classif.ranger"), ]
  dtlong = dtlong[which(dtlong$lrn == "classif.ksvm"), ]
  ggplot2::ggplot(dtlong, aes(x = method, y = mmce)) + geom_boxplot()
  ggplot2::ggplot(dtlong, aes(x = method, y = mmce)) + geom_boxplot() + facet_grid(. ~ major)
  ggplot2::ggplot(dtlong, aes(x = method, y = mmce)) + geom_boxplot() + facet_grid(. ~ testName)
  fig_mm = ggplot2::ggplot(dtlong, aes(x = method, y = mmce)) + geom_boxplot() + facet_grid(cols = vars(test_level), rows = vars(major)) + ggtitle(label = "gina-agnostic-30-70-lsvm-agg-rep30")
  ggsave("major_test_30_gina_agnostic_30_70_lsvm-c_agg_rep30.pdf", plot = fig_mm)
  ggplot2::ggplot(dtlong, aes(x = method, y = mmce)) + geom_boxplot() + facet_grid(. ~lrn)

  getHyper2string = function(par_list) {
   ns = names(par_list)
   va = as.character(par_list)
   paste(ns, va, sep = ":", collapse = "-")
  }

  list_res_test = lapply(res_test, function(entry) {
   #' entry = res_test[[1L]]
    entry$pvs_bs = getHyper2string(entry$pvs_bs)
    entry$pvs_pr = getHyper2string(entry$pvs_pr)
    entry
  })
  df_testPerf = rbindlist(list_res_test)
  plot(df_testPerf[, pr_test], df_testPerf[, bs_test])
}
