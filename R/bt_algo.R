library(BBmisc)
library(mlr)
library(mlrMBO)
library(magrittr)
library(ggplot2)
source("bt_measures_objs.R")
source("bt_helpers.R")


algo_rand = function(instance, lrn, list_measures, gperf_env, context) {
  cat(sprintf("\n\n\n %s beginned  \n\n\n", context))
  gperf_env$context =  context
  gperf_env$index = 1
  mgconf = getGconf()
  ctrl_rand = mlr::makeTuneMultiCritControlRandom(maxit = mgconf$MBO_ITERS + mgconf$INIT_DES)
  res = mlr::tuneParamsMultiCrit(learner = GET_LRN(lrn), task = instance$task, resampling = instance$rins, measures = list_measures, par.set = GET_PARSET_CLASSIF(lrn), control = ctrl_rand, show.info = TRUE)
  cat(sprintf("\n %s finished  \n", context))
  res
}

algo_mo = function(instance, lrn, mbo_design, list_measures, gperf_env, context) {
  cat(sprintf("\n\n\n %s beginned  \n\n\n", context))
  gperf_env$context =  context
  gperf_env$index = 1
  mgconf = getGconf()
  ctrl_pr = getTuneMethod("mbomulticritdefault", mgconf = mgconf)
  ctrl_pr$mbo.design = mbo_design  # use the same design
  res = mlr::tuneParamsMultiCrit(learner = GET_LRN(lrn), task = instance$task, resampling = instance$rins, measures = list_measures, par.set = GET_PARSET_CLASSIF(lrn), control = ctrl_pr, show.info = TRUE)
  cat(sprintf("\n %s finished  \n", context))
  res
}

algo_so = function(instance, lrn, mbo_design, list_measures, gperf_env, context, ctrl_bs = NULL) {
  cat(sprintf("\n\n\n %s beginned  \n\n\n", context))
  mgconf = getGconf()
  gperf_env$context =  context
  gperf_env$index = 1
  if (is.null(ctrl_bs)) ctrl_bs = getTuneMethod("mbodefault", mgconf = mgconf)
  ctrl_bs$mbo.design = mbo_design
  tune_res_bs = mlr::tuneParams(learner = GET_LRN(lrn), task = instance$task, resampling = instance$rins, measures = list_measures, par.set = GET_PARSET_CLASSIF(lrn), control = ctrl_bs, show.info = TRUE)  # only the first of the list_measures are being tuned
  cat(sprintf("\n\n\n %s finished  \n\n\n", context))
  return(tune_res_bs = tune_res_bs)
}

algo_rand2 = function(instance, lrn) {
  res = list()
  res$tune_res_rand = algo_rand(instance = instance, lrn = lrn, list_measures = list(meas_openbox_cv, measure_curator), gperf_env = gperf_env, context = "rand")
}


algo_mbo = function(instance, lrn) {
  res = list()
  gperf_env = new.env()   # gperf_env is only being modified in side measure function!
  ptmi = proc.time()
  mbo_design = getMBODesign(lrn, getGconf())   # design is regenerated each time to avoid bias
  gperf_env$current_best_loss_vec = rep(1, instance$curator_inbag_len)
  gperf_env$current_best_meas = mlr::brier$worst

  extra.args = list(instance = instance, gperf_env = gperf_env, perf_name2tune = "brier", measures2tune = mlr::brier, calMeasVec = calBrierVec)

  meas_openbox_cv = mk_measure(name = "meas_openbox_cv", extra.args, obj_fun = fun_measure_obj_openbox)
  #meas_openbox_nocv = mk_measure(name = "meas_openbox_nocv", extra.args, obj_fun = fun_measure_obj_openbox_nocv)
  measure_curator = mk_measure(name = "meas_curator", extra.args = extra.args, obj_fun = fun_measure_obj_curator)
  measure_th = mk_measure(name = "thresholdout", extra.args = extra.args, obj_fun = fun_obj_thresholdout)
  meas_ladder = mk_measure(name = "meas_ladder", extra.args = extra.args, obj_fun = fun_ladder_parafree)

  # we can only have one global variable here: we need a context object to know which algorithm we are using
  context = "fso_ladder"
  ctrl_bs = getTuneMethod("mbodefault", mgconf = mgconf, nugget = 1e-1)
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_ladder), gperf_env = gperf_env, context = context, ctrl_bs = ctrl_bs)


  context = "fso_th"
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(measure_th), gperf_env = gperf_env, context = context)
  print(proc.time() - ptmi)

  context = "fso5"
  extra.args$alpha = 0.5
  meas_alpha_so = mk_measure(name = "meas_alpha_so", extra.args = extra.args, obj_fun = fun_measure_obj_openbox_tr_curator_tune)
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_alpha_so), gperf_env = gperf_env, context = context)

  context = "fso2"
  extra.args$alpha = 0.2
  meas_alpha_so = mk_measure(name = "meas_alpha_so", extra.args = extra.args, obj_fun = fun_measure_obj_openbox_tr_curator_tune)
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_alpha_so), gperf_env = gperf_env, context = context)

  context = "fso8"
  extra.args$alpha = 0.8
  meas_alpha_so = mk_measure(name = "meas_alpha_so", extra.args = extra.args, obj_fun = fun_measure_obj_openbox_tr_curator_tune)
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_alpha_so), gperf_env = gperf_env, context = context)

  context = "rand"
  res[[context]] = algo_rand(instance = instance, lrn = lrn, list_measures = list(meas_openbox_cv, measure_curator), gperf_env = gperf_env, context = context)

  context = "lso_openbox"
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_openbox_cv, measure_curator), gperf_env = gperf_env, context = context)
  print(proc.time() - ptmi)
  
  #context = "rso_curator"
  #res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(measure_curator), gperf_env = gperf_env, context = context)
  #print(proc.time() - ptmi)

  #res$tune_res_lso_openbox_nocv = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_openbox_nocv, measure_curator), gperf_env = gperf_env, context = "lso_openbox_nocv")
  #print(proc.time() - ptmi)

  ### MultiObj
  context = "fmo"
  res[[context]] = algo_mo(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_openbox_cv, measure_curator), gperf_env = gperf_env, context = context)

  #res$tune_res_fmo_nocv = algo_mo(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_openbox_nocv, measure_curator), gperf_env = gperf_env, context = "fmo_nocv")
  print("algorithm finished")
  print(proc.time() - ptmi)
  res = list(tune_res = res, gperf_env = gperf_env, instance = instance)
  return(res)
}
#   conf = list(n_adapt_rounds = 10
#   ,signif_level = 0.0001           # cutoff level used to determine which predictors to consider in each round based on their p-values. set small here for bigger parsimosmally for quick convergence
#   ,thresholdout_threshold = 0.02 # T in the Thresholdout algorithm
#   ,thresholdout_sigma = 0.03     # sigma in the Thresholdout algorithm
#   ,thresholdout_noise_distribution = "norm" # choose between "norm" and "laplace"
#   ,verbose = TRUE
#   ,sanity_checks = FALSE
#   ,train_begin_ratio = 0.5  # the ratio of n_train_begin compared to n_train_total
#   )
# 
 
  #thresholdoutauc = algo_thresholdoutauc(instance = instance, conf = conf)  # enough repetition is sufficient to get unbiased result of the random adding of instances

#' @title
#' @description convert problem generator instance to thresholdoutauc input
#' @param instance instance from problem generator
#' @return list of input for ThresholdoutAUC
convertInst2ThresholdoutAUC = function(instance, conf) {
  tmf = instance$tmf
  tms = instance$tms
  tge = instance$tge
  pair = getTaskData(tmf, target.extra = T)
  x_train_total = pair$data
  y_train_total = pair$target
  pair = getTaskData(tms, target.extra = T)
  x_holdout = pair$data
  y_holdout = pair$target
  pair = getTaskData(tge, target.extra = T)
  x_test = pair$data
  y_test = pair$target
  n_train_begin = nrow(x_train_total) * conf$train_begin_ratio
  list(n_train = n_train_begin, x_train_total = x_train_total, y_train_total = y_train_total, x_holdout = x_holdout, y_holdout = y_holdout, x_test = x_test, y_test = y_test, tname = instance$tname, bname = instance$bname, p = instance$p)
}


algo_thresholdoutauc = function(instance, conf) {
  source("thresholdout4real_data/refactor_general_simulation.R", chdir = T)
  results = run_sim(data_fun = convertInst2ThresholdoutAUC, instance = instance, conf = conf)
  rowind_test = which((results$dataset == "test_auc") & (results$round == 10))  # the last round
  rowind_holdout = which((results$dataset == "holdout_auc") & (results$round == 10))
  results[rowind_test, ]  # test set
  results[rowind_holdout, ]  # test set
}




# only depend on the lockbox
algoCheating = function() {
 #tune_res_bs4 = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(mk_measure_local_tr_tune_remote_tune_nocv(extra.args), mmce), gperf_env = gperf_env, context = "bs4")
  #print("lso single obj proposal no cv finished:")
  ## redefine extra.args
  #   extra.args_bs2 =  getOpenBox2CuratorBoxID(instance)
  #   missingns = setdiff(names(extra.args), names(extra.args_bs2))
  #   extra.args_bs2 = c(extra.args_bs2, extra.args[missingns])
  #   tune_res_lso_openbox_curator = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(mk_measure_local2remote(extra.args_bs2), mmce), gperf_env = gperf_env, context = "bs2")  # mk_measure_local2remote inside call measure_curator to populate the gperf_env environment, so it is a bad idea to add measure_curator again, also because measure_curator added to the outside does not give us the right result since bs2 is a oracle which use all the d_mf and d_ms.
  #   print("baseline 2 single crit major + remote tuning finished")
  #   print(proc.time() - ptmi)
  # 
 # tune_res_lso_openbox_curator = tune_res_lso_openbox_curator, 
}


#' @descrption functions for different categories of algorithms
#' @example
#' addAlgorithm(name = "name1", fun = algo_funs[["name1"]])
#' algodesins[["name1"]] = dataframe
#' addExperiments(algo.design = algo_designs, repls = REPLS)
algo_names = c()       # list of strings defining algorithm names
algo_designs = list()  # algorithm function need extra parameters which will be defined here
algoaggs = list()  # list of functions for aggregation of result
#' To make the result analysis easy, the aggregation function for each algorithm must return a consistent result. For some algorithms, some field does not make sense, in this case, they are NA. The final data table would look like: algo_name(char), runtime(numeric), perf(numeric)[resample or simple train test], feat.subset(char)
algo_funs = list()

algo_names = c("mbo", algo_names)
algoaggs[[algo_names[1L]]] = function(res) {
  list()
}

algo_designs[[algo_names[1L]]] = data.frame(lrn = c("classif.ksvm", "classif.ranger", "classif.glmnet"), stringsAsFactors = FALSE)

algo_funs[[algo_names[1L]]] = function(job, data, instance, lrn) {
    res = algo_mbo(instance = instance, lrn = lrn)
    return(list(res = res, agg_fun = algoaggs[[algo_names[1L]]]))
}


# obsolete
algo_proposal_5 = function(instance, lrn = "classif.rpart", mbo_design) {
  ctrl_5 = getTuneMethod(method.str = "mbo5critdefault", mgconf = mgconf, n.objs = 4L)  # only 1 dataset left for testing
  ctrl_5$mbo.design = mbo_design  # use the same design
  res = tuneParamsMultiCrit(learner = GET_LRN(lrn), task = instance$task, resampling = instance$rins, measures = mkMultimeasuresList(instance$subset_inds, instance$ns, instance$major_level, instance$test_level), par.set = GET_PARSET_CLASSIF(lrn), control = ctrl_5, show.info = TRUE)
  testperf = getTestSetPerf(instance = instance, lrn.id = lrn, pvs = res$x[[1L]])
  res$y
  tuneperf  = apply(res$y, 1, mean)
  tuneperf  = mean(tuneperf)
  return(list(tuneRes = res, testperf = testperf, tuneperf = tuneperf))
}

agg_genTable = function(res) {
  agg = function(res) {
    agglist = lapply(names(res$tune_res), function(algo_name) {
      cat(sprintf("\n algorithm name: %s \n", algo_name))
      if (stringi::stri_detect(algo_name, regex = "mo")) return(agg_mo(res, algo_name = algo_name))
      if (stringi::stri_detect(algo_name, regex = "so")) return(agg_so(res, algo_name = algo_name))
      if (stringi::stri_detect(algo_name, regex = "rand")) return(agg_rand(res, algo_name = algo_name))
      stop("algorithm names wrong!")
    })
    names(agglist) = names(res$tune_res)
    rbindlist(agglist, use.names = TRUE)
  }
  lrn.id = res$tune_res[[names(res$tune_res)[1L]]]$learner$id
  dt = agg(res)
  instance = res$instance
  dt$openbox_name = instance$openbox_name
  dt$lockbox_name = instance$lockbox_name
  #dt$curator_outbag = apply(as.data.frame(dt[bag=="outbag"])[, instance$curator_names], 1, FUN = mean)
  #dt$openbox_outbag = as.vector(as.matrix(as.data.frame(dt[bag=="outbag"])[, instance$openbox_name]))
  #dt$lockbox_outbag = as.vector(as.matrix(as.data.frame(dt[bag=="outbag"])[, instance$lockbox_name]))
  dt$lrn = lrn.id
  dt
#  listofrow = apply(dt,1,as.list)
}

agg_rand = function(res_all, meas_name = "mmce", algo_name) {
  res = res_all$tune_res
  best_inds = res[[algo_name]]$ind  # get the dob of the pareto optimal
  pareto.list = res_all$gperf_env[[algo_name]][best_inds]

  pareto.list_inbag = lapply(pareto.list, function(res_iter) {
    hh = res_iter[["inbox"]]
    lapply(hh, function(x) x[[meas_name]])})
  dt_inbag = data.table::rbindlist(pareto.list_inbag)
  dt_inbag$bag = "inbag"
  dt_inbag$best_ind = best_inds

  pareto.list_outbag = lapply(pareto.list, function(res_iter) {
    hh = res_iter[["outbox"]]
    lapply(hh, function(x) x[[meas_name]])})
  dt_outbag = data.table::rbindlist(pareto.list_outbag)
  dt_outbag$bag = "outbag"
  dt_outbag$best_ind = best_inds
  dt = rbindlist(list(dt_inbag, dt_outbag))
  dt$algo = algo_name
  dt
}




agg_so = function(res_all, meas_name = "mmce", algo_name) {
  res = res_all$tune_res
  best_ind = res[[algo_name]]$mbo.result$best.ind  # get the dob of the pareto optimal

  so_inbag = res_all$gperf_env[[algo_name]][[best_ind]][["inbox"]]
  best.list_inbag = lapply(so_inbag, function(x) x[meas_name])
  sodt_inbag = data.table::as.data.table(best.list_inbag)
  sodt_inbag$bag = "inbag"

  so_outbag = res_all$gperf_env[[algo_name]][[best_ind]][["outbox"]]
  best.list_outbag = lapply(so_outbag, function(x) x[meas_name])
  sodt_outbag = data.table::as.data.table(best.list_outbag)
  sodt_outbag$bag = "outbag"

  sodt = rbindlist(list(sodt_inbag, sodt_outbag))
  sodt$best_ind = best_ind
  sodt$algo = algo_name
  return(sodt)
}

agg_mo = function(res_all, meas_name = "mmce", algo_name = "fmo_nocv") {
  res = res_all$tune_res
  best_inds = res[[algo_name]]$ind  # get the dob of the pareto optimal
  pareto.list = res_all$gperf_env[[algo_name]][best_inds]

  pareto.list_inbag = lapply(pareto.list, function(res_iter) {
    hh = res_iter[["inbox"]]
    lapply(hh, function(x) x[[meas_name]])})
  dt_inbag = data.table::rbindlist(pareto.list_inbag)
  dt_inbag$bag = "inbag"
  dt_inbag$best_ind = best_inds

  pareto.list_outbag = lapply(pareto.list, function(res_iter) {
    hh = res_iter[["outbox"]]
    lapply(hh, function(x) x[[meas_name]])})
  dt_outbag = data.table::rbindlist(pareto.list_outbag)
  dt_outbag$bag = "outbag"
  dt_outbag$best_ind = best_inds
  dt = rbindlist(list(dt_inbag, dt_outbag))
  dt$algo = algo_name
  dt
}

reduceResult = function() {
  reslist = reduceResultsList(ids = findDone(), fun = function(job, res) {
    # the replication does not help us aggregate the pareto front!!, it only make sense to aggregate the baseline model
    dt = agg_genTable(res$res)
    dt$repl = job$repl
    dt$dsna = job$prob.pars$dataset_name
    dt$lrn = job$algo.pars$lrn
    return(dt)})
  rbindlist(reslist)
}
