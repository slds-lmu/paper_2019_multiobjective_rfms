library(BBmisc)
library(mlr)
library(mlrMBO)
library(magrittr)
library(ggplot2)
source("bt_measures_objs.R")
source("bt_helpers.R")

algo_mo = function(instance, lrn, mbo_design, list_measures, gperf_env, context) {
  gperf_env$context =  context
  gperf_env$index = 1
  mgconf = getGconf()
  ctrl_pr = getTuneMethod("mbomulticritdefault", mgconf = mgconf)
  ctrl_pr$mbo.design = mbo_design  # use the same design
  res = mlr::tuneParamsMultiCrit(learner = GET_LRN(lrn), task = instance$task, resampling = instance$rins, measures = list_measures, par.set = GET_PARSET_CLASSIF(lrn), control = ctrl_pr, show.info = TRUE)
  res
}

algo_so = function(instance, lrn, mbo_design, list_measures, gperf_env, context) {
  mgconf = getGconf()
  gperf_env$context =  context
  gperf_env$index = 1
  ctrl_bs = getTuneMethod("mbodefault", mgconf = mgconf)
  ctrl_bs$mbo.design = mbo_design
  tune_res_bs = mlr::tuneParams(learner = GET_LRN(lrn), task = instance$task, resampling = instance$rins, measures = list_measures, par.set = GET_PARSET_CLASSIF(lrn), control = ctrl_bs, show.info = TRUE)  # first of the list_measures are being tuned
  cat(sprintf("\n %s finished  \n", context))
  return(tune_res_bs = tune_res_bs)
}



# we can only have one global variable here since we need to share the measure, we need a context object to know which algorithm we are using
algo = function(instance, lrn, alpha = 0.5) {
  gperf_env = new.env()   # gperf_env is only being modified in side measure function!
  ptmi = proc.time()
  mbo_design = getMBODesign(lrn, getGconf())   # design is regenerated each time algorithm is run
  extra.args = list(instance = instance, gperf_env = gperf_env, alpha = alpha, perf_name2tune = "brier", measures2tune = mlr::brier)  # alpha is used in fso

  meas_openbox_cv = mk_measure(name = "meas_openbox_cv", extra.args, obj_fun = fun_measure_obj_openbox)
  meas_openbox_nocv = mk_measure(name = "meas_openbox_nocv", extra.args, obj_fun = fun_measure_obj_openbox_nocv)
  measure_curator = mk_measure(name = "meas_curator", extra.args = extra.args, obj_fun = fun_measure_obj_curator)
  measure_th = mk_measure(name = "thresholdout", extra.args = extra.args, obj_fun = fun_obj_thresholdout)

 tune_res_fso_th_auc = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(measure_th), gperf_env = gperf_env, context = "fso_th_auc")  
  print(proc.time() - ptmi)

  tune_res_lso_openbox = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_openbox_cv, measure_curator), gperf_env = gperf_env, context = "lso_openbox")  # add mmce?
  print(proc.time() - ptmi)

  tune_res_rso_curator = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(measure_curator), gperf_env = gperf_env, context = "rso_curator")  # add mmce?
  print(proc.time() - ptmi)

  tune_res_lso_openbox_nocv = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_openbox_nocv, measure_curator), gperf_env = gperf_env, context = "lso_openbo_nocv") # add mmce to prove result? 
  print(proc.time() - ptmi)

  meas_alpha_so = mk_measure(name = "meas_alpha_so", extra.args = extra.args, obj_fun = fun_measure_obj_openbox_tr_curator_tune)

  tune_res_fso = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_alpha_so), gperf_env = gperf_env, context = "fso")  # add mmce to double check result? 

  agg_so = function(res, meas_name = "mmce", algo_name = "fso") {
    best_ind = res[[paste0("tune_res_", algo_name)]]$mbo.result$best.ind  # get the dob of the pareto optimal
    so = res$gperf_env[[algo_name]][best_ind]
    best.list = lapply(so, function(res_iter) {
      lapply(res_iter, function(x) x[[meas_name]])})
    sodt = data.table::rbindlist(best.list)
    sodt$algo = algo_name
    return(sodt)
  }


  ### MultiObj
  tune_res_fmo = algo_mo(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_openbox_cv, measure_curator), gperf_env = gperf_env, context = "fmo")
  print("fmo finished:")

  tune_res_fmo_nocv = algo_mo(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_openbox_nocv, measure_curator), gperf_env = gperf_env, context = "fmo_nocv")
  print("fmo_nocv finished:")

  agg_mo = function(res, meas_name = "mmce", algo_name = "fmo_nocv") {
    ind = res[[paste0("tune_res_", algo_name)]]$ind  # get the dob of the pareto optimal
    pareto.list = res$gperf_env[[algo_name]][ind]
    pareto.list = lapply(pareto.list, function(res_iter) {
      lapply(res_iter, function(x) x[[meas_name]])})
    list.dt = data.table::rbindlist(pareto.list)
    list.dt$algo = algo_name
    list.dt
  }

  print("algorithm finished")
  print(proc.time() - ptmi)
  res = list(tune_res_fso = tune_res_fso, tune_res_lso_openbox = tune_res_lso_openbox, tune_res_rso_curator = tune_res_rso_curator, tune_res_lso_openbox_nocv = tune_res_lso_openbox_nocv, tune_res_fmo = tune_res_fmo, tune_res_fmo_nocv = tune_res_fmo_nocv, tune_res_fso_th_auc = tune_res_fso_th_auc, gperf_env = gperf_env, instance = instance)
  genTable = function(res) {
    agg = function(res) {
      agglist = list()
      agglist$fso = agg_so(res, algo_name = "fso")
      agglist$lso_openbox = agg_so(res, algo_name = "lso_openbox")
      agglist$rso_curator = agg_so(res, algo_name = "rso_curator")
      agglist$fso_thauc = agg_so(res, algo_name = "fso_th_auc")
      agglist$fmo = agg_mo(res, algo_name = "fmo")
      agglist$fmo_nocv = agg_mo(res, algo_name = "fmo_nocv")
      rbindlist(agglist)
    }
    lrn.id = res$tune_res_fso$learner$id
    dt = agg(res)
    instance = res$instance
    dt$openbox_name = instance$openbox_name
    dt$lockbox_name = instance$lockbox_name
    dt$curator = apply(as.data.frame(dt)[, instance$curator_names], 1, FUN = mean)
    dt$openbox = as.vector(as.matrix(as.data.frame(dt)[, instance$openbox_name]))
    dt$lockbox = as.vector(as.matrix(as.data.frame(dt)[, instance$lockbox]))
    dt$lrn = lrn.id
    dt
    #  listofrow = apply(dt,1,as.list)
  }
  tb = genTable(res)
  res$tb = tb
  return(res)
}

#major_level = instance$major_level, test_name = getTestName(ns = instance$ns, major_level = instance$major_level, test_level = instance$test_level), test_level = instance$test_level, perf_side_bs = perf_side_bs1, perf_side_pr = perf_side_pr, 
#best_ind = tune_res_lso_openbox$mbo.result$best.ind #tune_res_lso$mbo.result$y

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

algo_names = c("proposal_mbo", algo_names)
algoaggs[[algo_names[1L]]] = function(res) {
  list()
}

algo_designs[[algo_names[1L]]] = data.frame(lrn = c("classif.ksvm", "classif.ranger", "classif.glmnet"), stringsAsFactors = FALSE)

algo_funs[[algo_names[1L]]] = function(job, data, instance, lrn) {
    res = algo(instance = instance, lrn = lrn)
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

getTestName = function(ns,  major_level, test_level) {
  ns1 = setdiff(ns, ns[major_level])
  ns1[test_level]
}

getOpenBox2CuratorBoxInd = function(instance) {
  ns = instance$ns
  major_level = instance$major_level
  test_level = instance$test_level
  tn = getTestName(ns = ns, major_level = major_level, test_level = test_level)
  msnas = setdiff(ns, c(tn, ns[major_level]))  # model selection dataset names
  indx = unlist(instance$dataset_index[c(msnas, ns[major_level])])
  # FIXME: indx has weird names
  return(list(local2remote_subset = indx))
}


reduceResult = function() {
  reslist = reduceResultsList(ids = findDone(), fun = function(job, res) {
    # the replication does not help us aggregate the pareto front!!, it only make sense to aggregate the baseline model
    dt = genTable(res$res)
    dt$repl = job$repl
    dt$dsna = job$prob.pars$dataset_name
    dt$lrn = job$algo.pars$lrn
    return(dt)})
  rbindlist(reslist)
}


