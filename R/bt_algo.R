library(BBmisc)
library(mlr)
library(mlrMBO)
library(magrittr)
library(ggplot2)
source("bt_measures.R")
source("bt_helpers.R")

agg_rand_mo = function(res_mbo_all, meas_name = "mmce", algo_name) {
  res = res_mbo_all$tune_res
  best_inds = res[[algo_name]]$ind  # get the dob of the pareto optimal
  pareto.list = res_mbo_all$gperf_env[[algo_name]][best_inds]

  pareto.list_inbag = lapply(pareto.list, function(res_iter) { hh = res_iter[["inbag"]] lapply(hh, function(x) x[[meas_name]])})
  dt_inbag = data.table::rbindlist(pareto.list_inbag)
  dt_inbag$bag = "inbag"
  dt_inbag$best_ind = best_inds

  pareto.list_outbag = lapply(pareto.list, function(res_iter) {
    hh = res_iter[["outbag"]]
    lapply(hh, function(x) x[[meas_name]])})
  dt_outbag = data.table::rbindlist(pareto.list_outbag)
  dt_outbag$bag = "outbag"
  dt_outbag$best_ind = best_inds
  dt = rbindlist(list(dt_inbag, dt_outbag))
  dt$algo = algo_name
  dt
}

algo_rand_mo = function(instance, lrn, list_measures, gperf_env, context) {
  cat(sprintf("\n\n\n %s beginned  \n\n\n", context))
  gperf_env$context =  context
  gperf_env$index = 1
  mgconf = getGconf()
  ctrl_rand = mlr::makeTuneMultiCritControlRandom(maxit = mgconf$MBO_ITERS + mgconf$INIT_DES)
  res = mlr::tuneParamsMultiCrit(learner = GET_LRN(lrn), task = instance$task, resampling = instance$rins, measures = list_measures, par.set = GET_PARSET_CLASSIF(lrn), control = ctrl_rand, show.info = TRUE)
  cat(sprintf("\n %s finished  \n", context))
  res
}

# getModelFromTask = function(major_task, lrn.id, pvs) {
#   lrn_basis = GET_LRN(lrn.id)
#   lrn_basis = setWraperHyperPars(lrn_obj = lrn_basis, pvs = pvs)
#   model = mlr::train(learner = lrn_basis, task = major_task) #   model
# }



selModelPareto = function(tune_res, instance, alpha = 0.5) {
    lrn.id =  tune_res$learner$id
    lrn_base_id = processLrnName(lrn.id)   # remove .preprocess
    lrn_wrap = GET_LRN(lrn_base_id)

    task = instance$task
    task_openbox_inbag = subsetTask(instance$task, instance$openbox_inbag_ind)
    task_curator_inbag = subsetTask(instance$task, instance$curator_inbags_oracle_inds)
    trypar = function(pvs) {
      lrn_wrap$next.learner$par.vals = pvs
      model = train(lrn_wrap, task_openbox_inbag)
      perf_ob = getSingleDatasetPerf(model, task_openbox_inbag, F)
      perf_cu = getSingleDatasetPerf(model, task_curator_inbag, F)
      #FIXME: change 1L to something else
      combo = alpha * perf_ob[1L] + (1 - alpha) * perf_cu[1L]
      combo
    }
    vec = sapply(tune_res$x, trypar)
    print(vec)
    which.min(vec)
}

getAlpha = function(tune_res, alphas, instance) {
  res = sapply(alphas, function(alpha) selModelPareto(tune_res, instance, alpha))
  res
}

agg_mo = function(res_mbo_all, meas_name = "mmce", algo_name) {
  res = res_mbo_all$tune_res
  best_inds = res[[algo_name]]$ind  # get the dob of the pareto optimal
  pareto.list = res_mbo_all$gperf_env[[algo_name]][best_inds]

  pareto.list_inbag = lapply(pareto.list, function(res_iter) {
    hh = res_iter[["inbag"]]
    lapply(hh, function(x) x[[meas_name]])})
  dt_inbag = data.table::rbindlist(pareto.list_inbag)
  dt_inbag$bag = "inbag"
  dt_inbag$best_ind = best_inds

  pareto.list_outbag = lapply(pareto.list, function(res_iter) {
    hh = res_iter[["outbag"]]
    lapply(hh, function(x) x[[meas_name]])})
  dt_outbag = data.table::rbindlist(pareto.list_outbag)
  dt_outbag$bag = "outbag"
  dt_outbag$best_ind = best_inds
  dt = rbindlist(list(dt_inbag, dt_outbag))
  dt$algo = algo_name
  dt
}

algo_mo = function(instance, lrn, mbo_design, list_measures, gperf_env, context, mombomethod = "parego") {
  cat(sprintf("\n\n\n %s beginned  \n\n\n", context))
  gperf_env$context =  context
  gperf_env$index = 1
  mgconf = getGconf()
  ctrl_pr = getTuneMethod("mbomulticritdefault", mgconf = mgconf, mombomethod = mombomethod)
  ctrl_pr$mbo.design = mbo_design  # use the same design
  res = mlr::tuneParamsMultiCrit(learner = GET_LRN(lrn), task = instance$task, resampling = instance$rins, measures = list_measures, par.set = GET_PARSET_CLASSIF(lrn), control = ctrl_pr, show.info = TRUE)
  cat(sprintf("\n %s finished  \n", context))
  res
}



agg_mo_5 = function(res, meas_name = "mmce", algo_name = "mo5") {
  tune_res = res$tune_res
  best_inds = tune_res$ind  # get the dob of the pareto optimal
  pareto.list = res$gperf_env[[algo_name]][best_inds]

  pareto.list_inbag = lapply(pareto.list, function(res_iter) {
    hh = res_iter[["inbag"]]
    lapply(hh, function(x) x[[meas_name]])})
  dt_inbag = data.table::rbindlist(pareto.list_inbag)
  dt_inbag$bag = "inbag"
  dt_inbag$best_ind = best_inds

  pareto.list_outbag = lapply(pareto.list, function(res_iter) {
    hh = res_iter[["outbag"]]
    lapply(hh, function(x) x[[meas_name]])})
  dt_outbag = data.table::rbindlist(pareto.list_outbag)
  dt_outbag$bag = "outbag"
  dt_outbag$best_ind = best_inds
  dt = rbindlist(list(dt_inbag, dt_outbag))
  dt$algo = algo_name
  dt
}

algo_mo_5 = function(instance, lrn = "classif.rpart", mbo_design, gperf_env, context, mombomethod, n_objs = 4) {
  extra.args = list(instance = instance, gperf_env = gperf_env, perf_name2tune = getGconf()$perf_name2tune, measures2tune = getGconf()$meas2tune)
  mo5measlist = mkMultimeasuresList(extra.args)
  cat(sprintf("\n\n\n %s beginned  \n\n\n", context))
  gperf_env$context =  context
  gperf_env$index = 1
  mgconf = getGconf()
  ctrl_5 = getTuneMethod(method.str = "mbo5critdefault", mgconf = mgconf, n.objs = n_objs, mombomethod = mombomethod)  # only 1 dataset left for testing
  ctrl_5$mbo.design = mbo_design  # use the same design
  tune_res = tuneParamsMultiCrit(learner = GET_LRN(lrn), task = instance$task, resampling = instance$rins, measures = mo5measlist, par.set = GET_PARSET_CLASSIF(lrn), control = ctrl_5, show.info = TRUE)
  tune_res
}


agg_so = function(res_mbo_all, meas_name = "mmce", algo_name) {
  res = res_mbo_all$tune_res
  best_ind = res[[algo_name]]$mbo.result$best.ind  # get the dob of the pareto optimal

  so_inbag = res_mbo_all$gperf_env[[algo_name]][[best_ind]][["inbag"]]
  best.list_inbag = lapply(so_inbag, function(x) x[meas_name])
  sodt_inbag = data.table::as.data.table(best.list_inbag)
  sodt_inbag$bag = "inbag"

  so_outbag = res_mbo_all$gperf_env[[algo_name]][[best_ind]][["outbag"]]
  best.list_outbag = lapply(so_outbag, function(x) x[meas_name])
  sodt_outbag = data.table::as.data.table(best.list_outbag)
  sodt_outbag$bag = "outbag"

  sodt = rbindlist(list(sodt_inbag, sodt_outbag))
  sodt$best_ind = best_ind
  sodt$algo = algo_name
  return(sodt)
}

algo_rand_so = function(instance, lrn, list_measures, gperf_env, context) {
  cat(sprintf("\n\n\n %s beginned  \n\n\n", context))
  gperf_env$context =  context
  gperf_env$index = 1
  mgconf = getGconf()
  ctrl_rand = mlr::makeTuneControlRandom(maxit = mgconf$MBO_ITERS + mgconf$INIT_DES)
  res = mlr::tuneParams(learner = GET_LRN(lrn), task = instance$task, resampling = instance$rins, measures = list_measures, par.set = GET_PARSET_CLASSIF(lrn), control = ctrl_rand, show.info = TRUE)
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
  # task_openbox_inbag = subsetTask(instance$task, instance$openbox_inbag_ind)
  # getModelFromTask(task_openbox_inbag, lrn.id = processLrnName(tune_res_bs$learner$id), pvs = tune_res_bs$x)
  tune_res_bs
}


agg_th_family = function(res, meas_name = "mmce", algo_name = "fso_th") {
  tune_res = res$tune_res_all
  best_ind = tune_res[[algo_name]]$mbo.result$best.ind  # get the dob of the pareto optimal
  so_inbag = res$gperf_env[[algo_name]][[best_ind]][["inbag"]]
  best.list_inbag = lapply(so_inbag, function(x) x[meas_name])
  sodt_inbag = data.table::as.data.table(best.list_inbag)
  sodt_inbag$bag = "inbag"

  so_outbag = res$gperf_env[[algo_name]][[best_ind]][["outbag"]]
  best.list_outbag = lapply(so_outbag, function(x) x[meas_name])
  sodt_outbag = data.table::as.data.table(best.list_outbag)
  sodt_outbag$bag = "outbag"

  sodt = rbindlist(list(sodt_inbag, sodt_outbag))
  sodt$best_ind = best_ind
  sodt$algo = algo_name
  return(sodt)
}

algo_th_family = function(instance, lrn, threshold, sigma) {
  res = list()
  gperf_env = new.env()   # gperf_env is only being modified in side measure function!
  ptmi = proc.time()
  mbo_design = getMBODesign(lrn, getGconf())   # design is regenerated each time to avoid bias
  #thresholdout_para = list("threshold" = 0.02, sigma = 0.03, noise_distribution = "norm", gamma = 0)
  thresholdout_para = list("threshold" = threshold, sigma = sigma, noise_distribution = "norm", gamma = 0)
  ###
  gperf_env$current_best_loss_vec = rep(getGconf()$ladder_worst_vec_ele, instance$curator_inbag_len)
  gperf_env$current_best_meas = getGconf()$ladder_worst_vec_ele
  ##
  extra.args = list(instance = instance, gperf_env = gperf_env, perf_name2tune = getGconf()$perf_name2tune, measures2tune = getGconf()$meas2tune, calMeasVec = getGconf()$fun_cal_ladder_vec, th_para = thresholdout_para, ladder_with_noise = F)
  measure_th = mk_measure(name = "thresholdout", extra.args = extra.args, obj_fun = fun_obj_thresholdout)

  context = "alpha5_ladder"
  extra.args$alpha = 0.5
  meas_alpha5_ladder = mk_measure(name = "meas_alpha_5_ladder", extra.args = extra.args, obj_fun = fun_measure_alpha_ladder)
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_alpha5_ladder), gperf_env = gperf_env, context = context)

  context = "alpha2_ladder"
  extra.args$alpha = 0.2
  meas_alpha5_ladder = mk_measure(name = "meas_alpha_2_ladder", extra.args = extra.args, obj_fun = fun_measure_alpha_ladder)
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_alpha5_ladder), gperf_env = gperf_env, context = context)

  context = "fso_th_infamily"
  try({
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(measure_th), gperf_env = gperf_env, context = context)
  print(proc.time() - ptmi)
  })
  return(list(tune_res_all = res, gperf_env = gperf_env, instance = instance))
}


agg_alpha_ladder = function(res) {
  list_res_onejob = lapply(names(res$tune_res), function(algo_name) {
    cat(sprintf("\n algorithm name: %s \n", algo_name))
    return(agg_so(res, algo_name = algo_name))
  })
  names(list_res_onejob) = names(res$tune_res)
  rbindlist(list_res_onejob, use.names = TRUE)
}
algo_alpha_ladder = function(instance, lrn) {
  res = list()
  gperf_env = new.env()   # gperf_env is only being modified in side measure function!
  ptmi = proc.time()
  mbo_design = getMBODesign(lrn, getGconf())   # design is regenerated each time to avoid bias
  thresholdout_para = list("threshold" = 0.005, sigma = 0.005, noise_distribution = "norm", gamma = 0)
  ###
  gperf_env$current_best_loss_vec = rep(getGconf()$ladder_worst_vec_ele, instance$curator_inbag_len)
  gperf_env$current_best_meas = getGconf()$ladder_worst_vec_ele
  ##
  extra.args = list(instance = instance, gperf_env = gperf_env, perf_name2tune = getGconf()$perf_name2tune, measures2tune = getGconf()$meas2tune, calMeasVec = getGconf()$fun_cal_ladder_vec, th_para = thresholdout_para, ladder_with_noise = F)
  measure_th = mk_measure(name = "thresholdout", extra.args = extra.args, obj_fun = fun_obj_thresholdout)

  context = "alpha5_ladder"
  extra.args$alpha = 0.5
  meas_alpha5_ladder = mk_measure(name = "meas_alpha_5_ladder", extra.args = extra.args, obj_fun = fun_measure_alpha_ladder)
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_alpha5_ladder), gperf_env = gperf_env, context = context)

  context = "alpha2_ladder"
  extra.args$alpha = 0.2
  meas_alpha5_ladder = mk_measure(name = "meas_alpha_2_ladder", extra.args = extra.args, obj_fun = fun_measure_alpha_ladder)
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_alpha5_ladder), gperf_env = gperf_env, context = context)

  context = "fso_th"
  try({
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(measure_th), gperf_env = gperf_env, context = context)
  print(proc.time() - ptmi)
  })
  return(list(tune_res_all = res, gperf_env = gperf_env, instance = instance))
}

agg_mbo = function(res) {
  list_res_onejob = lapply(names(res$tune_res), function(algo_name) {
    cat(sprintf("\n algorithm name: %s \n", algo_name))
    if (stringi::stri_detect(algo_name, regex = "mo")) return(agg_mo(res, algo_name = algo_name))
    if (stringi::stri_detect(algo_name, regex = "so")) return(agg_so(res, algo_name = algo_name))
    if (stringi::stri_detect(algo_name, regex = "rand_mo")) return(agg_rand_mo(res, algo_name = algo_name))
    if (stringi::stri_detect(algo_name, regex = "rand")) return(agg_rand_mo(res, algo_name = algo_name))  #FIXME: remove this in the future
    if (stringi::stri_detect(algo_name, regex = "pareto")) return(list())
    stop("algorithm names wrong!")
  })
  names(list_res_onejob) = names(res$tune_res)
  rbindlist(list_res_onejob, use.names = TRUE)
}
algo_mbo = function(instance, lrn) {
  alphas = seq(from = 0.1, to = 0.9, by = 0.1)
  res = list()
  gperf_env = new.env()   # gperf_env is only being modified in side measure function!
  ptmi = proc.time()
  mbo_design = getMBODesign(lrn, getGconf())   # design is regenerated each time to avoid bias
  gperf_env$current_best_loss_vec = rep(getGconf()$ladder_worst_vec_ele, instance$curator_inbag_len)
  gperf_env$current_best_meas = getGconf()$ladder_worst_vec_ele
  extra.args = list(instance = instance, gperf_env = gperf_env, perf_name2tune = getGconf()$perf_name2tune, measures2tune = getGconf()$meas2tune, calMeasVec = getGconf()$fun_cal_ladder_vec, ladder_with_noise = T)

  meas_openbox_cv = mk_measure(name = "meas_openbox_cv", extra.args, obj_fun = fun_measure_obj_openbox)
  measure_curator = mk_measure(name = "meas_curator", extra.args = extra.args, obj_fun = fun_measure_obj_curator)
  measure_th = mk_measure(name = "thresholdout", extra.args = extra.args, obj_fun = fun_obj_thresholdout)
  meas_ladder = mk_measure(name = "meas_ladder", extra.args = extra.args, obj_fun = fun_ladder_parafree)


  context = "rand_so"
  res[[context]] = algo_rand_so(instance = instance, lrn = lrn, list_measures = list(meas_openbox_cv), gperf_env = gperf_env, context = context)

  ### MultiObj
  ## extract best learner from pareto front
  res$pareto = list()

  context = "fmo"
  res[[context]] = algo_mo(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_openbox_cv, measure_curator), gperf_env = gperf_env, context = context)
  res$pareto[[context]] = getAlpha(res[[context]], alphas, instance)

  context = "rand_mo"
  res[[context]] = algo_rand_mo(instance = instance, lrn = lrn, list_measures = list(meas_openbox_cv, measure_curator), gperf_env = gperf_env, context = context)
  res$pareto[[context]] = getAlpha(res[[context]], alphas, instance)

  ## differential privacy
  context = "fso_ladder"
  try({
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_ladder), gperf_env = gperf_env, context = context)
  })


  context = "fso_th"
  try({
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(measure_th), gperf_env = gperf_env, context = context)
  print(proc.time() - ptmi)
  })

  context = "fso5"
  extra.args$alpha = 0.5
  meas_alpha_so = mk_measure(name = "meas_alpha_so5", extra.args = extra.args, obj_fun = fun_measure_obj_openbox_tr_curator_tune)
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_alpha_so), gperf_env = gperf_env, context = context)

  context = "fso2"
  extra.args$alpha = 0.2
  meas_alpha_so = mk_measure(name = "meas_alpha_so2", extra.args = extra.args, obj_fun = fun_measure_obj_openbox_tr_curator_tune)
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_alpha_so), gperf_env = gperf_env, context = context)

  context = "fso8"
  extra.args$alpha = 0.8
  meas_alpha_so = mk_measure(name = "meas_alpha_so8", extra.args = extra.args, obj_fun = fun_measure_obj_openbox_tr_curator_tune)
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_alpha_so), gperf_env = gperf_env, context = context)

  context = "lso"
  res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_openbox_cv, measure_curator), gperf_env = gperf_env, context = context)
  print(proc.time() - ptmi)
 
  #context = "rso_curator"
  # res[[context]] = algo_so(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(measure_curator), gperf_env = gperf_env, context = context) ## very likely to meet t.default(T) argument is not a matrix, but since rso is aspecial case of fso but lso is not a special case of fso
  #print(proc.time() - ptmi)
  print("algorithm finished")
  print(proc.time() - ptmi)
  res_all = list(tune_res = res, gperf_env = gperf_env, instance = instance)
  return(res_all)
}


agg_onlymo = function(res) {
  list_res_onejob = lapply(names(res$tune_res), function(algo_name) {
    cat(sprintf("\n algorithm name: %s \n", algo_name))
    if (stringi::stri_detect(algo_name, regex = "mo")) return(agg_mo(res, algo_name = algo_name))
    if (stringi::stri_detect(algo_name, regex = "pareto")) return(list())
    stop("algorithm names wrong!")
  })
  names(list_res_onejob) = names(res$tune_res)
  rbindlist(list_res_onejob, use.names = TRUE)
}

algo_onlymo = function(instance, lrn, mombomethod) {
  alphas = seq(from = 0.1, to = 0.9, by = 0.1)
  res = list()
  gperf_env = new.env()   # gperf_env is only being modified in side measure function!
  ptmi = proc.time()
  mbo_design = getMBODesign(lrn, getGconf())   # design is regenerated each time to avoid bias
  extra.args = list(instance = instance, gperf_env = gperf_env, perf_name2tune = getGconf()$perf_name2tune, measures2tune = getGconf()$meas2tune)

  meas_openbox_cv = mk_measure(name = "meas_openbox_cv", extra.args = extra.args, obj_fun = fun_measure_obj_openbox)
  measure_curator = mk_measure(name = "meas_curator", extra.args = extra.args, obj_fun = fun_measure_obj_curator)

  context = "fmo"
  res[[context]] = algo_mo(instance = instance, lrn = lrn, mbo_design = mbo_design, list_measures = list(meas_openbox_cv, measure_curator), gperf_env = gperf_env, context = context, mombomethod = mombomethod)
  res$pareto[[context]] = getAlpha(res[[context]], alphas, instance)

  context = "mo5"
  res[[context]] = algo_mo_5(instance = instance, lrn = lrn, mbo_design = mbo_design, gperf_env = gperf_env, context = context, mombomethod = mombomethod)
  res$pareto[[context]] = getAlpha(res[[context]], alphas, instance)

  print("algorithm finished")
  print(proc.time() - ptmi)
  res_all = list(tune_res = res, gperf_env = gperf_env, instance = instance)
  return(res_all)
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

#######


#algo_names = c("only_mo", algo_names)
#algo_designs[[algo_names[1L]]] = expand.grid(lrn = c("classif.glmnet", "classif.ksvm", "classif.ranger"), mombomethod = c("dib", "parego"), stringsAsFactors = FALSE)
#algo_funs[[algo_names[1L]]] = function(job, data, instance, lrn, mombomethod) {
#  res = algo_onlymo(instance = instance, lrn = lrn, mombomethod = mombomethod)
#  return(list(res = res, agg_fun = agg_onlymo))
#  #res = list()
#  #gperf_env = new.env()   # gperf_env is only being modified in side measure function!
#  #mbo_design = getMBODesign(lrn, getGconf())   # design is regenerated each time to avoid bias
#  #context = "mo5"
#  #tune_res = algo_mo_5(instance = instance, lrn = lrn, mbo_design = mbo_design, gperf_env = gperf_env, context = context, mombomethod = mombomethod)
#  #res_tune_gperf_ins = list(tune_res = tune_res, gperf_env = gperf_env, instance = instance)
#  #return(list(res = res_tune_gperf_ins, agg_fun = agg_mo_5))
#}

# algo_names = c("alpha_ladder", algo_names)
# algo_designs[[algo_names[1L]]] = data.frame(lrn = c("classif.glmnet", "classif.ranger", "classif.ksvm"), stringsAsFactors = FALSE)
# algo_funs[[algo_names[1L]]] = function(job, data, instance, lrn) {
#   res = algo_alpha_ladder(instance = instance, lrn = lrn)
#   return(list(res = res, agg_fun = agg_alpha_ladder))
# }
# 
source("bt_algo_thauc.R")

# algo_names = c("th", algo_names)
# algo_designs[[algo_names[1L]]] = expand.grid(lrn = c("classif.glmnet", "classif.ranger", "classif.ksvm"), stringsAsFactors = FALSE, threshold = seq(from = 0.001, to = 0.1, length.out = 3), sigma = seq(from = 0.01, to = 0.1, length.out = 3 ))
# algo_funs[[algo_names[1L]]] = function(job, data, instance, lrn, threshold, sigma) {
#   res = algo_th_family(instance = instance, lrn = lrn, threshold, sigma)
#   return(list(res = res, agg_fun = agg_th_family))
# }
# 
# algo_names = c("mbo", algo_names)
#algo_designs[[algo_names[1L]]] = data.frame(lrn = c("classif.ksvm", "classif.ranger", "classif.glmnet"), stringsAsFactors = FALSE)
#algo_funs[[algo_names[1L]]] = function(job, data, instance, lrn) {
#    res = algo_mbo(instance = instance, lrn = lrn)
#    return(list(res = res, agg_fun = agg_mo))
#}

agg_onejob = function(res, fun = NULL) {
  dt = fun(res)
  instance = res$instance
  dt$openbox_name = instance$openbox_name
  dt$lockbox_name = instance$lockbox_name
  df = as.data.frame(dt)
  dt$openbox = df[, instance$openbox_name]
  dt$lockbox = df[, instance$lockbox_name]
  dt$curator = apply(df[, instance$curator_names], 1, FUN = mean)  # FIXME: change to weighted average
  #lrn.id = res$tune_res[[names(res$tune_res)[1L]]]$learner$id
  #dt$lrn = lrn.id
  return(dt)
#  listofrow = apply(dt,1,as.list)
}

#' library("batchtools"); reg = loadRegistry("../output/georesponse", conf.file = NA, writeable = T); dt_res_geo_response = reduceResult(); saveRDS(dt_res_geo_response, file = "dt_res_geo_response.rds")
reduceResult = function(ids = findDone(), agg_fun = NULL) {
  reslist = reduceResultsList(ids = ids, fun = function(job, res) {
    # the replication does not help us aggregate the pareto front!!, it only make sense to aggregate the baseline model
    if (is.null(agg_fun)) agg_fun = res$agg_fun
    dt = agg_onejob(res$res, fun = agg_fun)
    dt$job_id = job$job.id
    dt$repl = job$repl
    dt$dsna = job$prob.pars$dataset_name
    dt$lrn = job$algo.pars$lrn
    dt$prob = job$problem$name
    dt$general_algo_name = job$algo.name
    dt$algo.pars = rep(list(job$algo.pars), nrow(dt))
    return(dt)
  })
  rbindlist(reslist)
}
