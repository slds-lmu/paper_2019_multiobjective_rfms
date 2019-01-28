#' this function depends on the naming of extra.args
funLogPerf2extra.argsEnv = function(list_perf_inbag, list_perf_outbag, list_perf_mixbag, extra.args) {
  env_gperf = extra.args$gperf_env   ## environment  use ls(env) or ls.str(env)
  contextname = get("context", envir = env_gperf)  # we can only use one global variable
  #env_gperf[[as.character(env_gperf$index)]] = list_perf_inbag  # this redundancy is used to fetch the pareto optimal multi-objective candidate
  env_gperf[[contextname]][[as.character(env_gperf$index)]][["inbag"]] =  list_perf_inbag
  env_gperf[[contextname]][[as.character(env_gperf$index)]][["outbag"]] =  list_perf_outbag
  env_gperf[[contextname]][[as.character(env_gperf$index)]][["mixbag"]] =  list_perf_mixbag
  env_gperf$index = env_gperf$index + 1L
}

debug_getPerf4DATAsITES_ORCAL = function() {
  subtask = subsetTask(task, model$subset)
  pred = mlr:::predict.WrappedModel(model, task = subtask)
  performance(pred)
  rsd_out = makeResampleDesc("CV", iters = 10)
  res = resample(learner = "classif.ksvm", task = subtask, resampling = rsd_out, measures = mlr::mmce, show.info = T)  # paramValueToString

  getSingleDatasetPerf(model, subtask)
  extra.args$instance$ns
  extra.args_temp = extra.args
  extra.args_temp$measures2tune = mlr::mmce
  fun_measure_obj_openbox(task = task, model = model, pred = NULL, feats = NULL, extra.args = extra.args_temp) 
}

getPerf4DataSites_Oracle = function(task, model, extra.args) {
  pvs = model$learner$par.vals
  cat(sprintf("\n openbox:%d,%s, lockbox:%d,%s \n", extra.args$instance$openbox_ind, extra.args$instance$openbox_name, extra.args$instance$lockbox_ind, extra.args$instance$lockbox_name))
  cat(sprintf("\n inbag: %s\n", names(extra.args$instance$dataset_index_inbag)))
  list_perf_inbag = lapply(extra.args$instance$dataset_index_inbag, function(subset_ind) {
    subtask = subsetTask(task, subset_ind)
    getSingleDatasetPerf(model, subtask)
  })
  names(list_perf_inbag) = names(extra.args$instance$dataset_index_inbag)

  cat(sprintf("\n outbag: %s\n", names(extra.args$instance$dataset_index_outbag)))
  list_perf_outbag = lapply(extra.args$instance$dataset_index_outbag, function(subset_ind) {
    subtask = subsetTask(task, subset_ind)
    getSingleDatasetPerf(model, subtask)
  })

  names(list_perf_outbag) = names(extra.args$instance$dataset_index_outbag)

  ##if (extra.args$gperf_env$index > getGconf()$MBO_ITERS + getGconf()$INIT_DES - 1L) {
  # FIXME: this can be removed if log is not done for every step
  #   list_perf_mixbag = lapply(extra.args$instance$list_alpha_bootstrap_index, function(alpha_bootstrap_index) {
  #     list_ob_vs_cu = lapply(alpha_bootstrap_index$list_boot, function(culb) {
  #       subtask = subsetTask(task, culb$ob_vs_cu)
  #       perfs = getSingleDatasetPerf(model, subtask, F)
  ### FIXME: change to name mmce instead of [1L]
  #       perfs[1L]
  #     })
  # 
  #     list_ob_vs_lb = lapply(alpha_bootstrap_index$list_boot, function(culb) {
  #       subtask = subsetTask(task, culb$ob_vs_lb)
  #       perfs = getSingleDatasetPerf(model, subtask, F)
  #       perfs[1L]
  #     })
  #     return(list(ob_vs_lb = list_ob_vs_lb, ob_vs_cu = list_ob_vs_cu))
  #   })
  list_perf_mixbag  = NULL

  funLogPerf2extra.argsEnv(list_perf_inbag, list_perf_outbag, list_perf_mixbag, extra.args)
  return(list_perf_inbag)  ## only need to return in-bag performance
}

#' @title measure function calculating the model prediction performance on the remote dataset
#' @description measure as objective function for mbo, given the hyper-parameter, model is fitted on open-box, returns measure on curator-box. Additionally, measure for all datasets are calculated.
#' @param task  mlr task which is combination of open-box, curator-box and lock-box
#' @param model mlr model fitted on the open-box
#' @param pred The prediction on the curator-box and lock-box
#' @param feats features of the dataset
#' @param extra.args additional arguments
#' @return average performance of all curator datasets
fun_measure_obj_curator = function(task, model, pred, feats, extra.args) {
  list.perf = getPerf4DataSites_Oracle(task, model, extra.args)
  sublist_all_perf = list.perf[extra.args$instance$curator_names]
  weight = unlist(extra.args$instance$curator_len_list)
  weight = weight / sum(weight)
  sublist = lapply(sublist_all_perf, function(x) x[extra.args$perf_name2tune])
  vec = unlist(sublist)
  h = sum(vec * weight)
  cat(sprintf("\n curator %s: %f \n", extra.args$perf_name, h))
  return(h)
}

# parameter free version of the Ladder algorithm

calBrierVec = function(pred) {
  truth = pred$data$truth
  prob = getPredictionProbabilities(pred)
  y = as.numeric(truth == pred$task.desc$positive)
  newvec = (y - prob) ^ 2
  return(newvec)
}

fun_ladder_parafree = function(task, model, pred, feats, extra.args) {
  nothing = getPerf4DataSites_Oracle(task, model, extra.args)  # only for log
  gperf_env = extra.args$gperf_env
  pred = predict(model, extra.args$instance$task_curator_inbag)
  newvec = extra.args$calMeasVec(pred)
  oldvec = gperf_env$current_best_loss_vec
  diffvec = newvec - oldvec
  th = sd(diffvec) / (sqrt(extra.args$instance$curator_len))
  new_meas = performance(pred, brier)   # FIXME: weight by datasize?
  gap = gperf_env$current_best_meas - new_meas
  if (gap > th) {
    gperf_env$current_best_meas = new_meas
    gperf_env$current_best_loss_vec = newvec
    cat(sprintf("current best meas %f", gperf_env$current_best_meas))
    return(gperf_env$current_best_meas)
  }
  rn = rnorm(1, sd = 1e-6)
  return(gperf_env$current_best_meas + rn)
}

fun_obj_thresholdout = function(task, model, pred, feats, extra.args) {
  weight = unlist(extra.args$instance$curator_len_list)
  weight = weight / sum(weight)
  if (is.null(extra.args$th_para))
    extra.args$th_para = list("threshold" = 0.02, sigma = 0.03, noise_distribution = "norm", gamma = 0)
  list.perf = getPerf4DataSites_Oracle(task, model, extra.args)
  list.perf.train = list.perf[extra.args$instance$openbox_name]
  list.perf.train = lapply(list.perf.train, function(x) x[[extra.args$perf_name2tune]])
  perf.train = unlist(list.perf.train)
  list.perf.test = list.perf[extra.args$instance$curator_names]
  list.perf.test = lapply(list.perf.test, function(x) x[[extra.args$perf_name2tune]])
  perf.test = unlist(list.perf.test)
  cat(sprintf("\n %s : ", extra.args$perf_name2tune))
  tr = perf.train  # without weight
  te = sum(weight * perf.test)
  threshout(tr, te, thresholdout_params = extra.args$th_para)
}

threshout <- function(train_auc, holdout_auc, thresholdout_params) {
  if (thresholdout_params$noise_distribution == "norm") {
    xi <- rnorm(1, sd = thresholdout_params$sigma)
    eta <- rnorm(1, sd = 4*thresholdout_params$sigma)
  } else if (thresholdout_params$noise_distribution == "laplace") {
    xi <- rlaplace(1, scale = thresholdout_params$sigma / sqrt(2))
    eta <- rlaplace(1, scale = 4*thresholdout_params$sigma / sqrt(2))
  }

  noisy_threshold <- thresholdout_params$threshold + thresholdout_params$gamma
  if (abs(holdout_auc - train_auc) > noisy_threshold + eta) {
    out <- holdout_auc + xi
    # thresholdout_params$budget_utilized <- thresholdout_params$budget_utilized + 1
    # regenerate noise added to the threshold
    # (set gamma = 0, because the initial gamma in the Thresholdout algorithm
    # may be way too large if you're unlucky, in which case the test and
    # train AUC will _never_ be close enough for the algorithm
    # to return any information about the test data...)
    thresholdout_params$gamma <- 0#rlaplace(1, scale = 2*thresholdout_params$sigma)
  } else {
    out <- train_auc
  }
  cat(sprintf("-openbox:%f, curator:%f, thres:%f", train_auc, holdout_auc, out))
  return(out)
}



# obj = alpha * cv(local) +  (1-alpha) error(remote)
# extra.args should contain alpha and the extra.args needed for func_measure_obj_remote
# @param extra.args additional arguments holding the instance index for each dataset, etc
#' extra.args$alpha
fun_measure_obj_openbox_tr_curator_tune = function(task, model, pred, feats, extra.args) {
  obj1 = fun_measure_obj_openbox(task, model, pred, feats, extra.args)  # no need for extra.args
  obj2 = fun_measure_obj_curator(task, model, pred, feats, extra.args)  # the performance for the current model is computed in this measure
  return(extra.args$alpha * obj1 + (1 - extra.args$alpha) * obj2)
}

getBaseLrnNameFromModel = function(model) {
  lrn.id = model$learner$id
  lrn.id = processLrnName(lrn.id)   # remove .preprocess
}

getHyperParFromModel = function(model) {
  baselrn_pvs = model$learner.model$next.model$learner$par.vals
  wrapper_pvs = model$learner$par.vals
  feats = model$features   # constant features removed
  flag_wrap = !is.null(model$learner.model$next.model)
  if (flag_wrap) {
    pvs = list(all = c(baselrn_pvs, wrapper_pvs), baselrn_pvs = baselrn_pvs, wrapper_pvs = wrapper_pvs, feats = feats)  # if is a wraper model
    return(pvs)
  }
  else {
    pvs = getHyperPars(model$learner)
    return(pvs)
  }
}

setWraperHyperPars = function(lrn_obj, pvs) {
  # mlr:::setHyperPars2.BaseWrapper
  ### pvs$all contains hyperpars from learner and wrapper in one single list
  ### pvs$wrapper_pvs is the par for wrapper
  ### pvs$baselrn_pvs is the par for base learner
  ### pvs$feats is the features
  #FIXME: add check to pvs$feats
  if (!is.null(pvs$all)) {
    lrn_obj$features = pvs$feats
    lrn_obj = setHyperPars(lrn_obj, par.vals = pvs$baselrn_pvs)
    getLearnerParVals(lrn_obj)
    return(lrn_obj)
  } else {
    return(setHyperPars(lrn_obj, pvs))
  }
}

fun_measure_obj_openbox = function(task, model, pred, feats, extra.args) {
  #'model$subset  # equivalent to which(df[, dataset_id] == dataset_names[2]) which get the row index for dataset 2
  openbox_task = subsetTask(task, model$subset)
  assert(all(model$subset == extra.args$instance$openbox_inbag_ind))
  lrn.id = getBaseLrnNameFromModel(model)
  #getHyperPars(model) only works for learner # no applicable getHyperPars' applied to an object of class "c('PreprocModel', 'BaseWrapperModel', 'WrappedModel')
  pvs = getHyperParFromModel(model)
  res = getCVPerf(openbox_task = openbox_task, lrn.id = lrn.id, pvs = pvs, measures = extra.args$measures2tune)
  cat(sprintf("\n openbox: %f \n", res$aggr))
  return(res$aggr)
}

#' @title
#' @description
#' @param openbox_task The mlr Task to carry CV
#' @param lrn.id learner id in character
#' @param pvs list of hyper-parameter
#' @return resampling result
getCVPerf = function(openbox_task, lrn.id, pvs, measures, iters = NULL) {
  if (is.null(iters)) iters = getGconf()$CV_ITER
  lrn_obj = GET_LRN(lrn.id)
  #lrn_obj = setHyperPars(lrn_obj, par.vals = pvs$wraper_pvs)
  lrn_obj = setWraperHyperPars(lrn_obj, pvs)
  rsd_out = makeResampleDesc("CV", iters = iters)
  res = resample(learner = lrn_obj, task = openbox_task, resampling = rsd_out, measures = measures, show.info = FALSE)  # paramValueToString
  res # res$aggr give the test mean
}


# the measures here are according to mlr naming, so one could call get("brier")
getSingleDatasetPerf = function(model, subtask, verbose = T) {
  pred = predict(model, subtask)
  list_meas = getGconf()$list_meas
  perf = mlr::performance(pred = pred, measures = list_meas)
  # brier.scaled sometimes is negative
  if (verbose) {
    cat("\n")
    print(perf)
    cat("\n")
  }
  return(perf)
}
