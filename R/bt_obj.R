#' @title measure function calculating the model prediction performance on the remote dataset
#' @description measure as objective function for mbo, given the hyper-parameter, model is fitted on open-box, returns mmce on curator-box. Additionally, measure for all datasets are calculated.
#' @param task  mlr task which is combination of open-box, curator-box and lock-box
#' @param model mlr model fitted on the open-box
#' @param pred The prediction on the curator-box and lock-box
#' @param feats features of the dataset
#' @param extra.args additional arguments
#' extra.args$subset_inds: holding the instance index for each dataset
#' extra.args$major_level
#' extra.args$test_level
#' extra.args$gperf_side
#' @return average mmce of all curator datasets
fun_measure_obj_curator = function(task, model, pred, feats, extra.args) {
pvs = model$learner$par.vals
list_dsname_insid = extra.args$instance$dataset_index
  list.res = lapply(list_dsname_insid, function(subset_ind) {
    subtask = subsetTask(task, subset_ind)
    getSingleDatasetPerf(model, subtask)$brier_score
  })
  names(list.res) = names(extra.args$instance$dataset_index)
  res = fun4extra.args(list.res, extra.args)
  cat(sprintf("\n curator: %f \n", res))
  return(res)
}

getListRes = function(task, model, extra.args) {
  pvs = model$learner$par.vals
  list_dsname_insid = extra.args$instance$dataset_index
  list.res = lapply(list_dsname_insid, function(subset_ind) {
    subtask = subsetTask(task, subset_ind)
    getSingleDatasetPerf(model, subtask)$auc
  })
  names(list.res) = names(extra.args$instance$dataset_index)
  return(list.res)
}

#' this function depends on the naming of extra.args
fun4extra.args = function(list.res, extra.args) {
  env_gperf = extra.args$gperf_env   ## environment  use ls(env) or ls.str(env)
  contextname = get("context", envir = env_gperf)  # we can only use one global variable
  #' extra.args$ns[extra.args$major_level]
  env_gperf[[as.character(env_gperf$index)]] = list.res  # this redundancy is used to fetch the pareto optimal multi-objective candidate
  env_gperf[[contextname]][[as.character(env_gperf$index)]] =  list.res
  env_gperf$index = env_gperf$index + 1L

  major_name = extra.args$instance$mna
  test_name = extra.args$instance$tna
  curator_name = extra.args$instance$sna
  res = unlist(list.res)[curator_name]
  h = mean(res)
  return(h)
}

fun_obj_thresholdout_auc = function(task, model, pred, feats, extra.args) {
  if (is.null(extra.args$th_para))
    extra.args$th_para = list("threshold" = 0.02, sigma = 0.03, noise_distribution = "norm", gamma = 0)
  reslist = getListRes(task, model, extra.args)
  perf.train = reslist[extra.args$instance$mna]
  perf.train = unlist(perf.train)
  perf.test = reslist[extra.args$instance$sna]
  perf.test = unlist(perf.test)
  # Compute train auc
  #pred.train = predict(model, task, subset = setdiff(seq_len(task$task.desc$size), pred$data$id))
  #perf.train = auc$fun(task, model, pred.train, feats, extra.args)
  # Compute test auc
  #perf.test  = auc$fun(task, model, pred, feats, extra.args)
  # Compute threshold auc
  fun4extra.args(reslist, extra.args)  # for log of results
  threshout_auc(mean(perf.train), mean(perf.test), thresholdout_params = extra.args$th_para)
  }

threshout_auc <- function(train_auc, holdout_auc, thresholdout_params) {
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
  cat(sprintf("openbox-auc:%f, curator-auc:%f, thresauc:%f", train_auc, holdout_auc, out))
  return(out)
}



# @description
# given a hyper-parameter set, calculate the cross validation aggregated mmce on the major dataset

fun_measure_obj_openbox_nocv = function(task, model, pred, feats, extra.args) {
  #'model$subset  # equivalent to which(df[, dataset_id] == dataset_names[2]) which get the row index for dataset 2
  major_task = subsetTask(task, model$subset)
  lrn.id = getLrnIDFromModel(model)
  pvs = getHyperParFromModel(model)
  model = getModelFromTask(major_task = major_task, lrn.id = lrn.id, pvs = pvs)
  pred = predict(model, major_task)
  mlr::performance(pred, measures = list(brier))
}

getLrnIDFromModel = function(model) {
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
  major_task = subsetTask(task, model$subset)
  lrn.id = getLrnIDFromModel(model)
  #getHyperPars(model) only works for learner # no applicable getHyperPars' applied to an object of class "c('PreprocModel', 'BaseWrapperModel', 'WrappedModel')
  pvs = getHyperParFromModel(model)
  res = getCVPerf(major_task = major_task, lrn.id = lrn.id, pvs)
  cat(sprintf("\n openbox: %f \n", res$aggr))
  return(res$aggr)
}

# obj = alpha * cv(local) +  (1-alpha) error(remote)
# extra.args should contain alpha and the extra.args needed for func_measure_obj_remote
# @param extra.args additional arguments holding the instance index for each dataset, etc
#' extra.args$alpha
#' extra.args$subset_inds
#' extra.args$major_level
#' extra.args$gperf_side
#' extra.args$test_level
fun_measure_obj_openbox_tr_curator_tune = function(task, model, pred, feats, extra.args) {
  obj1 = fun_measure_obj_openbox(task, model, pred, feats, extra.args)  # no need for extra.args
  obj2 = fun_measure_obj_curator(task, model, pred, feats, extra.args)  # the performance for the current model is computed in this measure
  return(extra.args$alpha * obj1 + (1 - extra.args$alpha * obj2))
}

fun_measure_obj_local_tr_tune_remote_tune_nocv = function(task, model, pred, feats, extra.args) {
  obj1 = fun_measure_obj_openbox_nocv(task, model, pred, feats, extra.args)  # no need for extra.args
  obj2 = fun_measure_obj_curator(task, model, pred, feats, extra.args)  # the performance for the current model is computed in this measure
  return(extra.args$alpha * obj1 + (1 - extra.args$alpha * obj2))
}



# model is from training only on the major dataset
fun_measure_obj_openbox2curator = function(task, model, pred, feats, extra.args) {
  major_task = subsetTask(task, extra.args$local2remote_subset)   # pool the local and remote dataset together
  lrn.id = model$learner$id
  pvs = model$learner$par.vals  # it does not matter there to the local trained model to extract the hyper-parameter since the hyper-parameter is decided by the tunner, not the local model.
  newmodel = getModelFromTask(major_task = major_task, lrn.id = lrn.id, pvs = pvs)
  
  fun_measure_obj_curator(task = task, model = newmodel, pred = pred, feats = feats, extra.args = extra.args)  # call the measure to update the environment, current context is bs2, only chagne is model here

  res = getCVPerf(major_task = major_task, lrn.id = lrn.id, pvs)
  return(res$aggr)
}

#' @title
#' @description
#' @param major_task The mlr Task to carry CV
#' @param lrn.id learner id in character
#' @param pvs list of hyper-parameter
#' @return resampling result
getCVPerf = function(major_task, lrn.id, pvs) {
  lrn_obj = GET_LRN(lrn.id)
  #lrn_obj = setHyperPars(lrn_obj, par.vals = pvs$wraper_pvs)
  lrn_obj = setWraperHyperPars(lrn_obj, pvs)
  rsd_out = makeResampleDesc("CV", iters = gconf$CV_ITER)
  res = resample(learner = lrn_obj, task = major_task, resampling = rsd_out, measures = mlr::brier, show.info = FALSE)  # paramValueToString
  res # res$aggr give the test mean
}

getSingleDatasetPerf = function(model, subtask) {
  pred = predict(model, subtask)
  # getCVPerf(subtask, "classif.ksvm", pvs = model$learner$par.vals) when predict mmce is 1, the cv score can be 0.4, why is this phenomena?
  # model = getModel(major_task = subtask, lrn.id = "classif.ksvm", pvs = model$learner$par.vals)
  auc = measureAUC(probabilities = getPredictionProbabilities(pred), truth = pred$data$truth, negative = pred$task.desc$negative, positive = pred$task.desc$positive)
  cat(sprintf("--auc is %s -- ", auc))
  brier_score = measureBrier(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
  cat(sprintf("--brier_score %s -- ", brier_score))
  tb = table(getPredictionResponse(pred), getPredictionTruth(pred))
  mmce = 1 - sum(diag(tb)) / sum(tb)
  cat(sprintf("--mmce: %s --", mmce))
  return(list(mmce = mmce, brier_score = brier_score, auc = auc))
}


