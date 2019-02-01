library(batchtools)
library(mlr)
library(mlrMBO)
if (!dir.exists("../output")) dir.create("../output")
print("conf being sourced")

###  input
# the aggregation actually does not matter, since the measure does not depend on the train or test, perf.test and perf.train look different since cv is random
mtrain.agg = makeAggregation(id = "mtrain.agg", name = "only calculate train", properties = "req.train", fun = function(task, perf.test, perf.train, measure, group, pred) {
  function() {
    group   # group can be factor(0), but this does not matter
    pred$data$iter  # the whole data is used
    pred$data$response  # it can happen that all response is one class, so AUC is na
    pred$data$set
  }
  cat("\n aggregation of measures:")
  #if(any(is.na(c(perf.train, perf.test)))) browser()
  #cat(sprintf("--perf.test: %s--", perf.test))
  cat(sprintf("--perf.train: %s-- \n", perf.train))
  perf.train
})

# parameter free version of the Ladder algorithm

calBrierVec = function(pred) {
  truth = pred$data$truth
  prob = getPredictionProbabilities(pred)
  y = as.numeric(truth == pred$task.desc$positive)
  newvec = (y - prob) ^ 2
  return(newvec)
}

cal01Vec = function(pred) {
  truth = pred$data$truth
  prob = getPredictionResponse(pred)
  vmatch = as.numeric(truth == pred$task.desc$positive)
  return(vmatch)
}

gconf = list(
  REPLS = 10L,
  CV_ITER = 10L,  # iteration for crossval
  ratio_inbag = 0.8,
  predict.type = "response",
  meas2tune = mlr::mmce,
  perf_name2tune = "mmce",
  meas_aggr = mtrain.agg,
  list_meas = list(mmce, ber),
  ladder_noise = 1e-3,
  ladder_meas = mlr::mmce,
  fun_cal_ladder_vec = cal01Vec,
  ladder_worst_vec_ele = 0,
  thresholdout_para = list("threshold" = 0.02, sigma = 0.03, noise_distribution = "norm", gamma = 0),
  #list_meas = list(auc, mmce, brier, brier.scaled, ber, logloss)
  CV_ITER_OUTER = 5L, # make it consistent with our 5 split 
  MBO_ITERS = 40L, # 16d
  INIT_DES = 20L,  # default 8d
  task.ids = c(14966, 3891, 10101, 31) # input
  # 14966 bioresponse
  # 3891 gina-agnostic
  # 9950 micro-mass  (20 classes)
  # 9981 cnae-9 (9 class to 2 class)
  # 167125 internet ads
  # 10101 5 features, 200 instance
  # 31 21 features, 1000 instance
  # https://www.openml.org/d/1132
)

#' probe(3891)
probe = function(task_id) {
  #task = getMlrTaskFromOML(task_id)
  res = list()
  task = loadDiskOMLMlrTask(task_id)
  res$n  = getTaskSize(task)
  res$p = getTaskNFeats(task)
  res$pn = (res$p / res$n)
  res$pn5 = (res$p / res$n) * 5
  cd = task$task.desc$class.distribution
  res$cd = min(cd) / max(cd)
  res
}

getGconf = function(variables) {
 if (exists("DEBUG_FLAG")) {
   if (get("DEBUG_FLAG") == TRUE) {
     gconf$MBO_ITERS = 2L
     gconf$INIT_DES = 5L
     return(gconf)
   }
 }
 return(gconf)
}


### dependencies
pre.source = c("utilities_datasite.R", "bt_algo.R", "bt_problem.R", "bt_learner_parsets.R", "bt_measures.R", "bt_helpers.R", "bt_obj.R")  # order of the sequence being sourced can not be changed!

lapply(pre.source, function(x) source(x))
pre.source = c("bt_conf.R", pre.source)
pre.packages = c("mlr", "BBmisc", "mlrMBO")  ## OpenML removed

### General
SEED_REGISTRY = 1273L
###


path = list()
path$geo = "../Data/data_cohorts_nonGerman.RData"
prob_inputs_conf = list(path = path)
