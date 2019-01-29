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


  gconf = list(
    REPLS = 10,
    CV_ITER = 10L,  # iteration for crossval
    predict.type = "response",
    meas2tune = mlr::mmce,
    perf_name2tune = "mmce",
    meas_aggr = mtrain.agg,
    list_meas = list(mmce, ber),
    #list_meas = list(auc, mmce, brier, brier.scaled, ber, logloss)
    CV_ITER_OUTER = 5L, # make it consistent with our 5 split 
    MBO_ITERS = 40L, # 16d
    INIT_DES = 20L,  # default 8d
    task.ids = c(14966, 3891) # input
# 14966 bioresponse
# 3891 gina-agnostic
# 9950 micro-mass  (20 classes)
# 9981 cnae-9 (9 class to 2 class)
# 167125 internet ads
)
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
pre.source = c(pre.source, "bt_conf.R")
pre.packages = c("mlr", "BBmisc", "mlrMBO")  ## OpenML removed

### General
SEED_REGISTRY = 1273L
###


path = list()
path$geo = "../Data/data_cohorts_nonGerman.RData"
#path$oml14966 = "../Data/temp/oml_14966_pca_0.1_clustered_classbalanced_TRUE.RData"
prob_inputs_conf = list(path = path)
