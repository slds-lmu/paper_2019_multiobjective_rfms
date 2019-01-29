library(batchtools)
library(mlr)
library(mlrMBO)
if (!dir.exists("../output")) dir.create("../output")
print("conf being sourced")



###  input

### dependencies
pre.source = c("utilities_datasite.R",  "bt_pre.R", "bt_algo.R", "bt_problem.R", "bt_learner_parsets.R", "bt_measures.R", "bt_helpers.R", "bt_obj.R")  # order of the sequence being sourced can not be changed!

lapply(pre.source, function(x) source(x))
pre.source = c(pre.source, "bt_conf.R")
pre.packages = c("mlr", "BBmisc", "mlrMBO")  ## OpenML removed

### General
SEED_REGISTRY = 1273L
###


path = list()
task.ids = c(14966, 34536, 3891, 9950, 9981) # input
path$geo = "../Data/data_cohorts_nonGerman.RData"
path$oml14966 = "../Data/temp/oml_14966_pca_0.1_clustered_classbalanced_TRUE.RData"
path$oml34536 = "../Data/temp/oml_34536_clustered_classbalanced_TRUE.RData"
path$oml3891 = "../Data/temp/oml_3891_clustered_classbalanced_TRUE.RData"
path$oml9950 = "../Data/temp/oml_9950_clustered_classbalanced_TRUE.RData"
path$oml9981 = "../Data/temp/oml_9981_clustered_classbalanced_TRUE.RData"
prob_inputs = list(path = path)


  gconf = list(
    REPLS = 30,
    CV_ITER = 10L,  # iteration for crossval
    predict.type = "response",
    meas2tune = mlr::mmce,
    perf_name2tune = "mmce",
    meas_aggr = mtrain.agg,
    list_meas = list(mmce, ber),
    #list_meas = list(auc, mmce, brier, brier.scaled, ber, logloss)
    CV_ITER_OUTER = 5L, # make it consistent with our 5 split 
    MBO_ITERS = 40L, # 16d
    INIT_DES = 20L  # default 8d
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
