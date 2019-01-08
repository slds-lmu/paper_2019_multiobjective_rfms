if(!dir.exists("../Data/temp")) dir.create("../Data/temp")
if(!dir.exists("../output")) dir.create("../output")


library(batchtools)
library(mlr)
library(mlrMBO)

print("conf being sourced")
### dependencies
pre.source = c("utilities_datasite.R", "bt_algo.R", "bt_problem.R", "bt_learner_parsets.R", "bt_measures_objs.R", "bt_helpers.R", "bt_obj.R")  # sequence being sourced can not be changed!
lapply(pre.source, function(x) source(x))
pre.source = c(pre.source, "bt_conf.R")
pre.packages = c("mlr", "BBmisc", "mlrMBO")  ## OpenML removed

### General
SEED_REGISTRY = 1273L
###
  gconf = list(
    REPLS = 30,
    CV_ITER = 10L,  # iteration for crossval
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
