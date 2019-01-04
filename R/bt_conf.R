library(batchtools)
library(mlr)
library(mlrMBO)
source("pre_bt.R")

print("conf being sourced")
### dependencies
pre.source = c("bt_algo.R", "bt_problem.R", "bt_learner_parsets.R", "pre_bt.R", "bt_measures_objs.R")
lapply(pre.source, function(x) source(x))
pre.source = c(pre.source, "bt_conf.R")
pre.packages = c("mlr", "BBmisc", "mlrMBO", "OpenML")


#try({OpenML::populateOMLCache(task.ids = c(3891, 14966, 34536))})
### General

#REG_FILE_DIR = ""  # do not change this definition, only change the symbolic link!!
SEED_REGISTRY = 1273L

###### data
# we will use data from 2 sources: a) datamicroarray b) OML
DMA_PACKAGE_NAME = 'datamicroarray'
DMA_DATA_NAMES = c("alon", "borovecki", "chiaretti", "chin", "chowdary", "gordon",
  "gravier", "pomeroy", "shipp", "singh", "subramanian", "tian", "west")
DMA_TARGET_NAME = "class"  # DMA has no name for y col




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
