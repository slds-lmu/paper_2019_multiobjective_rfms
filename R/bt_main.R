mkReg = function(reg.name, replace = FALSE, local = FALSE) {
  conf.file = ifelse(local, NA, batchtools:::findConfFile())
  lapply(pre.source, function(x) source(x))
  if (replace == TRUE) unlink(reg.name, recursive = TRUE)
  reg = makeExperimentRegistry(file.dir = reg.name,
    conf.file = conf.file,
    source = pre.source,
    packages = pre.packages,
    seed = SEED_REGISTRY)
  return(reg)
}

btInit = function(path = NULL, src = "bt_experiment.R", local = FALSE) {
 if (is.null(path)) path = REG_FILE_DIR
 lapply(pre.source, function(x) source(x))
 print("path:")
 print(path)
 reg = mkReg(path, replace = FALSE, local = local)  # by default do not replace
 source(src)
 getJobPars(reg = reg)
 return(reg)
}

btDelInit = function(path = NULL, src = "bt_experiment.R", local = FALSE, force = F) {
 if (is.null(path)) path = REG_FILE_DIR
 lapply(pre.source, function(x) source(x))
 print("path:")
 print(path)
 if (!force)  {
    up = readline(prompt = "are you sure you want to delete the registry and make a new one? yEs to continue")
    if (up != "yEs") return(NULL)
 }
 reg = mkReg(path, replace = TRUE, local = local)
 source(src)
 getJobPars(reg = reg)
 return(reg)
}

updateReg = function(reg.name = REG_FILE_DIR, src = "bt_experiment.R") {
 reg = loadRegistry(reg.name)
 source(src)
 getJobPars(reg = reg)
 return(reg)
}

checkStatus = function(reg.name = REG_FILE_DIR) {
 reg = loadRegistry(reg.name)
 getStatus(reg = reg)
}

btProbe = function(reg.name = REG_FILE_DIR) {
  reg = loadRegistry(reg.name)
  vec.algo = as.character(unique(getJobPars(reg = reg)[["algorithm"]]))
  selAlgo = function(x) {
    findExperiments(reg = reg, algo.name == x)[1L]
  }
  lapply(vec.algo, function(x) {
    submitJobs(selAlgo(x), reg = reg)})
  getStatus(reg = reg)
}

loadReg = function(reg.name = REG_FILE_DIR) {
  reg = loadRegistry(reg.name, writeable = TRUE)
  return(reg)
}

info = "\n run 'btDelInit()' to delete and initialize the registry \n run 'updateReg([name])', updating the reg will not delete the result if new jobs are not submitted, and probs and algos are indexed by string, so they only get updated when the exact strings are provided \n run  'btProbe' to  probe algorithms \n"
cat(info)

#' getJobPars(1L)[["algo.pars"]]
#' findExperiments(algo.pars = tune.ctrl.method == "mbo")
