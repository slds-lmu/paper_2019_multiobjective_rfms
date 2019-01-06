run_local = function() {
  DEBUG_FLAG = F
  source("bt_conf.R")
  source("bt_main.R")
  REG_FILE_DIR = "../output/geo"
  btDelInit(local = T, force = DEBUG_FLAG)
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$cluster.functions = makeClusterFunctionsMulticore(ncpus = 64)
  init(prob_names, prob_inputs, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
  testJob(1)
}

debug_cluster = function() {
  DEBUG_FLAG = T # if true: use low budget (only 7 iterations of mbo)
  source("bt_conf.R")
  source("bt_main.R")
  REG_FILE_DIR = "../output/debug"
  btDelInit(local = F)  # type "yEs" here
  ######################################
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$default.resources
  init(prob_names, prob_inputs, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
  submitJobs(597)
}

run_cluster = function() {
  DEBUG_FLAG = F # if true: use low budget (only 7 iterations of mbo)
  source("bt_conf.R")
  source("bt_main.R")
  REG_FILE_DIR = "../output/geo"
  btDelInit(local = F)  # type "yEs" here
  ######################################
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$default.resources
  init(prob_names, prob_inputs, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
  submitJobs(597)
}


submit_jobs = function() {
  submitJobs(1, resources = list(walltime = 100))
  getStatus()
  unwrap(getJobPars()[1:100, .(algo.pars)])
  submitJobs(597)
  showLog(597)
  index = seq.int(from = 1, to = 1800, by = 30)  # 30 replications
  submitJobs(index)
  submitJobs(index + 1)
  submitJobs(index + 2)
}

post_rs_local = function(i) {
  require(batchtools)
  REG_FILE_DIR = "../output/geo"
  reg = loadRegistry(REG_FILE_DIR, conf.file = NA)
  reg$writeable = T
  submitJobs(1)
  res = loadResult(i)
  compare(res$res)
  submitJobs(ids = findExpired(), resources = list(walltime=14400, memory = 2048))
  submitJobs(501:999, resources = list(walltime=14400, memory = 2048))
  submitJobs(501:999, resources = list(walltime=14400, memory = 2048))
  submitJobs(1:2, resources = list(walltime=3600, memory = 2048))
  submitJobs(seq(1,1800, by = 30)[-1], resources = list(walltime=5000, memory = 2048)) 
}
