# this file is for tmux+vim users
function() {
  source("utilities_datasite.R")
  # 3891, 9950, 9981, 14966, 34536
  task.ids = c(3891, 9950, 9981, 14966, 34536)
  try({OpenML::populateOMLCache(task.ids = task.ids)})
  create_rdata_cluster(pca_var_ratio = 0.7, tids = task.ids, n_datasets = 5, balanced = T)
  list.tname = lapply(task.ids, getOMLTaskTargetName)
  list.tname
}

createInput = function() {
  dataset_names_input = c("oml9981", "oml14966", "oml34536")
# list.data$geo = prepareDataSite(path = "../Data/data_cohorts_nonGerman.RData", dataset_id = "dataset_accn", targetname = "response")  # this data comes with repository
  prob_inputs_data = lapply(1:length(task.ids), function(i) {
    taskid = task.ids[i]
    print(taskid)
    prepareDataSite(path = sprintf("../Data/temp/oml_%s_clustered_classbalanced_TRUE.RData", taskid),  dataset_id = "dataset_accn", targetname = list.tname[[i]])})
  names(prob_inputs_data) = paste0("oml", task.ids)
  prob_inputs_data$oml9950 = NULL
}

debug_local = function() {
  DEBUG_FLAG = T
  source("bt_conf.R")
  source("bt_main.R")
  REG_FILE_DIR = "../output/localDebug"
  btDelInit(local = T, force = DEBUG_FLAG)
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$cluster.functions = makeClusterFunctionsMulticore(ncpus = 64)
  init(prob_names, prob_inputs_data, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
  testJob(1)
}

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
  regname = "../output/oml_9981_14966_34536"
  DEBUG_FLAG = F # if true: use low budget (only 7 iterations of mbo)
  source("bt_conf.R")
  source("bt_main.R")
  REG_FILE_DIR = regname
  btDelInit(local = F)  # type "yEs" here
  ######################################
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$default.resources
  init(prob_names, prob_inputs_data, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
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
