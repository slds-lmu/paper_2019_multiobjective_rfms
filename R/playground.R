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
  REG_FILE_DIR = "../output/omlstratif"
  btDelInit(local = T, force = DEBUG_FLAG)
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$cluster.functions = makeClusterFunctionsMulticore(ncpus = 64)
  init(prob_names, prob_inputs_conf, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
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
  init(prob_names, prob_inputs_conf, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
  submitJobs(597)
}

run_cluster = function() {
  regname = "../output/georesponse_alpha"
  regname = "../output/oml14966_alpha"
  regname = "../output/reg_all_jan29"
  regname = "../output/reg_all_jan31"
  regname = "../output/reg_feb2"
  regname = "../output/reg_feb3_rebalnce"
  regname = "../output/reg_feb5_add_randso"
  DEBUG_FLAG = F # if true: use low budget (only 7 iterations of mbo)
  source("bt_conf.R")
  source("bt_main.R")
  REG_FILE_DIR = regname
  btDelInit(path = regname, local = F)  # type "yEs" here
  ######################################
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$default.resources
  init(prob_names, prob_inputs_conf, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
  submitJobs(597)
}


subm = function(probname = "prob_oml_stratif", task_id = 31) {
    # probname = "prob_oml_stratif"
    # task_id = 31
    dt2sub = unwrap(getJobPars()[problem == probname][, .(job.id, prob.pars)])
    ind = (dt2sub$task_id == task_id) 
    dt2sub[ind, ]
=======
    # probname = "prob_oml_stratkif"
    unwrap(getJobPars()[, .(job.id, prob.pars)])[task_id == 3891]
    unwrap(getJobPars()[problem == "prob_oml_cluster", .(job.id, prob.pars)]) # task_id = 31 dt2sub = unwrap(getJobPars()[problem == probname][, .(job.id, prob.pars)])
    ind = (dt2sub$task_id == task_id) 
}

dt = subm(probname = "prob_oml_cluster", task_id = 14966)
dt = subm(probname = "prob_oml_cluster", task_id = 14966)
dt = subm(probname = "prob_oml_stratif", task_id = 3608)

submit_jobs = function() {
  submitJobs(1, resources = list(walltime = 100))
  getStatus()
  unwrap(getJobPars()[1:100, .(algo.pars)])
  submitJobs(597)
  showLog(597)
    unwrap(getJobPars()[problem == "prob_oml_stratif"][, .(prob.pars)])[task_id == 31]
    dt = unwrap(getJobPars()[problem == "prob_oml_stratif"][, .(job.id, prob.pars)])[task_id == 31]
    submitJobs(dt$job.id)
    tosub = getJobPars()[problem == "prob_oml_cluster"]$job.id
    getJobPars()[problem == "prob_oml_stratif"]$job.id
    submitJobs(tosub[1:600])
    unwrap(getJobPars()[problem == "prob_oml_cluster"][, .(prob.pars)])
  index = seq.int(from = 1, to = 1800, by = 30)  # 30 replications
  repl = 1:10
  tosub = c()
  for (i in seq_along(repl)) tosub = c(tosub, index + i)
  tosub2 = intersect(tosub, findNotSubmitted()$job.id)
  submitJobs(tosub2)
  submitJobs(index)
  submitJobs(index + 1)
  submitJobs(index + 2)
}

post_rs_local = function(i) {
  require(batchtools)
  REG_FILE_DIR = "../output/omlstratif"
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
