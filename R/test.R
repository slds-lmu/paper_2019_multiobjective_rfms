  DEBUG_FLAG = T
  source("pre_bt.R")
  source("bt_conf.R")
  source("bt_learner_parsets.R")
  source("bt_main.R")
  source("bt_problem.R")
  REG_FILE_DIR = "../output/testreg"
  btDelInit(local = T, force = DEBUG_FLAG)

  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$cluster.functions = makeClusterFunctionsMulticore(ncpus = 64)
  init(prob_names, prob_inputs, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
  testJob(1)

