  DEBUG_FLAG = T
  source("bt_conf.R")
  source("bt_main.R")
  REG_FILE_DIR = "../output/testreg"
  btDelInit(local = T, force = DEBUG_FLAG)
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$cluster.functions = makeClusterFunctionsMulticore()
  init(prob_names, prob_inputs, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
  testJob(1)
