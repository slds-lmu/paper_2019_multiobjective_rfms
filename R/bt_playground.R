debug_local = function() {
  DEBUG_FLAG = T
  source("bt_conf.R")
  source("bt_utils.R")
  REG_FILE_DIR = "../output/localDebug"
  btDelInit(path = REG_FILE_DIR, local = T, force = DEBUG_FLAG)
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$cluster.functions = makeClusterFunctionsMulticore(ncpus = 64)
  init(prob_names, prob_inputs_conf, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
}

run_local = function() {
  DEBUG_FLAG = F
  source("bt_conf.R")
  source("bt_utils.R")
  REG_FILE_DIR = "../output/ladder_alpha"
  btDelInit(path = REG_FILE_DIR, local = T, force = DEBUG_FLAG)
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$cluster.functions = makeClusterFunctionsMulticore(ncpus = 64)
  init(prob_names, prob_inputs_conf, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
}

debug_cluster = function() {
  DEBUG_FLAG = T # if true: use low budget (only 7 iterations of mbo)
  source("bt_conf.R")
  source("bt_utils.R")
  REG_FILE_DIR = "../output/debug"
  btDelInit(path = REG_FILE_DIR, local = F, force = T)  # type "yEs" here
  ######################################
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$default.resources
  init(prob_names, prob_inputs_conf, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
}

run_cluster = function() {
  regname = "../output/georesponse_alpha"
  DEBUG_FLAG = F # if true: use low budget (only 7 iterations of mbo)
  source("bt_conf.R")
  source("bt_utils.R")
  REG_FILE_DIR = regname
  btDelInit(path = regname, local = F)  # type "yEs" here
  ######################################
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$default.resources
  init(prob_names, prob_inputs_conf, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
}
