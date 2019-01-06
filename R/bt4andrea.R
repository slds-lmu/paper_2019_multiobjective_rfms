  DEBUG_FLAG = F # if true: use low budget (only 7 iterations of mbo) set to F
  source("bt_conf.R")
  source("bt_learner_parsets.R")
  source("bt_main.R")
  source("bt_problem.R")
  btInit(path = "registry1", local = F)   # set local=T to allow on local PC running
  ######################################
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$default.resources
  init(prob_names, prob_inputs, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
  #submitJobs(597)
