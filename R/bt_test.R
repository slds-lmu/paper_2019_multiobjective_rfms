# prob_Inputs_data depends on bt_prob.R
source("utilities_datasite.R")
prob_inputs_data = list()  # used for addProblem
prob_inputs_data$geo = prepareDataSite(path = "../Data/data_cohorts_nonGerman.RData")  # this data comes with repository
dataset_names_input = c("geo")

  DEBUG_FLAG = T
  source("bt_conf.R")
  source("bt_main.R")
  REG_FILE_DIR = "../output/only4testreg"
  btDelInit(local = T, force = DEBUG_FLAG)
  mgconf = getGconf()
  reg_input = batchtools::getDefaultRegistry()
  reg_input$cluster.functions = makeClusterFunctionsMulticore()
  init(prob_names, prob_inputs_data, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
  testJob(1)
