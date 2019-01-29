DEBUG_FLAG = T
source("bt_conf.R")
source("bt_main.R")
REG_FILE_DIR = "../output/only4testreg"
btDelInit(local = T, force = DEBUG_FLAG)
mgconf = getGconf()
reg_input = batchtools::getDefaultRegistry()
reg_input$cluster.functions = makeClusterFunctionsMulticore()
init(prob_names, prob_inputs_conf, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
testJob(1714)
getJobPars()[problem == "prob_geo"][, .(prob.pars)]
unwrap(getJobPars()[problem == "prob_oml_cluster"][, .(prob.pars)])
