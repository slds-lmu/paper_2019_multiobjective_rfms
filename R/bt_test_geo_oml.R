DEBUG_FLAG = T
source("bt_pre.R")  # download OpenML dataset
source("bt_conf.R")
source("bt_main.R")
REG_FILE_DIR = "../output/reg4Test"
btDelInit(path = REG_FILE_DIR, local = T, force = DEBUG_FLAG)
mgconf = getGconf()
reg_input = batchtools::getDefaultRegistry()
reg_input$cluster.functions = makeClusterFunctionsMulticore()
init(prob_names, prob_inputs_conf, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
unwrap(getJobPars()[problem == "prob_geo"][, .(job.id, prob.pars)])  # GEO dataset
unwrap(getJobPars()[problem == "prob_oml_cluster"][, .(job.id, prob.pars)])  # PCA-KMeans
unwrap(getJobPars()[problem == "prob_oml_stratif"][, .(job.id, prob.pars)])  # Random Stratified(class label) Split


testJob(1)  # since it is sequentially running all algorithm on one problem configuraiton, it takes 20 minutes to finish on a laptop
testJob(601)
