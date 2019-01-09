source("utilities_datasite.R")
source("bt_pre.R")
task.ids = c(14966) # input
prob_inputs_data = createInput(task.ids = task.ids, pca_var_ratio = 0.7, class_balance = T, recluster = T, n_datasets = 5, path_regx = "../Data/temp/oml_%s_clustered_classbalanced_TRUE.RData")
dataset_names_input = paste0("oml", task.ids)
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
