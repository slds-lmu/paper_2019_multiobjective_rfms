source("bt_pre.R")
task.ids = c(14966) # input
prob_inputs_data = createInput(task.ids = task.ids, pca_var_ratio = 0.7, class_balance = T, recluster = T, n_datasets = 5, path_regx = "../Data/temp/oml_%s_clustered_classbalanced_TRUE.RData")
dataset_names_input = paste0("oml", task.ids)
DEBUG_FLAG = F # if true: use low budget (only 7 iterations of mbo) set to F
source("bt_conf.R")
source("bt_main.R")
btInit(path = "registry1", local = F)   # set local=T to allow on local PC running
######################################
mgconf = getGconf()
reg_input = batchtools::getDefaultRegistry()
reg_input$default.resources
init(prob_names, prob_inputs_data, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
#submitJobs(597)
