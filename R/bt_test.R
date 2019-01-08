function() {
  source("utilities_datasite.R")
  # 3891, 9950, 9981, 14966, 34536
  try({OpenML::populateOMLCache(task.ids = c(3891, 9950, 9981, 14966, 34536))})
  list.tname = create_rdata_cluster(pca_var_ratio = 0.7, tids = c(3891, 9950, 9981, 14966, 34536), n_datasets = 5, balanced = T)
  list.tname
}
# prob_Inputs_data depends on bt_prob.R
prob_inputs_data = list()  # used for addProblem
prob_inputs_data$geo = prepareDataSite(path = "../Data/data_cohorts_nonGerman.RData", dataset_id = "dataset_accn", targetname = "response")  # this data comes with repository
#prob_inputs_data$oml3891 = prepareDataSite(path = "../Data/temp/oml_3891_clustered_classbalanced_TRUE.RData",  dataset_id = "dataset_accn", targetname = "label")
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
