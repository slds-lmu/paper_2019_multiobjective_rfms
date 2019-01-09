# this file is for tmux+vim users
if (!dir.exists("../Data/temp")) dir.create("../Data/temp")
source("utilities_datasite.R")
createInput = function(task.ids = c(3891), pca_var_ratio = 0.1, class_balance = T, recluster = T, n_datasets = 5, path_regx = "../Data/temp/oml_%s_clustered_classbalanced_TRUE.RData") {

  try({
    OpenML::populateOMLCache(task.ids = task.ids)  # openml can breakdown easily
  })

  if (recluster) create_rdata_cluster(pca_var_ratio = pca_var_ratio, tids = task.ids, n_datasets = 5, balanced = T)

  prob_inputs_data = lapply(1:length(task.ids), function(i) {
    taskid = task.ids[i]
    print(taskid)
    prepareDataSite(path = sprintf(path_regx, taskid))
  })
  names(prob_inputs_data) = paste0("oml", task.ids)
  # prob_inputs_data$oml9950 = NULL
  return(prob_inputs_data)
}

  # 3891, 9950, 9981, 14966, 34536
  #task.ids = c(3891, 9950, 9981, 14966, 34536)
task.ids = c(14966) # input
prob_inputs_data = createInput(task.ids = task.ids, pca_var_ratio = 0.7, class_balance = T, recluster = T, n_datasets = 5, path_regx = "../Data/temp/oml_%s_clustered_classbalanced    _TRUE.RData")
dataset_names_input = paste0("oml", task.ids)
