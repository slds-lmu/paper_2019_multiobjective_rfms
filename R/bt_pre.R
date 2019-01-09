# this file is for tmux+vim users
if (!dir.exists("../Data/temp")) dir.create("../Data/temp")
if (!dir.exists("../output")) dir.create("../output")
source("utilities_datasite.R")
  # 3891, 9950, 9981, 14966, 34536
  #task.ids = c(3891, 9950, 9981, 14966, 34536)
try({
  OpenML::populateOMLCache(task.ids = task.ids)})
create_rdata_cluster(pca_var_ratio = 0.7, tids = task.ids, n_datasets = 5, balanced = T)

  prob_inputs_data = lapply(1:length(task.ids), function(i) {
    taskid = task.ids[i]
    print(taskid)
    prepareDataSite(path = sprintf("../Data/temp/oml_%s_clustered_classbalanced_TRUE.RData", taskid))})
  names(prob_inputs_data) = paste0("oml", task.ids)
  prob_inputs_data$oml9950 = NULL
