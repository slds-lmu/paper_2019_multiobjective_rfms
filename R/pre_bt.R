# run this file to dump oml tasks from web to prevent future server problem
dumpOMLTask = function(tid = 3891) {
  require(mlr)
  mlr:::setMlrOption("show.info", TRUE)
  ot = OpenML::getOMLTask(tid)
  mt = OpenML::convertOMLTaskToMlr(ot)
  task_mlr = mt$mlr.task
  taskstr = sprintf("meta/oml%sMlrTask.RData", tid)
  save(task_mlr, file = taskstr)
}
prepareDataSite = function(path, dataset_id, targetname) {
  require(BBmisc)
  require(mlr)
  df = load2(path)
  dataset_names = unique(df[, dataset_id])
  list_dataset_index = lapply(1:length(dataset_names), function(i) which(df[, dataset_id] == dataset_names[i]))
  names(list_dataset_index) = dataset_names
  which(colnames(df) == dataset_id)
  df_dataset_accn = df[, dataset_id]
  df[dataset_id] = NULL   # FIXME: add args to allow deletion of other columns
  task = makeClassifTask(id = "holdoutHackTask", data = df, target = targetname)
  return(list(task = task, list_dataset_index = list_dataset_index, df_dataset_accn = df_dataset_accn))
}

source("utilities_datasite.R")
# 3891, 9950, 9981, 14966, 34536
create_rdata_cluster(pca_var_ratio = 0.5, tids = c(3891), n_datasets = 5, balanced = T)

list.data = list()
list.data$geo = prepareDataSite(path = "../Data/data_cohorts_nonGerman.RData", dataset_id = "dataset_accn", targetname = "response")  # this data comes with repository
list.data$oml14966 = prepareDataSite(path = "../Data/temp/14966_balanced_clustered.RData",  dataset_id = "dataset_accn", targetname = "target")
list.data$oml3891 = prepareDataSite(path = "../Data/temp/3891_balanced_clustered.RData",  dataset_id = "dataset_accn", targetname = "target")
prob_inputs_data = list.data  # used for addProblem
