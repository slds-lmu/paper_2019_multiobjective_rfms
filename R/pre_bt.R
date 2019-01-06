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


list.data = list()
list.data$geo = prepareDataSite(path = "../Data/data_cohorts_nonGerman.RData", dataset_id = "dataset_accn", targetname = "response")
list.data$oml14966 = prepareDataSite(path = "../Data/temp/14966_balanced_clustered.RData",  dataset_id = "dataset_accn", targetname = "target")
prob_inputs_data = list.data
#prob_inputs = list(omlid = c(3891, 9950, 9981, 14966, 34536), list.data = list.data)  # prob_inputs will be called in addProblem, any data can be carried there.



# run this file to create input data files to save time for
dumpOMLTask = function(tid = 3891) {
  require(mlr)
  mlr:::setMlrOption("show.info", TRUE)
  ot = OpenML::getOMLTask(tid)
  mt = OpenML::convertOMLTaskToMlr(ot)
  task_mlr = mt$mlr.task
  taskstr = sprintf("meta/oml%sMlrTask.RData", tid)
  save(task_mlr, file = taskstr)
}

createRandStratifPartition = function(taskid = 3891, nsplits = 5) {
  # Download Task
  require(OpenML)
  ot = OpenML::getOMLTask(taskid)
  mt = OpenML::convertOMLTaskToMlr(ot)
  task = mt$mlr.task
  df = getTaskData(task)

  # Stratify
  desc = task$task.desc
  dfp = df[which(df[, desc$target] == desc$positive), ]
  dfn = df[which(df[, desc$target] == desc$negative), ]

  posi = sample(seq_len(nrow(dfp)))
  negi = sample(seq_len(nrow(dfn)))

  data_accnp = rep(seq_len(nsplits), length.out = nrow(dfp))
  data_accnn = rep(seq_len(nsplits), length.out = nrow(dfn))
  dfp$dataset_accn = data_accnp[posi]
  dfn$dataset_accn = data_accnn[negi]
  df = rbind(dfp, dfn)

  list_dataset_index = split(seq_len(nrow(df)), df$dataset_accn)
  df_dataset_accn = as.factor(as.character(df$dataset_accn))
  
  # list_dataset_index is a list of indices for each dataset.
  # df_dataset_accn    is a vector of which datasite an operation is from.
  return(list(task = task, list_dataset_index = list_dataset_index, df_dataset_accn = df_dataset_accn))
}
