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

#if (!exists("list.data") || is.null(list.data) || length(list.data) == 0) {#}

list.data = list()
list.data$geo = prepareDataSite(path = "../Data/data_cohorts_nonGerman.RData", dataset_id = "dataset_accn", targetname = "response")
list.data$oml14966 = prepareDataSite(path = "../Data/temp/14966_balanced_clustered.RData",  dataset_id = "dataset_accn", targetname = "target")


prob_inputs = list(omlid = c(3891, 9950, 9981, 14966, 34536), list.data = list.data)  # prob_inputs will be called in addProblem, any data can be carried there.
funGenProb = function(data, job, major_level, test_level, dataset_name) {
  tuple = data$list.data[[dataset_name]]
  task = tuple$task
  dataset_index = tuple$list_dataset_index  # list of instance index for each dataset
  ns = names(dataset_index)
  mna = ns[major_level]
  tna = setdiff(ns, mna)[test_level]
  sna = setdiff(ns, c(mna, tna))
  train.inds = which(tuple$df_dataset_accn == mna)
  test.inds = which(tuple$df_dataset_accn %in% c(tna, sna))
  rins = makeFixedHoldoutInstance(train.inds, test.inds, getTaskSize(task))
  rins$desc$predict = "train" # must be both since some measure aggregation is test
  tname = mlr::getTaskTargetNames(task)
  p = getTaskNFeats(task)
  dfpair = mlr::getTaskData(task, target.extra = T) # dfpair$target, higher level(defined by sort) correspond to negative class of mlr, but positive class in ROCR::prediction function, for more refer to ROCR::prediction documentation
  secondlevel = sort(levels(dfpair$target))[2L]
  bname = mlr::getTaskDesc(task)$negative
  # it seems that secondlevel and bname are the same
  tmf = mlr::subsetTask(task, subset = dataset_index[[mna]])
  tms = mlr::subsetTask(task, subset = Reduce(c, dataset_index[sna]))
  tge = mlr::subsetTask(task, subset = dataset_index[[tna]])
  return(list(task = task, rins = rins, major_level = major_level, test_level = test_level, dataset_index = dataset_index, ns = ns, mna = mna, tna = tna, sna = sna, tmf = tmf, tms = tms, tge = tge, tname = tname, bname = bname, p = p, curator_names = sna, openbox_name = mna, lockbox_name = tna))
}



test_funGenProb = function() {
 instance = funGenProb(data = prob_inputs, job = NULL, major_level = 1L, test_level = 1L, dataset_name = "geo")
 source("bt_algo.R")
 algo_thresholdout(instance)
}

prob_names = c("prob")
prob_funs = list()
prob_funs[[prob_names[[1L]]]] = funGenProb
prob_designs = list()
prob_designs[[prob_names[1L]]] = expand.grid(major_level = 1:5, test_level = 1:4, dataset_name = c("geo", "oml14966"), stringsAsFactors = FALSE)
