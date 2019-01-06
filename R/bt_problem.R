if (!exists("prob_inputs_data") || is.null(prob_inputs_data) || length(prob_inputs_data) == 0) {
  stop("please run pre_bt.R to generate the data before running batchtools")
}
funGenProbOracle = function(data, job, openbox_ind, lockbox_ind, dataset_name) {
  tuple = data[[dataset_name]]
  task = tuple$task
  dataset_index = tuple$list_dataset_index  # list of instance index for each dataset
  ns = names(dataset_index)
  mna = ns[openbox_ind]
  tna = setdiff(ns, mna)[lockbox_ind]
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
  return(list(task = task, rins = rins, openbox_ind = openbox_ind, lockbox_ind = lockbox_ind, dataset_index = dataset_index, ns = ns, tmf = tmf, tms = tms, tge = tge, tname = tname, bname = bname, p = p, curator_names = sna, openbox_name = mna, lockbox_name = tna))
}



test_funGenProb = function() {
 instance = funGenProbOracle(data = prob_inputs_data, job = NULL, major_level = 1L, test_level = 1L, dataset_name = "geo")
 source("bt_algo.R")
 algo_thresholdout(instance)
}

prob_names = c("prob")
prob_funs = list()
prob_funs[[prob_names[[1L]]]] = funGenProbOracle
prob_designs = list()
prob_designs[[prob_names[1L]]] = expand.grid(major_level = 1:5, test_level = 1:4, dataset_name = c("geo", "oml14966"), stringsAsFactors = FALSE)
