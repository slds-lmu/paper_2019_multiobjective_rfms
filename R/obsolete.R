createImBalancedDf = function(tid) {
  findInlist = function(i, listinst) {
    idx = which(sapply(listinst, function(inst) i %in% inst))  # R python difference
    names(idx)
  }
  require(OpenML)
  taskstr = sprintf("../meta/data_accn_%s.RData", tid)
  list_dataset_index = BBmisc::load2(taskstr)
  sum(sapply(list_dataset_index, function(x) length(unique(x))))
  names(list_dataset_index) = paste0("ds", 1:5)
  length(unique(Reduce(c, lapply(list_dataset_index, unique))))
  # 3891, 14966, 34536
  taskstr = sprintf("../meta/oml%sMlrTask.RData", tid)
  task = BBmisc::load2(taskstr)
  df = getTaskData(task)
  checkmate::assert(!"dataset_accn" %in% colnames(df))
  dsnas = sapply(1:nrow(df), function(i) findInlist(i, list_dataset_index))
  dsnas = unlist(dsnas)
  length(dsnas)
  df$dataset_accn = dsnas
  df_dataset_accn = as.character(df$dataset_accn)
  df_dataset_accn = as.factor(df_dataset_accn)
  # makeResampleInstance always generate the same split which is not wanted
  return(list(task = task, list_dataset_index = list_dataset_index, df_dataset_accn = df_dataset_accn))
}

#' @title
#' @description convert staticProblem and problem designs to generate dynamic part of the problem: {obj, funDesign}
#' @param data is a user defined parameter and could have arbitrary structure
#' @param job is something that the framework need, sigma comes from prob.design parameter of the configuration
#' @param etc parameter comes from prob.design
#' @param major_level The main dataset used for training
#' @param test_level The left dataset(relative index) which the tuning algorithm does not see
#' @example addProblem(name = "prob1",  data = list(field1 = 1, field2 = 2), fun = funGenProb, seed = 1L)
funGenProb = function(data, job, major_level, test_level, dataset_name) {
  tuple = data$list.data[[dataset_name]]
  task = tuple$task
  dataset_index = tuple$list_dataset_index  # list of instance index for each dataset
  train.inds = which(tuple$df_dataset_accn == levels(tuple$df_dataset_accn)[major_level])
  test.inds = which(tuple$df_dataset_accn != levels(tuple$df_dataset_accn)[major_level])
  rins = makeFixedHoldoutInstance(train.inds, test.inds, getTaskSize(task))
  rins$desc$predict = "train" # must be both since some measure aggregation is test
  ns = names(dataset_index)
  mna = ns[major_level]
  tna = setdiff(ns, mna)[test_level]
  sna = setdiff(ns, c(mna, tna))
  tname = mlr::getTaskTargetNames(task)
  p = getTaskNFeats(task)
  dfpair = mlr::getTaskData(task, target.extra = T)
  # dfpair$target, higher level(defined by sort) correspond to negative class of mlr, but positive class in ROCR::prediction function, for more refer to ROCR::prediction documentation
  secondlevel = sort(levels(dfpair$target))[2L]
  bname = mlr::getTaskDesc(task)$negative
  # it seems that secondlevel and bname are the same
  tmf = mlr::subsetTask(task, subset = dataset_index[[mna]])
  tms = mlr::subsetTask(task, subset = Reduce(c, dataset_index[sna]))
  tge = mlr::subsetTask(task, subset = dataset_index[[tna]])
  return(list(task = task, rins = rins, major_level = major_level, test_level = test_level, dataset_index = dataset_index, ns = ns, mna = mna, tna = tna, sna = sna, tmf = tmf, tms = tms, tge = tge, tname = tname, bname = bname, p = p))
}


