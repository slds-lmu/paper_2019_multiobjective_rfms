# run this file to create input data files to save time for
dumpOMLTask = function(tid = 3891) {
  library(mlr)
  mlr:::setMlrOption("show.info", T)
  ot = OpenML::getOMLTask(tid)
  mt = OpenML::convertOMLTaskToMlr(ot)
  task_mlr = mt$mlr.task
  taskstr = sprintf("meta/oml%sMlrTask.RData", tid)
  save(task_mlr, file = taskstr)
}

createRandStratifPartition = function(taskid = 3891) {
  require(OpenML)
  ot = OpenML::getOMLTask(taskid)
  mt = OpenML::convertOMLTaskToMlr(ot)
  task = mt$mlr.task
  df = getTaskData(task)
  assert(!"dataset_accn" %in% colnames(df))
  desc = task$task.desc
  desc$positive
  desc$negative
  ind_p = which(df[, desc$target] == desc$positive)
  ind_n = which(df[, desc$target] == desc$negative)
  dfp = df[ind_p, ]
  dfn = df[ind_n, ]
  pi = sample(1:nrow(dfp))
  ni = sample(1:nrow(dfn))
  data_accnp = rep(1:5, length.out = nrow(dfp))
  data_accnn = rep(1:5, length.out = nrow(dfn))
  dfp$dataset_accn = data_accnp[pi]
  dfn$dataset_accn = data_accnn[ni]
  df = rbind(dfp, dfn)
  list_dataset_index = lapply(1:5, function(i) which(df[, "dataset_accn"] == i))
  names(list_dataset_index) = as.character(1:5)
  df_dataset_accn = as.character(df$dataset_accn)
  df_dataset_accn = as.factor(df_dataset_accn)
  # makeResampleInstance always generate the same split which is not wanted
  return(list(task = task, list_dataset_index = list_dataset_index, df_dataset_accn = df_dataset_accn))
}


selectOMLDataSet = function() {
  task.ids = OpenML::getOMLStudy("OpenML100")$tasks$task.id
  OpenML::listOMLStudies()
  require(OpenML)
  idt = lapply(task.ids, function(id) {
    ot = OpenML::getOMLTask(id)
    mt = convertOMLTaskToMlr(ot)
    sum(mt$mlr.task$task.desc$n.feat) >  0.2 * (mt$mlr.task$task.desc$size)
   }
   )
  idt = which(unlist(idt))
  idt = task.ids[idt]
  print(idt)
}
