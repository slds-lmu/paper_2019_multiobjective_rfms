if (!dir.exists("../Data/temp")) dir.create("../Data/temp")

selDataOML = function() {
  library(data.table)
  library(OpenML)
  df = listOMLTasks(task.type = "Supervised Classification", number.of.instances = c(500, 1000), number.of.features = c(100, 500), number.of.missing.values = 0, number.of.classes = 2)
  dt = as.data.table(df)
  c2 = dt[, number.of.features/number.of.instances] > 0.19
  c1 = dt[, minority.class.size / majority.class.size] > 0.5
  dt[c1&c2]
}

#' loadDiskOMLMlrTask(14966)
loadDiskOMLMlrTask = function(task_id, path_regx = "../Data/temp/oml_%s_task_mlr.RData") {
  readRDS(sprintf(path_regx, task_id))
}

#' dumpOMLTasks(14966)
dumpOMLTasks = function(task_ids, path_regx = "../Data/temp/oml_%s_task_mlr.RData") {
  lapply(task_ids, function(oml_task_id) {
    task_mlr = getMlrTaskFromOML(oml_task_id)
    saveRDS(task_mlr, sprintf(path_regx, oml_task_id))
  })
}

prepareDataSite = function(path) {
  require(BBmisc)
  require(mlr)
  tuple = load2(path)
  df = tuple$df
  targetname = tuple$targetname
  dataset_id = tuple$dataset_id
  dataset_names = unique(df[, dataset_id])
  list_dataset_index = lapply(1:length(dataset_names), function(i) which(df[, dataset_id] == dataset_names[i]))
  names(list_dataset_index) = dataset_names
  which(colnames(df) == dataset_id)
  df_dataset_accn = df[, dataset_id]
  df[dataset_id] = NULL   # FIXME: add args to allow deletion of other columns
  task = makeClassifTask(id = "holdoutHackTask", data = df, target = targetname)
  return(list(task = task, list_dataset_index = list_dataset_index, df_dataset_accn = df_dataset_accn))
}

#'getMlrTaskFromOML = function(14966)
getMlrTaskFromOML = function(oml_task_id) {
  require(OpenML)
  ot = OpenML::getOMLTask(oml_task_id)
  mt = OpenML::convertOMLTaskToMlr(ot)
  mlr_task = mt$mlr.task
  return(mlr_task)
}


#'@ description
#'Output is a list of indices for each dataset
#'@example
#'clusterMlrTask(mlr_task = getMlrTaskFromOML(14966), n_datasets = 5, balanced = T, pca_var_ratio = 0.7)
rebalance = function(list_oneclass_index ) {
  lens = sapply(list_oneclass_index, length)
  rel = order(lens, decreasing = F)
  #rel[1] is the smallest cluster
}

clusterMlrTask = function(mlr_task, n_datasets = 5L, balanced = T, pca_var_ratio = 0.7) {
  df = getTaskData(mlr_task, target.extra = TRUE)
  if (!balanced) {
    checkmate::assert(!"dataset_accn" %in% colnames(df))
    ctsk = makeClusterTask(id = "data_cluster", df$data)
    lrn = makeLearner("cluster.kmeans", centers = n_datasets)
    lrn = makePreprocWrapperCaret(lrn, thresh = pca_var_ratio)
    mod = train(lrn, ctsk)
    prds = predict(mod, ctsk)
    list_dataset_index = sapply(unique(prds$data$response), function(x) which(prds$data$response == x))
  } else {
    desc = mlr_task$task.desc
    dftargets = unique(df$target)
    pt = lapply(dftargets, function(x) {
      global_ind = which(df$target == x)
      oneclassdata = df$data[global_ind, ]
      cat(sprintf("\n class %s data size %s\n", x, nrow(oneclassdata)))
      ctsk = makeClusterTask(id = "data_cluster", oneclassdata)
      lrn = makeLearner("cluster.kmeans", centers = n_datasets)
      lrn = makePreprocWrapperCaret(lrn, thresh = pca_var_ratio)
      mod = train(lrn, ctsk)
      prds = predict(mod, ctsk)
      cluster_response = unique(prds$data$response)
      list_oneclass_index_rel = sapply(cluster_response, function(cluster_ind) which(prds$data$response == cluster_ind))
      list_oneclass_index = lapply(list_oneclass_index_rel, function(x) global_ind[x])  # transform to global index
    })
    allinds = c(pt[[1]], pt[[2]])
    checkmate::assert(length(unique(Reduce(c, allinds))) == getTaskSize(mlr_task))
    vec_len1 = sapply(pt[[1L]], length)
    vec_len2 = sapply(pt[[2L]], length)
    checkmate::assert(sum(vec_len1) + sum(vec_len2) == getTaskSize(mlr_task))
    rel1 = order(vec_len1, decreasing = T)
    rel2 = order(vec_len2, decreasing = F)
    list1 = pt[[1]][rel1]
    list2 = pt[[2]][rel2]
    checkmate::assert(length(unique(Reduce(c, c(list1, list2)))) == getTaskSize(mlr_task))
    list_dataset_index = lapply(seq_len(length(pt[[1]])), function(x) c(list1[[x]], list2[[x]]))
    return(list_dataset_index)
  }  # else
  checkmate::assert(sum(sapply(list_dataset_index, length)) == getTaskSize(mlr_task))
  checkmate::assert(length(unique(Reduce(c, list_dataset_index))) == getTaskSize(mlr_task))
  names(list_dataset_index) = paste0("ds", 1:n_datasets)
  return(list_dataset_index)
}

obsolete_create_rdata_cluster4tsne = function(pca_var_ratio, mlr_task, n_datasets = 5, balanced = T, path_regx = "../Data/temp/oml_%s_pca_%s_clustered_classbalanced_%s.RData", tid = NULL, persist = F) {
  dataset_id = "dataset_accn"
  checkmate::assertIntegerish(n_datasets)
  checkmate::assertLogical(balanced)
  list_dataset_index = clusterMlrTask(mlr_task, n_datasets = 5, balanced = balanced, pca_var_ratio = pca_var_ratio)
  # dflst is a permutation of original dataframe
  dflst = lapply(seq_len(length(list_dataset_index)),
    function(i) {
      tsk = subsetTask(mlr_task, subset = list_dataset_index[[i]])
      df = getTaskData(tsk)
      df[dataset_id] = paste0("ds", i)
      return(df)
  })
  data = do.call("rbind", dflst)
  df_feat = data
  tname = getTaskTargetNames(mlr_task)
  df_feat[c(dataset_id, tname)] = NULL
  tuple = list(df = data, targetname = tname, dataset_id = dataset_id, df_feat = df_feat)
  df_dataset_accn = data[, tuple$dataset_id]
  if (persist) {
    filena = sprintf(path_regx, tid, as.character(pca_var_ratio), balanced)
    save(tuple,  file = filena)  # saving data on disk is always a good idea since openml is not robust to download
  }
  tuple$df_dataset_accn = df_dataset_accn
  return(tuple)
}

# This function must be used inside the problem since this method is random, only running one time is not fair.
createRandomStratifPartition = function(task, nsplits = 5, persist = F, path_regx = "../Data/temp/oml_%s_stratified_tuple.RData") {
  df = getTaskData(task)
  # Stratify
  desc = task$task.desc
  dfp = df[which(df[, desc$target] == desc$positive), ]
  dfn = df[which(df[, desc$target] == desc$negative), ]

  posi_rel = sample(seq_len(nrow(dfp)))  # 296 1197  791  778 1751 1066  277  483 1732  847 1002  723  932
  negi_rel = sample(seq_len(nrow(dfn)))

  data_accnp = rep(seq_len(nsplits), length.out = nrow(dfp))  # 1 2 3 4 5 1 2 3 4 5
  data_accnn = rep(seq_len(nsplits), length.out = nrow(dfn))  # 1 2 3 4 5 1 2
  dfp$dataset_accn = data_accnp[posi_rel]   # c(1,2,3, 4, 5, 1, 2,3,4, 5)[3, 9, 7, 1,5], this operation is equivalent as shuffling the 1-5 assignment of each instance
  dfn$dataset_accn = data_accnn[negi_rel]
  df = rbind(dfp, dfn)
  list_dataset_index = split(seq_len(nrow(df)), df$dataset_accn)
  names(list_dataset_index) = paste0("ds", 1:nsplits)
  df_dataset_accn = paste0("ds", df$dataset_accn)
  # list_dataset_index is a list of indices for each dataset.
  # df_dataset_accn    is a vector of which datasite an operation is from.
  tuple = list(task = task, list_dataset_index = list_dataset_index, df_dataset_accn = df_dataset_accn)
  if (persist) saveRDS(tuple, file = sprintf(path_regx, taskid))
  return(tuple)
}
