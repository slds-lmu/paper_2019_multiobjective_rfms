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


# oml_task_id: 3891, 14966, 34536
createClassBalancedDfCluster = function(oml_task_id = 14966, n_datasets = 5, balanced = TRUE, pca_var_ratio = 0.7) {
  require(mlr)
  checkmate::assertIntegerish(n_datasets)
  checkmate::assertIntegerish(oml_task_id)
  checkmate::assertLogical(balanced)

  ot = OpenML::getOMLTask(oml_task_id)
  mt = OpenML::convertOMLTaskToMlr(ot)
  df = getTaskData(mt$mlr.task, target.extra = TRUE)

  if (!balanced) {
    checkmate::assert(!"dataset_accn" %in% colnames(df))
    ctsk = makeClusterTask(id = "data_cluster", df$data)
    lrn = makeLearner("cluster.kmeans", centers = n_datasets)
    lrn = makePreprocWrapperCaret(lrn, thresh = pca_var_ratio)
    mod = train(lrn, ctsk)
    prds = predict(mod, ctsk)
    out = sapply(unique(prds$data$response), function(x) which(prds$data$response == x))
  } else {
    desc = mt$mlr.task$task.desc
    pt = lapply(unique(df$target), function(x) {
      data = df$data[df$target == x, ]
      ctsk = makeClusterTask(id = "data_cluster", data)
      lrn = makeLearner("cluster.kmeans", centers = n_datasets)
      lrn = makePreprocWrapperCaret(lrn, thresh = pca_var_ratio)
      mod = train(lrn, ctsk)
      prds = predict(mod, ctsk)
      sapply(unique(prds$data$response), function(x) which(prds$data$response == x))
    })
    out = lapply(seq_len(length(pt[[1]])), function(x) c(pt[[1]][[x]], pt[[2]][[x]]))
  }
  # Out is a list of indices for each dataset
  return(list(task = mt$mlr.task, list_dataset_index = out, df_dataset_accn = out))
}


create_rdata_cluster = function(pca_var_ratio, tids = c(3891, 14966, 34536), n_datasets = 5, balanced = T, prefix = "../Data/temp/oml_") {
  list.tname = lapply(tids, function(x) {
      lst = createClassBalancedDfCluster(x, n_datasets, balanced, pca_var_ratio)
      dflst = lapply(seq_len(length(lst$list_dataset_index)),
        function(i) {
          tsk = subsetTask(lst$task,subset = lst$list_dataset_index[[i]])
          df = getTaskData(tsk)
          df$dataset_accn = paste0("ds", i)
          return(df)
      })
      data = do.call("rbind", dflst)
      filena = paste0(prefix, x, sprintf("_clustered_classbalanced_%s.RData", balanced))
      save(data,  file = filena)
      getTaskTargetNames(lst$task)
  })
  list.tname
}

# This function must be used inside the problem since this method is random, only running one time is not fair.
createRandomStratifPartition = function(taskid = 3891, nsplits = 5) {
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
