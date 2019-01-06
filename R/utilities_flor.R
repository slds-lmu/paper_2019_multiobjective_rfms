# oml_task_id: 3891, 14966, 34536
createBalancedDfCluster = function(oml_task_id = 14966, n_datasets = 5, balanced = TRUE) {

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
    lrn = makePreprocWrapperCaret(lrn, thresh = 0.9)
    mod = train(lrn, ctsk)
    prds = predict(mod, ctsk)
    out = sapply(unique(prds$data$response), function(x) which(prds$data$response == x))
  } else {
    desc = mt$mlr.task$task.desc
    pt = lapply(unique(df$target), function(x) {
      data = df$data[df$target == x, ]
      ctsk = makeClusterTask(id = "data_cluster", data)
      lrn = makeLearner("cluster.kmeans", centers = n_datasets)
      lrn = makePreprocWrapperCaret(lrn, thresh = 0.9)
      mod = train(lrn, ctsk)
      prds = predict(mod, ctsk)
      sapply(unique(prds$data$response), function(x) which(prds$data$response == x))
    })
    out = lapply(seq_len(length(pt[[1]])), function(x) c(pt[[1]][[x]], pt[[2]][[x]]))
  }
  # Out is a list of indices for each dataset
  return(list(task = mt$mlr.task, list_dataset_index = out, df_dataset_accn = out))
}


create_rdata_cluster = function(tids = c(3891, 14966, 34536)) {
  lapply(tids, function(x) {
      lst = createBalancedDfCluster(x, 5, TRUE)
      dflst = lapply(seq_len(length(lst$list_dataset_index)),
        function(i) {
          tsk = subsetTask(lst$task,subset = lst$list_dataset_index[[i]])
          df = getTaskData(tsk)
          df$dataset_accn = paste0("ds", i)
          return(df)
      })
      data = do.call("rbind", dflst)
      save(data,  file = paste0("../Data/", x, "_balanced_clustered.RData"))
  })
}
