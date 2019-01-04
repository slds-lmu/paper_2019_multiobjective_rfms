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


create_rdata_cluster = function() {
  lapply(c(3891, 14966, 34536), function(x) {
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

create_rdata_cluster()

threshout_auc <- function(train_auc, holdout_auc, thresholdout_params) {

  if (thresholdout_params$noise_distribution == "norm") {
    xi <- rnorm(1, sd = thresholdout_params$sigma)
    eta <- rnorm(1, sd = 4*thresholdout_params$sigma)
  } else if (thresholdout_params$noise_distribution == "laplace") {
    xi <- rlaplace(1, scale = thresholdout_params$sigma / sqrt(2))
    eta <- rlaplace(1, scale = 4*thresholdout_params$sigma / sqrt(2))
  }

  noisy_threshold <- thresholdout_params$threshold + thresholdout_params$gamma

  if (abs(holdout_auc - train_auc) > noisy_threshold + eta) {
    out <- holdout_auc + xi
    # thresholdout_params$budget_utilized <- thresholdout_params$budget_utilized + 1
    # regenerate noise added to the threshold
    # (set gamma = 0, because the initial gamma in the Thresholdout algorithm
    # may be way too large if you're unlucky, in which case the test and
    # train AUC will _never_ be close enough for the algorithm
    # to return any information about the test data...)
    thresholdout_params$gamma <- 0#rlaplace(1, scale = 2*thresholdout_params$sigma)
  } else {
    out <- train_auc
  }

  return(out)
}

auc.thout = makeMeasure("thout.auc", properties = auc$properties,
 best = 1, worst = 0, minimize = FALSE,
 fun = function (task, model, pred, feats, extra.args) {
  if(is.null(extra.args))
    extra.args = list("threshold" = 0.02, sigma = 0.03, noise_distribution = "norm")

  # Compute train auc
  pred.train = predict(model, task, subset = setdiff(seq_len(task$task.desc$size), pred$data$id))
  perf.train = auc$fun(task, model, pred.train, feats, extra.args)
  # Compute test auc
  perf.test  = auc$fun(task, model, pred, feats, extra.args)
  # Compute threshold auc
  threshout_auc(mean(perf.train), mean(perf.test), thresholdout_params = extra.args)
  }
)
