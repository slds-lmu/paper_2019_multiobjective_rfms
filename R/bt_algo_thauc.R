#' @title
#' @description convert problem generator instance to thresholdoutauc input
#' @param instance instance from problem generator
#' @return list of input for ThresholdoutAUC
convertInst2ThresholdoutAUC = function(instance, conf) {
  tmf = instance$tmf
  tms = instance$tms
  task_lockbox = instance$task_lockbox
  pair = getTaskData(tmf, target.extra = T)
  x_train_total = pair$data
  y_train_total = pair$target
  pair = getTaskData(tms, target.extra = T)
  x_holdout = pair$data
  y_holdout = pair$target
  pair = getTaskData(task_lockbox, target.extra = T)
  x_test = pair$data
  y_test = pair$target
  n_train_begin = nrow(x_train_total) * conf$train_begin_ratio
  list(n_train = n_train_begin, x_train_total = x_train_total, y_train_total = y_train_total, x_holdout = x_holdout, y_holdout = y_holdout, x_test = x_test, y_test = y_test, tname = instance$tname, bname = instance$bname, p = instance$p)
}


algo_thresholdoutauc = function(instance) {
  conf = list(n_adapt_rounds = 10
    ,signif_level = 0.0001           # cutoff level used to determine which predictors to consider in each round based on their p-values. set small here for bigger parsimosmally for quick convergence
    ,thresholdout_threshold = 0.02 # T in the Thresholdout algorithm
    ,thresholdout_sigma = 0.03     # sigma in the Thresholdout algorithm
    ,thresholdout_noise_distribution = "norm" # choose between "norm" and "laplace"
    ,verbose = TRUE
    ,sanity_checks = FALSE
    ,train_begin_ratio = 0.5  # the ratio of n_train_begin compared to n_train_total
    )
  #thresholdoutauc = algo_thresholdoutauc(instance = instance, conf = conf)  # enough repetition is sufficient to get unbiased result of the random adding of instances
  source("thresholdout4real_data/refactor_general_simulation.R", chdir = T)
  results = run_sim(data_fun = convertInst2ThresholdoutAUC, instance = instance, conf = conf)
  rowind_test = which((results$dataset == "test_auc") & (results$round == 10))  # the last round
  rowind_holdout = which((results$dataset == "holdout_auc") & (results$round == 10))
  results[rowind_test, ]  # test set
  results[rowind_holdout, ]  # test set
}


algo_names = c("thresholdoutauc", algo_names)
algo_designs[[algo_names[1L]]] = data.frame(lrn = c("classif.glmnet"), stringsAsFactors = FALSE)
algo_funs[[algo_names[1L]]] = function(job, data, instance, lrn) {
  res = algo_thresholdoutauc(instance = instance)
  return(list(res = res, agg_fun = NULL))
}
