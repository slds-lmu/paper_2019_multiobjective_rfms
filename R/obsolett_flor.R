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
