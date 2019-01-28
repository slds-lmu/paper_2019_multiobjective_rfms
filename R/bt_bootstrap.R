test_genBootstrapPool = function() {
  source("bt_conf.R")
  instance = funGenProbOracle(data = prob_inputs, job = NULL, openbox_ind = 1L, lockbox_ind = 1L, dataset_name = "geo")
  genBootstrapPool(instance)
}

genBootstrapPool = function(instance, alpha = 0.7, rep = 3L) {
  res = list()
  lockbox_ind_oracle = instance$dataset_index_outbag[[instance$lockbox_name]]
  openbox_outbag_ind_oracle = instance$dataset_index_outbag[[instance$openbox_name]]
  curator_outbag_ind_oracle = Reduce(c, instance$dataset_index_outbag[instance$curator_names])
  ob_pool_cu = c(openbox_outbag_ind_oracle, curator_outbag_ind_oracle)
  res$ob_pool_cu = ob_pool_cu
  ob_pool_lb = c(openbox_outbag_ind_oracle, lockbox_ind_oracle)
  res$ob_pool_lb = ob_pool_lb

  prob_ob = rep(alpha, length(openbox_outbag_ind_oracle))
  prob_lb = rep(1.0 - alpha, length(lockbox_ind_oracle))
  prob_cu = rep(1.0 - alpha, length(curator_outbag_ind_oracle))
  list_boot = lapply(1:rep, function(i) {
    rst = list()
    rst$ob_vs_cu = sample(ob_pool_cu, size = length(ob_pool_cu), replace = T, prob = c(prob_ob, prob_cu))
    rst$ob_vs_lb = sample(ob_pool_lb, size = length(ob_pool_lb), replace = T, prob = c(prob_ob, prob_lb))
    rst
  })
  res$list_boot = list_boot
  # res$list_boot$$ob_vs_cu
  # res$list_boot$$ob_vs_lb
  return(res)
}

plotBoot = function() {
  res = loadResult(1)
  res$res$gperf_env$fso8[['7']]$mixbag[['0.9']]$ob_vs_lb
  unlist(res$res$gperf_env$fso8[['7']]$mixbag[['0.9']]$ob_vs_cu)
  res$res$gperf_env$fmo[['7']]$mixbag[['0.9']]$ob_vs_cu
  unlist(res$res$gperf_env$fmo[['7']]$mixbag[['0.9']]$ob_vs_cu)
}

fun = function(res) {
  bootstrap_alphas = seq(from = 0.1, to = 0.9, length.out = 10)
  bootstrap_rep = 10L
  res$list_alpha_bootstrap_index = lapply(bootstrap_alphas, function(alpha) genBootstrapPool(res$res$instance, alpha = alpha, rep = bootstrap_rep))
  list_perf_mixbag = lapply(res$res$instance$list_alpha_bootstrap_index, function(alpha_bootstrap_index) {
    list_ob_vs_cu = lapply(alpha_bootstrap_index$list_boot, function(culb) {
      subtask = subsetTask(task, culb$ob_vs_cu)
      perfs = getSingleDatasetPerf(model, subtask, F)
      # FIXME: change to name mmce instead of [1L]
      perfs[1L]})

    list_ob_vs_lb = lapply(alpha_bootstrap_index$list_boot, function(culb) {
      subtask = subsetTask(task, culb$ob_vs_lb)
      perfs = getSingleDatasetPerf(model, subtask, F)
      perfs[1L]})
    return(list(ob_vs_lb = list_ob_vs_lb, ob_vs_cu = list_ob_vs_cu))
  })
}
