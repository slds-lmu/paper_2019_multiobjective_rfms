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

dtlong = readRDS("dtlong.rds")
dtlong = dtlong[, c("alpha", "value", "alphas")]
dtlong$alphas = as.character(dtlong$alphas)
fig_alpha = ggplot2::ggplot(dtlong, aes(x = alphas, y = value)) + geom_boxplot() + ggtitle("fso8openbox_curator") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave(fig_alpha, file = "demo.pdf")



###
getRes = function(alpha_bootstrap_index, name = "ob_vs_cu", algo_name = "fso8") {
  list_ob_vs_cu = lapply(alpha_bootstrap_index$list_boot, function(culb) {
    subtask = subsetTask(task, culb[[name]])
    lrn.id = res$res$tune_res[[algo_name]]$learner$id
    lrn_base_id = processLrnName(lrn.id)   # remove .preprocess
    lrn_wrap = GET_LRN(lrn_base_id)
    task = res$res$instance$task
    task_openbox_inbag = subsetTask(task, res$res$instance$openbox_inbag_ind)         
    model = train(lrn_wrap, task_openbox_inbag)
    perfs = getSingleDatasetPerf(model, subtask, F)
    # FIXME: change to name mmce instead of [1L]
    perfs[1L]})
}

reduceOneResult = function() {
    bootstrap_alphas = seq(from = 0.1, to = 0.9, length.out = 10)
    bootstrap_rep = 10L
    algo_name = "fso8"
    list_alpha_bootstrap_index = lapply(bootstrap_alphas, function(alpha) genBootstrapPool(res$res$instance, alpha = alpha, rep = bootstrap_rep))
    getResOneAlgo = function(algo_name = "fso8") {
      res = list()
      res$list_alpha_perf_mixbag_ob_vs_cu = lapply(list_alpha_bootstrap_index, function(alpha_bootstrap_index) {
        getRes(alpha_bootstrap_index = alpha_bootstrap_index, name = "ob_vs_cu", algo_name = "fso8")
      })
      res$list_alpha_perf_mixbag_ob_vs_lb = lapply(list_alpha_bootstrap_index, function(alpha_bootstrap_index) {
        getRes(alpha_bootstrap_index = alpha_bootstrap_index, name = "ob_vs_lb", algo_name = "fso8")
      })
      return(res)
    }
    res = getResOneAlgo(algo_name = "fmo")
    dt = rbindlist(list_alpha_perf_mixbag_ob_vs_cu)
    dt$alpha = bootstrap_alphas
    library(tidyr)
    dtlong = gather(dt, alpha)
    fig_cu = ggplot2::ggplot(dt, aes(x = alpha, y = value)) + geom_boxplot() 
    saveRDS(dtlong, file = "dtlong.rds") 
}
