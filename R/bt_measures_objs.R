source("bt_obj.R")
# the aggregation actually does not matter, since the measure does not depend on the train or test, perf.test and perf.train look different since cv is random
mtrain.agg = makeAggregation(id = "mtrain.agg", name = "only calculate train", properties = "req.train", fun = function(task, perf.test, perf.train, measure, group, pred) {
  function() {
    group   # group can be factor(0), but this does not matter
    pred$data$iter  # the whole data is used
    pred$data$response  # it can happen that all response is one class, so AUC is na
    pred$data$set
  }
  cat("\n aggregation of measures:")
  #if(any(is.na(c(perf.train, perf.test)))) browser()
  cat(sprintf("--perf.test: %s--", perf.test))
  cat(sprintf("--perf.train: %s-- \n", perf.train))
  perf.train
})

mk_measure_curator = function(extra.args) {
  makeMeasure(
    id = "meas_curator",
    name = "meas curator",
    properties = c("classif", "classif.multi", "req.pred", "req.truth"),
    minimize = TRUE,
    aggr = mtrain.agg,
    best = 0,
    worst = 1,
    fun = fun_measure_obj_curator,
    extra.args = extra.args
    )
}

#' @title
#' @description
#' @param extra.args extra.args$local2remote_subset should contain the index of instances
#' @return customized measure
mk_measure_openbox_tr_curator_tune_alpha = function(extra.args) {
  checkmate::assert_numeric(extra.args$alpha)
  makeMeasure(
    id = "local_tr_tune_remote_tune_alpha",
    name = "alpha mixture of local and remote objective",
    properties = c("classif", "classif.multi", "req.pred", "req.truth"),
    minimize = TRUE,
    aggr = mtrain.agg, best = 0,
    worst = 1,
    fun = fun_measure_obj_openbox_tr_curator_tune,
    extra.args = extra.args
    )
}

mk_measure_local_tr_tune_remote_tune_nocv = function(extra.args) {
  checkmate::assert_numeric(extra.args$alpha)
  makeMeasure(
    id = "local_tr_tune_remote_tune_alpha_nocv",
    name = "alpha mixture of local and remote objective nocv",
    properties = c("classif", "classif.multi", "req.pred", "req.truth"),
    minimize = TRUE,
    aggr = mtrain.agg,
    best = 0,
    worst = 1,
    fun = fun_measure_obj_local_tr_tune_remote_tune_nocv,
    extra.args = extra.args
    )
}


openbox_nocv = makeMeasure(
  id = "openbox_nocv",
  name = "openbox no cv",
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  minimize = TRUE,
  aggr = mtrain.agg,
  best = 0,
  worst = 1,
  fun = fun_measure_obj_openbox_nocv
  )



meas_openbox_cv = makeMeasure(
  id = "openbox_cv",
  name = "CV mmce on the openbox dataset",
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  minimize = TRUE,
  aggr = mtrain.agg,
  best = 0,
  worst = 1,
  fun = fun_measure_obj_openbox
  )


#' @title
#' @description
#' @param extra.args extra.args$local2remote_subset should contain the index of instances
#' @return customized measure
mk_measure_local2remote = function(extra.args) {
  measure_cv_local2remote = makeMeasure(
    id = "cv_local2remote",
    name = "CV mmce on the main dataset and model selection dataset",
    properties = c("classif", "classif.multi", "req.pred", "req.truth"),
    minimize = TRUE,
    aggr = mtrain.agg,
    best = 0,
    worst = 1,
    fun = fun_measure_obj_openbox2curator,
    extra.args = extra.args
    )
  return(measure_cv_local2remote)
}

# use fixed hyper-parameter to do cross validation with different training set and aggregate the generalization error
fun_measure_obj_ncv_local = function(variables) {

}

ncv_mmce_generalization = makeMeasure(
  id = "ncv_mmce_major",
  name = "Nested CV mmce on the main dataset",
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  minimize = TRUE,
  best = 0,
  worst = 1,
  fun = fun_measure_obj_ncv_local
  )

mkSingleDsMeasure = function(subset_ind) {
  measure_fun = function(task, model, pred, feats, extra.args) {
    subtask = subsetTask(task, subset_ind)
    getSingleDatasetPerf(model, subtask)$mmce
  }
  return(measure_fun)
}

# major_level is the same from problem generator function, test_level is also from problem generator, it refers to the relative index of all the datasets other than the major datasets
# for each problem, there is a 5 objective  measure list
mkMultimeasuresList = function(subset_inds, dsnames, major_level, test_level) {
  non_major = setdiff(dsnames, dsnames[major_level])
  test_ds_na = non_major[test_level]
  tune_ds_nas = c(non_major[-test_level], major_level)
  list.measures = lapply(tune_ds_nas, function(dsname) {
      makeMeasure(
        id = paste0("mmce_ds", dsname),
        name = paste0("My Mean Misclassification Error on side auxilliary dataset", dsname),
        properties = c("classif", "classif.multi", "req.pred", "req.truth"),
        minimize = TRUE,
        aggr = mtrain.agg,
        best = 0,
        worst = 1,
        fun = mkSingleDsMeasure(subset_inds[dsname]),
        extra.args = list()
        )
  })
  return(list.measures)
}



mk_measure_auc_thout = function(extra.args) {
  #
  ##
  #if(extra.args$perf_name2tune !="auc") stop("this function can only be used for auc!")
  makeMeasure(
    id = "thout.auc",
    name = "meas thout auc",
    properties = auc$properties,
    minimize = auc$minimize,  ## !!!
    aggr = mtrain.agg,
    best = auc$best,        ## !!
    worst = auc$worst,
    fun = fun_obj_thresholdout,
    extra.args = extra.args
    )
}

mk_any_measure_thout = function(extra.args) {
  makeMeasure(
    id = sprintf("thout.%s", extra.args$perf_name2tune),
    name = sprintf("measure thout.%s", extra.args$perf_name2tune),
    properties = auc$properties,
    minimize = FALSE,  ## !!!
    aggr = mtrain.agg,
    best = 0,        ## !!
    worst = 0,
    fun = fun_obj_thresholdout,
    extra.args = extra.args
    )
}
