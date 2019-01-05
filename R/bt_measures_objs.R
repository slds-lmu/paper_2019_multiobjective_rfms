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


mk_measure = function(name, extra.args, obj_fun = fun_measure_obj_curator) {
  makeMeasure(
    id = name,
    name = name,
    properties = extra.args$measures2tune$properties,
    minimize = extra.args$measures2tune$minimize,
    aggr = mtrain.agg,
    best =  extra.args$measures2tune$best,
    worst = extra.args$measures2tune$worst,
    fun = obj_fun,
    extra.args = extra.args
    )
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




