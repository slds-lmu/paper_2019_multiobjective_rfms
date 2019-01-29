source("bt_obj.R")

mk_measure = function(name, extra.args, obj_fun = fun_measure_obj_curator) {
  makeMeasure(
    id = name,
    name = name,
    properties = unique(c(extra.args$measures2tune$properties, "req.train")),
    minimize = extra.args$measures2tune$minimize,
    aggr = getGconf()$meas_aggr,
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
        aggr = getGconf()$meas_aggr,
        best = 0,
        worst = 1,
        fun = mkSingleDsMeasure(subset_inds[dsname]),
        extra.args = list()
        )
  })
  return(list.measures)
}




