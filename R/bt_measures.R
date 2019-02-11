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


# for each problem, there is a multi(more than 2) objective  measure list
mkMultimeasuresList = function(extra.args) {
  extra.args$instance$dataset_index_inbag
  tune_ds_nas = names(extra.args$instance$dataset_index_inbag)
  tune_ds_nas = setdiff(tune_ds_nas, extra.args$instance$openbox_name)
  list.measures = lapply(tune_ds_nas, function(dsname) {
      mk_measure(name = paste0("mmce_ds", dsname),
        extra.args = extra.args,
        obj_fun = mkSingleDsPerf(dsname)
        )
  })
  return(list.measures)
}

mkSingleDsPerf = function(dsname) {
  singleDsPerf = function(task, model, pred, feats, extra.args) {
    list.perf = getPerf4DataSites_Oracle(task, model, extra.args)
    list.perf[[dsname]][extra.args$perf_name2tune]
}
  singleDsPerf
}
