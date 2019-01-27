getTestName = function(ns,  major_level, test_level) {
  ns1 = setdiff(ns, ns[major_level])
  ns1[test_level]
}

getOpenBox2CuratorBoxInd = function(instance) {
  ns = instance$ns
  major_level = instance$major_level
  test_level = instance$test_level
  tn = getTestName(ns = ns, major_level = major_level, test_level = test_level)
  msnas = setdiff(ns, c(tn, ns[major_level]))  # model selection dataset names
  indx = unlist(instance$dataset_index[c(msnas, ns[major_level])])
  # FIXME: indx has weird names
  return(list(local2remote_subset = indx))
}


getMajorSet2TestSet = function(instance) {
  testName = getTestName(instance$ns, major_level = instance$major_level, test_level = instance$test_level)
  testTask = mlr::subsetTask(instance$task, instance$dataset_index[[testName]])
  majorname = instance$ns[[instance$major_level]]
  majorTask =  mlr::subsetTask(instance$task, instance$dataset_index[[majorname]])
  return(list(majorTask = majorTask, testTask = testTask))
}

getModelFromInstance = function(instance, lrn.id, pvs) {
  pair = getMajorSet2TestSet(instance)
  model = getModelFromTask(pair$majorTask, lrn.id = lrn.id, pvs = pvs)
  return(model)
}

getTestSetPerf = function(instance, lrn.id, pvs) {
  pair = getMajorSet2TestSet(instance)
  model = getModelFromTask(pair$majorTask, lrn.id = lrn.id, pvs = pvs)
  pred_test =  predict(model, pair$testTask)
  mlr::performance(pred_test)
}


mkSingleDsMeasure = function(subset_ind) {
  measure_fun = function(task, model, pred, feats, extra.args) {
    subtask = subsetTask(task, subset_ind)
    getSingleDatasetPerf(model, subtask)$mmce
  }
  return(measure_fun)
}

fun_measure_obj_openbox_nocv = function(task, model, pred, feats, extra.args) {
  #'model$subset  # equivalent to which(df[, dataset_id] == dataset_names[2]) which get the row index for dataset 2
  major_task = subsetTask(task, model$subset)
  lrn.id = getLrnIDFromModel(model)
  pvs = getHyperParFromModel(model)
  model = getModelFromTask(major_task = major_task, lrn.id = lrn.id, pvs = pvs)
  pred = predict(model, major_task)
  mlr::performance(pred, measures = list(extra.args$measures2tune))
}


fun_measure_obj_local_tr_tune_remote_tune_nocv = function(task, model, pred, feats, extra.args) {
  obj1 = fun_measure_obj_openbox_nocv(task, model, pred, feats, extra.args)  # no need for extra.args
  obj2 = fun_measure_obj_curator(task, model, pred, feats, extra.args)  # the performance for the current model is computed in this measure
  return(extra.args$alpha * obj1 + (1 - extra.args$alpha * obj2))
}



# model is from training only on the major dataset
fun_measure_obj_openbox2curator = function(task, model, pred, feats, extra.args) {
  major_task = subsetTask(task, extra.args$local2remote_subset)   # pool the local and remote dataset together
  lrn.id = model$learner$id
  pvs = model$learner$par.vals  # it does not matter there to the local trained model to extract the hyper-parameter since the hyper-parameter is decided by the tunner, not the local model.
  newmodel = getModelFromTask(major_task = major_task, lrn.id = lrn.id, pvs = pvs)
  fun_measure_obj_curator(task = task, model = newmodel, pred = pred, feats = feats, extra.args = extra.args)  # call the measure to update the environment, current context is bs2, only chagne is model here
  res = getCVPerf(major_task = major_task, lrn.id = lrn.id, pvs = pvs, measures = extra.args$measures2tune)
  return(res$aggr)
}


