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




require("mlrMBO")
getModelFromTask = function(major_task, lrn.id, pvs) {
  lrn_basis = GET_LRN(lrn.id)
  lrn_basis = setWraperHyperPars(lrn_obj = lrn_basis, pvs = pvs)
  model = mlr::train(learner = lrn_basis, task = major_task)
  model
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



init_mbo_ctrl_so = function(iters) {
  control = makeMBOControl()
  control = setMBOControlTermination(control, iters = iters)
  control = setMBOControlInfill(control, crit = makeMBOInfillCritEI())  # opt = "focussearch"
  control
}

init_mbo_ctrl_mo = function(n.obj, iters) {
  control = makeMBOControl(n.objectives = n.obj)
  control = setMBOControlTermination(control, iters = iters)
  control = setMBOControlMultiObj(control, method = "parego")   # dib, mspot # dib generate the error: Domains[,1] must be less than or equal to Domains[,2] from package rgenoud
  control = setMBOControlInfill(control, crit = makeMBOInfillCritEI())  # opt = "nsga2", "ea", "cmaes"
  control
}


# ?DiceKriging::km for nugget
# mlr learner: The extra parameter `jitter` (default is `FALSE`) enables adding a very small jitter (order 1e-12) to the x-values before prediction,
getMlrTuneCtrlMO = function(iters, n.obj = 2, nugget = 1e-6, jitter = TRUE) {
  surrogate = mlr::makeLearner("regr.km", predict.type = "se", config = list(show.learner.output = FALSE, on.learner.error = "warn", on.par.out.of.bounds = "warn"), jitter = jitter, nugget = nugget)
  control = init_mbo_ctrl_mo(n.obj = n.obj, iters = iters)
  mlr::makeTuneMultiCritControlMBO(learner = surrogate, mbo.control = control)
}

getMlrTuneCtrlSO = function(iters, nugget = 1e-6, jitter = TRUE) {
  surrogate = mlr::makeLearner("regr.km", predict.type = "se", config = list(show.learner.output = FALSE, on.learner.error = "warn", on.par.out.of.bounds = "warn"), jitter = jitter, nugget = nugget)
  control = init_mbo_ctrl_so(iters)
  mlr::makeTuneControlMBO(learner = surrogate, mbo.control = control)
}

# this makes MBO_ITERS MACRO reconfigurable
getTuneMethod = function(method.str, mgconf, n.objs = 2, nugget = 1e-6, jitter = T) {
  TUNE_METHODS = list(
    mbodefault = getMlrTuneCtrlSO(iters = mgconf$MBO_ITERS, nugget = nugget, jitter = jitter),
    mbomulticritdefault = getMlrTuneCtrlMO(iters = mgconf$MBO_ITERS, n.obj = 2L, nugget = nugget, jitter = jitter),
    mbo5critdefault = getMlrTuneCtrlMO(iters = mgconf$MBO_ITERS, n.obj = n.objs, nugget = nugget, jitter = jitter)
    )
  return(TUNE_METHODS[[method.str]])
}
