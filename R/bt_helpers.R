require("mlrMBO")
init_mbo_ctrl_so = function(iters) {
  control = makeMBOControl()
  control = setMBOControlTermination(control, iters = iters)
  control = setMBOControlInfill(control, crit = makeMBOInfillCritEI())  # opt = "focussearch"
  control
}

init_mbo_ctrl_mo = function(n.obj, iters, mombomethod = "parego") {
  control = makeMBOControl(n.objectives = n.obj)
  control = setMBOControlTermination(control, iters = iters)
  control = setMBOControlMultiObj(control, method = mombomethod)   # dib, mspot # dib generate the error: Domains[,1] must be less than or equal to Domains[,2] from package rgenoud
  control = setMBOControlInfill(control, crit = makeMBOInfillCritEI())  # opt = "nsga2", "ea", "cmaes"
  control
}


# ?DiceKriging::km for nugget
# mlr learner: The extra parameter `jitter` (default is `FALSE`) enables adding a very small jitter (order 1e-12) to the x-values before prediction,
getMlrTuneCtrlMO = function(iters, n.obj = 2, nugget = 1e-6, jitter = TRUE, mombomethod="parego") {
  surrogate = mlr::makeLearner("regr.km", predict.type = "se", config = list(show.learner.output = FALSE, on.learner.error = "warn", on.par.out.of.bounds = "warn"), jitter = jitter, nugget = nugget)
  control = init_mbo_ctrl_mo(n.obj = n.obj, iters = iters, mombomethod = "parego")
  mlr::makeTuneMultiCritControlMBO(learner = surrogate, mbo.control = control)
}

getMlrTuneCtrlSO = function(iters, nugget = 1e-6, jitter = TRUE) {
  surrogate = mlr::makeLearner("regr.km", predict.type = "se", config = list(show.learner.output = FALSE, on.learner.error = "warn", on.par.out.of.bounds = "warn"), jitter = jitter, nugget = nugget)
  control = init_mbo_ctrl_so(iters)
  mlr::makeTuneControlMBO(learner = surrogate, mbo.control = control)
}

# this makes MBO_ITERS MACRO reconfigurable
getTuneMethod = function(method.str, mgconf, n.objs = 2, nugget = 1e-6, jitter = T, mombomethod ="parego") {
  TUNE_METHODS = switch(EXPR = method.str, 
    mbodefault = getMlrTuneCtrlSO(iters = mgconf$MBO_ITERS, nugget = nugget, jitter = jitter),
    mbomulticritdefault = getMlrTuneCtrlMO(iters = mgconf$MBO_ITERS, n.obj = 2L, nugget = nugget, jitter = jitter, mombomethod = mombomethod),
    mbo5critdefault = getMlrTuneCtrlMO(iters = mgconf$MBO_ITERS, n.obj = n.objs, nugget = nugget, jitter = jitter,mombomethod = mombomethod)
    )
  return(TUNE_METHODS)
}
