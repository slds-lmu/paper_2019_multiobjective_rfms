getNCVPerf = function(major_task, lrn.id, meas = mlr::brier) {
  lrn_basis = GET_LRN(lrn.id)
  ps = GET_PARSET_CLASSIF(lrn.id)
  #ctrl = makeTuneControlMBO(maxit = gconf$MBO_ITERS)
  ctrl_bs = getTuneMethod("mbodefault", mgconf = getGconf(), n.objs = 1, nugget = 1e-4)
  ctrl_bs$mbo.design = getMBODesign(lrn.id, mgconf = getGconf())
  rsd_in = makeResampleDesc("CV", iters = gconf$CV_ITER)
  rsd_out = makeResampleDesc("CV", iters = gconf$CV_ITER_OUTER)
  #tuneParams(lrn_basis, major_task, resampling = rsd, measures = mmce, par.set = ps, control = ctrl)
  lrn_tune = makeTuneWrapper(lrn_basis, resampling = rsd_in, par.set = ps, control = ctrl_bs, measures = mmce, show.info = FALSE)
  #res = train(learner = lrn_tune, task = major_task)
  res = resample(learner = lrn_tune, task = major_task, resampling = rsd_out, measures = meas, extract = getTuneResult, models = TRUE, show.info = FALSE)
  #hyperPara = res$extract[[3]]$x
  res
  # res$aggr give the test mean
}

test = function(){
  source("bt_conf.R")
  source("bt_utilities_datasite.R")
  prob_inputs_data = list()  # used for addProblem
  prob_inputs_data$geo = prepareDataSite(path = "../Data/data_cohorts_nonGerman.RData")  # this data comes with repository
  dataset_names_input = c("geo")
  instance = funGenProbOracle(data = prob_inputs_data, job = NULL, openbox_ind = 1L, lockbox_ind = 1L, dataset_name = "geo")
  # instance$task_openbox_all
  # pair = getMajorSet2TestSet(instance)
  # major_task = pair$majorTask
  res = getNCVPerf(major_task = instance$task_openbox_all, lrn.id = "classif.ksvm")
  res$measures.test # outter loop cross validation 
  res$extract
  optpath1 = res$extract[[1]]$opt.path
  optpath1df = as.data.frame(trafoOptPath(optpath1))
  as.data.frame(optpath1)
  opt.paths_outer = getNestedTuneResultsOptPathDf(res, trafo = FALSE)   # outter and inner loop cross validation
  head(opt.paths_outer, 10)
  selectedHyperPars = getNestedTuneResultsX(res)
  innerOptTestPerf = lapply(1:nrow(optpath1df), function(i) {
    pvs = optpath1df[i, "C"]
    pvs = list(C = pvs)
    getTestSetPerf(instance = instance, lrn.id = "classif.ksvm", pvs = pvs)
  })
  hh = unlist(innerOptTestPerf)
  plot(hh)
}

#Error in (function (fn, nvars, max = FALSE, pop.size = 1000, max.generations = 100,  :
#      Domains[,2] must be less than or equal to Domains[,2]
#timeit::timeit(compare(1),times = 1)

