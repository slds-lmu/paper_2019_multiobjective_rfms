getNCVPerf = function(major_task, lrn.id) {
  major_df = getTaskData(major_task)
  # has.blocking = TRUE
  major_task = makeClassifTask(id = "hack", major_df, target = "response")
  lrn_basis = GET_LRN(lrn.id)
  ps = GET_PARSET_CLASSIF(lrn.id)
  #ctrl = makeTuneControlMBO(maxit = gconf$MBO_ITERS)
  ctrl_bs = getTuneMethod("mbodefault", mgconf = getGconf())
  ctrl_bs$mbo.design = getMBODesign(lrn.id, mgconf = getGconf())
  rsd_in = makeResampleDesc("CV", iters = gconf$CV_ITER)
  rsd_out = makeResampleDesc("CV", iters = gconf$CV_ITER_OUTER)
  #tuneParams(lrn_basis, major_task, resampling = rsd, measures = mmce, par.set = ps, control = ctrl)
  lrn_tune = makeTuneWrapper(lrn_basis, resampling = rsd_in, par.set = ps, control = ctrl_bs, measures = mmce, show.info = FALSE)
  #res = train(learner = lrn_tune, task = major_task)
  res = resample(learner = lrn_tune, task = major_task, resampling = rsd_out, measures = mmce, extract = getTuneResult, models = TRUE, show.info = FALSE)
  #hyperPara = res$extract[[3]]$x
  res
  # res$aggr give the test mean
}

test = function(){
  instance = funGenProb(data = prob_inputs, job = NULL, major_level = 1L, test_level = 1L)
  pair = getMajorSet2TestSet(instance)
  major_task = pair$majorTask
  res = getNCVPerf(major_task = major_task, lrn.id = "classif.ksvm")
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

