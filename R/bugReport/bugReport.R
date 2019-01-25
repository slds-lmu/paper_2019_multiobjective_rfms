library(mlr)
library(mlrMBO)

mtrain.agg = makeAggregation(id = "mtrain.agg", name = "only calculate train", properties = "req.train", fun = function(task, perf.test, perf.train, measure, group, pred) {
  perf.train
})



  subtask = readRDS(file = "task_mmce_cv0.3_train_all1.rds")

  lrn_obj = makeLearner("classif.ksvm", kernel = "rbfdot", predict.type = "prob")
  lrn_wrap = makeRemoveConstantFeaturesWrapper(lrn_obj)
  model = train(lrn_wrap, subtask)
  pred = mlr:::predict.WrappedModel(model, task = subtask)
  performance(pred)
  rsd_out = makeResampleDesc("CV", iters = 10)
  #res = resample(learner = lrn_wrap, task = subtask, resampling = rsd_out, measures = mlr::mmce, show.info = T)  # paramValueToString




ps.ksvm = makeParamSet(
  makeNumericParam("C", lower = -15, upper = 15, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -15, upper = 15, trafo = function(x) 2^x)
)

  surrogate = mlr::makeLearner("regr.km", predict.type = "se", config = list(show.learner.output = FALSE, on.learner.error = "warn", on.par.out.of.bounds = "warn"), jitter = T, nugget = 1e-6)
  control = makeMBOControl()
  control = setMBOControlTermination(control, iters = 3)
  control = setMBOControlInfill(control, crit = makeMBOInfillCritEI())  # opt
  ctrl_bs = mlr::makeTuneControlMBO(learner = surrogate, mbo.control = control)
  #ctrl_bs$mbo.design = generateDesign(n = 4, par.set = ps.ksvm, fun = lhs::randomLHS)
  ctrl_bs$mbo.design = readRDS("mboDesign.rds")
  openbox_inbag_ind = readRDS("openbox_inbag_ind.rds")
  task_oracle = readRDS("task_all.rds")
  test_inds = setdiff(1:getTaskSize(task_oracle), openbox_inbag_ind)
  rins = makeFixedHoldoutInstance(train.inds = openbox_inbag_ind, test.inds = test_inds, getTaskSize(task_oracle))
  rins$desc$predict = "train" # must be both since some measure aggregation is test

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

fun_measure_obj_openbox = function(task, model, pred, feats, extra.args) {
  subtask = subsetTask(task, model$subset)  # equivalent to which(df[, dataset_id] == dataset_names[2]) which get the row index for dataset 2

  pred = mlr:::predict.WrappedModel(model, task = subtask)
  pred = predict(model, task = subtask)
  perf = performance(pred)
  print(perf)
  perf
  browser()
}


extra.args = list(measures2tune = brier)
meas_open = mk_measure("oen", extra.args = extra.args, obj_fun = fun_measure_obj_openbox)

tune_res_bs = mlr::tuneParams(learner = lrn_wrap, task = task_oracle, resampling = rins, measures = meas_open, par.set = ps.ksvm, control = ctrl_bs, show.info = TRUE)  # only the first of the list_measures are being tuned
