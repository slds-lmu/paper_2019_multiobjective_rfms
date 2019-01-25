library(mlr)
 subtask = readRDS(file = "task_mmce_cv0.3_train_all1.rds")

  lrn_obj = makeLearner("classif.ksvm", kernel = "rbfdot", predict.type = "prob")
  lrn_wrap = makeRemoveConstantFeaturesWrapper(lrn_obj)
  model = train(lrn_wrap, subtask)
  pred = mlr:::predict.WrappedModel(model, task = subtask)
  performance(pred)
  rsd_out = makeResampleDesc("CV", iters = 10)




  lrn_obj = makeLearner("classif.ksvm", kernel = "rbfdot", predict.type = "prob")
  lrn_obj2 = mlr::setHyperPars(lrn_obj, par.vals = readRDS("hyperpars.rds"))
  lrn_wrap2 = makeRemoveConstantFeaturesWrapper(lrn_obj2)

  res = resample(learner = lrn_wrap2, task = subtask, resampling = rsd_out, measures = mlr::mmce, show.info = T)  # paramValueToString
  res2 = resample(learner = lrn_obj2, task = subtask, resampling = rsd_out, measures = mlr::mmce, show.info = T)  # paramValueToString
  model2 = train(lrn_wrap2, subtask)
  pred2 = predict(model2, subtask)
  performance(pred2)

  model3 = train(lrn_obj2, subtask)
  pred3 = predict(model3, subtask)
  performance(pred3)

