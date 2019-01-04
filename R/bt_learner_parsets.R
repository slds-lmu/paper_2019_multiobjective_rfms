# generate parameter set for different learners for hyperparameter tuning, same for classification and regression
library(ParamHelpers)
ps.ksvm = makeParamSet(
  makeNumericParam("C", lower = -15, upper = 15, trafo = function(x) 2^x),
  makeNumericParam("sigma", lower = -15, upper = 15, trafo = function(x) 2^x)
  # makeNumericParam("fw.perc", lower = 0.001, upper = 0.8)  # feature selection percentage
)

ps.glmnet = makeParamSet(
  makeNumericParam("alpha", lower = 0, upper = 1),
  makeNumericParam("s", lower = -10, upper = 10, trafo = function(x) 2^x)   # fixme s too big is a disaster?
)

ps.xgboost = makeParamSet(
  makeNumericParam("eta", lower = 0.001, upper = 0.3),
  #makeNumericParam("gamma", lower = -10, upper = 10, traf = function(x) 2^x),
  makeIntegerParam("max_depth", lower = 1L, upper = 15L),
  makeNumericParam("subsample", lower = 0.5, upper = 1),
  makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
  #makeNumericParam("colsample_bylevel", lower = 0.5, upper = 1), # categorical
  #makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x), not important
  #makeNumericParam("lambda_bias", lower = -10, upper = 10, trafo = function(x) 2^x),
  #makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),not important
  makeNumericParam("min_child_weight", lower = 0, upper = 50)
  # makeNumericParam("fw.perc", lower = 0.001, upper = 0.8)  # feature selection percentage
  #makeIntegerParam("nrounds", lower = 500L, upper = 5000L)
  #makeNumericParam("rate_drop", lower = 0, upper = 1),
  #makeNumericParam("skip_drop", lower = 0, upper = 1)
)

ps.ranger = function(p) {
  makeParamSet(
    # FIXME: mtry must depend on the other parameter "fw.perc"
    #makeIntegerParam("mtry", lower = as.integer(p/10), upper = as.integer(p/1.5)),
    makeIntegerParam("min.node.size", lower = 1L, upper = 50L, default = 5L),
    makeIntegerParam("num.trees", lower = 100, upper = 5000, default = 500L),
    makeNumericParam("sample.fraction", lower = 0.1, upper = 1, default = 0.5)
    # makeDiscreteParam("fw.perc", values = PERF_GRID))
    # makeNumericParam("fw.perc", lower = 0.001, upper = 0.8))  # feature selection percentage
    )
}

ps.rpart = makeParamSet(
    makeNumericParam("cp", lower = 0.001, upper = 1)
    )


ps.knn = function() {
  makeParamSet(
    makeIntegerParam(id = "k", default = 1L, lower = 1L, upper = 10L)
    )
    # makeNumericParam("fw.perc", lower = 0.001, upper = 0.8))  # feature selection percentage
}

ps.nbs = function() {
  makeParamSet(
  makeNumericParam(id = "laplace", default = 0, lower = 0, upper = 0.01),
  #makeNumericParam("fw.perc", lower = 0.001, upper = 0.8))  # feature selection percentage
  )
}

# makeParamSet(makeDiscreteParam("fw.perc", values = PERF_GRID))

GET_PARSET_CLASSIF = function(lrn.cl, extra.pars = NULL) {
  switch(lrn.cl,
    "classif.xgboost" = ps.xgboost,
    "classif.ksvm" = ps.ksvm,
    "classif.glmnet" = ps.glmnet,
    "classif.ranger" = ps.ranger(extra.pars),
    "classif.knn" = ps.knn(),
    "classif.rpart" = ps.rpart,
    "classif.naiveBayes" = ps.nbs(),
    NULL)
}


processLrnName = function(lrn.id) {
  checkmate::assertCharacter(lrn.id)
  lrn.id = stringi::stri_replace_all(str = lrn.id, replacement = "", regex=".preproc")
  lrn.id
}

GET_LRN = function(lrn) {
  lrnmap = list(
    "classif.ksvm" =  makeLearner("classif.ksvm", kernel = "rbfdot", predict.type = "prob"), # vanilladot is a special case of rbf kernel
    "classif.glmnet" = makeLearner("classif.glmnet", predict.type = "prob"),
    "classif.ranger" = makeLearner("classif.ranger", predict.type = "prob")
    )
  lrn_obj = lrnmap[[lrn]]
  lrn_wrap = makeRemoveConstantFeaturesWrapper(lrn_obj)
  return(lrn_wrap)
}

getMBODesign = function(lrn, mgconf) {
  par.set = GET_PARSET_CLASSIF(lrn)
  des = generateDesign(n = mgconf$INIT_DES, par.set = par.set, fun = lhs::randomLHS)
  # des$y
  print("initial design")
  print(des)
  return(des)
}
