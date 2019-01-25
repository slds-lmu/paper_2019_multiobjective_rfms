library(mlr)
subtask = readRDS(file = "task_mmce_cv0.3_train_all1.rds")
lrn_obj = makeLearner("classif.ksvm", kernel = "rbfdot", predict.type = "prob")
lrn_obj_noprob = makeLearner("classif.ksvm", kernel = "rbfdot")

# default hyper-parameter
model = train(lrn_obj, subtask)
pred =  predict(model, task = subtask)
performance(pred)

# use another hyper-parameter
par.vals = readRDS("hyperpars.rds")
lrn_obj2 = mlr::setHyperPars(lrn_obj, par.vals = par.vals)
lrn_obj2_noprob = mlr::setHyperPars(lrn_obj_noprob, par.vals = par.vals)

rsd_out = makeResampleDesc("CV", iters = 10, predict = "both")
# cross validation using this hyper-parameter
train_mce = mlr::setAggregation(mlr::mmce, train.mean)
test_mce = mlr::mmce

res1 = resample(learner = lrn_obj2, task = subtask, resampling = rsd_out, measures = test_mce, show.info = T)
res1

res2 = resample(learner = lrn_obj2, task = subtask, resampling = rsd_out, measures = train_mce, show.info = T)
res2

res3 = resample(learner = lrn_obj2_noprob, task = subtask, resampling = rsd_out, measures = train_mce, show.info = T)
res4 = resample(learner = lrn_obj2_noprob, task = subtask, resampling = rsd_out, measures = test_mce, show.info = T)
res3



# training and testing on the same dataset using the specified hyper-parameter
model2 = train(lrn_obj2, subtask)
pred2 = predict(model2, subtask)
performance(pred2)

data = getTaskData(subtask)
mk = model$learner.model
head(kernlab::predict(mk, data, type = "probabilities"))
head(kernlab::predict(mk, data))
