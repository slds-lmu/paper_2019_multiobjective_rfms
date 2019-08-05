source("bt_conf.R")
instance = funGenProb_geo(data = prob_inputs_conf, job = NULL, openbox_ind = 1, lockbox_ind = 1)
lrn = makeLearner("classif.featureless")
instance
model = train(lrn, instance$task_openbox_all)
pred = predict(model, instance$task_curator_inbag)
performance(pred)


source("bt_pre.R")
source("bt_main.R")
#btInit(path = "registrydebuagsg", local = T)   # set local=T to allow on local PC running
DEBUG_FLAG = F # if true: use low budget (only 7 iterations of mbo) set to F
reg = mkReg("reg_cheat_feb4", replace = T, local = T) 
reg$cluster.functions = makeClusterFunctionsMulticore(ncpus = 50)
######################################
mgconf = getGconf()
reg_input = batchtools::getDefaultRegistry()
reg_input$default.resources

algofeatless = function(job, data, instance) {
  lrn = makeLearner("classif.featureless")
  instance
  model = train(lrn, instance$task_openbox_all)
  pred = predict(model, instance$task_curator_inbag)
  performance(pred)
}



addProblem(name = "prob",  data = prob_inputs_conf, fun = funGenProb, seed = 1L)
addAlgorithm(name = "featless", fun = algofeatless)
addExperiments(prob.design = prob_designs, algo.design = algo_designs, repls = 10)


