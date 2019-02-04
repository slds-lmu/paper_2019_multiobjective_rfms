#' source("bt_conf.R")
#' funGenProbCheat(data = prob_inputs_conf, job = NULL, lockbox_ind = 1)
funGenProbCheat = function(data, job, lockbox_ind) {
  ratio_inbag = getGconf()$ratio_inbag
  tuple = prepareDataSite(path = data$path[["geo"]])
  res = list()
  res$lockbox_ind = lockbox_ind
  task_oracle = tuple$task
  res$task = task_oracle
  dataset_index = tuple$list_dataset_index  # list of instance index for each dataset
  ns = names(dataset_index)
  checkmate::assert(!is.null(ns))
  res$ns = ns
  res$dataset_index = dataset_index
  lockbox_name = ns[lockbox_ind]
  res$lockbox_name = lockbox_name

  ob_cu_names = setdiff(ns, c(lockbox_name))
  res$ob_cu_names = ob_cu_names

  ob_cu_oracle_ids = which(tuple$df_dataset_accn %in% ob_cu_names)
  obcu_inbag_ind_rel = sample(length(ob_cu_oracle_ids), size = length(ob_cu_oracle_ids) * ratio_inbag)

  obcu_inbag_ind = ob_cu_oracle_ids[obcu_inbag_ind_rel]
  res$obcu_inbag_ind = obcu_inbag_ind
  obcu_outbag_ind =  ob_cu_oracle_ids[-obcu_inbag_ind_rel]
  test.inds = setdiff(1:getTaskSize(task_oracle), obcu_inbag_ind)  # test ind does not matter
  rins = makeFixedHoldoutInstance(obcu_inbag_ind, test.inds, getTaskSize(task_oracle))
  rins$desc$predict = "train" # must be both since some measure aggregation is test
  res$rins = rins

  obcu_list = lapply(ob_cu_names, function(x) {
    inds = which(tuple$df_dataset_accn == x)
    len = length(inds)
    inbag_inds_rel = sample(len, size = len * ratio_inbag)
    inbag = inds[inbag_inds_rel]
    outbag = inds[-inbag_inds_rel]
    list(inbag = inbag, outbag = outbag, len = len)
  })

  obcu_inbags_oracle_inds = Reduce(c, lapply(obcu_list, function(x) x$inbag))
  lockbox_oracle_inds = which(tuple$df_dataset_accn == lockbox_name)

  dataset_index_outbag = lapply(obcu_list, function(x) x$outbag)
  names(dataset_index_outbag) = ob_cu_names
  dataset_index_outbag[[lockbox_name]] =  lockbox_oracle_inds
  res$dataset_index_outbag = dataset_index_outbag

  dataset_index_inbag = lapply(obcu_list, function(x) x$inbag)
  names(dataset_index_inbag) = ob_cu_names
  dataset_index_inbag[[lockbox_name]] = lockbox_oracle_inds
  res$dataset_index_inbag =  dataset_index_inbag

  res$obcu_index = Reduce(c, dataset_index[ob_cu_names])
  res$curator_len = length(res$obcu_index)
  res$task_obcu_inbag = mlr::subsetTask(task_oracle, subset = unlist(dataset_index_inbag[ob_cu_names]))
  res$obcu_inbag_len = getTaskSize(res$task_obcu_inbag)
  res$task_lockbox = mlr::subsetTask(task_oracle, subset = dataset_index[[lockbox_name]])
  return(res)
}


###
prob_designs = list()
prob_funs = list()
#
prob_names = c("prob_geo_cheat")
prob_funs[[prob_names[[1L]]]] = funGenProbCheat
prob_designs[[prob_names[1L]]] = expand.grid(lockbox_ind = 1:5, stringsAsFactors = FALSE)


# only depend on the lockbox
algoCheating = function(instance, lrn) {
  context = "cheat"
  res = list()
  gperf_env = new.env()   # gperf_env is only being modified in side measure function!
  gperf_env$index = 1
  ptmi = proc.time()
  mbo_design = getMBODesign(lrn, getGconf())
  extra.args = list(instance = instance, gperf_env = gperf_env, perf_name2tune = getGconf()$perf_name2tune, measures2tune = getGconf()$meas2tune)
  cmeas  = mk_measure(name = "meas_ob_cu_cv", extra.args, obj_fun = fun_measure_obj_cso)
  mgconf = getGconf()
  ctrl_bs = getTuneMethod("mbodefault", mgconf = mgconf)
  ctrl_bs$mbo.design = mbo_design
  tune_res_cso = mlr::tuneParams(learner = GET_LRN(lrn), task = instance$task, resampling = instance$rins, measures = list(cmeas), par.set = GET_PARSET_CLASSIF(lrn), control = ctrl_bs, show.info = TRUE)  # only the first of the list_measures are being tuned
  print(proc.time() - ptmi)
  tune_res_cso 
}

algo_names = c()       # list of strings defining algorithm names
algo_designs = list()  # algorithm function need extra parameters which will be defined here
algoaggs = list()  # list of functions for aggregation of result
#' To make the result analysis easy, the aggregation function for each algorithm must return a consistent result. For some algorithms, some field does not make sense, in this case, they are NA. The final data table would look like: algo_name(char), runtime(numeric), perf(numeric)[resample or simple train test], feat.subset(char)
algo_funs = list()

algo_names = c("cheat", algo_names)
algoaggs[[algo_names[1L]]] = function(res) {
  list()
}

algo_designs[[algo_names[1L]]] = data.frame(lrn = c("classif.ksvm", "classif.ranger", "classif.glmnet"), stringsAsFactors = FALSE)

algo_funs[[algo_names[1L]]] = function(job, data, instance, lrn) {
    res = algoCheating(instance = instance, lrn = lrn)
    return(list(res = res, agg_fun = algoaggs[[algo_names[1L]]]))
}
CHEAT_LOCAL = T
source("bt_pre.R")
DEBUG_FLAG = T # if true: use low budget (only 7 iterations of mbo) set to F
source("bt_conf.R")
source("bt_main.R")
#btInit(path = "registrydebuagsg", local = T)   # set local=T to allow on local PC running
reg = mkReg("haha", replace = T, local = T) 
######################################
mgconf = getGconf()
reg_input = batchtools::getDefaultRegistry()
reg_input$default.resources
#init(prob_names, prob_inputs_conf, prob_funs, algo_names, algo_funs, reg_input, algo_designs, repls = mgconf$REPLS)
#
addProblem(name = prob_names[[1L]],  data = prob_inputs_conf, fun = prob_funs[[1]], seed = 1L)
addAlgorithm(name = algo_names, fun = algo_funs[[algo_names]])
addExperiments(prob.design = prob_designs, algo.design = algo_designs, repls = 10)

testJob(1)
