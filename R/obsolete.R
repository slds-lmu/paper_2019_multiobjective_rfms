###### data
# we will use data from 2 sources: a) datamicroarray b) OML
DMA_PACKAGE_NAME = 'datamicroarray'
DMA_DATA_NAMES = c("alon", "borovecki", "chiaretti", "chin", "chowdary", "gordon",
  "gravier", "pomeroy", "shipp", "singh", "subramanian", "tian", "west")
DMA_TARGET_NAME = "class"  # DMA has no name for y col


genTable = function(onres) {
  res = onres$res$res
  # single obj
  so = res$gperf_side$bs1
  bs_ind = res$tune_res_bs$mbo.result$best.ind
  sodt = data.table::rbindlist(so[bs_ind])
  sodt$algo = "bs1"

  so2 = res$gperf_side$bs2
  bs_ind = res$tune_res_bs2$mbo.result$best.ind
  sodt2 = data.table::rbindlist(so2[bs_ind])
  sodt2$algo = "bs2"

  so3 = res$gperf_side$bs3
  bs_ind = res$tune_res_bs2$mbo.result$best.ind
  sodt3 = data.table::rbindlist(so3[bs_ind])
  sodt3$algo = "bs3"



  #bs_mmce = res$res$tune_res_bs$mbo.result$y
  lrn.id = res$tune_res_bs$learner$id
  print(lrn.id)
  ## multi obj
  ind = res$tune_res_pr$ind
  res$tune_res_pr$y
  pareto.list = res$gperf_side$pr[ind]
  list.dt = data.table::rbindlist(pareto.list)
  list.dt$algo = "pr"
  ns = names(pareto.list[[1]])
  major_name = ns[res$major_level]
  msns = setdiff(ns, c(major_name, res$test_name))
  bspr = rbind(sodt, sodt2, sodt3, list.dt)
  bspr = as.data.frame(bspr)
  bspr$major_name = major_name
  bspr$test_name = res$test_name
  bspr$lrn.id = lrn.id
  bspr$mf = bspr[, major_name]
  bspr$ge = bspr[, res$test_name]
  bspr$ms = rowMeans(bspr[, msns])
  bspr$ds = onres$ds
  bspr$repl = onres$repl
  bspr
}





mk_measure_auc_thout = function(extra.args) {
  #
  ##
  #if(extra.args$perf_name2tune !="auc") stop("this function can only be used for auc!")
  makeMeasure(
    id = "thout.auc",
    name = "meas thout auc",
    properties = auc$properties,
    minimize = auc$minimize,  ## !!!
    aggr = mtrain.agg,
    best = auc$best,        ## !!
    worst = auc$worst,
    fun = fun_obj_thresholdout,
    extra.args = extra.args
    )
}

mk_any_measure_thout = function(extra.args) {
  makeMeasure(
    id = sprintf("thout.%s", extra.args$perf_name2tune),
    name = sprintf("measure thout.%s", extra.args$perf_name2tune),
    properties = auc$properties,
    minimize = FALSE,  ## !!!
    aggr = mtrain.agg,
    best = 0,        ## !!
    worst = 0,
    fun = fun_obj_thresholdout,
    extra.args = extra.args
    )
}

ncv_mmce_generalization = makeMeasure(
  id = "ncv_mmce_major",
  name = "Nested CV mmce on the main dataset",
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  minimize = TRUE,
  best = 0,
  worst = 1,
  fun = fun_measure_obj_ncv_local
  )


# openbox_nocv = makeMeasure(
#   id = "openbox_nocv",
#   name = "openbox no cv",
#   properties = c("classif", "classif.multi", "req.pred", "req.truth"),
#   minimize = TRUE,
#   aggr = mtrain.agg,
#   best = 0,
#   worst = 1,
#   fun = fun_measure_obj_openbox_nocv
#   )
# 
# 

# meas_openbox_cv = makeMeasure(
#   id = "openbox_cv",
#   name = "CV mmce on the openbox dataset",
#   properties = c("classif", "classif.multi", "req.pred", "req.truth"),
#   minimize = TRUE,
#   aggr = mtrain.agg,
#   best = 0,
#   worst = 1,
#   fun = fun_measure_obj_openbox
#   )
# 
# 





# mk_measure_curator = function(extra.args) {
#   makeMeasure(
#     id = "meas_curator",
#     name = "meas curator",
#     properties = c("classif", "classif.multi", "req.pred", "req.truth"),
#     minimize = TRUE,
#     aggr = mtrain.agg,
#     best = 0,
#     worst = 1,
#     fun = fun_measure_obj_curator,
#     extra.args = extra.args
#     )
# }
# 
# ' @title
# ' @description
# ' @param extra.args extra.args$local2remote_subset should contain the index of instances
# ' @return customized measure
# mk_measure_openbox_tr_curator_tune_alpha = function(extra.args) {
#   checkmate::assert_numeric(extra.args$alpha)
#   makeMeasure(
#     id = "local_tr_tune_remote_tune_alpha",
#     name = "alpha mixture of local and remote objective",
#     properties = c("classif", "classif.multi", "req.pred", "req.truth"),
#     minimize = TRUE,
#     aggr = mtrain.agg, best = 0,
#     worst = 1,
#     fun = fun_measure_obj_openbox_tr_curator_tune,
#     extra.args = extra.args
#     )
# }
# 
# mk_measure_local_tr_tune_remote_tune_nocv = function(extra.args) {
#   checkmate::assert_numeric(extra.args$alpha)
#   makeMeasure(
#     id = "local_tr_tune_remote_tune_alpha_nocv",
#     name = "alpha mixture of local and remote objective nocv",
#     properties = c("classif", "classif.multi", "req.pred", "req.truth"),
#     minimize = TRUE,
#     aggr = mtrain.agg,
#     best = 0,
#     worst = 1,
#     fun = fun_measure_obj_local_tr_tune_remote_tune_nocv,
#     extra.args = extra.args
#     )
# }
# 
# 
' @title
' @description
' @param extra.args extra.args$local2remote_subset should contain the index of instances
' @return customized measure
# mk_measure_local2remote = function(extra.args) {
#   measure_cv_local2remote = makeMeasure(
#     id = "cv_local2remote",
#     name = "CV mmce on the main dataset and model selection dataset",
#     properties = c("classif", "classif.multi", "req.pred", "req.truth"),
#     minimize = TRUE,
#     aggr = mtrain.agg,
#     best = 0,
#     worst = 1,
#     fun = fun_measure_obj_openbox2curator,
#     extra.args = extra.args
#     )
#   return(measure_cv_local2remote)
# }

# use fixed hyper-parameter to do cross validation with different training set and aggregate the generalization error
fun_measure_obj_ncv_local = function(variables) {

}




























createImBalancedDf = function(tid) {
  findInlist = function(i, listinst) {
    idx = which(sapply(listinst, function(inst) i %in% inst))  # R python difference
    names(idx)
  }
  require(OpenML)
  taskstr = sprintf("../meta/data_accn_%s.RData", tid)
  list_dataset_index = BBmisc::load2(taskstr)
  sum(sapply(list_dataset_index, function(x) length(unique(x))))
  names(list_dataset_index) = paste0("ds", 1:5)
  length(unique(Reduce(c, lapply(list_dataset_index, unique))))
  # 3891, 14966, 34536
  taskstr = sprintf("../meta/oml%sMlrTask.RData", tid)
  task = BBmisc::load2(taskstr)
  df = getTaskData(task)
  checkmate::assert(!"dataset_accn" %in% colnames(df))
  dsnas = sapply(1:nrow(df), function(i) findInlist(i, list_dataset_index))
  dsnas = unlist(dsnas)
  length(dsnas)
  df$dataset_accn = dsnas
  df_dataset_accn = as.character(df$dataset_accn)
  df_dataset_accn = as.factor(df_dataset_accn)
  # makeResampleInstance always generate the same split which is not wanted
  return(list(task = task, list_dataset_index = list_dataset_index, df_dataset_accn = df_dataset_accn))
}

#' @title
#' @description convert staticProblem and problem designs to generate dynamic part of the problem: {obj, funDesign}
#' @param data is a user defined parameter and could have arbitrary structure
#' @param job is something that the framework need, sigma comes from prob.design parameter of the configuration
#' @param etc parameter comes from prob.design
#' @param major_level The main dataset used for training
#' @param test_level The left dataset(relative index) which the tuning algorithm does not see
#' @example addProblem(name = "prob1",  data = list(field1 = 1, field2 = 2), fun = funGenProb, seed = 1L)
funGenProb = function(data, job, major_level, test_level, dataset_name) {
  tuple = data$list.data[[dataset_name]]
  task = tuple$task
  dataset_index = tuple$list_dataset_index  # list of instance index for each dataset
  train.inds = which(tuple$df_dataset_accn == levels(tuple$df_dataset_accn)[major_level])
  test.inds = which(tuple$df_dataset_accn != levels(tuple$df_dataset_accn)[major_level])
  rins = makeFixedHoldoutInstance(train.inds, test.inds, getTaskSize(task))
  rins$desc$predict = "train" # must be both since some measure aggregation is test
  ns = names(dataset_index)
  mna = ns[major_level]
  tna = setdiff(ns, mna)[test_level]
  sna = setdiff(ns, c(mna, tna))
  tname = mlr::getTaskTargetNames(task)
  p = getTaskNFeats(task)
  dfpair = mlr::getTaskData(task, target.extra = T)
  # dfpair$target, higher level(defined by sort) correspond to negative class of mlr, but positive class in ROCR::prediction function, for more refer to ROCR::prediction documentation
  secondlevel = sort(levels(dfpair$target))[2L]
  bname = mlr::getTaskDesc(task)$negative
  # it seems that secondlevel and bname are the same
  tmf = mlr::subsetTask(task, subset = dataset_index[[mna]])
  tms = mlr::subsetTask(task, subset = Reduce(c, dataset_index[sna]))
  tge = mlr::subsetTask(task, subset = dataset_index[[tna]])
  return(list(task = task, rins = rins, major_level = major_level, test_level = test_level, dataset_index = dataset_index, ns = ns, mna = mna, tna = tna, sna = sna, tmf = tmf, tms = tms, tge = tge, tname = tname, bname = bname, p = p))
}

 auc = measureAUC(probabilities = getPredictionProbabilities(pred), truth = pred$data$truth, negative = pred$task.desc$negative, positive = pred$task.desc$positive)
  cat(sprintf("--auc is %s -- ", auc))
  brier = measureBrier(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
  cat(sprintf("--brier_score %s -- ", brier_score))
  brier.scaled = measureBrierScaled(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
  cat(sprintf("--brier.scaled %s -- ", brier.scaled))
  contingency_tb = table(getPredictionResponse(pred), getPredictionTruth(pred))
  mmce = 1 - sum(diag(contingency_tb)) / sum(contingency_tb)
  cat(sprintf("--mmce: %s --", mmce))
  ber = measureBER(truth = getPredictionTruth(pred), response = getPredictionResponse(pred))
  cat(sprintf("--ber: %s --", ber))
  return(list(mmce = mmce, brier = brier, brier.scaled = brier.scaled, ber = ber, auc = auc))


selectOMLDataSet = function() {
  task.ids = OpenML::getOMLStudy("OpenML100")$tasks$task.id
  OpenML::listOMLStudies()
  require(OpenML)
  idt = lapply(task.ids, function(id) {
    ot = OpenML::getOMLTask(id)
    mt = convertOMLTaskToMlr(ot)
    sum(mt$mlr.task$task.desc$n.feat) >  0.2 * (mt$mlr.task$task.desc$size)
   }
   )
  idt = which(unlist(idt))
  idt = task.ids[idt]
  print(idt)
}
