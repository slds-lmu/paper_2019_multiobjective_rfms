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
