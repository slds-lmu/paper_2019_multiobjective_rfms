source("bt_bootstrap.R")
#' source("bt_conf.R"); funGenProb_geo(data = prob_inputs, job = NULL, openbox_ind = 1, lockbox_ind = 1)
funGenProb_geo = function(data, job, openbox_ind, lockbox_ind) {
  tuple = prepareDataSite(path = data$path[["geo"]])
  funGenProb(tuple, job, openbox_ind, lockbox_ind)
}

#' funGenProb_oml_cluster(data = NULL, job = NULL, openbox_ind = 1, lockbox_ind = 1, task_id = 14966, pca_var_ratio = 0.1)
funGenProb_oml_cluster = function(data, job, openbox_ind, lockbox_ind, task_id, pca_var_ratio) {
  task_mlr = loadDiskOMLMlrTask(task_id)
  list_dataset_index = clusterMlrTask(task_mlr, n_datasets = 5L, balanced = T, pca_var_ratio = pca_var_ratio)
  names(list_dataset_index) = paste0("ds", 1L:5L)  # names got lost during return
  df_dataset_accn = sapply(1:getTaskSize(task_mlr), function(x) {
    bvec = sapply(list_dataset_index, function(vec) x %in% vec)
    ind = which(bvec)
    checkmate::assert(length(ind) == 1L)
    ind
  })
  df_dataset_accn = paste0("ds", df_dataset_accn)
  tuple = list(task = task_mlr, list_dataset_index = list_dataset_index, df_dataset_accn = df_dataset_accn)
  funGenProb(tuple, job, openbox_ind, lockbox_ind)
}

#' funGenProb_oml_stratif(data = NULL, job = NULL, openbox_ind = 1, lockbox_ind = 1, task_id = 14966)
funGenProb_oml_stratif = function(data, job, openbox_ind, lockbox_ind, task_id) {
  task_mlr = loadDiskOMLMlrTask(task_id)
  tuple = createRandomStratifPartition(task_mlr)
  funGenProb(tuple, job, openbox_ind, lockbox_ind)
}

funGenProb = function(data, job, openbox_ind, lockbox_ind) {
  ratio_inbag = getGconf()$ratio_inbag
  tuple = data
  res = list()
  res$openbox_ind = openbox_ind
  res$lockbox_ind = lockbox_ind
  task_oracle = tuple$task
  res$task = task_oracle
  dataset_index = tuple$list_dataset_index  # list of instance index for each dataset
  ns = names(dataset_index)
  checkmate::assert(!is.null(ns))
  res$ns = ns
  res$dataset_index = dataset_index
  openbox_name = ns[openbox_ind]
  res$openbox_name = openbox_name
  lockbox_name = setdiff(ns, openbox_name)[lockbox_ind]
  res$lockbox_name = lockbox_name
  curator_names = setdiff(ns, c(openbox_name, lockbox_name))
  res$curator_names = curator_names

  openbox_oracle_ids = which(tuple$df_dataset_accn == openbox_name)
  openbox_inbag_ind_rel = sample(length(openbox_oracle_ids), size = length(openbox_oracle_ids) * ratio_inbag)
  openbox_inbag_ind = openbox_oracle_ids[openbox_inbag_ind_rel]
  res$openbox_inbag_ind = openbox_inbag_ind
  openbox_outbag_ind =  openbox_oracle_ids[-openbox_inbag_ind_rel]
  test.inds = setdiff(1:getTaskSize(task_oracle), openbox_inbag_ind)  # test ind does not matter
  rins = makeFixedHoldoutInstance(openbox_inbag_ind, test.inds, getTaskSize(task_oracle))
  rins$desc$predict = "train" # must be both since some measure aggregation is test
  res$rins = rins
  curator_list = lapply(curator_names, function(x) {
    inds = which(tuple$df_dataset_accn == x)
    len = length(inds)
    inbag_inds_rel = sample(len, size = len * ratio_inbag)
    inbag = inds[inbag_inds_rel]
    outbag = inds[-inbag_inds_rel]
    list(inbag = inbag, outbag = outbag, len = len)
  })

  curator_inbags_oracle_inds = Reduce(c, lapply(curator_list, function(x) x$inbag))
  lockbox_oracle_inds = which(tuple$df_dataset_accn == lockbox_name)

  dataset_index_outbag = lapply(curator_list, function(x) x$outbag)
  names(dataset_index_outbag) = curator_names
  dataset_index_outbag[[openbox_name]] =  openbox_outbag_ind
  dataset_index_outbag[[lockbox_name]] =  lockbox_oracle_inds
  res$dataset_index_outbag = dataset_index_outbag

  dataset_index_inbag = lapply(curator_list, function(x) x$inbag)
  names(dataset_index_inbag) = curator_names
  dataset_index_inbag[[openbox_name]] = openbox_inbag_ind
  dataset_index_inbag[[lockbox_name]] = lockbox_oracle_inds
  res$dataset_index_inbag =  dataset_index_inbag

  res$tname = mlr::getTaskTargetNames(task_oracle)
  res$p = getTaskNFeats(task_oracle)
  dfpair = mlr::getTaskData(task_oracle, target.extra = T) # dfpair$target, higher level(defined by sort) correspond to negative class of mlr, but positive class in ROCR::prediction function, for more refer to ROCR::prediction documentation
  secondlevel = sort(levels(dfpair$target))[2L]
  res$bname = mlr::getTaskDesc(task_oracle)$negative
  #FIXME: add assert to check secondlevel and bname must be the same
  res$curator_index = Reduce(c, dataset_index[curator_names])
  res$curator_len = length(res$curator_index)
  res$task_openbox_all = mlr::subsetTask(task_oracle, subset = dataset_index[[openbox_name]])
  res$task_curator_inbag = mlr::subsetTask(task_oracle, subset = unlist(dataset_index_inbag[curator_names]))
  res$curator_inbag_len = getTaskSize(res$task_curator_inbag)
  res$task_lockbox = mlr::subsetTask(task_oracle, subset = dataset_index[[lockbox_name]])
  res$curator_len_list = lapply(curator_list, function(x) x$len)
  #bootstrap_alphas = seq(from = 0.1, to = 0.9, length.out = 10)
  #bootstrap_rep = 10L
  #res$list_alpha_bootstrap_index = lapply(bootstrap_alphas, function(alpha) genBootstrapPool(res, alpha = alpha, rep = bootstrap_rep))
  #names(res$list_alpha_bootstrap_index) = bootstrap_alphas
  return(res)
}


###
prob_designs = list()
prob_funs = list()
#
prob_names = c("prob_geo")
prob_funs[[prob_names[[1L]]]] = funGenProb_geo
prob_designs[[prob_names[1L]]] = expand.grid(openbox_ind = 1:5, lockbox_ind = 1:4, dataset_name = "geo", stringsAsFactors = FALSE)
#
prob_names = c("prob_oml_cluster", prob_names)
prob_funs[[prob_names[[1L]]]] = funGenProb_oml_cluster
prob_designs[[prob_names[1L]]] = expand.grid(openbox_ind = 1:5, lockbox_ind = 1:4, task_id = getGconf()$task.ids, dataset_name = paste0("oml_t_", getGconf()$task.ids), pca_var_ratio = c(0.1, 0.5), stringsAsFactors = FALSE)
#
prob_names = c("prob_oml_stratif", prob_names)
prob_funs[[prob_names[[1L]]]] = funGenProb_oml_stratif
prob_designs[[prob_names[1L]]] = expand.grid(openbox_ind = 1:5, lockbox_ind = 1:4, task_id = getGconf()$task.ids, dataset_name = paste0("oml_t_", getGconf()$task.ids), stringsAsFactors = FALSE)
