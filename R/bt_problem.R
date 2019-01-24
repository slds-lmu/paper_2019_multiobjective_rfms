# if (!exists("prob_inputs_data") || is.null(prob_inputs_data) || length(prob_inputs_data) == 0) {
#   stop("please run pre_bt.R to generate the data before running batchtools")
# }

dataset_names_input = c("geo")

test_funGenProb = function() {
 instance = funGenProbOracle(data = prob_inputs_data, job = NULL, openbox_name = 1L, lockbox_name = 1L, dataset_name = "geo")
 source("bt_algo.R")
 algo_thresholdout(instance)
}

funGenProbOracle = function(data, job, openbox_ind, lockbox_ind, dataset_name) {
  res = list()
  res$openbox_ind = openbox_ind
  res$lockbox_ind = lockbox_ind
  ratio = 0.8  # when ratio = 1,  it went back to the original
  tuple = data[[dataset_name]]
  #tid = as.integer(stringi::stri_replace(str = dataset_name, regex = "oml", replacement = ""))
  #tuple = createClassBalancedDfCluster(oml_task_id = tid, n_datasets = 5, balanced = TRUE, pca_var_ratio = 0.7)
  task_oracle = tuple$task
  res$task = task_oracle
  dataset_index = tuple$list_dataset_index  # list of instance index for each dataset
  ns = names(dataset_index)
  openbox_name = ns[openbox_ind]
  lockbox_name = setdiff(ns, openbox_name)[lockbox_ind]
  curator_names = setdiff(ns, c(openbox_name, lockbox_name))
  res$ns = ns

  openbox_oracle_ids = which(tuple$df_dataset_accn == openbox_name)
  openbox_inbag_ind_rel = sample(length(openbox_oracle_ids), size = length(openbox_oracle_ids) * ratio)
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
    inbag_inds = sample(len, size = len * ratio)
    inbag = inds[inbag_inds]
    outbag = inds[-inbag_inds]
    list(inbag = inbag, outbag = outbag, len = len)
  })

  curator_inbags_oracle_inds = Reduce(c, lapply(curator_list, function(x) x$inbag))
  lockbox_oracle_inds = which(tuple$df_dataset_accn == lockbox_name)

  dataset_index_outbag = lapply(curator_list, function(x) x$outbag)
  names(dataset_index_outbag) = curator_names
  dataset_index_outbag[[openbox_name]] =  openbox_outbag_ind
  dataset_index_outbag[[lockbox_name]] =  lockbox_oracle_inds

  dataset_index_inbag = lapply(curator_list, function(x) x$inbag)
  names(dataset_index_inbag) = curator_names
  dataset_index_inbag[[openbox_name]] = openbox_inbag_ind
  dataset_index_inbag[[lockbox_name]] = lockbox_oracle_inds

  tname = mlr::getTaskTargetNames(task_oracle)
  p = getTaskNFeats(task_oracle)
  dfpair = mlr::getTaskData(task_oracle, target.extra = T) # dfpair$target, higher level(defined by sort) correspond to negative class of mlr, but positive class in ROCR::prediction function, for more refer to ROCR::prediction documentation
  secondlevel = sort(levels(dfpair$target))[2L]
  bname = mlr::getTaskDesc(task_oracle)$negative
  # it seems that secondlevel and bname are the same
  curator_index = Reduce(c, dataset_index[curator_names])
  res$task_openbox_all = mlr::subsetTask(task_oracle, subset = dataset_index[[openbox_name]])

  res$task_curator_inbag = mlr::subsetTask(task_oracle, subset = unlist(dataset_index_inbag[curator_names]))
  res$curator_inbag_len = getTaskSize(res$task_curator_inbag)
  tge = mlr::subsetTask(task_oracle, subset = dataset_index[[lockbox_name]])
  curator_len_list = lapply(curator_list, function(x) x$len)
  res = c(res, list(dataset_index = dataset_index, tge = tge, tname = tname, bname = bname, p = p, curator_names = curator_names, openbox_name = openbox_name, lockbox_name = lockbox_name, curator_index = curator_index, curator_len = length(curator_index), dataset_index_outbag = dataset_index_outbag, dataset_index_inbag = dataset_index_inbag, curator_len_list = curator_len_list))
  return(res)
}



prob_names = c("prob")
prob_funs = list()
prob_funs[[prob_names[[1L]]]] = funGenProbOracle
prob_designs = list()
prob_designs[[prob_names[1L]]] = expand.grid(openbox_ind = 1:5, lockbox_ind = 1:4, dataset_name = dataset_names_input, stringsAsFactors = FALSE)
