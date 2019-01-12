# if (!exists("prob_inputs_data") || is.null(prob_inputs_data) || length(prob_inputs_data) == 0) {
#   stop("please run pre_bt.R to generate the data before running batchtools")
# }

test_funGenProb = function() {
 instance = funGenProbOracle(data = prob_inputs_data, job = NULL, openbox_name = 1L, lockbox_name = 1L, dataset_name = "geo")
 source("bt_algo.R")
 algo_thresholdout(instance)
}

funGenProbOracle = function(data, job, openbox_ind, lockbox_ind, dataset_name) {
  ratio = 0.8  # when ratio = 1,  it went back to the original
  tuple = data[[dataset_name]]
  #tid = as.integer(stringi::stri_replace(str = dataset_name, regex = "oml", replacement = ""))
  #tuple = createClassBalancedDfCluster(oml_task_id = tid, n_datasets = 5, balanced = TRUE, pca_var_ratio = 0.7)
  task_oracle = tuple$task
  dataset_index = tuple$list_dataset_index  # list of instance index for each dataset
  ns = names(dataset_index)
  openbox_name = ns[openbox_ind]
  lockbox_name = setdiff(ns, openbox_name)[lockbox_ind]
  curator_names = setdiff(ns, c(openbox_name, lockbox_name))

  openbox_oracle_ids = which(tuple$df_dataset_accn == openbox_name)
  openbox_inbox_ind = sample(length(openbox_oracle_ids), size = length(openbox_oracle_ids) * ratio)
  train.inds = openbox_oracle_ids[openbox_inbox_ind]
  openbox_outbox_ind =  openbox_oracle_ids[-openbox_inbox_ind]

  curator_list = lapply(curator_names, function(x) {
    inds = which(tuple$df_dataset_accn == x)
    len = length(inds)
    inbox_inds = sample(len, size = len * ratio)
    inbox = inds[inbox_inds]
    outbox = inds[-inbox_inds]
    list(inbox = inbox, outbox = outbox, len = len)
  })

  curator_inboxs_oracle_inds = Reduce(c, lapply(curator_list, function(x) x$inbox))
  lockbox_oracle_inds = which(tuple$df_dataset_accn == lockbox_name)

  dataset_index_outbox = lapply(curator_list, function(x) x$outbox)
  names(dataset_index_outbox) = curator_names
  dataset_index_outbox[[openbox_name]] =  openbox_outbox_ind
  dataset_index_outbox[[lockbox_name]] =  lockbox_oracle_inds

  dataset_index_inbox = lapply(curator_list, function(x) x$inbox)
  names(dataset_index_inbox) = curator_names
  dataset_index_inbox[[openbox_name]] = openbox_inbox_ind
  dataset_index_inbox[[lockbox_name]] = lockbox_oracle_inds

  test.inds = setdiff(1:getTaskSize(task_oracle), train.inds)
  rins = makeFixedHoldoutInstance(train.inds, test.inds, getTaskSize(task_oracle))
  rins$desc$predict = "train" # must be both since some measure aggregation is test
  tname = mlr::getTaskTargetNames(task_oracle)
  p = getTaskNFeats(task_oracle)
  dfpair = mlr::getTaskData(task_oracle, target.extra = T) # dfpair$target, higher level(defined by sort) correspond to negative class of mlr, but positive class in ROCR::prediction function, for more refer to ROCR::prediction documentation
  secondlevel = sort(levels(dfpair$target))[2L]
  bname = mlr::getTaskDesc(task_oracle)$negative
  # it seems that secondlevel and bname are the same
  curator_index = Reduce(c, dataset_index[curator_names])
  tmf = mlr::subsetTask(task_oracle, subset = dataset_index[[openbox_name]])
  tms = mlr::subsetTask(task_oracle, subset = curator_index)
  tge = mlr::subsetTask(task_oracle, subset = dataset_index[[lockbox_name]])
  return(list(task = task_oracle, rins = rins, openbox_ind = openbox_ind, lockbox_ind = lockbox_ind, dataset_index = dataset_index, ns = ns, tmf = tmf, tms = tms, tge = tge, tname = tname, bname = bname, p = p, curator_names = curator_names, openbox_name = openbox_name, lockbox_name = lockbox_name, curator_index = curator_index, curator_task = tms, curator_len = length(curator_index), dataset_index_outbox = dataset_index_outbox, dataset_index_inbox = dataset_index_inbox))
}



prob_names = c("prob")
prob_funs = list()
prob_funs[[prob_names[[1L]]]] = funGenProbOracle
prob_designs = list()
prob_designs[[prob_names[1L]]] = expand.grid(openbox_ind = 1:5, lockbox_ind = 1:4, dataset_name = dataset_names_input, stringsAsFactors = FALSE)
