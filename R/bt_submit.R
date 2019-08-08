# oml_task_id
dt = unwrap(getJobPars()[problem == "prob_oml_stratif"][, .(job.id, prob.pars)])[task_id == 14966]
submitJobs(dt$job.id)

submit_jobs_by_replication = function() {
  index = seq.int(from = 1, to = 1800, by = 30)  # 30 replications
  repl = 1:10
  tosub = c()
  for (i in seq_along(repl)) tosub = c(tosub, index + i)
  tosub2 = intersect(tosub, findNotSubmitted()$job.id)
  submitJobs(tosub2)
  submitJobs(index)
  submitJobs(index + 1)
  submitJobs(index + 2)
  submitJobs(ids = findExpired(), resources = list(walltime = 14400, memory = 2048))
}
