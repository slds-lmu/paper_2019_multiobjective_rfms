library(batchtools)
reg = LoadRegistry("registry1")
res = reduceResultsList(ids = findDone(), fun = function(job, res) {
    list(repl = job$repl, prob.pars = job$prob.pars, algo.pars = job$algo.pars, gperf_env = res$res$gperf_env, instance = res$res$instance)
})
save(res, file = "DortmundClusterRes.RData")
