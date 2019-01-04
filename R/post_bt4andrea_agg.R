library(batchtools)
reg = LoadRegistry("registry1")
res = reduceResultsList(ids = findDone(), fun = function(job, res) {
    list(repl = job$repl, prob.pars = job$prob.pars, algo.pars = job$algo.pars, res = res)
})
save(res, file = "DortmundClusterRes.RData")
