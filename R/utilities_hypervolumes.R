library(data.table)
library(hrbrthemes)
library(ggplot2)

## combine results using pca-kmeans from all datasets
genDT4plot_pcacluster = function() {
  dat1 = as.data.table(readRDS(file = "output/dt_14966_pca1.rds"))
  colnames(dat1)
  unique(dat1$algo)
  dat1$algo = sapply(dat1$algo, function(x) {
     if (x == "rand") return("rand_mo")
     return(x)
  })
  dat1$dataset = "oml14966"
  dat2 = as.data.table(readRDS(file = "output/dt_3608_pca1.rds"))
  dat2$dataset = "oml3608"
  colnames(dat2)
  unique(dat2$algo)

  dat3 = as.data.table(readRDS(file = "output/dt_3891_pca1.rds"))
  dat3$dataset = "oml3891"
  dat = rbind(dat1, dat2, dat3, fill = TRUE)
  dat$split = "cluster"
  return(dat)
}


## combine results using random stratification from all datasets
genDT4plot_rand_stratif = function() {
  dat1 = as.data.table(readRDS(file = "output/dt_14966_stratif.rds"))
  dat1$dataset = "oml14966"
  dat1$algo = sapply(dat1$algo, function(x) {
     if (x == "rand") return("rand_mo")
     return(x)
  })
  dat2 = as.data.table(readRDS(file = "output/dt_3608_stratif.rds"))
  dat2$dataset = "oml3608"
  dat3 = as.data.table(readRDS(file = "output/dt_3891_stratif.rds"))
  dat3$dataset =  "oml3891"
  dat = rbind(dat1, dat2, dat3, fill = TRUE)
  dat$split = "srs"
  return(dat)
}

genhv = function(dat) {
  list_res = list()
  dat = dat[bag == "outbag", ]
  if (!"dataset" %in% colnames(dat)) dat$dataset = "unknown"
  unique_ids_algo4job = c("algo", "openbox_name", "lockbox_name", "lrn", "repl", "dataset")
  unique_ids_job = c("openbox_name", "lockbox_name", "lrn", "repl", "dataset")
  dat = dat[with(dat, !(algo %in% kickout)), ]

  ns = dat[, .N, by = unique_ids_algo4job]
  table(ns$algo, ns$N)  # fmo only has 179 out of 600 occurrences to return 1 result

  hv = function(x) {
    pf = mco::paretoFilter(as.matrix(x))
    pf = unique(pf)
    mco::dominatedHypervolume(pf, rep(1, 3))
  }

  dat[, .N, by = unique_ids_algo4job]  # number of algos multiplied by number of jobs
  dat[, .N, by = unique_ids_job]
  dat2 = dat[, list(dhv = hv(.SD)), by = unique_ids_algo4job, .SDcols = c("curator", "lockbox", "openbox")]  # when there are multiple datasets, hv take all datasets into consideration, so if there are some replications that are not finished, the nrow of dat2 would be smaller than $#algo*#experiment$

  ns2 = dat2[, .N, by = unique_ids_algo4job]
  unique(ns2$N) # fmo gen multiple points on pareto front, hv calculation merge all of them
  table(ns2$algo, ns2$N)  # fmo only has 179 out of 600 occurrences to return 1 result


  list_res$dat2s = split(dat2, dat2$lrn)

  list_res$dat3 = dat2[, list(mdhv = mean(dhv)), by = unique_ids_algo4job]

  # Number of wins and losses
  fr = function(x, algo) {
    design = expand.grid(i = seq_along(x), j = seq_along(x))
    design = design[design[, 1] != design[, 2], ]
    wins = apply(design, 1, function(a) {
      if (x[a[1]] > x[a[2]]) 1
      else if (x[a[1]] == x[a[2]]) 0.5
      else 0
})
    data.table(algo1 = algo[design[, 1]], algo2 = algo[design[, 2]], wins = wins)
  }

  dat2[, .N, by = unique_ids_job]
  dat4 = dat2[, fr(dhv, algo), by = unique_ids_job]
  list_res$dat4a = dat4[, list(wins = sum(wins)), by = c("algo1", "algo2")]
  list_res$n.exp = nrow(dat[, .N, unique_ids_job])
  return(list_res)
}
