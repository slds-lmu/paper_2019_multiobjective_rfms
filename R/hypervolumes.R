library(data.table)
library(hrbrthemes)
library(ggplot2)
context = "testcombine"
context = "geo"
context = "oml14966_pca1"
context = "oml10101_stratif"
context = "oml14966_stratif"
context = "oml3608_pca1"
context = "oml_combined_stratif"
context = "oml_3891_pca1"
context = "oml_3891_stratif"
context = "oml_combined_pca_3sets"
context = "oml_combined_stratif_3sets"
context = "debug"



single = function() {
dat = as.data.table(readRDS(file = "dt_10101_stratif.rds"))
dat = as.data.table(readRDS(file = "dt_10101_stratif.rds"))
dat = as.data.table(readRDS(file = "dt_lambdaJan31.rds"))
dat = as.data.table(readRDS(file = "dt_3608_pca1.rds"))
dat = as.data.table(readRDS(file = "dt_3891_pca1.rds"))
dat = as.data.table(readRDS(file = "dt_3891_stratif.rds"))
}

pcacluster = function() {
  dat1 = as.data.table(readRDS(file = "dt_14966_pca1.rds"))
  colnames(dat1)
  unique(dat1$algo)
  dat1$algo = sapply(dat1$algo, function(x) {
     if(x=="rand") return("rand_mo")
     return(x)})
  dat1$dataset = "oml14966"
  dat2 = as.data.table(readRDS(file = "dt_3608_pca1.rds"))
  dat2$dataset = "oml3608"
  colnames(dat2)
  unique(dat2$algo)

  dat3 = as.data.table(readRDS(file = "dt_3891_pca1.rds"))
  dat3$dataset = "oml3891"
  dat = rbind(dat1, dat2, dat3, fill = TRUE)
  dat$split = "cluster"
}


stratif = function() {
  dat1 = as.data.table(readRDS(file = "dt_14966_stratif.rds"))
  dat1$dataset = "oml14966"
  dat1$algo = sapply(dat1$algo, function(x) {
     if(x=="rand") return("rand_mo")
     return(x)})
  dat2 = as.data.table(readRDS(file = "dt_3608_stratif.rds"))
  dat2$dataset = "oml3608"
  dat3 = as.data.table(readRDS(file = "dt_3891_stratif.rds"))
  dat3$dataset =  "oml3891"
  dat = rbind(dat1, dat2, dat3, fill = TRUE)
  dat$split = "srs"
}

genhv = function(dat) {
  list_res = list()
  dat = dat[bag == "outbag", ]
  if (!"dataset" %in% colnames(dat)) dat$dataset = "unknown"
  unique_ids_algo4job = c("algo", "openbox_name", "lockbox_name", "lrn", "repl", "dataset")
  unique_ids_job = c("openbox_name", "lockbox_name", "lrn", "repl", "dataset")
  kickout = c("fso_ladder")
  #kickout = c("fso_ladder", "fso_th")

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

list_res = genhv(dat)

pdf(sprintf("wins_and_losses_%s.pdf", context), width = 8, height = 6)
n.exp = list_res$n.exp
ggplot(data = list_res$dat4a, mapping = aes(x = algo1, y = algo2)) + geom_tile(aes(fill = wins)) + geom_text(aes(label = wins, color = abs(wins - n.exp / 2) >= 125), size = 5) +
  scale_color_manual(guide = FALSE, values = c("black", "white")) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", name = "Times",
    midpoint = n.exp / 2) + xlab("Winner") + ylab("Loser") + theme_bw() + theme(axis.text=element_text(size=24, face="bold"), axis.title=element_text(size = 24, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), legend.title = element_text(size = 24)) 
dev.off()




pdf(file = sprintf("mean_hypervolumes_%s.pdf", context), height = 7, width = 10)
ggplot(data = list_res$dat3, mapping = aes(y = mdhv, x = algo, fill = algo)) + geom_boxplot() + theme_bw() + scale_fill_ipsum() + xlab("Algorithm") + ylab("Mean dominated hyper volume") + theme(axis.text=element_text(size=24, face="bold"), axis.title=element_text(size = 24, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) + facet_wrap("lrn") + theme(strip.text.x = element_text(size = 24, colour = "black"), legend.text=element_text(size=24), legend.title=element_text(size=24))
dev.off()


pdf(file = sprintf("hypervolumes_%s.pdf", context), height = 7, width = 10)
lapply(list_res$dat2s, function(d) {
  ggplot(data = d, mapping = aes(y = dhv, x = algo, color = algo)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
    xlab("Algorithm") + ylab("Dominated hyper volume") +
    ggtitle(paste("Rows: openbox, columns: lockbox, learner:", unique(d$lrn))) +
    facet_grid(openbox_name ~ lockbox_name)
})
dev.off()
