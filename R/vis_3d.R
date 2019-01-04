#Every two plots sitting in one page belong to the same combination: Modeling fitting dataset and Model Generlization Dataset (for definition, see the paper draft). Their correponding learner is printed before the two plots in each page.

#The first plot is the   star plot, for each "star", each edge correpond to mmce on the correponding dataset, which direction correpond to which is denoted in the legend (the 5 edge polygon)

#The second plot is 3d plot, with 2d denoteing the pareto front(due to resoluion, sometimes it seems not to be pareto but they are) and the 3rd dimension is the generlization error on the generalization dataset. 
#bs means baseline, and pr means proposal.





options(warn = -1)
source("mystars.R")
library("batchtools")
library(ggradar)
library(scatterplot3d)
library(rgl)
library(magrittr)
library(data.table)
library(ggplot2)
library(ggiraphExtra)
library(plot3D)


genplot = function(onres) {
  source("mystars.R")
  library(ggradar)
  res = onres$res
  # single obj
  so = res$res$gperf_side$bs1
  bs_ind = res$res$tune_res_bs$mbo.result$best.ind
  sodt = data.table::rbindlist(so[bs_ind])
  sodt$algo = "bs1"
  #bs_mmce = res$res$tune_res_bs$mbo.result$y
  lrn.id = res$res$tune_res_bs$learner$id
  print(lrn.id)
  ## multi obj
  ind = res$res$tune_res_pr$ind
  res$res$tune_res_pr$y
  pareto.list = res$res$gperf_side$pr[ind]
  list.dt = data.table::rbindlist(pareto.list)
  list.dt$algo = "pr"

  ns = names(pareto.list[[1]])
  bspr = rbind(sodt, list.dt)
  bspr$major_name = ns[res$res$major_level]
  bspr$test_name = res$res$test_name
  bspr$lrn.id = lrn.id
  majorname = ns[res$res$major_level]
  testname = res$res$test_name
  msname = setdiff(ns, c(majorname, testname))
  side = apply(as.data.frame(bspr)[, msname], 1, FUN = mean)
  major = bspr[[majorname]]
  test = bspr[[testname]]
  bspr$algo = as.factor(bspr$algo)
  algolevels = levels(bspr$algo)
  algocolors = as.numeric(bspr$algo)
  legend1 = as.character(bspr$algo[which(algocolors == 1)])
  legend2 = as.character(bspr$algo[which(algocolors == 2)])[1]
  as.numeric(bspr$algo)

starplot = mystars(bspr[, 1:5], key.cex = 0.2, key.labelsize = 1, key.loc = c(5, 3), xpd  = T, scale = F, len = 1, frame.plot = F, key.labels = ns, axes = T, labels = as.character(bspr$algo), xlab = paste0("major:", as.character(bspr$major_name[1L]), "-test:", as.character(bspr$test_name[1L])), lwd = 2)

#ggRadar(data=iris,aes(color=Species))
#ggRadar(data=iris,aes(color=Species),interactive=TRUE)

forRadar = bspr[, 1:6]
forRadar$algo = paste0(forRadar$algo, 1:nrow(forRadar))
radarplot = ggRadar(data = forRadar, aes(color = algo), scale = F, alpha = 0.1, ylim = c(0,1))
plot(radarplot)

print("without using ylim")
radarplot = ggRadar(data = forRadar, aes(color = algo), scale = F, alpha = 0.1)
plot(radarplot)

print("with scale")
radarplot = ggRadar(data = forRadar, aes(color = algo), scale = T, alpha = 0.1)
plot(radarplot)

print("scatterplot3d")

s3d = scatterplot3d(data = bspr, x = major, y = side, z = test, angle = 55, xticklabs = seq(1,0,-0.01), type = "h", pch = algocolors, main = paste0("major:", as.character(bspr$major_name[1L]), "-test:", as.character(bspr$test_name[1L])))
legend(s3d$xyz.convert(-0.2, 0, 1), legend = c(legend1, legend2), pch = c(1, 2), lty = 1, cex = 1.8)

print(bspr)

dt_3d = data.table(major = major, side = side, test = test, algo = bspr$algo)
#scatter3D(x = major, y = side, z = test, colvar = as.integer(bspr$algo), col =as.integer(bspr$algo), ticktype = "detailed")
#scatter3D(x = major, y = side, z = test, colvar = as.integer(bspr$algo), col =as.integer(bspr$algo), ticktype = "detailed", type = "h", bty = "g", pch = 19, cex = 0.5, phi = 0)
return(list(dt_3d = dt_3d, radarplot = radarplot, starplot = starplot))
}


#   library(BBmisc)
#   source("bt_conf.R")
#   library("batchtools")
#   REG_FILE_DIR = "openml_gina_kmeans_snicker"
#   reg = loadRegistry(file.dir = REG_FILE_DIR, work.dir = getwd(), conf.file = NA)
#   reg$writeable = TRUE  # never execute this line when the process is running.
#   ids = findDone(reg = reg)
# 
# res.list = reduceResultsList(ids = findDone(), fun = function(job, res) {list(prob.pars = job$prob.pars, algo.pars = job$algo.pars, res = res)})
# 
index = seq.int(from = 1, to = 600, by = 10)
#index = intersect(index, findDone()$job.id)

res.list = res
 big.list = lapply(res.list[index], genplot)

data.list = lapply(big.list, function(x) x$dt_3d)
bigtable = rbindlist(data.list)

#scatter3D(x = bigtable$major, y = bigtable$side, z = bigtable$test, colvar = as.integer(bigtable$algo), col =as.integer(bigtable$algo), ticktype = "detailed", type = "p", bty = "g", pch = 19, cex = 0.5, phi = 0, labels = c("bs", "pr"))
#scatter3D(x = bigtable$major, y = bigtable$side, z = bigtable$test, colvar = as.integer(bigtable$algo), col = as.integer(bigtable$algo), ticktype = "detailed", type = "h", bty = "g", pch = 19, cex = 0.5, phi = 0, labels = c("bs", "pr"), colkey = F)
#scatter3D(x = bigtable$major, y = bigtable$side, z = bigtable$test, col = as.integer(bigtable$algo), ticktype = "detailed", type = "p", bty = "g", pch = 19, cex = 0.5, phi = 0, labels = c("bs", "pr"), colkey = FALSE)  # colkey is to turn off legend

#scatter3D(x = bigtable$major, y = bigtable$side, z = bigtable$test, colvar = as.integer(bigtable$algo), col = c("#1B9E77", "#D95F02"), ticktype = "detailed", type = "p", bty = "g", pch = 19, cex = 0.5, phi = 0, labels = c("bs", "pr"), colkey = list(at = c(2, 3, 4), side = 1, addlines = TRUE, length = 0.5, width = 0.5, labels = as.character(bigtable$algo)))
#scatter3D(x = bigtable$major, y = bigtable$side, z = bigtable$test, colvar = as.integer(bigtable$algo), col = c("#1B9E77", "#D95F02"), ticktype = "detailed", type = "p", bty = "g", pch = 19, cex = 0.5, phi = 0, labels = c("bs", "pr"), colkey = list(at = c(2, 3, 4), side = 1, addlines = TRUE, length = 0.5, width = 0.5, labels = c("bs", "pr")))











hh = function(){
par(cex = 0.01)
#library(ggradar)
#ggradar(plot.data = bspr[, 1:5], axis.labels = colnames(bspr))
#stars(bspr[,1:5], key.loc = c(2,4), xpd  = T, scale = F, len = 1, cex = 0.8, key.labels = ns)
with(list.df, scatterplot3d(x, y, z, angle = 55, color = q, xticklabs = seq(1,0,-0.01), type = "h"))
with(list.df, scatterplot3d(x, y, z, angle = 55, xticklabs = seq(1,0,-0.01), type = "h", pch = q))

x = pareto.list[[1]]$GSE16446
y = pareto.list[[1]]$GSE20194
z = pareto.list[[1]]$GSE20207

df = unwrap(getJobPars()[1, list(prob.pars, algo.pars)])
df[, major_level]
df[, test_level]

library(latticeExtra)
d <- read.table(text='q  x   y     z
1 0.2   0.5   0.3
3 0.5   0.22   0.4
1 0.7   0.4   0.5
3 0.2   0.8   0.6
1 0.10   0.11  0.7
3 0.99   0.3   0.8
1 0.10  0.50  0.9
3 0.45  0.12   0.2
1 0.76  0.35   0.1', header=TRUE)
d$q = as.factor(d$q)

cloud(z~x+y, d, panel.3d.cloud=panel.3dbars, col=d$q, screen = list(z = 30, x = -60),
      xbase=0.001, ybase=0.001, scales=list(arrows=FALSE, col=1), 
      par.settings = list(axis.line = list(col = "transparent")),  ticks = T)
panel.grid()


library(scatterplot3d)
with(d, scatterplot3d(x, y, z, xticklabs = seq(0,1,0.01), type = "h", angle = 55, color = q))
pdf("hi.pdf")
with(d, scatterplot3d(x, y, z, angle = 55, color = q, xticklabs = seq(1,0,-0.01), type = "h"))
dev.off()

png("star.png")
stars(mtcars[, 1:7], key.loc = c(14, 1.5), main = "Motor Trend Cars : full stars()")
dev.off()
 
stars(d[, c("x", "y", "z")], len = 1, cex =1, nrow = 5, ncol = 2, key.loc = c(2,7), frame.plot = T)
stars(d[, c("x", "y", "z")], len = 1, cex =0.6,  key.loc = c(1.2,7), frame.plot = T)
stars(d, scale = T, labels = 1:9)
stars(d, scale = T, labels = 1:9, key.loc = c(2, 4), )
stars(d[, c("x", "y", "z")], scale = T, labels = 1:9, key.loc = c(1, 8), draw.segments = T, axes = T, add = T)

library(GPareto)
x = c(0.2, 0.4, 0.6, 0.8)
y <- c(0.8, 0.7, 0.5, 0.1)

plot(x, y, col = "green", pch = 20) 

plotParetoEmp(cbind(x, y), col = "green")
## Alternative
plotParetoEmp(cbind(x, y), col = "red", add = FALSE)

## With maximization
plotParetoEmp(cbind(x, y), col = "blue", max = TRUE)

## 3D plots
library(rgl)
set.seed(5)
X <- matrix(runif(60), ncol=3)
Xnd <- t(nondominated_points(t(X)))
plot3d(X)
plot3d(Xnd, col="red", size=8, add=TRUE)
plot3d(x=min(Xnd[,1]), y=min(Xnd[,2]), z=min(Xnd[,3]), col="green", size=8, add=TRUE)
X.range <- diff(apply(X,2,range))
bounds <- rbind(apply(X,2,min)-0.1*X.range,apply(X,2,max)+0.1*X.range)
plotParetoEmp(nondominatedPoints = Xnd, add=TRUE, bounds=bounds, alpha=0.5)
}
#```
