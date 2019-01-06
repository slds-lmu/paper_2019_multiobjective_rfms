#' @title
#' @description
#' @param data value
#' @param var.names vector of names for two objective
#' @param group.name learner name for example
#' @param replication.name Column indicating which replicatino is used
#' @param data.name dataset name
#' @param percentiles value
#' @return returndes
#' @examples 
#' library(eaf)
#' eaf.path <- system.file(package="eaf")
#' x <- read.data.sets(file.path(eaf.path, "extdata","example1_dat"))
#' eafs(points = x[,1:2], sets = x[,3])
#' df = rbind(x, x)
#' df$ds = "ds1"
#' df[(nrow(x) +1) : nrow(df),"ds"] = "ds2"
#' df$group = "group1"
#' eafGGPlot(data = df, var.names = c("V1", "V2"), group.name = "group", replication.name = "set", data.name = "ds")


reduceFromList = function() {
  library(data.table)
  res.list = lapply(res, FUN = function(res) {
    onres = res
    onres$repl = res$repl
    onres$ds = "ds1"
    tb = genTable(onres = onres)
    tb
})
  res.table = rbindlist(res.list)
  setkey(res.table, x)
  dtb = res.table[algo %in% c("bs3", "bs1")]

}
function() {
  library(batchtools)
  reg = loadRegistry("reg_dortmund_snicker", conf.file = NA)
  reg = loadRegistry("openml_gina_kmeans_snicker", conf.file = NA)
  reg$writeable = T
  res.table = reduceResultsDataTable(ids = findDone(), fun = function(job, res) {
    onres = res
    onres$repl = job$repl
    onres$ds = "ds1"
    tb = genTable(onres = onres)
    tb
})

  dtb = rbindlist(res.table[, result])
  save(dtb, file = "gina_kmeans_snicker_res_table.RData")
  #df_glmnet =  dtb[lrn.id == "classif.glmnet"]
  #eafGGPlot(data = as.data.frame(df_glmnet), var.names = c("mf", "ms"), group.name = "algo", replication.name = "repl", data.name = "lrn.id", percentiles = c(0, 50, 100))
  #
  #
  pdf("eaf_geo_andrea_bs23.pdf")
  eafGGPlot(data = as.data.frame(dtb), var.names = c("mf", "ms"), group.name = "algo", replication.name = "repl", data.name = "lrn.id", percentiles = c(0, 50, 100))
  dev.off()

  pdf("eaf_geo_andrea_ms_ge_bs12.pdf")
  eafGGPlot(data = as.data.frame(dtb), var.names = c("ms", "ge"), group.name = "algo", replication.name = "repl", data.name = "lrn.id", percentiles = c(0, 50, 100))
  dev.off()

  pdf("eaf_geo_andrea_mf_ge_bs23.pdf")
  eafGGPlot(data = as.data.frame(dtb), var.names = c("mf", "ge"), group.name = "algo", replication.name = "repl", data.name = "lrn.id", percentiles = c(0, 50, 100))
  dev.off()

 pdf("eaf_geo_andrea_mf_bs3_pr.pdf")
  eafGGPlot(data = as.data.frame(dtb), var.names = c("mf", "ge"), group.name = "algo", replication.name = "repl", data.name = "lrn.id", percentiles = c(0, 50, 100))
  dev.off()



  #   car::scatter3d(x = dtb$mf, y = dtb$ms, z = dtb$ge, groups = as.factor(dtb$algo), grid = F, surface = T, ellipsoid = F, surface.col = c("#999999", "#E69F00"))
  #   car::scatter3d(x = dtb$mf, y = dtb$ms, z = dtb$ge, groups = as.factor(dtb$algo), grid = F, surface = F, ellipsoid = T, surface.col = c("#999999", "#E69F00"))
  #   car::scatter3d(x = dtb$mf, y = dtb$ms, z = dtb$ge, groups = as.factor(dtb$algo), grid = F, surface = F)
  # 
  mcolors <- c("#00FF00", "#FF0000", "#0000FF")
  colors <- mcolors[as.integer(as.factor(dtb$lrn.id))]
  selcols = c("mf", "ms", "ge")
  df = as.data.frame(dtb[, ..selcols])
  scatterplot3d(df, pch = 16, color=colors)
  open3d(windowRect=c(100,100,700,700))
  rgl.postscript("scatter3d.pdf",fmt="pdf")
  #plot3d(x=df$var1, y=df$var2, z=df$var3, col=as.numeric(df$var4), size=0.5, type='s',xlab="var1",ylab="var2",zlab="var3")
  source('addgrids3d.r')
  pdf("scatterplot3d.pdf")
  pch2 = c(1,3)
  pchs = pch2[as.integer(as.factor(dtb$algo))]
  scatterplot3d(df, pch = 16, grid=FALSE, box=FALSE, colors = colors)
  s3d = scatterplot3d(df, pch = pchs, grid=FALSE, box=FALSE, colors = colors)
  addgrids3d(df, grid = c("xy", "xz", "yz"))
  text(s3d$xyz.convert(df), labels = dtb$lrn.id,cex= 0.7, col = "steelblue")
  legend("bottom", legend = levels(as.factor(dtb$algo)), col =  c("#00FF00", "#FF0000", "#0000FF"), pch = c(1,3), inset = -0.08, xpd = TRUE, horiz = TRUE)
  dev.off()
}

normalizeTable = function(dtb) {
  dtb2 = dtb[lrn.id == "classif.glmnet"]
  #best = dtb2[, .(smf = min(mf), sge = min(ge), sms = min(ms)), by = .(lrn.id)]
  best = dtb2[, .(smf = min(mf), sge = min(ge), sms = min(ms))]
  dtc = dtb2
  fun.sub = function(x) x - best
  temp = dtb2[, fun.sub(.SD[, .(mf, ge, ms)]), by = seq_len(nrow(dtb2))]
  dtc[, c("mf", "ge", "ms") := temp[, .(mf, ge, ms)]]
  #dtc[, .(mf, ge, ms) := temp[, .(mf, ge, ms)]]
  dtc
  final = dtc[, lapply(.SD, mean), .SDcols = c("mf", "ge", "ms"), by = .(algo)]
  final$algo
  pchs = c(1, 3)
  pchs = pchs[as.integer(as.factor(final$algo))]
  pdf("agg_openml_gina.pdf")
  s3d = scatterplot3d(data = final, x = final$mf, y = final$ms, z = final$ge, angle = 55, type = "h", pch = pchs)
  legend("bottom", legend = levels(as.factor(final$algo)), pch = pchs, inset = -0.08, xpd = TRUE, horiz = TRUE)
  dev.off()
}



genTable = function(onres) {
  res = onres$res$res
  # single obj
  so = res$gperf_side$bs1
  bs_ind = res$tune_res_bs$mbo.result$best.ind
  sodt = data.table::rbindlist(so[bs_ind])
  sodt$algo = "bs1"

  so2 = res$gperf_side$bs2
  bs_ind = res$tune_res_bs2$mbo.result$best.ind
  sodt2 = data.table::rbindlist(so2[bs_ind])
  sodt2$algo = "bs2"

  so3 = res$gperf_side$bs3
  bs_ind = res$tune_res_bs2$mbo.result$best.ind
  sodt3 = data.table::rbindlist(so3[bs_ind])
  sodt3$algo = "bs3"



  #bs_mmce = res$res$tune_res_bs$mbo.result$y
  lrn.id = res$tune_res_bs$learner$id
  print(lrn.id)
  ## multi obj
  ind = res$tune_res_pr$ind
  res$tune_res_pr$y
  pareto.list = res$gperf_side$pr[ind]
  list.dt = data.table::rbindlist(pareto.list)
  list.dt$algo = "pr"
  ns = names(pareto.list[[1]])
  major_name = ns[res$major_level]
  msns = setdiff(ns, c(major_name, res$test_name))
  bspr = rbind(sodt, sodt2, sodt3, list.dt)
  bspr = as.data.frame(bspr)
  bspr$major_name = major_name
  bspr$test_name = res$test_name
  bspr$lrn.id = lrn.id
  bspr$mf = bspr[, major_name]
  bspr$ge = bspr[, res$test_name]
  bspr$ms = rowMeans(bspr[, msns])
  bspr$ds = onres$ds
  bspr$repl = onres$repl
  bspr
}




eafGGPlot = function(data, var.names, group.name, replication.name, data.name, percentiles = 50 ) {
   require(ggplot2)
   d = do.call(rbind, lapply(split(data, data[, data.name]), function(d) {
     eaf = eaf:::eafs(points = d[, var.names], sets = d[, replication.name], groups = d[, group.name], percentiles = percentiles)
     names(eaf) = c(var.names, "percentiles", group.name)
     eaf$dataset = d[1, data.name]
     eaf
   }))

   x.max = max(d[, var.names[1L]])
   y.max = max(d[, var.names[2L]])

   d.splitted = split(d, d[, c(group.name, "percentiles", "dataset")])

   f = function(x) {
     x.min2 = min(x[, var.names[1L]])
     y.min2 = min(x[, var.names[2L]])
     group = x[1L, group.name]
     percentiles = x[1L, "percentiles"]

     points = data.frame(c(x.min2, x.max), c(y.max, y.min2), 
percentiles, group)
     names(points) = c(var.names, "percentiles", group.name)
     points$dataset = x[1, "dataset"]

     rbind(points[1L, ], x, points[2L, ])

   }

   d.splitted2 = lapply(d.splitted, f)
   d2 = do.call(rbind, d.splitted2)
   d2$percentiles = as.factor(d2$percentiles)

   p = ggplot(data = d2, ggplot2::aes_string(var.names[1L], var.names[2L],
     colour = group.name, linetype = "percentiles")) +
     geom_line() + facet_wrap("dataset") + scale_linetype_manual(values=c("solid", "longdash", "dotted"))
   p
}
