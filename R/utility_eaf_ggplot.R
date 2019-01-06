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
eafGGPlot = function(data, var.names, group.name, replication.name, data.name, percentiles = 50){
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

     points = data.frame(c(x.min2, x.max), c(y.max, y.min2), percentiles, group)
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




example = function() {
 #df_glmnet =  dtb[lrn.id == "classif.glmnet"]
  #eafGGPlot(data = as.data.frame(df_glmnet), var.names = c("mf", "ms"), group.name = "algo", replication.name = "repl", data.name = "lrn.id", percentiles = c(0, 50, 100))
  #
  #
  pdf("eaf_geo_andrea_bs23.pdf")
  eafGGPlot(data = as.data.frame(dtb), var.names = c("curator", "lockbox"), group.name = "algo", replication.name = "repl", data.name = "lrn", percentiles = c(0, 50, 100))
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


