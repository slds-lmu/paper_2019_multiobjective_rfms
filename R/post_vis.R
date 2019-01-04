### post analysis
visRes = function(res) {
  head(as.data.frame(trafoOptPath(res$opt.path)))
  as.data.frame(trafoOptPath(res$opt.path))
  plotTuneMultiCritResult(res)
}


save2png = function(name = "myplot.png", fig) {
  png(name)
  print(fig)
  dev.off()
}

#' merge the opt.path of the two methods to compare and plot which is better
compare = function(res2) {
  require(magrittr)
  require(ggplot2)
  lrn_name = res2$tune_res_bs$learner$id
  lrn_name = stringr::str_replace(lrn_name, "\\.", "_")
  tune_res_bs = res2$tune_res_bs
  tune_res_pr = res2$tune_res_pr
  major_level = res2$major_level
  df_bs_trafo = tune_res_bs$opt.path %>% trafoOptPath %>% as.data.frame
  df_bs_notrafo = tune_res_bs$opt.path %>% as.data.frame
  ind = which(df_bs_trafo[, names(tune_res_bs$x)] == tune_res_bs$x)[1]
  #df_bs =
  #     tune_res_bs$y %>%
  #     t %>%
  #     as.data.frame
  # 
  df_bs = df_bs_trafo[ind, c("cv_mmce_major.test.mean", "mmce_side.test.mean")]
  ##  
  tune_res_pr$ind  # pareto front indices
  df_pr = tune_res_pr$opt.path %>%
  as.data.frame
  df_pr = df_pr[tune_res_pr$ind, ][, c("cv_mmce_major.test.mean", "mmce_side.test.mean")]
  df_pr$cat = "proposal"
  df_bs$cat = "baseline"
  df_all = rbind(df_pr, df_bs)
  mjitter = 0.01
  coords = paste(format(df_bs$cv_mmce_major.test.mean, digits = 3), format(df_bs$mmce_side.test.mean, digits = 3), sep = ",")
  fig_front = ggplot(data = df_all, mapping = aes(x = cv_mmce_major.test.mean, y = mmce_side.test.mean)) +
    geom_point(alpha = 1, mapping = aes(shape = cat), size = 3) + ggtitle(paste0("pfront_", lrn_name, "_dataset", major_level)) + theme(text = element_text(size=7), axis.text.x = element_text(angle=90, hjust=1))
  # + geom_title(label=coords)
  #+ geom_label(mapping = aes(cv_mmce_major.test.mean + mjitter, mmce_side.test.mean + mjitter, label=coords), data = df_bs)
  # + geom_label(aes(cv_mmce_major.test.mean + jitter, mmce_side.test.mean + jitter, label=coords)) 
  #+xlim(0,1) + ylim(0,1)
  df_pr_all = as.data.frame(tune_res_pr$opt.path)
  df_pr_all$cat = "pr"
  df_bs_all = df_bs_trafo
  df_bs_all$cat = "bs"
  #df_all = rbind(df_bs_all, df_pr_all)   # problematic
  df_all = rbind(df_bs_all[, c("cv_mmce_major.test.mean", "mmce_side.test.mean", "cat")], df_pr_all[, c("cv_mmce_major.test.mean", "mmce_side.test.mean", "cat")])   # problematic
  fig_all = ggplot(data = df_all, mapping = aes(x = cv_mmce_major.test.mean, y = mmce_side.test.mean)) +
    geom_point(alpha = 1, mapping = aes(shape = cat, col = cat), size = 3) + ggtitle(paste0("optpath_", lrn_name, "_dataset", major_level))  
  # 
  # + xlim(0,1) + ylim(0,1)


  # ggplot(data = df_all, mapping = aes(x = cv_mmce_major.test.mean, y = mmce_side.test.mean)) +
  #   geom_point(alpha = 1, mapping = aes(shape = cat), size = 3) +
  #  geom_path(aes(x = cv_mmce_major.test.mean, y = mmce_side.test.mean, col = cat), size = 1.5,alpha = 1, arrow=arrow())
  #save2png(paste0("mboSingleMultiCrit100Front_", lrn_name, "_", major_level, ".png"), fig_front)
  #
  #save2png(paste0("mboSingleMultiCrit100Path_", lrn_name, "_", major_level, ".png"), fig_all)
  #tune_res_pr$x
  #class(tune_res_pr$y)
  return(list(fig_front = fig_front, fig_all = fig_all))
}


