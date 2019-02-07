library(data.table)
library(hrbrthemes)
library(ggplot2)

plot_geo = function() {
   library(Rtsne)
   tuple = BBmisc::load2("../Data/data_cohorts_nonGerman.RData")
   df = tuple$df
   udata = df[!duplicated(df[, 4:ncol(df)]), ]
   tsne = Rtsne(udata, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)  # pca_dim is not specified in R package, in c++, default is 50.
   library(ggplot2)
   ggplot(as.data.frame(tsne$Y), aes(x = V1, y = V2, color = df$dataset_accn)) + geom_point() +theme_bw() + scale_fill_ipsum() + theme(axis.text=element_text(size=24, face="bold"), axis.title=element_text(size = 24, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))  + theme(strip.text.x = element_text(size = 24, colour = "orange"), legend.text=element_text(size=24), legend.title=element_text(size=24)) + guides(color=guide_legend(title="data sites")) 
   ggsave(filename = "tnsecluster_geo.pdf")
}


plot_cluster = function(tuple, pca_var_ratios = c(0.1, 0.5)) {
   source("bt_conf.R")
   mlr_task = loadDiskOMLMlrTask(14966)
   list_dataset_index_1 = clusterMlrTask(mlr_task, n_datasets = 5, balanced = T, pca_var_ratio = 10)
   list_dataset_index_5 = clusterMlrTask(mlr_task, n_datasets = 5, balanced = T, pca_var_ratio = 50)

  tuple = createRandomStratifPartition(mlr_task, nsplits = 5, persist = F, path_regx = NULL)
  list_dataset_index_stra =  tuple$list_dataset_index
   # tsne is stochastic, to avoid this, we just run one time tsne, so the data is identical on the 2-d space, but the data is colored differently due to different clustering
   getmetacol = function(list_dataset_index) {
      dataset_accn = sapply(1:getTaskSize(mlr_task), function(rind) {
     which(unlist(lapply(list_dataset_index, function(vec) rind %in% vec)))})
      df_dataset_accn = paste0("ds", dataset_accn)
      df_dataset_accn
   }
   library(Rtsne)
   df_feat = getTaskData(mlr_task, target.extra = T)$data
   udata = df_feat[!duplicated(df_feat), ]
   tsne = Rtsne(udata, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)  # pca_dim is not specified in R package, in c++, default is 50.
   library(ggplot2)
   col1 = getmetacol(list_dataset_index_1)
   col5 = getmetacol(list_dataset_index_5)
   col_stra = getmetacol(list_dataset_index_stra)
   ggplot(as.data.frame(tsne$Y), aes(x = V1, y = V2, color = col1)) + geom_point() +theme_bw() + scale_fill_ipsum() + theme(axis.text=element_text(size=24, face="bold"), axis.title=element_text(size = 24, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))  + theme(strip.text.x = element_text(size = 24, colour = "orange"), legend.text=element_text(size=24), legend.title=element_text(size=24)) + guides(color=guide_legend(title="data sites")) 

 
   ggsave(filename = "tnsecluster_pca1.pdf")
   dev.new()
   ggplot(as.data.frame(tsne$Y), aes(x = V1, y = V2, color = col5)) + geom_point() +theme_bw() + scale_fill_ipsum() + theme(axis.text=element_text(size=24, face="bold"), axis.title=element_text(size = 24, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))  + theme(strip.text.x = element_text(size = 24, colour = "orange"), legend.text=element_text(size=24), legend.title=element_text(size=24)) +guides(color=guide_legend(title="data sites")) 

 
   ggsave(filename = "tnsecluster_pca5.pdf")
   dev.new()
   ggplot(as.data.frame(tsne$Y), aes(x = V1, y = V2, color = col_stra)) + geom_point() +theme_bw() + scale_fill_ipsum() + theme(axis.text=element_text(size=24, face="bold"), axis.title=element_text(size = 24, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))  + theme(strip.text.x = element_text(size = 24, colour = "orange"), legend.text=element_text(size=24), legend.title=element_text(size=24)) + guides(color=guide_legend(title="data sites")) 

 
   ggsave(filename = "tnsecluster_stratif.pdf")
}
