plot_geo = function() {
   library(Rtsne)
   tuple = BBmisc::load2("../Data/data_cohorts_nonGerman.RData")
   df = tuple$df
   udata = df[!duplicated(df[, 4:ncol(df)]), ]
   tsne = Rtsne(udata, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)  # pca_dim is not specified in R package, in c++, default is 50.
   library(ggplot2)
   ggplot(as.data.frame(tsne$Y), aes(x = V1, y = V2, color = df$dataset_accn)) + geom_point()
}


plot_cluster = function(tuple) {
   library(Rtsne)
   df = tuple$df
   udata = df[!duplicated(df[, 4:ncol(df)]), ]
   tsne = Rtsne(udata, dims = 2, perplexity = 30, verbose = TRUE, max_iter = 500)  # pca_dim is not specified in R package, in c++, default is 50.
   library(ggplot2)
   ggplot(as.data.frame(tsne$Y), aes(x = V1, y = V2, color = df$dataset_accn)) + geom_point()
}
