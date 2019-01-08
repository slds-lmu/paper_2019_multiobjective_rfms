source("utilities_datasite.R")
# 3891, 9950, 9981, 14966, 34536
try({OpenML::populateOMLCache(task.ids = c(3891, 9950, 9981, 14966, 34536))})
if(!dir.exists("../Data/temp")) dir.create("../Data/temp")
if(!dir.exists("../output")) dir.create("../output")

list.tname = create_rdata_cluster(pca_var_ratio = 0.7, tids = c(3891, 9950, 9981, 14966, 34536), n_datasets = 5, balanced = T)
list.tname
