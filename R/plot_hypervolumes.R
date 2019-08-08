source("utilities_hypervolumes.R")
kickout = c("fso_ladder", "fso_th")  # algorithms not used for plotting
context = "oml_combined_pca_kmeans_3sets"
dat = fread("oml_3datasets_pca_kmeans.csv")
dat = genDT4plot_pcacluster()  # combine results from all OML datasets
dat[, .N]
dat[, .N / 2, by = algo]  # multi-objective has more counts than single objective methods
dat[, .N / 2, by = repl]
dat[, .N / 2, by = dataset]
dat[, .N / 2, by = job_id]
fwrite(dat, "oml_3datasets_pca_kmeans.csv")
list_res = genhv(dat)
source("plot_hypervolum_standalone.R")

context = "oml_combined_random_stratif_3sets"
dat = genDT4plot_rand_stratif()  # combine results from all OML datasets
dat[, .N / 2, by = algo]  # multi-objective has more counts than single objective methods
list_res = genhv(dat)
source("plot_hypervolum_standalone.R")
