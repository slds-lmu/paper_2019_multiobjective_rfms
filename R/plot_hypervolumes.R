source("utilities_hypervolumes.R")
kickout = c("fso_ladder", "fso_th")  # algorithms not used for plotting

context = "oml_combined_pca_kmeans_3sets"
dat = fread("../csv_result/oml_3datasets_pca_kmeans.csv")
dat[, .N]
dat[, .N / 2, by = algo]  # multi-objective has more counts than single objective methods
dat[, .N / 2, by = repl]
dat[, .N / 2, by = dataset]
dat[, .N / 2, by = job_id]
list_res = genhv(dat)
source("plot_hypervolum_standalone.R")

context = "oml_combined_random_stratif_3sets"
dat = fread("../csv_result/oml_3datasets_rand_stratif.csv")
dat[, .N / 2, by = algo]  # multi-objective has more counts than single objective methods
list_res = genhv(dat)
source("plot_hypervolum_standalone.R")
