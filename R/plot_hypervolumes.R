source("utilities_hypervolumes.R")
kickout = c("fso_ladder", "fso_th")  # algorithms not used for plotting
context = "oml_combined_pca_kmeans_3sets"
dat = genDT4plot_pcacluster()
list_res = genhv(dat)
source("plot_hypervolum_standalone.R")


context = "oml_combined_random_stratif_3sets"
dat = genDT4plot_rand_stratif()
list_res = genhv(dat)
source("plot_hypervolum_standalone.R")
