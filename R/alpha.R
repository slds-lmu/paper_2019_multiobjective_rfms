library(data.table)

dat = as.data.table(readRDS(file = "dt_3608_pca1.rds"))
dtin = as.data.table(readRDS(file = "dt_3891_pca1.rds"))
dtin = readRDS("dt_res_geo_response.rds")

dtin = readRDS("dt_lambdaJan31.rds")

#' dtin[bag =="inbag", alpha_mix(openbox, curator, 0.5)]
alpha_mix = function(f1, f2, alpha) {
 alpha * f1 + (1 - alpha)  * f2
}
alphas = seq(from = 0.1, to = 0.9, by = 0.1)

#dt2plot = dtin[(openbox_name=="GSE16446") & (lockbox_name=="GSE20194"),]
dt2plot = dtin
# no change now
dt2plot[, cid := seq_len(.N), by = .(openbox_name, lockbox_name, lrn, repl, algo, bag)]
#dt2plot[, seq_len(.N), by = runids]
dt2plot$cid

dt_obcu = dt2plot[, c(.SD, setNames(lapply(alphas, function(x) alpha_mix(openbox, curator, x)), paste0("alpha", 1:9))), by = seq_len(nrow(dt2plot))] # the benefit is get a unique identifier

dt_oblb = dt2plot[, c(.SD, setNames(lapply(alphas, function(x) alpha_mix(openbox, lockbox, x)), paste0("alpha", 1:9))), by = seq_len(nrow(dt2plot))]

dt_oblb_og = dt2plot[bag!="inbag", c(.SD, setNames(lapply(alphas, function(x) alpha_mix(openbox, lockbox, x)), paste0("alpha", 1:9)))]

dt_obcu_ig = dt_obcu[bag=="inbag"]
dt_obcu_og = dt_obcu[bag=="outbag"]

library(ggplot2)

dtl_og = tidyr::gather(dt_obcu_og, key = alpha, value = mmce, alpha1:alpha9)
dtl_ig = tidyr::gather(dt_obcu_ig, key = alpha, value = mmce, alpha1:alpha9)

ggplot2::ggplot(dtl_ig, aes(x = alpha, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-curator-inbag")



ggplot2::ggplot(dtl_og, aes(x = alpha, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-curator-outbag")

ggplot2::ggplot(dtl_og, aes(x = algo, y = mmce)) + geom_boxplot() + facet_grid(rows = vars(lrn), cols = vars(alpha)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-curator-outbag")
ggsave(file = "openbox-curator-alpha.pdf")

dtl2 = tidyr::gather(dt_oblb_og, key = alpha, value = mmce, alpha1:alpha9)
ggplot2::ggplot(dtl2, aes(x = alpha, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-lockbox")
ggsave(file = "openbox-lockbox-alpha.pdf")
ggplot2::ggplot(dtl2, aes(x = alpha, y = mmce)) + geom_boxplot() + facet_grid(rows = vars(lrn), cols = vars(algo)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-lockbox")




takeind = function(x) {
  y = x[, paste0("alpha", 1:9)]
  best_ind_pareto = apply(y, 2, which.min)
  if(any(best_ind_pareto > nrow(x))) stop("excced nrow!")
  #checkmate::assert(best_ind_pareto <=nrow(x))
  as.list(best_ind_pareto)
}

dt_obcu_ig[1:40, takeind(.SD), by = .(algo, openbox_name, lockbox_name, lrn, repl)]
dt_obcu_ig[, paste0("sel_ind", 1:9) := takeind(.SD), by = .(algo, openbox_name, lockbox_name, lrn, repl)]

unids = c("best_ind", "job_id", "cid", "openbox_name", "lockbox_name", "lrn", "repl")
unids_merge = c("job_id", "openbox_name", "lockbox_name", "lrn", "repl")
unids2 = c("algo", "openbox_name", "lockbox_name", "lrn", "repl")
obcu_sub = dt_obcu_ig[, c(paste0("sel_ind", 1:9), unids), with = F]

obcu_sub[, unids_merge, with = F]
dt_obcu_og[, unids_merge, with = F]
#dtm = merge(obcu_sub, dt_obcu_og, by = unids_merge, all.y = all)
dtm = merge(obcu_sub, dt_obcu_og)
dtmlb = merge(obcu_sub, dt_oblb_og)

fun = function(x) {
  lre = lapply(1:9, function(i) {
    col = paste0("sel_ind", i)
    ind = x[, col, with = F][[col]][1]
    x[ind, paste0("alpha", i), with = F]
  })
  #names(lre) = paste0("alphan", 1:9)
  if (any(is.na(unlist(lre)))) browser()
  as.data.table(lre)
}

dtm[, .N, by = unids2]
dtm[algo == "fmo", .N, by = unids2]
dtmr_fmo = dtm[algo == "fmo", fun(.SD), by = unids2]
dtmr = dtm[, fun(.SD), by = unids2]
dtmr_lb = dtmlb[, fun(.SD), by = unids2]
dtmr
dtmr[, .N, by = unids2][,N]

dtmrl = tidyr::gather(dtmr, key = alpha, value = mmce, alpha1:alpha9)
dtmrl_lb = tidyr::gather(dtmr_lb, key = alpha, value = mmce, alpha1:alpha9)

ggplot2::ggplot(dtmrl, aes(x = algo, y = mmce, fill = algo)) + geom_violin() + facet_grid(rows = vars(lrn), cols = vars(alpha)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-curator")
ggsave(file = "openbox-curator-alpha.pdf")

ggplot2::ggplot(dtmrl_lb, aes(x = algo, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn), cols = vars(alpha)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-lockbox")

ggplot2::ggplot(dtmrl_lb, aes(x = algo, y = mmce, fill = algo)) + geom_violin() + facet_grid(rows = vars(lrn), cols = vars(alpha)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-lockbox") # no need to specify count here
