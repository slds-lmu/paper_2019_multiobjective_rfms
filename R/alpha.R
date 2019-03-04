library(data.table)

#dat = as.data.table(readRDS(file = "dt_3608_pca1.rds"))
#dtin = as.data.table(readRDS(file = "dt_3891_pca1.rds"))

dat = readRDS("dt_lambdaJan31.rds")

dat = dat[algo!="fso_ladder"]
dat = dat[algo!="fso_th"]
#' dtin[bag =="inbag", get_alpha_mix_obj(openbox, curator, 0.5)]
get_alpha_mix_obj = function(f1, f2, alpha) {
 alpha * f1 + (1 - alpha)  * f2
}
alphas = seq(from = 0.1, to = 0.9, by = 0.1)

#dt2plot = dtin[(openbox_name=="GSE16446") & (lockbox_name=="GSE20194"),]
# no change now
dat[, solution_id := seq_len(.N), by = .(openbox_name, lockbox_name, lrn, repl, algo, bag)]
#dt2plot[, seq_len(.N), by = runids]
#dtin$solution_id

dt_obcu = dat[, c(.SD, setNames(lapply(alphas, function(x) get_alpha_mix_obj(openbox, curator, x)), paste0("alpha", 1:9))), by = seq_len(nrow(dat))] # the benefit of using "by" is get a unique identifier

dt_oblb = dat[, c(.SD, setNames(lapply(alphas, function(x) get_alpha_mix_obj(openbox, lockbox, x)), paste0("alpha", 1:9))), by = seq_len(nrow(dat))]

#dt_oblb_og = dat[bag == "inbag", c(.SD, setNames(lapply(alphas, function(x) get_alpha_mix_obj(openbox, lockbox, x)), paste0("alpha", 1:9)))]

dt_obcu_ig = dt_obcu[bag == "inbag"]
dt_obcu_og = dt_obcu[bag == "outbag"]

dt_oblb_ig = dt_oblb[bag == "inbag"]
dt_oblb_og = dt_oblb[bag == "outbag"]

### only for plotting
#library(ggplot2)
#dtl_og = tidyr::gather(dt_obcu_og, key = alpha, value = mmce, alpha1:alpha9)
#dtl_ig = tidyr::gather(dt_obcu_ig, key = alpha, value = mmce, alpha1:alpha9)

#ggplot2::ggplot(dtl_ig, aes(x = alpha, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-curator-inbag")

#ggplot2::ggplot(dtl_og, aes(x = alpha, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-curator-outbag")

#ggplot2::ggplot(dtl_og, aes(x = algo, y = mmce)) + geom_boxplot() + facet_grid(rows = vars(lrn), cols = vars(alpha)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-curator-outbag")
#ggsave(file = "openbox-curator-alpha.pdf")


# openbox and lockbox
#dtl2 = tidyr::gather(dt_oblb_og, key = alpha, value = mmce, alpha1:alpha9)
#ggplot2::ggplot(dtl2, aes(x = alpha, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-lockbox")
#ggsave(file = "openbox-lockbox-alpha.pdf")
#ggplot2::ggplot(dtl2, aes(x = alpha, y = mmce)) + geom_boxplot() + facet_grid(rows = vars(lrn), cols = vars(algo)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-lockbox")
### end of plotting

takeind = function(x) {
  y = x[, paste0("alpha", 1:9)]
  best_ind_pareto = apply(y, 2, which.min)
  checkmate::assert(all(best_ind_pareto <= nrow(x)))
  as.list(best_ind_pareto)
}

fun_selPareto = function(x) {
  lre = lapply(1:9, function(i) {
    col = paste0("sel_ind", i)
    ind = x[, col, with = F][[col]][1]
    checkmate::assert(ind <= nrow(x))
    x[ind, paste0("alpha", i, ".y"), with = F]
  })
  if (any(is.na(unlist(lre)))) stop("fun_selPareto")
  as.data.table(lre)
}



dt_obcu_ig[, paste0("sel_ind", 1:9) := takeind(.SD), by = .(algo, openbox_name, lockbox_name, lrn, repl)]  # take the best index for different alpha
unids_merge = c("algo", "best_ind", "openbox_name", "lockbox_name", "lrn", "repl")
dt_merge = merge(dt_obcu_ig, dt_obcu_og, by = unids_merge, all = T)
checkmate::assert(nrow(dt_merge) == nrow(dt_obcu_ig))
dtmr_fmo = dt_merge[algo == "fmo", fun_selPareto(.SD), by = c("algo", "openbox_name", "lockbox_name", "lrn", "repl")]
dtmr = dt_merge[, fun_selPareto(.SD), by = c("algo", "openbox_name", "lockbox_name", "lrn", "repl")]
dtmrl = tidyr::gather(dtmr, key = alpha, value = mmce, alpha1.y:alpha9.y)








library(ggplot2)
ggplot2::ggplot(dtmrl, aes(x = algo, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn), cols = vars(alpha)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-curator")
ggsave(file = "openbox-curator-alpha.pdf")


dt_oblb_ig[, paste0("sel_ind", 1:9) := takeind(.SD), by = .(algo, openbox_name, lockbox_name, lrn, repl)]  # take the best index for different alpha
unids_merge = c("algo", "best_ind", "openbox_name", "lockbox_name", "lrn", "repl")
dt_merge = merge(dt_oblb_ig, dt_oblb_og, by = unids_merge, all = T)
checkmate::assert(nrow(dt_merge) == nrow(dt_oblb_ig))
dtmr_fmo = dt_merge[algo == "fmo", fun_selPareto(.SD), by = c("algo", "openbox_name", "lockbox_name", "lrn", "repl")]
dtmr = dt_merge[, fun_selPareto(.SD), by = c("algo", "openbox_name", "lockbox_name", "lrn", "repl")]
dtmrl = tidyr::gather(dtmr, key = alpha, value = mmce, alpha1.y:alpha9.y)

ggplot2::ggplot(dtmrl, aes(x = algo, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn), cols = vars(alpha)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-lockbox")
ggsave(file = "openbox-lockbox-alpha.pdf")





#ggplot2::ggplot(dtmrl_lb, aes(x = algo, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn), cols = vars(alpha)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-lockbox")

#ggplot2::ggplot(dtmrl_lb, aes(x = algo, y = mmce, fill = algo)) + geom_violin() + facet_grid(rows = vars(lrn), cols = vars(alpha)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-lockbox") # no need to specify count here
