alpha = function(f1, f2, alpha) {
 alpha * f1 + (1-alpha)  * f2
}
alphas = seq(from = 0.1, to = 0.9, by = 0.1)

library(data.table)
dtmmcenew = readRDS("dt_lambdaJan31.rds")
dtmmcenew[bag =="inbag", alpha(openbox, curator, 0.5)]





dt_obcu = dtmmcenew[, c(.SD, setNames(lapply(alphas, function(x) alpha(openbox, curator, x)), paste0("alpha", 1:9))), by = seq_len(nrow(dtmmcenew))]
dt_oblb = dtmmcenew[, c(.SD, setNames(lapply(alphas, function(x) alpha(openbox, lockbox, x)), paste0("alpha", 1:9))), by = seq_len(nrow(dtmmcenew))]


dt_obcu_ig = dt_obcu[bag=="inbag"]
dt_obcu_og = dt_obcu[bag=="outbag"]
dt_oblb_og = dt_oblb[bag=="outbag"]

library(ggplot)

dtl = tidyr::gather(dt_obcu_og, key = alpha, value = mmce, alpha1:alpha9)

ggplot2::ggplot(dtl, aes(x = alpha, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-curator")
ggsave(file = "openbox-curator-alpha.pdf")

dtl2 = tidyr::gather(dt_oblb_og, key = alpha, value = mmce, alpha1:alpha9)
ggplot2::ggplot(dtl2, aes(x = alpha, y = mmce, fill = algo)) + geom_boxplot() + facet_grid(rows = vars(lrn)) +  theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.title = element_text(hjust = 0.5)) + ggtitle("alpha plot: openbox-lockbox")
ggsave(file = "openbox-lockbox-alpha.pdf")


dt_obcu_ig[, idx:=apply(.SD, 2, which.min), by = .(algo, openbox_name, lockbox_name, lrn, repl), .SDcols = paste0("alpha", 1:9)]

dt_obcu_ig[, t(apply(.SD, 2, which.min)), by = .(algo, openbox_name, lockbox_name, lrn, repl), .SDcols = paste0("alpha", 1:9)]

takeind = function(x) {
  y = x[, paste0("alpha", 1:9)]
  best_ind_pareto = apply(y, 2, which.min)
  as.list(best_ind_pareto)
}
dt_obcu_ig[, paste0("ind", 1:9) := takeind(.SD), by = .(algo, openbox_name, lockbox_name, lrn, repl)]
dt_obcu_ig[1:4, takeind(.SD), by = .(algo, openbox_name, lockbox_name, lrn, repl)]

unids = c("best_ind", "openbox_name", "lockbox_name", "lrn", "repl")
unids2 = c("algo", "openbox_name", "lockbox_name", "lrn", "repl")
obcu_sub = dt_obcu_ig[, c(paste0("ind", 1:9), unids), with = F]

dtm = merge(obcu_sub, dt_obcu_og, by = unids)

fun = function(x) {
  lre = lapply(1:9, function(i) {
    col = paste0("ind", i)
    ind = x[, col, with = F][[col]][1]
    x[ind, paste0("alpha", i), with = F] 
  })
  names(lre) = paste0("alphan", 1:9)
  as.data.table(lre)
}

dtm[, .N, by = unids2]
dtm[algo == "fmo", .N, by = unids2]
dtmr = dtm[algo == "fmo", fun(.SD), by = unids2]

dtmr[, .N, by = unids2][,N]
