alpha = function(f1, f2, alpha) {
 alpha * f1 + (1-alpha)  * f2
}
alphas = seq(from = 0.1, to = 0.9, by = 0.1)
dtmmcenew[, alpha(openbox, curator, 0.5), by = seq_len(nrow(dtmmcenew))]






dtmmcenew[, lapply(alphas, function(x) alpha(openbox, curator, x)), by = seq_len(nrow(dtmmcenew))]
dt_obcu = dtmmcenew[, c(.SD, lapply(alphas, function(x) alpha(openbox, curator, x))), by = seq_len(nrow(dtmmcenew))]
dt_oblb = dtmmcenew[, c(.SD, lapply(alphas, function(x) alpha(openbox, lockbox, x))), by = seq_len(nrow(dtmmcenew))]
