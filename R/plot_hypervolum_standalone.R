#' require variable: context, list_res
if (!exists(c("context", "list_res"))) stop("require definition of variable 'context' and 'list_res', mean_hypervolumes and win-loss plot could not be executed alone!")
pdf(sprintf("wins_and_losses_%s.pdf", context), width = 8, height = 6)
n.exp = list_res$n.exp
print(ggplot(data = list_res$dat4a, mapping = aes(x = algo1, y = algo2)) + geom_tile(aes(fill = wins)) + geom_text(aes(label = wins, color = abs(wins - n.exp / 2) >= 125), size = 5) +
  scale_color_manual(guide = FALSE, values = c("black", "white")) +
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "darkred", name = "Times",
    midpoint = n.exp / 2) + xlab("Winner") + ylab("Loser") + theme_bw() + theme(axis.text=element_text(size=24, face="bold"), axis.title=element_text(size = 24, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0), legend.title = element_text(size = 24))) # use print(ggplot(...)) could resolve the corrupted pdf format error when sourcing this file instead of running via console
dev.off()




pdf(file = sprintf("mean_hypervolumes_%s.pdf", context), height = 7, width = 10)
print(ggplot(data = list_res$dat3, mapping = aes(y = mdhv, x = algo, fill = algo)) + geom_boxplot() + theme_bw() + scale_fill_ipsum() + xlab("Algorithm") + ylab("Mean dominated hyper volume") + theme(axis.text = element_text(size = 24, face = "bold"), axis.title = element_text(size = 24, face = "bold"), axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) + facet_wrap("lrn") + theme(strip.text.x = element_text(size = 24, colour = "black"), legend.text = element_text(size = 24), legend.title = element_text(size = 24)))
dev.off()


pdf(file = sprintf("hypervolumes_%s.pdf", context), height = 7, width = 10)
lapply(list_res$dat2s, function(d) {
  print(ggplot(data = d, mapping = aes(y = dhv, x = algo, color = algo)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0)) +
    xlab("Algorithm") + ylab("Dominated hyper volume") +
    ggtitle(paste("Rows: openbox, columns: lockbox, learner:", unique(d$lrn))) +
    facet_grid(openbox_name ~ lockbox_name))
})
dev.off()
