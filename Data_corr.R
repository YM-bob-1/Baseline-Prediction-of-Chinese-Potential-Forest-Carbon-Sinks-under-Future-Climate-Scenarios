library(corrplot)

# import Data
Data <- read.csv("……Data……", header=T)

Data_corr <- Data[, 7:13]

scaled_data <- apply(Data_corr, 2, function(x) (x - min(x)) / (max(x) - min(x)))

corr <- cor (scaled_data, method="kendall")

# res <- cor.mtest(scaled_data, conf.level = .95)
# p <- res$p

pdf("…………", width = 5, height=4)

corrplot.mixed(corr, lower = 'circle', upper = 'number',diag="u", outline="gray",
               tl.col = "black", tl.cex = 0.8, tl.srt = 45, tl.pos = "lt")

dev.off()


