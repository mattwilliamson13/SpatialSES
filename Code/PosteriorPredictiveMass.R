#code to estimate the posterior predictive mass of the parameters in the global model
library(rstanarm)

infolder <- "location of ModResults.RData"
load(paste0(infolder,"ModResults.RData")

global$stan_function <- "stan_glmer" #necessary for newer version of rstanarm

#estimat PPM for each parameter
post.global <- as.data.frame(global)
perc_pos <- apply(post.global, 2, function(x) (length(which(x > 0))/length(x) * 100))
perc_neg <- apply(post.global, 2, function(x) (length(which(x < 0))/length(x) * 100))

