#code for conducting Leave-one-out information criterion model comparison
library(rstanarm)
library(loo)

infolder <- "Location of ModResults.RData"
load(paste0(infolder,"ModResults.RData")

#model comparison based on leave-one-out estimates
diff <- loo::compare(g_loo, soc_loo,ins_loo, eco_loo, soc_eco_loo, inst_eco_loo, soc_inst_loo, int_loo)
