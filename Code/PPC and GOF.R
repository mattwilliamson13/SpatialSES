##code for generating posterior predictive check plots and Bayesian R2

library(rstanarm)
library(loo)
library(bayesplot)
library(cowplot)

infolder <- location where model fit results are stored from ModelFits.R
load(paste0(infolder,"ModResults.RData"))


##Posterior predictive checks 

pp.dens.g <- pp_check(global, plotfun = "dens_overlay") + xlim(0,700) + theme(legend.position = "none")
pp.stat.med <- pp_check(global, plotfun="stat", stat="median") + xlim(0,30) + theme(legend.position = "none")
pp.stat.mean <- pp_check(global, plotfun="stat", stat="mean") + xlim(0,150) + theme(legend.position = "none")
pp.stat.sd <- pp_check(global, plotfun="stat", stat="sd") + xlim(0,700) + theme(legend.position = "none")

p <- cowplot::plot_grid(pp.dens.g, pp.stat.med, pp.stat.mean, pp.stat.sd, align = "h", axis="tb", labels=c("a)","b)","c)","d)"), hjust=-0.95, label_fontfamily = "Times", ncol=2)


##Bayesian R2
glob.R2 <- median(bayes_R2(global))
soc.R2 <- median(bayes_R2(soc_only))
ins.R2 <- median(bayes_R2(ins_only))
eco.R2 <- median(bayes_R2(eco_only))
soc_eco.R2 <- median(bayes_R2(soc_eco))
soc_inst.R2 <- median(bayes_R2(soc_inst))
inst_eco.R2 <- median(bayes_R2(inst_eco))
int.R2 <- median(bayes_R2(int_only))
