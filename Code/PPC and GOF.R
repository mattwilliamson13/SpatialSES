##code for generating posterior predictive check plots and Bayesian R2

library(rstanarm)
library(loo)
library(bayesplot)
library(cowplot)
library(extrafont)
font_import()

infolder <- location where model fit results are stored from ModelFits.R
load(paste0(infolder,"ModResults.RData"))


##Posterior predictive checks 

##Posterior predictive checks 

pp.dens.g <- pp_check(global, plotfun = "dens_overlay") + xlim(0,700) + 
  legend_none() +
  xaxis_title(size = 14, family = "Times New Roman") +
  yaxis_title(size = 14, family = "Times New Roman") +
  labs(y = "Density of estimate", x = "Number of easements")
  
  
pp.stat.med <- pp_check(global, plotfun="stat", stat="median") + xlim(0,30) +  
  legend_none() +
  xaxis_title(size = 14, family = "Times New Roman") +
  yaxis_title(size = 14, family = "Times New Roman") +
  labs(y = "Frequency of estimate", x = "Median number of easements")

pp.stat.mean <- pp_check(global, plotfun="stat", stat="mean") + xlim(0,150) + 
  legend_none() +
  xaxis_title(size = 14, family = "Times New Roman") +
  yaxis_title(size = 14, family = "Times New Roman") +
  labs(y = "Frequency of estimate", x = "Mean number of easements")

pp.stat.sd <- pp_check(global, plotfun="stat", stat="sd") + xlim(0,700) + 
  legend_none() +
  xaxis_title(size = 14, family = "Times New Roman") +
  yaxis_title(size = 14, family = "Times New Roman") +
  labs(y = "Frequency of estimate", x = "Standard deviation of \n number of easements")


p <- cowplot::plot_grid(pp.dens.g, pp.stat.med, pp.stat.mean, pp.stat.sd, align = "hv", axis="tb", labels=c("a","b","c","d"), hjust=-5.1, label_fontfamily = "Times New Roman", ncol=2)

ggsave(filename=paste0(infolder, "Williamson_SpaSES_SIFig2Rev.tiff"), plot =p, device = NULL, path = NULL,
       scale = 1, width = 6, height = 6, units = "in",
       dpi = 400, limitsize = TRUE)

##Bayesian R2
glob.R2 <- median(bayes_R2(global))
soc.R2 <- median(bayes_R2(soc_only))
ins.R2 <- median(bayes_R2(ins_only))
eco.R2 <- median(bayes_R2(eco_only))
soc_eco.R2 <- median(bayes_R2(soc_eco))
soc_inst.R2 <- median(bayes_R2(soc_inst))
inst_eco.R2 <- median(bayes_R2(inst_eco))
int.R2 <- median(bayes_R2(int_only))
