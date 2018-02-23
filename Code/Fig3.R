#generate marginal effects plots in Figure 3
library(rstanarm)

infolder <- "Location of ModResults.RData"
load(paste0(infolder,"ModResults.Rdata"))

#marginal effects plots Nonprofits; because all variables were scaled and standardized holding them at their mean is equivalent to setting the value to 0
#generate new data for CA
nd.1 <- data.frame(totNP16 = seq(-3,3, length.out = nrow(df_stan)),
          MedInc10 = 0,
          percDeg10 = 0,
          Value=0,
          TOTEMP=0,
          totGrant=0,
          mxRWR=0,
          WCsd=0,
          meanhm=0,
          state=1)
#generate new data for OR
nd.2 <- data.frame(totNP16 = seq(-3,3, length.out = nrow(df_stan)),
                   MedInc10 = 0,
                   percDeg10 = 0,
                   Value=0,
                   TOTEMP=0,
                   totGrant=0,
                   mxRWR=0,
                   WCsd=0,
                   meanhm=0,
                   state=2)
#generate new data for WA
nd.3 <- data.frame(totNP16 = seq(-3,3, length.out = nrow(df_stan)),
                   MedInc10 = 0,
                   percDeg10 = 0,
                   Value=0,
                   TOTEMP=0,
                   totGrant=0,
                   mxRWR=0,
                   WCsd=0,
                   meanhm=0,
                   state=3)

pp.1 <- replicate(100, posterior_predict(global, newdata = nd.1)) # maybe reps=50 or 100 or something 
plotdata.1 <- apply(pp.1, 2, rowMeans)
quants <- apply(plotdata.1, 2, quantile, probs = c(.1, .5, .9))  # quantiles over mcmc samples
nd.1 <- cbind(nd.1, t(quants))

pp.2 <- replicate(100, posterior_predict(global, newdata = nd.2)) # maybe reps=50 or 100 or something 
plotdata.2 <- apply(pp.2, 2, rowMeans)
quants <- apply(plotdata.2, 2, quantile, probs = c(.1, .5, .9))  # quantiles over mcmc samples
nd.2 <- cbind(nd.2, t(quants))

pp.3 <- replicate(100, posterior_predict(global, newdata = nd.3)) # maybe reps=50 or 100 or something 
plotdata.3 <- apply(pp.3, 2, rowMeans)
quants <- apply(plotdata.3, 2, quantile, probs = c(.1, .5, .9))  # quantiles over mcmc samples
nd.3 <- cbind(nd.3, t(quants))

nd <- rbind(nd.1,nd.2, nd.3)


np.plot <-ggplot() +
  geom_line(aes(y=`50%`, x=totNP16, color=as.factor(state),lty=as.factor(state) ),lwd=1.25, data = nd) +
  geom_ribbon(aes(ymax = `90%`, ymin = `10%`, x=totNP16, fill = as.factor(state)), data = nd, alpha = 0.2) +
  scale_color_grey("", guide="none") +
  scale_fill_grey("", guide="none") +
  labs(x="Total Number of Non-profits (sd)", y = "Predicted easement activity") +
  theme(legend.position="none", text=element_text(size=14,  family="Times"))


## Effects of increasing RD grants
#generate new data for CA
nd.1 <- data.frame(totNP16 = 0,
                   MedInc10 = 0,
                   percDeg10 = 0,
                   Value=0,
                   TOTEMP=0,
                   totGrant=seq(-3,3, length.out = nrow(df_stan)),
                   mxRWR=0,
                   WCsd=0,
                   meanhm=0,
                   state=1)
#generate new data for OR
nd.2 <- data.frame(totNP16 = 0,
                   MedInc10 = 0,
                   percDeg10 = 0,
                   Value=0,
                   TOTEMP=0,
                   totGrant=seq(-3,3, length.out = nrow(df_stan)),
                   mxRWR=0,
                   WCsd=0,
                   meanhm=0,
                   state=2)
#generate new data for WA
nd.3 <- data.frame(totNP16 = 0,
                   MedInc10 = 0,
                   percDeg10 = 0,
                   Value=0,
                   TOTEMP=0,
                   totGrant=seq(-3,3, length.out = nrow(df_stan)),
                   mxRWR=0,
                   WCsd=0,
                   meanhm=0,
                   state=3)

pp.1 <- replicate(100, posterior_predict(global, newdata = nd.1)) # maybe reps=50 or 100 or something 
plotdata.1 <- apply(pp.1, 2, rowMeans)
quants <- apply(plotdata.1, 2, quantile, probs = c(.1, .5, .9))  # quantiles over mcmc samples
nd.1 <- cbind(nd.1, t(quants))

pp.2 <- replicate(100, posterior_predict(global, newdata = nd.2)) # maybe reps=50 or 100 or something 
plotdata.2 <- apply(pp.2, 2, rowMeans)
quants <- apply(plotdata.2, 2, quantile, probs = c(.1, .5, .9))  # quantiles over mcmc samples
nd.2 <- cbind(nd.2, t(quants))

pp.3 <- replicate(100, posterior_predict(global, newdata = nd.3)) # maybe reps=50 or 100 or something 
plotdata.3 <- apply(pp.3, 2, rowMeans)
quants <- apply(plotdata.3, 2, quantile, probs = c(.1, .5, .9))  # quantiles over mcmc samples
nd.3 <- cbind(nd.3, t(quants))

nd <- rbind(nd.1,nd.2, nd.3)

rd.plot <-ggplot() +
  geom_line(aes(y=`50%`, x=totGrant, color=as.factor(state),lty=as.factor(state) ),lwd=1.25, data = nd) +
  geom_ribbon(aes(ymax = `90%`, ymin = `10%`, x=totGrant, fill = as.factor(state)), data = nd, alpha = 0.2) +
  scale_color_grey("", guide="none") +
  scale_fill_grey("", guide="none") +
  labs(x="Total amount of rural \ndevelopment grants (sd)", y = "") +
  theme(legend.position="none", text=element_text(size=14,  family="Times"))


comb.plot <- cowplot::plot_grid(np.plot, rd.plot, align = "h", axis="tb", labels=c("a)","b)"), label_fontfamily = "Times")
