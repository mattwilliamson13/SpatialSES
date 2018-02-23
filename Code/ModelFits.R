library(bayesplot)
library(rethinking)
library(rstanarm)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

infolder <- "location of CDCS_casedf.csv" #from PredictorPreperation.R
df <- read.csv(paste0(infolder,"CDCS_casedf.csv"), colClasses = "character")

df$state <- coerce_index(substr(df$GEOID, start = 1, stop=2)) #create index for varying intercept
df[,c(2,4:14)] <- apply(df[,c(2,4:14)], 2, function(x) as.numeric(x))

df_stan <- df[,c(2,4:12,15)]#data for modelling

global <- stan_glmer(sumAct ~ MedInc10 + percDeg10 + Value + totNP16 + TOTEMP + totGrant + mxRWR + WCsd + meanhm + (1|state), family = Gamma(link=log), prior_intercept = cauchy(), data=df_stan, chains=3, adapt_delta = 0.97, iter=5000, seed=082980)
g_loo <- loo(global, k_threshold = 0.7)

soc_only <- stan_glmer(sumAct ~ MedInc10 + percDeg10 + Value + (1|state), family = Gamma(link=log), prior_intercept = cauchy(), data=df_stan, chains=3, adapt_delta = 0.98, iter=5000, seed=082980)
soc_loo <- loo(soc_only, k_threshold = 0.7)


ins_only <- stan_glmer(sumAct ~ totNP16 + TOTEMP + totGrant + (1|state), family = Gamma(link=log), prior_intercept = cauchy(), data=df_stan, chains=3, adapt_delta = 0.99, iter=5000, seed=082980)
ins_loo <- loo(ins_only, k_threshold = 0.7)

eco_only <- stan_glmer(sumAct ~ mxRWR + WCsd + meanhm + (1|state), family = Gamma(link=log), prior_intercept = cauchy(), data=df_stan, chains=3, adapt_delta = 0.98, iter=5000, seed=082980)
eco_loo <- loo(eco_only, k_threshold = 0.7)

int_only <- stan_glmer(sumAct ~  (1|state), family = Gamma(link=log), data=df_stan, chains=3, adapt_delta = 0.99, iter=4000, seed=082980)
int_loo <- loo(int_only, k_threshold = 0.7)

soc_inst <- stan_glmer(sumAct ~ MedInc10 + percDeg10 + Value + totNP16 + TOTEMP + totGrant + (1|state), family = Gamma(link=log), prior_intercept = cauchy(), data=df_stan, chains=3, adapt_delta = 0.99, iter=5000, seed=082980)
soc_inst_loo <- loo(soc_inst, k_threshold = 0.7)

soc_eco <- stan_glmer(sumAct ~ MedInc10 + percDeg10 + Value + mxRWR + WCsd + meanhm + (1|state), family = Gamma(link=log), prior_intercept = cauchy(), data=df_stan, chains=3, adapt_delta = 0.98, iter=5000, seed=082980)
soc_eco_loo <- loo(soc_eco, k_threshold = 0.7)

inst_eco <- stan_glmer(sumAct ~ totNP16 + TOTEMP + totGrant + mxRWR + WCsd + meanhm + (1|state), family = Gamma(link=log), prior_intercept = cauchy(), data=df_stan, chains=3, adapt_delta = 0.98, iter=5000, seed=082980)
inst_eco_loo <- loo(inst_eco, k_threshold = 0.7)

save.image(paste0(infolder,"ModResults.RData") #save all stanreg fits for evaluation and plotting
