#code for generating the four-panel plot of figure 2

# Load packages -----------------------------------------------------------
library(ggplot2)
library(sf)
library(tigris)
library(ggplot2)
library(viridis)
library(classInt)
library(cowplot)
library(tidyverse)
library(raster)
options(tigris_use_cache = TRUE)


# load raster basemap data ------------------------------------------------

alt <- getData('alt', country="USA")
alt <- alt[[1]]
slope <- raster::terrain(alt, opt="slope")
asp <- terrain(alt, opt="aspect")
hills <- hillShade(slope=slope, aspect=asp, angle=45, direction=315)
AOE_ext <- extent(-125,-114,32,50)
hills.crop <- crop(hills, AOE_ext)

##convert to SpatialPixelsDataFrame for ggplot
hills.spdf <- as(hills.crop, "SpatialPixelsDataFrame")
hills.df <- as.data.frame(hills.spdf)
colnames(hills.df) <- c("value", "x", "y")


# load model fit data and generate predictions ------------------------------------
infolder <- "location of ModResults.RData"
load(paste0(infolder,"ModResults.RData"))

#get fitted estimates for each model
glob_pred <- fitted(global)
ins_pred <- fitted(ins_only)
soc_pred <- fitted(soc_only)
eco_pred <- fitted(eco_only)
#attach to dataframe
df$globPred <- glob_pred
df$insPred <- ins_pred
df$socPred <- soc_pred
df$ecoPred <- eco_pred
df$gresid <- df$globPred - df$sumAct
df$insresid <- df$insPred - df$sumAct
df$socresid <- df$socPred - df$sumAct
df$ecoresid <- df$ecoPred - df$sumAct





# Get geometry ------------------------------------------------------------

st <- c("CA", "OR", "WA")
us_states <- unique(fips_codes$state)[1:51]
state.all <- states() #downloads all states
cty <- counties(st) #downloads counties within AOE

#convert to SF
st.sf <- as(state.all, "sf")
continental_states <- us_states[!us_states %in% c("AK", "HI")] # create lookup for subsetting to CONUS
conus <- st.sf[st.sf$STUSPS %in% continental_states,]
conus.crop <- crop(state.all,hills.crop)
AOE <- conus[conus$STUSPS %in% st, ]
cty_sf <- as(cty, "sf")
AOE_sf <- as(AOE, "sf")
conus.crop <- as(conus.crop,"sf")

#join Easement data to spatial data
easement_resid <- cty_sf %>% left_join(., df[,c(1,2,19:22)], by="GEOID")

# Project Data ------------------------------------------------------------

st.p <- st.sf %>% st_transform(crs = as.character(raster::crs(hills.crop)))
AOE.p <- AOE_sf %>% st_transform(crs = as.character(raster::crs(hills.crop)))
cty.p <- cty_sf %>% st_transform(crs = as.character(raster::crs(hills.crop)))
conus.p <- conus %>% st_transform(crs = as.character(raster::crs(hills.crop)))
easement.p <- easement_resid %>% st_transform(crs = as.character(raster::crs(hills.crop)))


# create map theme  -------------------------------------------------------

theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "white", size = 0.002),
      panel.grid.minor = element_line(color = "white", size = 0.002),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),
      ...
    )
}

# Make maps of different fits --------------------------------------------------------
all.res <- as.vector(as.matrix(df[,20:23])) #get all residuals from each fit
bk_vals <- subset(all.res, all.res < 200) #select those less than 200 to presever variability
bks2 <- classIntervals(bk_vals, 5, style="quantile", dataPrecision = 2) #set breaks so that color ramp for all models is the same
bks2b <- bks2[[2]]


globe.map <- ggplot() +
  geom_raster(data=hills.df,aes(x=x, y=y, alpha=value)) +
  scale_alpha(name="", range = c(0.75, 0), guide = F) +
  geom_sf(data = easement.p, aes(fill=gresid), colour = alpha("gray", 1 / 2), size = 0.2) +
  geom_sf(data=subset(easement.p, gresid > 200), fill = "red") +
  geom_sf(data=cty_sf, colour = "gray60", size= 0.1, fill = NA) +
  geom_sf(data=conus.crop, colour="black", size=0.1, fill=NA) +
  geom_sf(data=AOE_sf, colour = "black", size = 0.8, fill = NA) +
  coord_sf() +
  scale_fill_distiller("Residuals",palette="RdYlBu", guide="colorbar",limits=c(-200,200)) +
  theme_map() +
  guides(fill = guide_colorbar(nbin = 5, direction = "horizontal", title.position = "top", label.position="bottom", barwidth=10)) +
  theme(legend.position = "none", legend.justification = "right",legend.margin=margin(0.1,0.1,0.1,0.1),
        text = element_text(color="black")) 

inst.map <- ggplot() +
  geom_raster(data=hills.df,aes(x=x, y=y, alpha=value)) +
  scale_alpha(name="", range = c(0.75, 0), guide = F) +
  geom_sf(data = easement.p, aes(fill=insresid), colour = alpha("gray", 1 / 2), size = 0.2) +
  geom_sf(data=subset(easement.p, insresid > 200), fill = "red") +
  geom_sf(data=cty_sf, colour = "gray60", size= 0.1, fill = NA) +
  geom_sf(data=conus.crop, colour="black", size=0.1, fill=NA) +
  geom_sf(data=AOE_sf, colour = "black", size = 0.8, fill = NA) +
  coord_sf() +
  scale_fill_distiller("Residuals",palette="RdYlBu", guide="colorbar",limits=c(-200,200)) +
  theme_map() +
  guides(fill = guide_colorbar(nbin = 5, direction = "horizontal", title.position = "top", label.position="bottom", barwidth=8)) +
  theme(legend.position = "none", legend.justification = "right",legend.margin=margin(0.1,0.1,0.1,0.1),
        text = element_text(color="black")) 

soc.map <- ggplot() +
  geom_raster(data=hills.df,aes(x=x, y=y, alpha=value)) +
  scale_alpha(name="", range = c(0.75, 0), guide = F) +
  geom_sf(data = easement.p, aes(fill=socresid),colour = alpha("gray", 1 / 2), size = 0.2) +
  geom_sf(data=subset(easement.p, socresid > 200), fill = "red") +
  geom_sf(data=cty_sf, colour = "gray60", size= 0.1, fill = NA) +
  geom_sf(data=conus.crop, colour="black", size=0.1, fill=NA) +
  geom_sf(data=AOE_sf, colour = "black", size = 0.8, fill = NA) +
  coord_sf() +
  scale_fill_distiller("Residuals",palette="RdYlBu", guide="colorbar",limits=c(-200,200)) +
  theme_map() +
  guides(fill = guide_colorbar(nbin = 5, direction = "horizontal", title.position = "top", label.position="bottom", barwidth=8)) +
  theme(legend.position = "bottom", legend.justification = "left",legend.margin=margin(0.1,0.1,0.1,0.1),
        text = element_text(color="black")) 

eco.map <- ggplot() +
  geom_raster(data=hills.df,aes(x=x, y=y, alpha=value)) +
  scale_alpha(name="", range = c(0.75, 0), guide = F) +
  geom_sf(data = easement.p, aes(fill=ecoresid), colour = alpha("gray", 1 / 2), size = 0.2) +
  geom_sf(data=subset(easement.p, ecoresid > 200), fill = "red") +
  geom_sf(data=cty_sf, colour = "gray60", size= 0.1, fill = NA) +
  geom_sf(data=conus.crop, colour="black", size=0.1, fill=NA) +
  geom_sf(data=AOE_sf, colour = "black", size = 0.8, fill = NA) +
  coord_sf() +
  scale_fill_distiller("Residuals",palette="RdYlBu", guide="colorbar",limits = c(-200,300)) +
  theme_map() +
  guides(fill = guide_colorbar(nbin = 5, direction = "horizontal", title.position = "top", label.position="bottom", barwidth=8)) +
  theme(legend.position = "none", legend.justification = "left",legend.margin=margin(0.1,0.1,0.1,0.1),
        text = element_text(color="black")) 


# 4 panel plot ------------------------------------------------------------

all_map <- plot_grid(globe.map  + theme(plot.margin = unit(c(-0.5,0,-1,0), "cm"),legend.position="none"),
                   inst.map + theme(plot.margin = unit(c(-0.5,0,-1,-0.5), "cm"),legend.position="none"),
                   soc.map + theme(plot.margin = unit(c(-0.5,-0.5,-1,-0.5), "cm"),legend.position = "none"),
                   eco.map + theme(plot.margin = unit(c(-0.5,0,-1,-0.5), "cm"),legend.position="none"),
                   align = 'vh',
                   labels = c("a)", "b)", "c)", "d)"),
                   hjust = -1.25,
                   vjust= 5.5,
                   nrow = 1
)

legend_b <- get_legend(globe.map + theme(legend.position="bottom", legend.justification="center", legend.margin = margin(t = -1.5, r = 0, b = 0, l = 0, unit = "pt")))

p <- plot_grid( all_map, legend_b, ncol = 1, rel_heights = c(1.25, .2))

cowplot::ggsave("D:/Data/CDCS_example/Williamson_SpaSES_Fig2.tiff",plot=p, dpi=300)

