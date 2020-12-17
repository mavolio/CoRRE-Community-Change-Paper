#emily's working directory
setwd("/Users/egrman/Dropbox/C2E/Products/CommunityChange/March2018 WG")

#meghan's working directory
setwd("C:\\Users\\mavolio2\\Dropbox\\C2E\\Products\\CommunityChange\\March2018 WG")
setwd("~/Dropbox/C2E/Products/CommunityChange/March2018 WG")


#kevin's working directory
#setwd("C:\\Users\\wilco\\Dropbox\\C2E\\Products\\CommunityChange\\March 2018 WG")

library(tidyverse)
library(ggthemes)
library(grid)
library(vegan)
library(rsq)
library(car)
library(gridExtra)


theme_set(theme_bw(12))
theme_update(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

### stealing Kevin's code for creating Glass's delta to compare T vs C at each timestep

### Read in data 


change_metrics <- read.csv("MetricsTrts_March2019.csv") %>%
  mutate(abs_richness_change = abs(richness_change),
         abs_evenness_change = abs(evenness_change))

subset<-read.csv("experiment_trt_subset_may2019.csv")

dir<-read.csv("C:\\Users\\mavolio2\\Dropbox\\C2E/Products/CommunityChange/Summer2018_Results/diff_directinal_slopes.csv")

### Control data
change_control <- change_metrics %>%
  filter(plot_mani==0) %>%
  dplyr::select(treatment_year, treatment_year2, abs_richness_change, abs_evenness_change, 
                rank_change, gains, losses, site_project_comm, treatment, plot_mani) %>%
  rename(abs_richness_change_ctrl = abs_richness_change,
         abs_evenness_change_ctrl = abs_evenness_change,
         rank_change_ctrl = rank_change,
         gains_ctrl = gains,
         losses_ctrl = losses
  ) %>%
  group_by(site_project_comm, treatment_year2) %>%
  summarise_at(vars(abs_richness_change_ctrl:losses_ctrl), list(mean=mean, sd=sd), na.rm=T)

change_glass_d <- change_metrics %>%
  filter(plot_mani != 0) %>%
  group_by(site_project_comm, treatment, treatment_year2, plot_mani) %>%
  summarise(abs_richness_change = mean(abs_richness_change,na.rm=T),
            abs_evenness_change = mean(abs_evenness_change, na.rm=T),
            rank_change = mean(rank_change, na.rm=T),
            gains = mean(gains, na.rm=T),
            losses = mean(losses, na.rm=T)) %>%
  left_join(change_control, by=c("site_project_comm","treatment_year2")) %>%
  mutate(abs_richness_glass = (abs_richness_change-abs_richness_change_ctrl_mean)/abs_richness_change_ctrl_sd,
         abs_evenness_glass = (abs_evenness_change-abs_evenness_change_ctrl_mean)/abs_evenness_change_ctrl_sd,
         rank_glass = (rank_change-rank_change_ctrl_mean)/rank_change_ctrl_sd,
         gains_glass = (gains-gains_ctrl_mean)/gains_ctrl_sd,
         losses_glass = (losses-losses_ctrl_mean)/losses_ctrl_sd
  )%>%
  select(site_project_comm, treatment, treatment_year2, plot_mani, abs_richness_glass, abs_evenness_glass, rank_glass, gains_glass, losses_glass)

#change_glass_d is the thing that we want

## replace Inf with NAs in change_glass_d and select only treatments I want
change_glass_d <- change_glass_d %>%
  mutate(abs_richness_glass=replace(abs_richness_glass, abs_richness_glass=="Inf"|abs_richness_glass=="NaN", NA)) %>%
  mutate(abs_evenness_glass=replace(abs_evenness_glass, abs_evenness_glass=="Inf"|abs_evenness_glass=="NaN", NA)) %>%
  mutate(rank_glass=replace(rank_glass, rank_glass=="Inf"|rank_glass=="NaN", NA)) %>%
  mutate(gains_glass=replace(gains_glass, gains_glass=="Inf"|gains_glass=="NaN", NA)) %>%
  mutate(losses_glass=replace(losses_glass, losses_glass=="Inf"|losses_glass=="NaN", NA))%>%
  right_join(subset)
  
# read in site level predictor variables
info.spc=read.csv("ForAnalysis_allAnalysisAllDatasets_04082019.csv") %>%
  mutate(site_project_comm = paste(site_code, project_name, community_type, sep="_"))%>%
  select(site_project_comm, rrich, anpp, MAT, MAP)%>%
  unique()
##getting site Evar for each location
controls<-change_metrics%>%
  select(site_code, project_name, community_type, plot_id, plot_mani)%>%
  unique()%>%
  filter(plot_mani==0)%>%
  mutate(site_project_comm = paste(site_code, project_name, community_type, sep="_"))

div_evar<-read.csv("CORRE_DivEvar_June2020.csv")

evenness<-div_evar%>%
  right_join(controls)%>%
  group_by(site_project_comm)%>%
  summarise(Evenness=mean(Evar, na.rm=T))

# read in treatment variables for subsetting later
info.trt=info.trt<-read.csv("C:\\Users\\mavolio2\\Dropbox\\converge_diverge/datasets/LongForm/ExperimentInformation_March2019.csv")%>%
  select(site_code, project_name, community_type, treatment,plot_mani, trt_type)%>%
  unique()%>%
  filter(plot_mani!=0)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  mutate(use=ifelse(trt_type=="N"|trt_type=="P"|trt_type=="CO2"|trt_type=="irr"|trt_type=="temp"|trt_type=="N*P"|trt_type=="mult_nutrient"|trt_type=='precip_vari', 1, 0))%>%
  mutate(trt_type2=ifelse(trt_type=="N"|trt_type=="control","N", 
                          ifelse(trt_type=="P", "P", 
                                 ifelse(trt_type=="CO2", "CO2",
                                        ifelse(trt_type=="irr", "Irrigation",
                                               ifelse(trt_type=="temp", "Temperature", 
                                                      ifelse(trt_type=="N*P"|trt_type=="mult_nutrient", "Mult. Nuts.", 
                                                             ifelse(trt_type=="drought", "drought", 
                                                                    ifelse(trt_type=="CO2*temp", "CO2*temp", 
                                                                           ifelse(trt_type=="drought*temp", "drought*temp", 
                                                                                  ifelse(trt_type=="irr*temp", "irr*temp",
                                                                                         ifelse(trt_type=="irr*CO2*temp"|trt_type=="N*CO2*temp"|trt_type=="N*irr*temp"|trt_type=="N*irr*CO2*temp", "mult_res*temp", 
                                                                                                ifelse(trt_type=="irr*herb_removal"|trt_type=="irr*plant_mani"|trt_type=="irr*plant_mani*herb_removal", "irr*NR",
                                                                                                       ifelse(trt_type=="herb_removal"|trt_type=="till"|trt_type=="mow_clip"|trt_type=="burn"|trt_type=="plant_mani"|trt_type=="stone"|trt_type=="graze"|trt_type=="burn*graze"|trt_type=="fungicide"|trt_type=="plant_mani*herb_removal"|trt_type=="burn*mow_clip", "NR", 
                                                                                                              ifelse(trt_type=="precip_vari", "Precip. Vari.",  
                                                                                                                     ifelse(trt_type=="N*plant_mani"|trt_type=="N*burn"|trt_type=="N*mow_clip"|trt_type=="N*till"|trt_type=="N*stone"|trt_type=="N*burn*graze"|trt_type=="N*burn*mow_clip", "N*NR", 
                                                                                                                            ifelse(trt_type=="N*temp", "N*temp", 
                                                                                                                                   ifelse(trt_type=="N*CO2", "N*CO2",
                                                                                                                                          ifelse(trt_type=="irr*CO2", "irr*CO2",
                                                                                                                                                 ifelse(trt_type=="N*irr", "N*irr",
                                                                                                                                                        ifelse(trt_type=="mult_nutrient*herb_removal"|trt_type=="mult_nutrient*fungicide"|trt_type=="N*P*burn*graze"|trt_type=="N*P*burn"|trt_type=="*P*mow_clip"|trt_type=="N*P*burn*mow_clip"|trt_type=="N*P*mow_clip", "mult_nutrients*NR",
                                                                                                                                                               ifelse(trt_type=="P*mow_clip"|trt_type=="P*burn"|trt_type=="P*burn*graze"|trt_type=="P*burn*mow_clip", "P*NR", 
                                                                                                                                                                      ifelse(trt_type=="precip_vari*temp", "precip_vari*temp", 
                                                                                                                                                                             ifelse(trt_type=="N*irr*CO2", "mult_res", 999))))))))))))))))))))))))

### calculate mean change through time and combine with predictor variables
change_glass_d_mean <- change_glass_d %>%
  group_by(site_project_comm, treatment, plot_mani) %>%
  summarise_at(vars(abs_richness_glass, abs_evenness_glass, rank_glass, gains_glass, losses_glass), funs(mean), na.rm=T) %>%
  left_join(info.spc) %>%
  left_join(evenness)%>%
  left_join(info.trt)
  

#looking for correlations among predictor variables
pred=as.matrix(change_glass_d_mean[, c("MAP", "MAT", "rrich", "anpp", "Evenness")])
cor(pred)
pairs(pred)
png(paste0("Summer2018_Results/site predictors of SERGL/MR predictor variables SITE LEVEL pairs plot.png"), width=11, height=8, units="in", res=600)
print(pairs(pred))
dev.off()

#-------------Standardized multiple regression with only site predictors
#first using all the studies
# usethese=change_metrics[change_metrics$use==1, c("site_project_comm", "treatment", "use")]
# use_change_glass_d_mean=merge(change_glass_d_mean, unique(usethese), by=c("site_project_comm", "treatment"))
# #write.csv(use_change_glass_d_mean, "use for site predictors of SERGL.csv")

#note that some response var (evenness, losses, gains) have NA (for every year there was no variation among the controls; sd=0 and glass's delta was undefined for every year)

#-----1a) treating all experiments in this subset as independent data points:

change_glass_d_mean$sMAP<-scale(change_glass_d_mean$MAP)
change_glass_d_mean$sMAT<-scale(change_glass_d_mean$MAT)
change_glass_d_mean$srrich<-scale(change_glass_d_mean$rrich)
change_glass_d_mean$sanpp<-scale(change_glass_d_mean$anpp)
change_glass_d_mean$seven<-scale(change_glass_d_mean$Evenness)

rich=lm(abs_richness_glass ~ sMAP + sMAT + srrich + sanpp + seven, data=change_glass_d_mean)
#vif(rich) #need the car package for this.
summary(rich)
rsq.partial(rich)

#making object to contain results
richresults=data.frame(response="rich", 
                       predictor=names(rich$coefficients), 
                       slope=as.numeric(rich$coefficients), 
                       pval=as.numeric(summary(rich)$coef[,4]), 
                       rsq=c(NA, rsq.partial(rich)$partial.rsq))


even=lm(abs_evenness_glass~sMAP + sMAT + srrich + sanpp +seven, data=change_glass_d_mean)
summary(even)
p.adjust(.0002751, method = "BH", n=5)# p = 0.001
rsq.partial(even)
evenresults=data.frame(response="even", predictor=names(even$coefficients), slope=as.numeric(even$coefficients), pval=as.numeric(summary(even)$coef[,4]), rsq=c(NA, rsq.partial(even)$partial.rsq))

rank=lm(rank_glass~sMAP + sMAT + srrich + sanpp + seven, data=change_glass_d_mean)
summary(rank)
p.adjust(0.01067, method = "BH", n=5) p = 0.053
rsq.partial(rank)
rankresults=data.frame(response="rank", predictor=names(rank$coefficients), slope=as.numeric(rank$coefficients), pval=as.numeric(summary(rank)$coef[,4]), rsq=c(NA, rsq.partial(rank)$partial.rsq))

gains=lm(gains_glass~sMAP + sMAT + srrich + sanpp +seven, data=change_glass_d_mean)
summary(gains)
p.adjust(0.00002981, method = "BH", n=5) #p = 0.00014 
rsq.partial(gains)
gainsresults=data.frame(response="gains", predictor=names(gains$coefficients), slope=as.numeric(gains$coefficients), pval=as.numeric(summary(gains)$coef[,4]), rsq=c(NA, rsq.partial(gains)$partial.rsq))

losses=lm(losses_glass~sMAP + sMAT + srrich + sanpp +seven, data=change_glass_d_mean)
summary(losses)
p.adjust(0.02411, method = "BH", n=5) #p = 0.1205

rsq.partial(losses)
lossesresults=data.frame(response="losses", predictor=names(losses$coefficients), slope=as.numeric(losses$coefficients), pval=as.numeric(summary(losses)$coef[,4]), rsq=c(NA, rsq.partial(losses)$partial.rsq))

fulldataset=rbind(richresults, evenresults, rankresults, gainsresults, lossesresults)
#write.csv(fulldataset, "Summer2018_Results/site predictors of SERGL/site predictors of SERGL, all studies.csv", row.names=F)


# doing for each GCD separately -------------------------------------------


#-------1b) Only a subset of studies

# #----------N addition studies:
# rich=lm(abs_richness_glass ~ sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="N",])
# vif(rich)
# summary(rich)
# richresults=data.frame(response="rich", predictor=names(rich$coefficients), slope=as.numeric(rich$coefficients), pval=as.numeric(summary(rich)$coef[,4]), rsq=c(NA, rsq.partial(rich)$partial.rsq))
# 
# even=lm(abs_evenness_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="N",])
# summary(even)
# evenresults=data.frame(response="even", predictor=names(even$coefficients), slope=as.numeric(even$coefficients), pval=as.numeric(summary(even)$coef[,4]), rsq=c(NA, rsq.partial(even)$partial.rsq))
# 
# rank=lm(rank_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="N",])
# summary(rank)
# rankresults=data.frame(response="rank", predictor=names(rank$coefficients), slope=as.numeric(rank$coefficients), pval=as.numeric(summary(rank)$coef[,4]), rsq=c(NA, rsq.partial(rank)$partial.rsq))
# 
# gains=lm(gains_glass~sMAP + sMAT + srrich + sanpp,data=change_glass_d_mean[change_glass_d_mean$trt_type2=="N",])
# summary(gains)
# gainsresults=data.frame(response="gains", predictor=names(gains$coefficients), slope=as.numeric(gains$coefficients), pval=as.numeric(summary(gains)$coef[,4]), rsq=c(NA, rsq.partial(gains)$partial.rsq))
# 
# losses=lm(losses_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="N",])
# summary(losses)
# lossesresults=data.frame(response="losses", predictor=names(losses$coefficients), slope=as.numeric(losses$coefficients), pval=as.numeric(summary(losses)$coef[,4]), rsq=c(NA, rsq.partial(losses)$partial.rsq))
# 
# onlyNadditions=rbind(richresults, evenresults, rankresults, gainsresults, lossesresults)
# 
# 
# #----------P addition studies:
# 
# rich=lm(abs_richness_glass ~ sMAP + sMAT + srrich + sanpp,data=change_glass_d_mean[change_glass_d_mean$trt_type2=="P",])
# summary(rich)
# richresults=data.frame(response="rich", predictor=names(rich$coefficients), slope=as.numeric(rich$coefficients), pval=as.numeric(summary(rich)$coef[,4]), rsq=c(NA, rsq.partial(rich)$partial.rsq))
# 
# even=lm(abs_evenness_glass~sMAP + sMAT + srrich + sanpp,data=change_glass_d_mean[change_glass_d_mean$trt_type2=="P",])
# summary(even)
# evenresults=data.frame(response="even", predictor=names(even$coefficients), slope=as.numeric(even$coefficients), pval=as.numeric(summary(even)$coef[,4]), rsq=c(NA, rsq.partial(even)$partial.rsq))
# 
# rank=lm(rank_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="P",])
# summary(rank)
# rankresults=data.frame(response="rank", predictor=names(rank$coefficients), slope=as.numeric(rank$coefficients), pval=as.numeric(summary(rank)$coef[,4]), rsq=c(NA, rsq.partial(rank)$partial.rsq))
# 
# gains=lm(gains_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="P",])
# summary(gains)
# gainsresults=data.frame(response="gains", predictor=names(gains$coefficients), slope=as.numeric(gains$coefficients), pval=as.numeric(summary(gains)$coef[,4]), rsq=c(NA, rsq.partial(gains)$partial.rsq))
# 
# losses=lm(losses_glass~sMAP + sMAT + srrich + sanpp,data=change_glass_d_mean[change_glass_d_mean$trt_type2=="P",])
# summary(losses)
# lossesresults=data.frame(response="losses", predictor=names(losses$coefficients), slope=as.numeric(losses$coefficients), pval=as.numeric(summary(losses)$coef[,4]), rsq=c(NA, rsq.partial(losses)$partial.rsq))
# 
# onlyPadditions=rbind(richresults, evenresults, rankresults, gainsresults, lossesresults)
# 
# 
# #----------CO2 addition studies:
# 
# rich=lm(abs_richness_glass ~ sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="CO2",])
# summary(rich)
# richresults=data.frame(response="rich", predictor=names(rich$coefficients), slope=as.numeric(rich$coefficients), pval=as.numeric(summary(rich)$coef[,4]), rsq=c(NA, rsq.partial(rich)$partial.rsq))
# 
# even=lm(abs_evenness_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="CO2",])
# summary(even)
# evenresults=data.frame(response="even", predictor=names(even$coefficients), slope=as.numeric(even$coefficients), pval=as.numeric(summary(even)$coef[,4]), rsq=c(NA, rsq.partial(even)$partial.rsq))
# 
# rank=lm(rank_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="CO2",])
# summary(rank)
# rankresults=data.frame(response="rank", predictor=names(rank$coefficients), slope=as.numeric(rank$coefficients), pval=as.numeric(summary(rank)$coef[,4]), rsq=c(NA, rsq.partial(rank)$partial.rsq))
# 
# gains=lm(gains_glass~sMAP + sMAT + srrich + sanpp,data=change_glass_d_mean[change_glass_d_mean$trt_type2=="CO2",])
# summary(gains)
# gainsresults=data.frame(response="gains", predictor=names(gains$coefficients), slope=as.numeric(gains$coefficients), pval=as.numeric(summary(gains)$coef[,4]), rsq=c(NA, rsq.partial(gains)$partial.rsq))
# 
# losses=lm(losses_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="CO2",])
# summary(losses)
# lossesresults=data.frame(response="losses", predictor=names(losses$coefficients), slope=as.numeric(losses$coefficients), pval=as.numeric(summary(losses)$coef[,4]), rsq=c(NA, rsq.partial(losses)$partial.rsq))
# 
# onlyCO2additions=rbind(richresults, evenresults, rankresults, gainsresults, lossesresults)
# 
# #----------Irr manipulation studies:
# 
# rich=lm(abs_richness_glass ~ sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Irrigation",])
# summary(rich)
# richresults=data.frame(response="rich", predictor=names(rich$coefficients), slope=as.numeric(rich$coefficients), pval=as.numeric(summary(rich)$coef[,4]), rsq=c(NA, rsq.partial(rich)$partial.rsq))
# 
# even=lm(abs_evenness_glass~sMAP + sMAT + srrich + sanpp,data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Irrigation",])
# summary(even)
# evenresults=data.frame(response="even", predictor=names(even$coefficients), slope=as.numeric(even$coefficients), pval=as.numeric(summary(even)$coef[,4]), rsq=c(NA, rsq.partial(even)$partial.rsq))
# 
# rank=lm(rank_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Irrigation",])
# summary(rank)
# rankresults=data.frame(response="rank", predictor=names(rank$coefficients), slope=as.numeric(rank$coefficients), pval=as.numeric(summary(rank)$coef[,4]), rsq=c(NA, rsq.partial(rank)$partial.rsq))
# 
# gains=lm(gains_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Irrigation",])
# summary(gains)
# gainsresults=data.frame(response="gains", predictor=names(gains$coefficients), slope=as.numeric(gains$coefficients), pval=as.numeric(summary(gains)$coef[,4]), rsq=c(NA, rsq.partial(gains)$partial.rsq))
# 
# losses=lm(losses_glass~sMAP + sMAT + srrich + sanpp,data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Irrigation",])
# summary(losses)
# lossesresults=data.frame(response="losses", predictor=names(losses$coefficients), slope=as.numeric(losses$coefficients), pval=as.numeric(summary(losses)$coef[,4]), rsq=c(NA, rsq.partial(losses)$partial.rsq))
# 
# onlyIrrmanipulations=rbind(richresults, evenresults, rankresults, gainsresults, lossesresults)
# 
# #----------mult nuts studies:
# 
# rich=lm(abs_richness_glass ~ sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Mult. Nuts.",])
# summary(rich)
# richresults=data.frame(response="rich", predictor=names(rich$coefficients), slope=as.numeric(rich$coefficients), pval=as.numeric(summary(rich)$coef[,4]), rsq=c(NA, rsq.partial(rich)$partial.rsq))
# 
# even=lm(abs_evenness_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Mult. Nuts.",])
# summary(even)
# evenresults=data.frame(response="even", predictor=names(even$coefficients), slope=as.numeric(even$coefficients), pval=as.numeric(summary(even)$coef[,4]), rsq=c(NA, rsq.partial(even)$partial.rsq))
# 
# rank=lm(rank_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Mult. Nuts.",])
# summary(rank)
# rankresults=data.frame(response="rank", predictor=names(rank$coefficients), slope=as.numeric(rank$coefficients), pval=as.numeric(summary(rank)$coef[,4]), rsq=c(NA, rsq.partial(rank)$partial.rsq))
# 
# gains=lm(gains_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Mult. Nuts.",])
# summary(gains)
# gainsresults=data.frame(response="gains", predictor=names(gains$coefficients), slope=as.numeric(gains$coefficients), pval=as.numeric(summary(gains)$coef[,4]), rsq=c(NA, rsq.partial(gains)$partial.rsq))
# 
# losses=lm(losses_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Mult. Nuts.",])
# summary(losses)
# lossesresults=data.frame(response="losses", predictor=names(losses$coefficients), slope=as.numeric(losses$coefficients), pval=as.numeric(summary(losses)$coef[,4]), rsq=c(NA, rsq.partial(losses)$partial.rsq))
# 
# onlyMultNutsmanipulations=rbind(richresults, evenresults, rankresults, gainsresults, lossesresults)
# 
# #----------temp studies:
# 
# rich=lm(abs_richness_glass ~ sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Temperature",])
# summary(rich)
# richresults=data.frame(response="rich", predictor=names(rich$coefficients), slope=as.numeric(rich$coefficients), pval=as.numeric(summary(rich)$coef[,4]), rsq=c(NA, rsq.partial(rich)$partial.rsq))
# 
# even=lm(abs_evenness_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Temperature",])
# summary(even)
# evenresults=data.frame(response="even", predictor=names(even$coefficients), slope=as.numeric(even$coefficients), pval=as.numeric(summary(even)$coef[,4]), rsq=c(NA, rsq.partial(even)$partial.rsq))
# 
# rank=lm(rank_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Temperature",])
# summary(rank)
# rankresults=data.frame(response="rank", predictor=names(rank$coefficients), slope=as.numeric(rank$coefficients), pval=as.numeric(summary(rank)$coef[,4]), rsq=c(NA, rsq.partial(rank)$partial.rsq))
# 
# gains=lm(gains_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Temperature",])
# summary(gains)
# gainsresults=data.frame(response="gains", predictor=names(gains$coefficients), slope=as.numeric(gains$coefficients), pval=as.numeric(summary(gains)$coef[,4]), rsq=c(NA, rsq.partial(gains)$partial.rsq))
# 
# losses=lm(losses_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Temperature",])
# summary(losses)
# lossesresults=data.frame(response="losses", predictor=names(losses$coefficients), slope=as.numeric(losses$coefficients), pval=as.numeric(summary(losses)$coef[,4]), rsq=c(NA, rsq.partial(losses)$partial.rsq))
# 
# onlyTempmanipulations=rbind(richresults, evenresults, rankresults, gainsresults, lossesresults)

#----------temp + IRR studies: there are too few observations to run this one

# rich=lm(abs_richness_glass ~ sMAP + sMAT+ srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Irr + Temp",])
# summary(rich)
# richresults=data.frame(response="rich", predictor=names(rich$coefficients), slope=as.numeric(rich$coefficients), pval=as.numeric(summary(rich)$coef[,4]), rsq=c(NA, rsq.partial(rich)$partial.rsq))
# 
# even=lm(abs_evenness_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Irr + Temp",])
# summary(even)
# evenresults=data.frame(response="even", predictor=names(even$coefficients), slope=as.numeric(even$coefficients), pval=as.numeric(summary(even)$coef[,4]), rsq=c(NA, rsq.partial(even)$partial.rsq))
# 
# rank=lm(rank_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Irr + Temp",])
# summary(rank)
# rankresults=data.frame(response="rank", predictor=names(rank$coefficients), slope=as.numeric(rank$coefficients), pval=as.numeric(summary(rank)$coef[,4]), rsq=c(NA, rsq.partial(rank)$partial.rsq))
# 
# gains=lm(gains_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Irr + Temp",])
# summary(gains)
# gainsresults=data.frame(response="gains", predictor=names(gains$coefficients), slope=as.numeric(gains$coefficients), pval=as.numeric(summary(gains)$coef[,4]), rsq=c(NA, rsq.partial(gains)$partial.rsq))
# 
# losses=lm(losses_glass~sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean[change_glass_d_mean$trt_type2=="Irr + Temp",])
# summary(losses)
# lossesresults=data.frame(response="losses", predictor=names(losses$coefficients), slope=as.numeric(losses$coefficients), pval=as.numeric(summary(losses)$coef[,4]), rsq=c(NA, rsq.partial(losses)$partial.rsq))
# 
# onlyTempIrrmanipulations=rbind(richresults, evenresults, rankresults, gainsresults, lossesresults)

# Making figure -----------------------------------------------------------

#-------make a figure

fulldataset$studies="All manipulations"

# onlyNadditions$studies="N"
# onlyPadditions$studies="P"
# onlyIrrmanipulations$studies="Water"
# onlyCO2additions$studies="CO2"
# #onlyTempIrrmanipulations$studies = "Water + Warming"
# onlyTempmanipulations$studies = "Warming"
# onlyMultNutsmanipulations$studies = "Mult. Nuts."

# forbigfig=rbind(fulldataset, onlyNadditions, onlyPadditions, onlyCO2additions, onlyIrrmanipulations, onlyTempmanipulations, onlyIrrmanipulations, onlyMultNutsmanipulations)
# levels(forbigfig$response)=c("Richness change", "Evenness change", "Rank change", "Species gains", "Species losses")
# levels(forbigfig$predictor)=c("(Intercept)", "ANPP", "MAP", "MAT", "Regional SR")
# forbigfig$significant=as.factor(1*(forbigfig$pval<0.05))
# forbigfig$star.location=ifelse(forbigfig$slope>0, forbigfig$slope+0.1, forbigfig$slope-0.1)

rsqvalues<-data.frame(response=c("rich", "rank", "even", "gains", "losses"), 
                      rsq = c(0.01,0.05,0.08,0.10,0.04),
                      pval = c("n.s.", "n.s.","*",  "*","n.s."),
                      combinded=c("0.01 n.s.","0.05 n.s.","0.08*","0.10*","0.04 n.s."))%>%
  mutate(response2=factor(response, levels=c("rich", "even", "rank", "gains", "losses")))

forbigfig<-fulldataset%>%
  mutate(response2=factor(response, levels = c("rich", "even", "rank", "gains", "losses")),
         predictor2=ifelse(predictor=="sanpp", "ANPP", ifelse(predictor=="sMAP", "MAP", ifelse(predictor=="sMAT", "MAT", ifelse(predictor=="srrich", "Regional SR", ifelse(predictor=="seven", "Site Evenness", "(Intercept)"))))), 
         significant=as.factor(1*(pval<0.05)),
         sig2=ifelse(response=='rank'|response=="losses"|response=="rich", 0, as.numeric(as.character((significant)))),
         star.location=ifelse(slope>0, slope+0.02, slope-0.02))

parameter<-c(
  rich = "Richness change",
  even = "Evenness change",
  rank = "Rank change",
  gains = "Species gains",
  losses = "Species losses"
)


mr<-
ggplot(data=subset(forbigfig, predictor2!="(Intercept)"), aes(x=predictor2, y=slope, fill=rsq)) + 
  geom_col() + 
  geom_point(aes(predictor2, star.location, shape=as.factor(sig2))) + 
  facet_wrap(~response2, ncol = 5, labeller=labeller(response2 = parameter)) + 
  theme(axis.text.x=element_text(angle = 90, vjust = 0.4, hjust=1)) + 
  xlab("Ecosystem Property") + 
  ylab("Slope from standardized\nmultiple regression") +
  guides(fill = guide_colorbar(title = expression(paste("Partial R"^2)))) + 
  scale_shape_manual(values=c(NA, 8), guide=FALSE)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_text(data=rsqvalues, aes(x=0.5, y = 0.22, label = combinded), hjust=0, vjust=1.5)


#ggsave("Summer2018_Results/site predictors of SERGL/site predictors of SERGL.pdf", width=7, height=9.5)
#ggsave("Summer2018_Results/site predictors of SERGL/site predictors of SERGL.png", width=7, height=9.5)

# ggplot(aes(response, slope, fill=rsq), data=forbigfig[!forbigfig$predictor=="(Intercept)" & !forbigfig$pval>0.05,]) + geom_col() + facet_grid(studies~predictor) + theme(axis.text.x=element_text(angle = 90, vjust = 0.4)) + xlab("") + ylab("Effect on aspect of community change\n(slope from standardized multiple regression)") + guides(fill = guide_colorbar(title = "Partial R2"))
# ggsave("Summer2018_Results/site predictors of SERGL/significant site predictors of SERGL.pdf", width=8, height=9.5)




# #-----------------taking a stepwise regression approach.
# #rich
# summary(lm(abs_richness_glass ~ sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean))
# rsq.partial(lm(abs_richness_glass ~ sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean))
# #not sig
# #even
# stepAIC(lm(abs_evenness_glass ~ sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean))
# summary(lm(abs_evenness_glass ~  sMAP + srrich + sanpp, data=change_glass_d_mean))
# rsq.partial(lm(abs_evenness_glass ~  sMAP + srrich + sanpp, data=change_glass_d_mean))
# 
# stepAIC(lm(rank_glass ~ sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean))
# summary(lm(rank_glass ~ sMAP, data=change_glass_d_mean))
# rsq.partial(lm(rank_glass ~ sMAP, data=change_glass_d_mean))
# 
# stepAIC(lm(gains_glass ~ sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean))
# summary(lm(gains_glass ~ sMAP + srrich, data=change_glass_d_mean))
# rsq.partial(lm(gains_glass ~ sMAP + srrich, data=change_glass_d_mean))
# 
# stepAIC(lm(losses_glass ~ sMAP + sMAT + srrich + sanpp, data=change_glass_d_mean))
# summary(lm(losses_glass ~ sanpp, data=change_glass_d_mean))
# rsq.partial(lm(losses_glass ~ sanpp, data=change_glass_d_mean))

#Making a correlation figure

change_glass_d_mean2<-change_glass_d_mean[,c("site_project_comm", "treatment","abs_richness_glass", "abs_evenness_glass", "rank_glass", "gains_glass", "losses_glass", "sanpp", "sMAP", "sMAT", "srrich", "seven")]

tograph_cor<-change_glass_d_mean2%>%
  gather(parm, value, sanpp:seven)%>%
  gather(vari_metric, vari_value, abs_richness_glass:losses_glass)%>%
  mutate(parm_group=factor(parm, levels = c("sanpp","sMAP","sMAT","srrich", "seven")),
         vari_group=factor(vari_metric, levels=c("abs_richness_glass","abs_evenness_glass","rank_glass", 'gains_glass','losses_glass')))

rvalues <- tograph_cor %>% 
  group_by(vari_group, parm_group) %>%
  summarize(r.value = round((cor.test(vari_value, value)$estimate), digits=3),
            p.value = (cor.test(vari_value, value)$p.value))%>%
  mutate(adjp=p.adjust(p.value, method="BH", n=25),
         sig=ifelse(adjp<0.05, 1, 0))

parameter2<-c(
  sanpp = "Site ANPP",
  sMAP = "MAP",
  sMAT = "MAT",
  srrich = "Regional SR",
  seven = "Site Evenness"
)

vari<-c(
  abs_richness_glass = "Richness change",
  abs_evenness_glass = 'Evenness change',
  rank_glass = 'Rank change',
  gains_glass = 'Species gains',
  losses_glass = 'Species losses'
)

cor<-
ggplot(data=tograph_cor, aes(x = value, y = vari_value))+
  geom_point()+
  facet_grid(row = vars(vari_group), cols = vars(parm_group), scales="free", labeller=labeller(vari_group = vari, parm_group = parameter2))+
  xlab("Standardized Value")+
  ylab("Glass's D")+
  #geom_smooth(data=subset(tograph_cor, vari_group=="abs_evenness_glass"&parm_group=="sMAP"), method="lm", se=F, color = "black")+
  #geom_smooth(data=subset(tograph_cor, vari_group=="abs_evenness_glass"&parm_group=="srrich"), method="lm", se=F, color = "black")+
  #geom_smooth(data=subset(tograph_cor, vari_group=="rank_glass"&parm_group=="srrich"), method="lm", se=F, color = "black")+
  #geom_smooth(data=subset(tograph_cor, vari_group=="gains_glass"&parm_group=="sMAP"), method="lm", se=F, color = "black")+
  geom_smooth(data=subset(tograph_cor, vari_group=="gains_glass"&parm_group=="srrich"), method="lm", se=F, color = "black")+
  #geom_smooth(data=subset(tograph_cor, vari_group=="losses_glass"&parm_group=="sMAT"), method="lm", se=F, color = "black")+
  geom_smooth(data=subset(tograph_cor, vari_group=="losses_glass"&parm_group=="sanpp"), method="lm", se=F, color = "black")+
  geom_smooth(data=subset(tograph_cor, vari_group=="abs_evenness_glass"&parm_group=="seven"), method="lm", se=F, color = "black")+
  geom_text(data=rvalues, mapping=aes(x=Inf, y =Inf, label = r.value), hjust=1.05, vjust=1.5)

grid.arrange(mr, cor, ncol=1, heights=c(1.8,4))

##-------------glass's D analyses
# 
# t.test(change_glass_d_mean$abs_richness_glass, mu=0)#over all sig.
# t.test(change_glass_d_mean$abs_evenness_glass, mu=0)#overall sig.
# t.test(change_glass_d_mean$rank_glass, mu=0)#not sig
# t.test(change_glass_d_mean$gains_glass, mu=0)#not sig
# t.test(change_glass_d_mean$losses_glass, mu=0)#sig.
# 
# #water
# irr<-subset(change_glass_d_mean, trt_type2=="Irrigation")
# t.test(irr$abs_richness_glass, mu=0)#not sig
# t.test(irr$abs_evenness_glass, mu=0)#not sig
# t.test(irr$rank_glass, mu=0)#not sig
# t.test(irr$gains_glass, mu=0)#not sig
# t.test(irr$losses_glass, mu=0)#not sig
# 
# #N
# N<-subset(change_glass_d_mean, trt_type2=="N")
# t.test(N$abs_richness_glass, mu=0)#sig
# t.test(N$abs_evenness_glass, mu=0)#sig
# t.test(N$rank_glass, mu=0)# sig
# t.test(N$gains_glass, mu=0)# sig
# t.test(N$losses_glass, mu=0)# sig
# 
# #P
# P<-subset(change_glass_d_mean, trt_type2=="P")
# t.test(P$abs_richness_glass, mu=0)#not sig
# t.test(P$abs_evenness_glass, mu=0)#not sig
# t.test(P$rank_glass, mu=0)# not sig
# t.test(P$gains_glass, mu=0)# not sig
# t.test(P$losses_glass, mu=0)#not sig
# 
# #CO2
# CO2<-subset(change_glass_d_mean, trt_type2=="CO2")
# t.test(CO2$abs_richness_glass, mu=0)#not sig
# t.test(CO2$abs_evenness_glass, mu=0)#not sig
# t.test(CO2$rank_glass, mu=0)# not sig
# t.test(CO2$gains_glass, mu=0)# not sig
# t.test(CO2$losses_glass, mu=0)#not sig
# 
# #mult nuts
# multnuts<-subset(change_glass_d_mean, trt_type2=="Mult. Nuts.")
# t.test(multnuts$abs_richness_glass, mu=0)# sig
# t.test(multnuts$abs_evenness_glass, mu=0)# sig
# t.test(multnuts$rank_glass, mu=0)# not sig
# t.test(multnuts$gains_glass, mu=0)# not sig
# t.test(multnuts$losses_glass, mu=0)# sig
# 
# #temperature
# temp<-subset(change_glass_d_mean, trt_type2=="Temperature")
# t.test(temp$abs_richness_glass, mu=0)# not sig
# t.test(temp$abs_evenness_glass, mu=0)# not sig
# t.test(temp$rank_glass, mu=0)# not sig
# t.test(temp$gains_glass, mu=0)# not sig
# t.test(temp$losses_glass, mu=0)# not sig
# 
# #water and temperature
# irrtemp<-subset(change_glass_d_mean, trt_type2=="Irr + Temp")
# t.test(irrtemp$abs_richness_glass, mu=0)# not sig
# t.test(irrtemp$abs_evenness_glass, mu=0)# not sig
# t.test(irrtemp$rank_glass, mu=0)# not sig
# t.test(irrtemp$gains_glass, mu=0)# not sig
# t.test(irrtemp$losses_glass, mu=0)# not sig
# 
# ###graphing this.
# glassD_trt<-change_glass_d_mean%>%
#   group_by(trt_type2)%>%
#   summarize(rich=mean(abs_richness_glass),
#             sd_rich=sd(abs_richness_glass),
#             even=mean(abs_evenness_glass),
#             sd_even=sd(abs_evenness_glass),
#             rank=mean(rank_glass),
#             sd_rank=sd(rank_glass),
#             gain=mean(gains_glass),
#             sd_gain=sd(gains_glass),
#             loss=mean(losses_glass),
#             sd_loss=sd(losses_glass),
#             num=length(abs_richness_glass))%>%
#   mutate(se_rich=sd_rich/sqrt(num),
#          se_even=sd_even/sqrt(num),
#          se_rank=sd_rank/sqrt(num),
#          se_gain=sd_gain/sqrt(num),
#          se_loss=sd_loss/sqrt(num))%>%
#   filter(trt_type2=="N"|trt_type2=="Mult. Nuts."|trt_type2=="Irrigation"|trt_type2=="CO2"|trt_type2=="P"|trt_type2=="Temperature"|trt_type2=="Irr + Temp")
# 
# glassD_all<-change_glass_d_mean%>%
#   ungroup()%>%
#   summarize(rich=mean(abs_richness_glass),
#             sd_rich=sd(abs_richness_glass),
#             even=mean(abs_evenness_glass),
#             sd_even=sd(abs_evenness_glass),
#             rank=mean(rank_glass),
#             sd_rank=sd(rank_glass),
#             gain=mean(gains_glass),
#             sd_gain=sd(gains_glass),
#             loss=mean(losses_glass),
#             sd_loss=sd(losses_glass),
#             num=length(abs_richness_glass))%>%
#   mutate(se_rich=sd_rich/sqrt(num),
#          se_even=sd_even/sqrt(num),
#          se_rank=sd_rank/sqrt(num),
#          se_gain=sd_gain/sqrt(num),
#          se_loss=sd_loss/sqrt(num))%>%
#   mutate(trt_type2="All Trts")
# 
# glassD_bargraph<-rbind(glassD_all, glassD_trt)
# glassD_bargraph_mean<-glassD_bargraph[,c("trt_type2", "rich", "even","rank","gain","loss")]%>%
#   gather(metric, mean, rich:loss)
# 
# glassD_bargraph_se<-glassD_bargraph[,c('trt_type2',"se_rich",'se_even','se_rank','se_gain','se_loss')]%>%
#   gather(metric_se, se, se_rich:se_loss)
# 
# glassD_toplot<-cbind(glassD_bargraph_mean, glassD_bargraph_se)
# glassD_toplot2<-glassD_toplot[,-4]%>%
#   mutate(sig=ifelse(trt_type2=='All Trts'&metric=="rich", 1, ifelse(trt_type2=='All Trts'&metric=="even", 1, ifelse(trt_type2=='All Trts'&metric=="loss", 1, ifelse(trt_type2=='N', 1, ifelse(trt_type2=='Mult. Nuts.'&metric=="rich", 1, ifelse(trt_type2=='Mult. Nuts'&metric=="even", 1,ifelse(trt_type2=='Mult. Nuts.'&metric=="loss", 1,0))))))))%>%
#   mutate(location=ifelse(sig==1, mean+0.3,NA))%>%
#   mutate(metric_group=factor(metric, levels=c("rich",'even','rank',"gain","loss")))
# 
# vari<-c(
#   rich = "Richness Change",
#   even = 'Eveness Change',
#   rank = 'Rank Change',
#   gain = 'Species Gains',
#   loss = 'Species Losses'
# )
# 
# ggplot(data=glassD_toplot2, aes(x=trt_type2, y=mean, fill=trt_type2))+
#   geom_bar(position=position_dodge(), stat="identity")+
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se),position= position_dodge(0.9), width=0.2)+
#   ylab("Glass's D")+
#   xlab("")+
#   scale_x_discrete(limits=c("All Trts","CO2","Irrigation","Temperature","Irr + Temp","N","P","Mult. Nuts."), labels=c("All Trts", "CO2","Irrigation", "Temp","Irr + Temp","Nitrogen","Phosphorus","Mult Nuts"))+
#   scale_fill_manual(values=c("black","green3",'purple','blue','darkorange','orange','gold3','red'))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
#   geom_vline(xintercept = 1.5, size = 1)+
#   geom_point(aes(trt_type2, location), shape=8)+
#   facet_wrap(~metric_group, ncol=1, labeller = labeller(metric_group=vari))

