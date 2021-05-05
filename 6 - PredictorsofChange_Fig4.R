################################################################################
##  This script uses regressions to test the ability of site abiotic and biotic
##  conditions to predict all community change measures
##
##  Author: Emily Grman and Meghan Avolio
##  Date: March 20, 2018
##  Update: May 4, 2021
################################################################################

library(tidyverse)
library(grid)
library(vegan)
library(rsq)
library(car)
library(gridExtra)

#Meghan's working directory
setwd("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\")

theme_set(theme_bw(12))
theme_update(panel.grid.major=element_blank(), panel.grid.minor=element_blank())



# reading in the data -----------------------------------------------------
change_metrics <- read.csv("CoRRE_RAC_Measures.csv") %>%
  mutate(abs_richness_change = abs(richness_change),
         abs_evenness_change = abs(evenness_change))

subset<-read.csv("Experiment_Trt_Subset.csv")

plot_mani<-read.csv("ExperimentInformation_March2019.csv")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  select(site_project_comm, plot_mani, treatment)%>%
  unique()

# read in site level predictor variables
info.spc=read.csv("SiteExperimentDetails_March2019.csv") %>%
  mutate(site_project_comm = paste(site_code, project_name, community_type, sep="_"))%>%
  select(site_project_comm, rrich, anpp, MAT, MAP)%>%
  unique()

##getting site Evar for each location
controls<-change_metrics%>%
  left_join(plot_mani)%>%
  filter(plot_mani==0)%>%
  unique()

div_evar<-read.csv("CORRE_RichEvar.csv")

evenness<-div_evar%>%
  right_join(controls)%>%
  group_by(site_project_comm)%>%
  summarise(Evenness=mean(Evar, na.rm=T))


# read in site level predictor variables
info.spc=read.csv("SiteExperimentDetails_March2019.csv") %>%
  mutate(site_project_comm = paste(site_code, project_name, community_type, sep="_"))%>%
  select(site_project_comm, rrich, anpp, MAT, MAP)%>%
  unique()


# Calculating Glass's D ---------------------------------------------------
### Control data
change_control <- change_metrics %>%
  left_join(plot_mani)%>%
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
  left_join(plot_mani)%>%
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

## replace Inf with NAs in change_glass_d and select only treatments I want
change_glass_d <- change_glass_d %>%
  mutate(abs_richness_glass=replace(abs_richness_glass, abs_richness_glass=="Inf"|abs_richness_glass=="NaN", NA)) %>%
  mutate(abs_evenness_glass=replace(abs_evenness_glass, abs_evenness_glass=="Inf"|abs_evenness_glass=="NaN", NA)) %>%
  mutate(rank_glass=replace(rank_glass, rank_glass=="Inf"|rank_glass=="NaN", NA)) %>%
  mutate(gains_glass=replace(gains_glass, gains_glass=="Inf"|gains_glass=="NaN", NA)) %>%
  mutate(losses_glass=replace(losses_glass, losses_glass=="Inf"|losses_glass=="NaN", NA))%>%
  right_join(subset)
  
### calculate mean change through time and combine with predictor variables
change_glass_d_mean <- change_glass_d %>%
  group_by(site_project_comm, treatment, plot_mani) %>%
  summarise_at(vars(abs_richness_glass, abs_evenness_glass, rank_glass, gains_glass, losses_glass), funs(mean), na.rm=T) %>%
  left_join(info.spc) %>%
  left_join(evenness)
  

# Correlations between predictor variables --------------------------------

pred=as.matrix(change_glass_d_mean[, c("MAP", "MAT", "rrich", "anpp", "Evenness")])
cor(pred)
pairs(pred)


# Doing standardized multiple regressions ---------------------------------

change_glass_d_mean$sMAP<-scale(change_glass_d_mean$MAP)
change_glass_d_mean$sMAT<-scale(change_glass_d_mean$MAT)
change_glass_d_mean$srrich<-scale(change_glass_d_mean$rrich)
change_glass_d_mean$sanpp<-scale(change_glass_d_mean$anpp)
change_glass_d_mean$seven<-scale(change_glass_d_mean$Evenness)

rich=lm(abs_richness_glass ~ sMAP + sMAT + srrich + sanpp + seven, data=change_glass_d_mean)
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
p.adjust(0.0001829, method = "BH", n=5)# p = 0.001
rsq.partial(even)
evenresults=data.frame(response="even", predictor=names(even$coefficients), slope=as.numeric(even$coefficients), pval=as.numeric(summary(even)$coef[,4]), rsq=c(NA, rsq.partial(even)$partial.rsq))

rank=lm(rank_glass~sMAP + sMAT + srrich + sanpp + seven, data=change_glass_d_mean)
summary(rank)
p.adjust(0.01378, method = "BH", n=5) #p = 0.0689
rsq.partial(rank)
rankresults=data.frame(response="rank", predictor=names(rank$coefficients), slope=as.numeric(rank$coefficients), pval=as.numeric(summary(rank)$coef[,4]), rsq=c(NA, rsq.partial(rank)$partial.rsq))

gains=lm(gains_glass~sMAP + sMAT + srrich + sanpp +seven, data=change_glass_d_mean)
summary(gains)
p.adjust(0.00002123, method = "BH", n=5) #p = 0.0001 
rsq.partial(gains)
gainsresults=data.frame(response="gains", predictor=names(gains$coefficients), slope=as.numeric(gains$coefficients), pval=as.numeric(summary(gains)$coef[,4]), rsq=c(NA, rsq.partial(gains)$partial.rsq))

losses=lm(losses_glass~sMAP + sMAT + srrich + sanpp +seven, data=change_glass_d_mean)
summary(losses)
p.adjust(0.04196, method = "BH", n=5) #p = 0.2098

rsq.partial(losses)
lossesresults=data.frame(response="losses", predictor=names(losses$coefficients), slope=as.numeric(losses$coefficients), pval=as.numeric(summary(losses)$coef[,4]), rsq=c(NA, rsq.partial(losses)$partial.rsq))

fulldataset=rbind(richresults, evenresults, rankresults, gainsresults, lossesresults)


# Making figure 4-----------------------------------------------------------
rsqvalues<-data.frame(response=c("rich", "rank", "even", "gains", "losses"), 
                      rsq = c(0.02,0.04,0.09,0.11,0.03),
                      pval = c("n.s.", "n.s.","*",  "*","n.s."),
                      combinded=c("0.02 n.s.","0.04 n.s.","0.09*","0.11*","0.03 n.s."))%>%
  mutate(response2=factor(response, levels=c("rich", "even", "rank", "gains", "losses")))

forbigfig<-fulldataset%>%
  mutate(response2=factor(response, levels = c("rich", "even", "rank", "gains", "losses")),
         predictor2=ifelse(predictor=="sanpp", "ANPP", ifelse(predictor=="sMAP", "MAP", ifelse(predictor=="sMAT", "MAT", ifelse(predictor=="srrich", "Regional SR", ifelse(predictor=="seven", "Site Evenness", "(Intercept)"))))), 
         significant=as.factor(1*(pval<0.05)),
         sig2=ifelse(response=='rank'|response=="losses"|response=="rich", 0, as.numeric(as.character((significant)))),
         star.location=ifelse(slope>0, slope+0.02, slope-0.02))

parameter<-c(
  rich = "Richness\nchange",
  even = "Evenness\nchange",
  rank = "Rank\nchange",
  gains = "Species\ngains",
  losses = "Species\nlosses"
)

fig4<-
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
  geom_text(data=rsqvalues, aes(x=0.5, y = 0.25, label = combinded), hjust=0, vjust=1.5)


ggsave("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Fig4.pdf", fig4, dpi=600, units="mm", width=173, height = 90)


# Making figure S5 --------------------------------------------------------

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

figS5<-
ggplot(data=tograph_cor, aes(x = value, y = vari_value))+
  geom_point()+
  facet_grid(row = vars(vari_group), cols = vars(parm_group), scales="free", labeller=labeller(vari_group = vari, parm_group = parameter2))+
  xlab("Standardized Value")+
  ylab("Glass's D")+
  geom_smooth(data=subset(tograph_cor, vari_group=="gains_glass"&parm_group=="srrich"), method="lm", se=F, color = "black")+
  geom_smooth(data=subset(tograph_cor, vari_group=="losses_glass"&parm_group=="sanpp"), method="lm", se=F, color = "black")+
  geom_smooth(data=subset(tograph_cor, vari_group=="abs_evenness_glass"&parm_group=="seven"), method="lm", se=F, color = "black")+
  geom_text(data=rvalues, mapping=aes(x=Inf, y =Inf, label = r.value), hjust=1.05, vjust=1.5)

