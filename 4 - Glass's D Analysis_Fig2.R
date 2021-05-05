################################################################################
##  This script calculates Glass's D for all treatments for analyses
##
##  Author: Meghan Avolio (meghan.avolio@jhu.edu) and Kevin Wilcox
##  Date: March 20, 2018
##  Update: May 4, 2021
################################################################################


library(tidyverse)
library(grid)
library(gridExtra)

#Meghan's working directory
setwd("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\")


theme_set(theme_bw(12))


# Read in necessary datasets ----------------------------------------------

change_metrics <- read.csv("CoRRE_RAC_Measures.csv") %>%
  mutate(abs_richness_change = abs(richness_change),
         abs_evenness_change = abs(evenness_change))

plot_mani<-read.csv("ExperimentInformation_March2019.csv")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  select(site_project_comm, plot_mani, treatment)%>%
  unique()
  

subset<-read.csv("Experiment_Trt_Subset.csv")%>%
  select(-trt_type2)

# read in treatment variables for sub-setting later
##info on GCD treatment
info.trt<-read.csv("ExperimentInformation_March2019.csv")%>%
  select(site_code, project_name, community_type, treatment,plot_mani, resource_mani, trt_type)%>%
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

##info on GCD treatment type
info.trt2<-read.csv("SiteExperimentDetails_March2019.csv") %>%
  mutate(site_project_comm = paste(site_code, project_name, community_type, sep="_"))%>%
  select(site_project_comm, treatment, trt_type)%>%
  unique()%>%
  mutate(trt_type3=ifelse(trt_type=="drought"|trt_type=="irr"|trt_type=="N"|trt_type=="precip_vari"|trt_type=="P"|trt_type=="CO2"|trt_type=="other_resource", "R", ifelse(trt_type=="mow_clip"|trt_type=="temp"|trt_type=="plant_mani"|trt_type=="other_nonresource"|trt_type=="herb_removal"|trt_type=="NxN", "NonR", ifelse(trt_type=="RxR"|trt_type=="RxRxR", "Mult R", ifelse(trt_type=="threeway"|trt_type=="RxN", "R+NonR", "oops")))))%>%
  select(-trt_type)



# Calculate Glass's D -----------------------------------------------------


### Control data
change_control <- change_metrics %>%
  left_join(plot_mani)%>%
  filter(plot_mani==0) %>%
  select(treatment_year, treatment_year2, abs_richness_change, abs_evenness_change, 
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

## replace Inf with NAs in change_glass_d
change_glass_d <- change_glass_d %>%
  mutate(abs_richness_glass=replace(abs_richness_glass, abs_richness_glass=="Inf"|abs_richness_glass=="NaN", NA)) %>%
  mutate(abs_evenness_glass=replace(abs_evenness_glass, abs_evenness_glass=="Inf"|abs_evenness_glass=="NaN", NA)) %>%
  mutate(rank_glass=replace(rank_glass, rank_glass=="Inf"|rank_glass=="NaN", NA)) %>%
  mutate(gains_glass=replace(gains_glass, gains_glass=="Inf"|gains_glass=="NaN", NA)) %>%
  mutate(losses_glass=replace(losses_glass, losses_glass=="Inf"|losses_glass=="NaN", NA))



### Select treatments I want and calculate mean change through time and combine with predictor variables
GlassD<-change_glass_d%>%
  select(-plot_mani)%>%
  right_join(subset)%>%
  rename(richness_change_abs=abs_richness_glass,
         evenness_change_abs=abs_evenness_glass,
         rank_change=rank_glass,
         gains=gains_glass,
         losses=losses_glass)%>%
  gather(response_var, glassd, richness_change_abs:losses)%>%
  left_join(info.trt)%>%
  left_join(info.trt2)%>%
  ungroup()


# t-test analysis of Glass's D by GCD treatment and type - all raw data  ----------------------------------

###doing with all years for gcd treatments
allyears_all <- GlassD %>%
  group_by(site_project_comm, treatment, response_var, trt_type2, use) %>%
  summarise(mglassd=mean(glassd, na.rm=T))

###doing with all years for gcd type
allyears_all_trt3 <- GlassD %>%
  group_by(site_project_comm, treatment, response_var, trt_type3) %>%
  summarise(mglassd=mean(glassd, na.rm=T))


sig_alla_overall<-allyears_all%>%
  group_by(response_var)%>%
  summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
  mutate(adjp=p.adjust(pval, method = "BH", n=60),
         sig=ifelse(adjp<0.05, 1, 0),
         trt_type2="All GCDs")

sig_alla_trts<-allyears_all%>%
  filter(use==1)%>%
  group_by(response_var, trt_type2)%>%
  summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
  mutate(adjp=p.adjust(pval, method = "BH", n=60),
         sig=ifelse(adjp<0.05, 1, 0))

sig_alla_trts3<-allyears_all_trt3%>%
  group_by(response_var, trt_type3)%>%
  summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
  mutate(adjp=p.adjust(pval, method = "BH", n=60),
         sig=ifelse(adjp<0.05, 1, 0))%>%
  rename(trt_type2=trt_type3)

sig_alla<-sig_alla_overall%>%
  bind_rows(sig_alla_trts, sig_alla_trts3)




# Making figure 2 ---------------------------------------------------------


glassD_trt<-allyears_all%>% 
  filter(use==1)

glassD_allGCD<-allyears_all%>%
  ungroup()%>%
  mutate(trt_type2="All GCDs")

glassD_allRNonR<-allyears_all_trt3%>%
  ungroup()%>%
  rename(trt_type2=trt_type3)

glassD_alldata<-glassD_allGCD%>%
  bind_rows(glassD_trt,glassD_allRNonR)%>%
  left_join(sig_alla)%>%
  mutate(location=ifelse(sig==1&response_var=="richness_change_abs",3.5,ifelse(sig==1&response_var=="evenness_change_abs", 6.5, ifelse(sig==1&response_var=="rank_change", 3, ifelse(sig==1&response_var=="losses", 2.5, ifelse(sig==1&response_var=="gains", 2.5, NA))))))%>%
  mutate(response_var2=factor(response_var, level=c("evenness_change_abs","rank_change",'gains','losses',"richness_change_abs")))

response_label<-c(
    evenness_change_abs="Evenness Change",
    rank_change="Rank Change",
    gains = "Species Gains",
    losses="Species Losses",
    richness_change_abs="Richness Change")

fig2<-ggplot(data=glassD_alldata, aes(x=trt_type2, y=mglassd, fill=trt_type2))+
  geom_boxplot()+
  ylab("Glass's D")+
  xlab("")+
  scale_x_discrete(limits=c("All GCDs","NonR", 'R', "Mult R", "R+NonR", "CO2","Irrigation","Precip. Vari." ,"Temperature","N","P", "Mult. Nuts."), labels=c("All GCDs", "Non-Res.","Single Res.", "Multiple Res.", "Res.+Non-Res.",   "CO2","Irrigation","Precip. Vari.", "Temp","Nitrogen","Phosphorus", "Mult. Nuts."))+
  scale_fill_manual(values=c("black", "green3",'blue',"snow4",'darkorange', 'orange',  "gray", 'gold3','lightblue',"lightgray","darkgray", "red"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
  geom_vline(xintercept = 5.5, size = 0.5)+
  geom_vline(xintercept = 1.5, size = 0.5)+
  geom_hline(yintercept = 0)+
  geom_point(aes(trt_type2, location), shape=8, size=3)+
  facet_wrap(~response_var2, labeller=labeller(response_var2=response_label), ncol=2, scales="free_y")


ggsave("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Fig2.pdf", fig2, dpi=600, units="mm", width=173, height = 170)

#for esa
# ggplot(data=subset(glassD_alldata, trt_type2=="R"|trt_type2=="Mult R"|trt_type2=="NonR"|trt_type2=="R+NonR"), aes(x=trt_type2, y=mglassd, fill=trt_type2))+
#   geom_boxplot()+
#   ylab("Glass's D")+
#   xlab("")+
#   #scale_x_discrete(limits=c("NonR", 'R', "Mult R", "R+NonR"), labels=c("Non-Res.","Single Res.", "Multiple Res.", "Res.+Non-Res."))+
#   scale_fill_manual(name = "Treatment\nType", values=c("red", "gray", "orange", "gold"))+
#   theme(axis.text.x = element_blank())+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   geom_hline(yintercept = 0)+
#   geom_point(aes(trt_type2, location), shape=8, size=3)+
#   facet_wrap(~response_var2, labeller=labeller(response_var2=response_label), ncol=3, scales="free_y")
# 
# ggplot(data=subset(glassD_alldata, trt_type2!="R"&trt_type2!="Mult R"&trt_type2!="NonR"&trt_type2!="R+NonR"), aes(x=trt_type2, y=mglassd, fill=trt_type2))+
#   geom_boxplot()+
#   ylab("Glass's D")+
#   xlab("")+
#   #scale_x_discrete(limits=c("All GCDs", "CO2","Irrigation","Precip. Vari." ,"Temperature","N","P", "Mult. Nuts."), labels=c("All GCDs",  "CO2","Irrigation","Precip. Vari.", "Temp","Nitrogen","Phosphorus", "Mult. Nuts."))+
#   scale_fill_manual(name = "GCD Treatment", values=c("black", "green3",'blue','darkorange', 'orange', 'gold3','lightblue', "red"))+
#   theme(axis.text.x = element_blank())+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   geom_vline(xintercept = 1.5, size = 0.5)+
#   geom_hline(yintercept = 0)+
#   geom_point(aes(trt_type2, location), shape=8, size=3)+
#   facet_wrap(~response_var2, labeller=labeller(response_var2=response_label), ncol=3, scales="free_y")



# Using absolute value data for Figure S4 ---------------------------------


###using all the data with ABSOLUTE VALUE
sig_alla_overall_abs<-allyears_all%>%
  group_by(response_var)%>%
  summarize(pval=t.test(abs(mglassd), mu=0)$p.value)%>%
  mutate(adjp=p.adjust(pval, method = "BH", n=60),
         sig=ifelse(adjp<0.05, 1, 0),
         trt_type2="All GCDs")

sig_alla_trts_abs<-allyears_all%>%
  filter(use==1)%>%
  group_by(response_var, trt_type2)%>%
  summarize(pval=t.test(abs(mglassd), mu=0)$p.value)%>%
  mutate(adjp=p.adjust(pval, method = "BH", n=60),
         sig=ifelse(adjp<0.05, 1, 0))

sig_alla_trts3_abs<-allyears_all_trt3%>%
  group_by(response_var, trt_type3)%>%
  summarize(pval=t.test(abs(mglassd), mu=0)$p.value)%>%
  mutate(adjp=p.adjust(pval, method = "BH", n=60),
         sig=ifelse(adjp<0.05, 1, 0))%>%
  rename(trt_type2=trt_type3)

sig_alla_abs<-sig_alla_overall_abs%>%
  bind_rows(sig_alla_trts_abs, sig_alla_trts3_abs)

glassD_trta_box<-allyears_all%>% 
  filter(use==1)

glassD_alla_box<-allyears_all%>%
  ungroup()%>%
  mutate(trt_type2="All GCDs")

glassD_allRNonR<-allyears_all_trt3%>%
  ungroup()%>%
  rename(trt_type2=trt_type3)

glassD_alldata_box<-glassD_alla_box%>%
  bind_rows(glassD_trta_box,glassD_allRNonR)%>%
  left_join(sig_alla_abs)%>%
  mutate(location=ifelse(sig==1, -0.25, NA))%>%
  mutate(response_var2=factor(response_var, level=c("evenness_change_abs","rank_change",'gains','losses', "richness_change_abs")))


response_label<-c(
  evenness_change_abs="Evenness Change",
  rank_change="Rank Change",
  gains = "Species Gains",
  losses="Species Losses",
  richness_change_abs="Richness Change")

ggplot(data=glassD_alldata_box, aes(x=trt_type2, y=abs(mglassd), fill=trt_type2))+
  geom_boxplot()+
  ylab("|Glass's D|")+
  xlab("")+
  scale_x_discrete(limits=c("All GCDs","NonR", 'R', "Mult R", "R+NonR", "CO2","Irrigation","Precip. Vari." ,"Temperature","N","P", "Mult. Nuts."), labels=c("All GCDs", "Non-Res.","Single Res.", "Multiple Res.", "Res.+Non-Res.",   "CO2","Irrigation","Precip. Vari.", "Temp","Nitrogen","Phosphorus", "Mult. Nuts."))+
  scale_fill_manual(values=c("black", "green3",'blue',"snow4",'darkorange', 'orange',  "gray", 'gold3','lightblue',"lightgray","darkgray", "red"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
  geom_vline(xintercept = 5.5, size = 0.5)+
  geom_vline(xintercept = 1.5, size = 0.5)+
  geom_hline(yintercept = 0)+
  geom_point(aes(trt_type2, location), shape=8, size=3)+
  facet_wrap(~response_var2, labeller=labeller(response_var2=response_label), ncol=2, scales="free_y")




# Figure S2 - Relationship between treatment level and community change ---------------
info.trt.levels<-read.csv("converge_diverge/datasets/LongForm/ExperimentInformation_March2019.csv")%>%
  select(site_code, project_name, community_type, treatment,plot_mani, resource_mani, trt_type, n, p, temp, precip, CO2)%>%
  unique()%>%
  filter(plot_mani!=0)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  mutate(use=ifelse(trt_type=="N"|trt_type=="P"|trt_type=="CO2"|trt_type=="irr"|trt_type=="temp", 1, 0))

trt.level<-allyears_all%>%
  select(-use)%>%
  left_join(info.trt.levels)%>%
  filter(use==1)%>%
  mutate(amt=ifelse(trt_type=="N", n, ifelse(trt_type=="P", p, ifelse(trt_type=="CO2", CO2, ifelse(trt_type=="irr", precip, ifelse(trt_type=="temp", temp, 999))))))%>%
  mutate(response_var=factor(response_var, levels=c("richness_change_abs", "evenness_change_abs", "rank_change", "gains", "losses")))


#figure S2 for paper
stats<-trt.level%>%
  group_by(response_var, trt_type)%>%
  summarize(r.value = round(cor.test(mglassd, amt)$estimate, 3),
         p.value = cor.test(mglassd, amt)$p.value)%>%
  mutate(ajdp=p.adjust(p.value, "BH"))

#make new labels for facet_wrap step  
collabel<-c(
  CO2="CO2", 
  irr="Irrigation",
  N="Nitrogen",
  P="Phosphorus",
  temp="Temperature")

rowlabel<-c(
  richness_change_abs="Richness\nchange", 
  evenness_change_abs="Evenness\nchange",
  rank_change="Rank\nchange",
  gains="Gains",
  losses="Losses")


ggplot(data=trt.level, aes(x=amt, y=mglassd))+
  geom_point()+
  facet_grid(response_var~trt_type, scales="free", labeller=labeller(response_var=rowlabel, trt_type=collabel))+
  geom_text(data=stats, aes(x=Inf, y=Inf, label=r.value), hjust=1.05, vjust=1.5)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Amount added")+
  ylab("Mean Glass' D")


  

