library(tidyverse)
library(ggthemes)
library(grid)
library(gridExtra)

#meghan's working directory
setwd("C:\\Users\\mavolio2\\Dropbox\\")
setwd("~/Dropbox/")

theme_set(theme_bw(12))

### Read in data 

# sig<-read.csv("C2E/Products/CommunityChange/Summer2018_Results/gam_comparison_table_last_year.csv")%>%
#   select(site_proj_comm, treatment, response_var, sig_diff_cntrl_trt)


change_metrics <- read.csv("C2E/Products/CommunityChange/March2018 WG/MetricsTrts_March2019.csv") %>%
  mutate(abs_richness_change = abs(richness_change),
         abs_evenness_change = abs(evenness_change))

subset<-read.csv("C2E/Products/CommunityChange/March2018 WG/experiment_trt_subset_may2019.csv")%>%
  select(-trt_type2)

### Control data
change_control <- change_metrics %>%
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
  mutate(losses_glass=replace(losses_glass, losses_glass=="Inf"|losses_glass=="NaN", NA))

# read in treatment variables for subsetting later
info.trt<-read.csv("converge_diverge/datasets/LongForm/ExperimentInformation_March2019.csv")%>%
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


info.trt2<-read.csv("C2E\\Products\\CommunityChange\\March2018 WG\\ForAnalysis_allAnalysisAllDatasets_04082019.csv") %>%
  mutate(site_project_comm = paste(site_code, project_name, community_type, sep="_"))%>%
  select(site_project_comm, treatment, trt_type)%>%
  unique()%>%
  mutate(trt_type3=ifelse(trt_type=="drought"|trt_type=="irr"|trt_type=="N"|trt_type=="precip_vari"|trt_type=="P"|trt_type=="CO2"|trt_type=="other_resource", "R", ifelse(trt_type=="mow_clip"|trt_type=="temp"|trt_type=="plant_mani"|trt_type=="other_nonresource"|trt_type=="herb_removal"|trt_type=="NxN", "NonR", ifelse(trt_type=="RxR"|trt_type=="RxRxR", "Mult R", ifelse(trt_type=="threeway"|trt_type=="RxN", "R+NonR", "oops")))))%>%
  select(-trt_type)

### calculate mean change through time and combine with predictor variables
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

#how many replictes of trt type 3
reps<-GlassD%>%
  ungroup()%>%
  select(site_project_comm, treatment, trt_type3)%>%
  unique()%>%
  group_by(trt_type3)%>%
  summarize(n=length(trt_type3))

###doing with all years, not just the last few years.
allyears_all <- GlassD %>%
  group_by(site_project_comm, treatment, response_var, trt_type2, use) %>%
  summarise(mglassd=mean(glassd, na.rm=T))

allyears_all_trt3 <- GlassD %>%
  group_by(site_project_comm, treatment, response_var, trt_type3) %>%
  summarise(mglassd=mean(glassd, na.rm=T))

  # allyears_sigonly <- GlassD %>%
#   filter(sig_diff_cntrl_trt=="yes")%>%
#   group_by(site_project_comm, treatment, response_var, trt_type2, use) %>%
#   summarise(mglassd=mean(glassd, na.rm=T))
# 
# allyears_sigonly_direction<-allyears_sigonly%>%
#   mutate(sign = ifelse(mglassd>0, 1, 0))%>%
#   group_by(response_var, sign)%>%
#   summarise(n=length(sign))

###graphing this ALL YEARS A - all data, B - sig only

##A
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

# glassD_trta<-allyears_all%>%
#   group_by(response_var, trt_type2)%>%
#   summarize(mean=mean(mglassd),
#             sd=sd(mglassd),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   filter(trt_type2=="N"|trt_type2=="Mult. Nuts."|trt_type2=="Irrigation"|trt_type2=="CO2"|trt_type2=="P"|trt_type2=="Temperature")
# 
# glassD_alla<-allyears_all%>%
#   ungroup()%>%
#   group_by(response_var)%>%
#   summarize(mean=mean(mglassd),
#             sd=sd(mglassd),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   mutate(trt_type2="All GCDs")
# 
# glassD_alldata<-glassD_alla%>%
#   bind_rows(glassD_trta)%>%
#   left_join(sig_alla)%>%
#   mutate(location=ifelse(sig==1, mean+0.3,NA))%>%
#   mutate(response_var2=factor(response_var, level=c("richness_change_abs","evenness_change_abs","rank_change",'gains','losses')))
# 
# response_label<-c(
#   evenness_change_abs="Evenness Change",
#   rank_change="Rank Change",
#   gains = "Species Gains",
#   losses="Species Losses", 
#   richness_change_abs="Richness Change")
# 
# ggplot(data=glassD_alldata, aes(x=trt_type2, y=mean, fill=trt_type2))+
#   geom_bar(position=position_dodge(), stat="identity")+
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se),position= position_dodge(0.9), width=0.2)+
#   ylab("Glass's D")+
#   xlab("")+
#   scale_x_discrete(limits=c("All GCDs","CO2","Irrigation","Temperature","N","P","Mult. Nuts."), labels=c("All GCDs", "CO2","Irrigation", "Temp","Nitrogen","Phosphorus","Mult Nuts"))+
#   scale_fill_manual(values=c("black","green3",'blue','darkorange','orange','gold3','red'))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
#   geom_point(aes(trt_type2, location), shape=8, size=3)+
#   geom_vline(xintercept = 1.5, size = 1)+
#   facet_wrap(~response_var2, labeller=labeller(response_var2=response_label), ncol=1, scales="free_y")

# ##B
# 
# sig_allb_sig<-allyears_sigonly%>%
#   group_by(response_var)%>%
#   summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
#   mutate(sig=ifelse(pval<0.05, 1, 0),
#          trt_type2="All GCDs")
# 
# sig_allb_sig_trts<-allyears_sigonly%>%
#   filter(use==1)%>%
#   group_by(response_var, trt_type2)%>%
#   summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
#   mutate(sig=ifelse(pval<0.05, 1, 0))
# 
# sig_allb<-sig_allb_sig%>%
#   bind_rows(sig_allb_sig_trts)
# 
# 
# glassD_trtb<-allyears_sigonly%>%
#   group_by(response_var, trt_type2)%>%
#   summarize(mean=mean(mglassd),
#             sd=sd(mglassd),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   filter(trt_type2=="N"|trt_type2=="Mult. Nuts."|trt_type2=="Irrigation"|trt_type2=="CO2"|trt_type2=="P"|trt_type2=="Temperature")
# 
# glassD_allb<-allyears_sigonly%>%
#   ungroup()%>%
#   group_by(response_var)%>%
#   summarize(mean=mean(mglassd),
#             sd=sd(mglassd),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   mutate(trt_type2="All GCDs")
# 
# glassD_alldatb<-glassD_allb%>%
#   bind_rows(glassD_trtb)%>%
#   left_join(sig_allb)%>%
#   mutate(location=ifelse(sig==1, mean+0.3,NA))%>%
#   mutate(response_var2=factor(response_var, level=c("richness_change_abs","evenness_change_abs","rank_change",'gains','losses')))
# 
# response_label<-c(
#   evenness_change_abs="Evenness Change",
#   rank_change="Rank Change",
#   gains = "Species Gains",
#   losses="Species Losses", 
#   richness_change_abs="Richness Change")
# 
# 
# ggplot(data=glassD_alldatb, aes(x=trt_type2, y=mean, fill=trt_type2))+
#   geom_bar(position=position_dodge(), stat="identity")+
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se),position= position_dodge(0.9), width=0.2)+
#   ylab("Glass's D")+
#   xlab("")+
#   scale_x_discrete(limits=c("All GCDs","CO2","Irrigation","Temperature","N","P","Mult. Nuts."), labels=c("All GCDs", "CO2","Irrigation", "Temp","Nitrogen","Phosphorus","Mult Nuts"))+
#   scale_fill_manual(values=c("black","green3",'blue','darkorange','orange','gold3','red'))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
#   geom_vline(xintercept = 1.5, size = 1)+
#   geom_point(aes(trt_type2, location), shape=8, size=3)+
#   facet_wrap(~response_var2, labeller=labeller(response_var2=response_label), ncol=2, scales="free_y")
# sig_allb_sig<-allyears_sigonly%>%
#   group_by(response_var)%>%
#   summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
#   mutate(sig=ifelse(pval<0.05, 1, 0),
#          trt_type2="All GCDs")

###doing as a boxplot NOW using all the data

# glassD_trtb_box<-allyears_sigonly%>%
#    filter(use==1)
#
# glassD_allb_box<-allyears_sigonly%>%
#   ungroup()%>%
#     mutate(trt_type2="All GCDs")
#
# glassD_alldatb_box<-glassD_allb_box%>%
#   bind_rows(glassD_trtb_box)%>%
#   left_join(sig_allb)%>%
#   mutate(location=ifelse(sig==1&response_var=="richness_change_abs",3.5,ifelse(sig==1&response_var=="evenness_change_abs", 6.5, ifelse(sig==1&response_var=="rank_change", 3, ifelse(sig==1&response_var=="losses", 2.5, NA)))))%>%
#   mutate(response_var2=factor(response_var, level=c("richness_change_abs","evenness_change_abs","rank_change",'gains','losses')))
#
# ggplot(data=glassD_alldatb_box, aes(x=trt_type2, y=mglassd, fill=trt_type2))+
#   geom_boxplot()+
#   ylab("Glass's D")+
#   xlab("")+
#   scale_x_discrete(limits=c("All GCDs","CO2","Irrigation","Precip. Vari." ,"Temperature","N","P", "Mult. Nuts."), labels=c("All GCDs", "CO2","Irrigation","Precip. Vari.", "Temp","Nitrogen","Phosphorus", "Mult. Nuts."))+
#   scale_fill_manual(values=c("black","green3",'blue','darkorange','orange', 'gold3','lightblue','red'))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
#   geom_vline(xintercept = 1.5, size = 1)+
#   geom_hline(yintercept = 0)+
#   geom_point(aes(trt_type2, location), shape=8, size=3)+
#   facet_wrap(~response_var2, labeller=labeller(response_var2=response_label), ncol=1, scales="free_y")

###using all the data
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

ggplot(data=glassD_alldata, aes(x=trt_type2, y=mglassd, fill=trt_type2))+
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

#for esa
ggplot(data=subset(glassD_alldata, trt_type2=="R"|trt_type2=="Mult R"|trt_type2=="NonR"|trt_type2=="R+NonR"), aes(x=trt_type2, y=mglassd, fill=trt_type2))+
  geom_boxplot()+
  ylab("Glass's D")+
  xlab("")+
  scale_x_discrete(limits=c("NonR", 'R', "Mult R", "R+NonR"), labels=c("Non-Res.","Single Res.", "Multiple Res.", "Res.+Non-Res."))+
  scale_fill_manual(values=c("red", "gray", "orange", "gold"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
  geom_hline(yintercept = 0)+
  geom_point(aes(trt_type2, location), shape=8, size=3)+
  facet_wrap(~response_var2, labeller=labeller(response_var2=response_label), ncol=3, scales="free_y")

ggplot(data=subset(glassD_alldata, trt_type2!="R"&trt_type2!="Mult R"&trt_type2!="NonR"&trt_type2!="R+NonR"), aes(x=trt_type2, y=mglassd, fill=trt_type2))+
  geom_boxplot()+
  ylab("Glass's D")+
  xlab("")+
  scale_x_discrete(limits=c("All GCDs", "CO2","Irrigation","Precip. Vari." ,"Temperature","N","P", "Mult. Nuts."), labels=c("All GCDs",  "CO2","Irrigation","Precip. Vari.", "Temp","Nitrogen","Phosphorus", "Mult. Nuts."))+
  scale_fill_manual(values=c("black", "green3",'blue','darkorange', 'orange', 'gold3','lightblue', "red"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
  geom_vline(xintercept = 1.5, size = 0.5)+
  geom_hline(yintercept = 0)+
  geom_point(aes(trt_type2, location), shape=8, size=3)+
  facet_wrap(~response_var2, labeller=labeller(response_var2=response_label), ncol=3, scales="free_y")


##how often are rank changes greater/lesser than contorls
nrow(subset(glassD_alldata_box, response_var2=="rank_change"&mglassd>0))
nrow(subset(glassD_alldata_box, response_var2=="rank_change"&mglassd<0))
##how oftern are gains greater/less than contorls
nrow(subset(glassD_alldata_box, response_var2=="gains"&mglassd>0))
nrow(subset(glassD_alldata_box, response_var2=="gains"&mglassd<0))


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


# WE DECIDED TO USE ALL THE DATA SINCE THERE WERE NO BIG DIFFERENCES AMONG THE DIFFERNT YEAR SUBSETS. 
# ###doing with 5th year only
# five_all <- GlassD %>%
#   filter(treatment_year2==5)%>%
#   group_by(site_project_comm, treatment, response_var, trt_type2, use) %>%
#   summarise(mglassd=mean(glassd, na.rm=T))
# 
# five_sigonly <- GlassD %>%
#   filter(sig_diff_cntrl_trt=="yes")%>%
#   filter(treatment_year2==5)%>%
#   group_by(site_project_comm, treatment, response_var, trt_type2, use) %>%
#   summarise(mglassd=mean(glassd, na.rm=T))
# 
# ###graphing this 5th year only A - all data, B - sig only
# 
# ##A
# sig_fivea_overall<-five_all%>%
#   group_by(response_var)%>%
#   summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
#   mutate(sig=ifelse(pval<0.05, 1, 0),
#          trt_type2="All GCDs")
# 
# sig_fivea_trts<-five_all%>%
#   filter(use==1, trt_type2!="Irr + Temp")%>%
#   group_by(response_var, trt_type2)%>%
#   summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
#   mutate(sig=ifelse(pval<0.05, 1, 0))
# 
# sig_fivea<-sig_fivea_overall%>%
#   bind_rows(sig_fivea_trts)
# 
# glassD_trta<-five_all%>%
#   group_by(response_var, trt_type2)%>%
#   summarize(mean=mean(mglassd),
#             sd=sd(mglassd),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   filter(trt_type2=="N"|trt_type2=="Mult. Nuts."|trt_type2=="Irrigation"|trt_type2=="CO2"|trt_type2=="P"|trt_type2=="Temperature")
# 
# glassD_fivea<-five_all%>%
#   ungroup()%>%
#   group_by(response_var)%>%
#   summarize(mean=mean(mglassd),
#             sd=sd(mglassd),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   mutate(trt_type2="All GCDs")
# 
# glassD_fivedata<-glassD_fivea%>%
#   bind_rows(glassD_trta)%>%
#   left_join(sig_fivea)%>%
#   mutate(location=ifelse(sig==1, mean+0.3,NA))
# 
# a<-ggplot(data=glassD_fivedata, aes(x=trt_type2, y=mean, fill=trt_type2))+
#   geom_bar(position=position_dodge(), stat="identity")+
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se),position= position_dodge(0.9), width=0.2)+
#   ylab("Glass's D")+
#   xlab("")+
#   scale_x_discrete(limits=c("All GCDs","CO2","Irrigation","Temperature","N","P","Mult. Nuts."), labels=c("All GCDs", "CO2","Irrigation", "Temp","Nitrogen","Phosphorus","Mult Nuts"))+
#   scale_fill_manual(values=c("black","green3",'blue','darkorange','orange','gold3','red'))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
#   geom_point(aes(trt_type2, location), shape=8, size=3)+
#   geom_vline(xintercept = 1.5, size = 1)+
#   facet_wrap(~response_var, ncol=1, scales="free_y")+
#   ggtitle("All Data - 5")
# 
# ##B
# 
# sig_fiveb_overall<-five_sigonly%>%
#   group_by(response_var)%>%
#   summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
#   mutate(sig=ifelse(pval<0.05, 1, 0),
#          trt_type2="All GCDs")
# 
# sig_fiveb_trts<-five_sigonly%>%
#   filter(use==1, trt_type2!="Irr + Temp")%>%
#   group_by(response_var, trt_type2)%>%
#   summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
#   mutate(sig=ifelse(pval<0.05, 1, 0))
# 
# sig_fiveb<-sig_fiveb_overall%>%
#   bind_rows(sig_fiveb_trts)
# 
# 
# glassD_trtb<-five_sigonly%>%
#   group_by(response_var, trt_type2)%>%
#   summarize(mean=mean(mglassd),
#             sd=sd(mglassd),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   filter(trt_type2=="N"|trt_type2=="Mult. Nuts."|trt_type2=="Irrigation"|trt_type2=="CO2"|trt_type2=="P"|trt_type2=="Temperature")
# 
# glassD_fiveb<-five_sigonly%>%
#   ungroup()%>%
#   group_by(response_var)%>%
#   summarize(mean=mean(mglassd),
#             sd=sd(mglassd),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   mutate(trt_type2="All GCDs")
# 
# glassD_fivedatb<-glassD_fiveb%>%
#   bind_rows(glassD_trtb)%>%
#   left_join(sig_fiveb)%>%
#   mutate(location=ifelse(sig==1, mean+0.3,NA))
# 
# b<-ggplot(data=glassD_alldatb, aes(x=trt_type2, y=mean, fill=trt_type2))+
#   geom_bar(position=position_dodge(), stat="identity")+
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se),position= position_dodge(0.9), width=0.2)+
#   ylab("Glass's D")+
#   xlab("")+
#   scale_x_discrete(limits=c("All GCDs","CO2","Irrigation","Temperature","N","P","Mult. Nuts."), labels=c("All GCDs", "CO2","Irrigation", "Temp","Nitrogen","Phosphorus","Mult Nuts"))+
#   scale_fill_manual(values=c("black","green3",'blue','darkorange','orange','gold3','red'))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
#   geom_vline(xintercept = 1.5, size = 1)+
#   geom_point(aes(trt_type2, location), shape=8, size=3)+
#   facet_wrap(~response_var, ncol=1, scales="free_y")+
#   ggtitle("Sig Only - 5")
# 
# grid.arrange(a, b, ncol=2)
# 
# 
# ###doing with last year only
# last_all <- GlassD %>%
#   group_by(site_project_comm, treatment)%>%
#   mutate(lstyr=max(treatment_year2))%>%
#   filter(treatment_year2==lstyr)%>%
#   group_by(site_project_comm, treatment, response_var, trt_type2, use) %>%
#   summarise(mglassd=mean(glassd, na.rm=T))
# 
# last_sigonly <- GlassD %>%
#   filter(sig_diff_cntrl_trt=="yes")%>%
#   group_by(site_project_comm, treatment)%>%
#   mutate(lstyr=max(treatment_year2))%>%
#   filter(treatment_year2==lstyr)%>%
#   group_by(site_project_comm, treatment, response_var, trt_type2, use) %>%
#   summarise(mglassd=mean(glassd, na.rm=T))
# 
# ###graphing this last year only A - all data, B - sig only
# 
# ##A
# sig_lasta_overall<-last_all%>%
#   group_by(response_var)%>%
#   summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
#   mutate(sig=ifelse(pval<0.05, 1, 0),
#          trt_type2="All GCDs")
# 
# sig_lasta_trts<-last_all%>%
#   filter(use==1, trt_type2!="Irr + Temp")%>%
#   group_by(response_var, trt_type2)%>%
#   summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
#   mutate(sig=ifelse(pval<0.05, 1, 0))
# 
# sig_lasta<-sig_lasta_overall%>%
#   bind_rows(sig_lasta_trts)
# 
# glassD_trta<-last_all%>%
#   group_by(response_var, trt_type2)%>%
#   summarize(mean=mean(mglassd, na.rm=T),
#             sd=sd(mglassd, na.rm=T),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   filter(trt_type2=="N"|trt_type2=="Mult. Nuts."|trt_type2=="Irrigation"|trt_type2=="CO2"|trt_type2=="P"|trt_type2=="Temperature")
# 
# glassD_lasta<-last_all%>%
#   ungroup()%>%
#   group_by(response_var)%>%
#   summarize(mean=mean(mglassd, na.rm=T),
#             sd=sd(mglassd, na.rm=T),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   mutate(trt_type2="All GCDs")
# 
# glassD_lastdata<-glassD_lasta%>%
#   bind_rows(glassD_trta)%>%
#   left_join(sig_lasta)%>%
#   mutate(location=ifelse(sig==1, mean+0.3,NA))
# 
# a<-ggplot(data=glassD_lastdata, aes(x=trt_type2, y=mean, fill=trt_type2))+
#   geom_bar(position=position_dodge(), stat="identity")+
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se),position= position_dodge(0.9), width=0.2)+
#   ylab("Glass's D")+
#   xlab("")+
#   scale_x_discrete(limits=c("All GCDs","CO2","Irrigation","Temperature","N","P","Mult. Nuts."), labels=c("All GCDs", "CO2","Irrigation", "Temp","Nitrogen","Phosphorus","Mult Nuts"))+
#   scale_fill_manual(values=c("black","green3",'blue','darkorange','orange','gold3','red'))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
#   geom_point(aes(trt_type2, location), shape=8, size=3)+
#   geom_vline(xintercept = 1.5, size = 1)+
#   facet_wrap(~response_var, ncol=1, scales="free_y")+
#   ggtitle("All Data - Last")
# 
# ##B
# 
# sig_lastb_sig<-last_sigonly%>%
#   group_by(response_var)%>%
#   summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
#   mutate(sig=ifelse(pval<0.05, 1, 0),
#          trt_type2="All GCDs")
# 
# sig_lastb_sig_trts<-last_sigonly%>%
#   filter(use==1, trt_type2!="Irr + Temp")%>%
#   group_by(response_var, trt_type2)%>%
#   summarize(pval=t.test(mglassd, mu=0)$p.value)%>%
#   mutate(sig=ifelse(pval<0.05, 1, 0))
# 
# sig_lastb<-sig_lastb_sig%>%
#   bind_rows(sig_lastb_sig_trts)
# 
# 
# glassD_trtb<-last_sigonly%>%
#   group_by(response_var, trt_type2)%>%
#   summarize(mean=mean(mglassd, na.rm=T),
#             sd=sd(mglassd, na.rm=T),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   filter(trt_type2=="N"|trt_type2=="Mult. Nuts."|trt_type2=="Irrigation"|trt_type2=="CO2"|trt_type2=="P"|trt_type2=="Temperature")
# 
# glassD_lastb<-last_sigonly%>%
#   ungroup()%>%
#   group_by(response_var)%>%
#   summarize(mean=mean(mglassd, na.rm=T),
#             sd=sd(mglassd, na.rm=T),
#             num=length(mglassd))%>%
#   mutate(se=sd/sqrt(num))%>%
#   mutate(trt_type2="All GCDs")
# 
# glassD_lastdatb<-glassD_lastb%>%
#   bind_rows(glassD_trtb)%>%
#   left_join(sig_lastb)%>%
#   mutate(location=ifelse(sig==1, mean+0.3,NA))
# 
# b<-ggplot(data=glassD_lastdatb, aes(x=trt_type2, y=mean, fill=trt_type2))+
#   geom_bar(position=position_dodge(), stat="identity")+
#   geom_errorbar(aes(ymin=mean-se, ymax=mean+se),position= position_dodge(0.9), width=0.2)+
#   ylab("Glass's D")+
#   xlab("")+
#   scale_x_discrete(limits=c("All GCDs","CO2","Irrigation","Temperature","N","P","Mult. Nuts."), labels=c("All GCDs", "CO2","Irrigation", "Temp","Nitrogen","Phosphorus","Mult Nuts"))+
#   scale_fill_manual(values=c("black","green3",'blue','darkorange','orange','gold3','red'))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none")+
#   geom_vline(xintercept = 1.5, size = 1)+
#   geom_point(aes(trt_type2, location), shape=8, size=3)+
#   facet_wrap(~response_var, ncol=1, scales="free_y")+
#   ggtitle("Sig Only - 5")
# 
# grid.arrange(a, b, ncol=2)