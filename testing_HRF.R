#meghan's working directory
setwd("C:\\Users\\mavolio2\\Dropbox\\")
setwd("~/Dropbox/")
library(tidyverse)
library(gridExtra)

theme_set(theme_bw(12))

change_metrics <- read.csv("C2E/Products/CommunityChange/March2018 WG/MetricsTrts_March2019.csv") %>%
  mutate(richness_change_abs = abs(richness_change),
         evenness_change_abs = abs(evenness_change))%>%
  select(-X, -richness_change, -evenness_change, -composition_change, -site_code, -project_name, -community_type, -trt_type, -use, -composition_change, -dispersion_change)

subset_studies<-read.csv("C2E/Products//CommunityChange//March2018 WG//experiment_trt_subset_May2019.csv")

subset_studies2<-subset_studies%>%
  select(site_project_comm)%>%
  unique()

#not doing this.
# sig_com<-read.csv('C2E\\Products\\CommunityChange\\Summer2018_Results\\gam_com_sig_change.csv')%>%
#   mutate(site_project_comm = site_proj_comm)

gam_sig<-read.csv("C2E/Products/CommunityChange/Summer2018_Results/gam_metrics_sig_change_Dec2020.csv")%>%
  filter(response_var!="richness_change_abs")%>%
  right_join(subset_studies)%>%
  select(site_project_comm, treatment, response_var, sig_diff_cntrl_trt)%>%
  filter(sig_diff_cntrl_trt=="yes")

#check number of sig results
exp<-gam_sig%>%
  select(site_project_comm, treatment)%>%
  unique()

#most of experiments only see one change.
check<-gam_sig%>%
  group_by(site_project_comm, treatment)%>%
  summarize(n=length(response_var))

#doing Glass' D and max diff. kEEPING THIS APPROACH
## Control data
change_control <- change_metrics %>%
  filter(plot_mani==0) %>%
  select(-treatment, -plot_mani)%>%
  rename(richness_change_abs_ctrl = richness_change_abs,
         evenness_change_abs_ctrl = evenness_change_abs,
         rank_change_ctrl = rank_change,
         gains_ctrl = gains,
         losses_ctrl = losses) %>%
  group_by(site_project_comm, treatment_year2) %>%
  summarise_at(vars(c(rank_change_ctrl:losses_ctrl, richness_change_abs_ctrl, evenness_change_abs_ctrl)), list(mean=mean, sd=sd), na.rm=T)

change_glass_d <- change_metrics %>%
  filter(plot_mani != 0) %>%
  group_by(site_project_comm, treatment, treatment_year2, plot_mani) %>%
  summarise(richness_change_abs = mean(richness_change_abs,na.rm=T),
            evenness_change_abs = mean(evenness_change_abs, na.rm=T),
            rank_change = mean(rank_change, na.rm=T),
            gains = mean(gains, na.rm=T),
            losses = mean(losses, na.rm=T)) %>%
  left_join(change_control, by=c("site_project_comm","treatment_year2")) %>%
  mutate(abs_richness_glass = (richness_change_abs-richness_change_abs_ctrl_mean)/richness_change_abs_ctrl_sd,
         abs_evenness_glass = (evenness_change_abs-evenness_change_abs_ctrl_mean)/evenness_change_abs_ctrl_sd,
         rank_glass = (rank_change-rank_change_ctrl_mean)/rank_change_ctrl_sd,
         gains_glass = (gains-gains_ctrl_mean)/gains_ctrl_sd,
         losses_glass = (losses-losses_ctrl_mean)/losses_ctrl_sd
  ) %>%
  select(site_project_comm:plot_mani, abs_richness_glass:losses_glass) %>%
  ungroup()

#change_glass_d is the thing that we want

## replace Inf with 0 in change_glass_d. I am replaceing with 0 because I am ranking it all and this way it wont' rank.
change_glass_d <- change_glass_d %>%
  mutate(abs_richness_glass=replace(abs_richness_glass, abs_richness_glass=="Inf", NA)) %>%
  mutate(rank_glass=replace(rank_glass, rank_glass=="Inf", 0)) %>%
  mutate(gains_glass=replace(gains_glass, gains_glass=="Inf", 0)) %>%
  mutate(losses_glass=replace(losses_glass, losses_glass=="Inf", 0))%>%
  mutate(abs_evenness_glass=replace(abs_evenness_glass, abs_evenness_glass=="NaN", 0))%>%
  mutate(rank_glass=replace(rank_glass, rank_glass=="NaN", 0)) %>%
  mutate(gains_glass=replace(gains_glass, gains_glass=="NaN", 0))%>%
  mutate(losses_glass=replace(losses_glass, losses_glass=="NaN", 0))%>%
  right_join(subset_studies)

##getting max glass D for each metric
max <- change_glass_d%>%
  group_by(site_project_comm, treatment)%>%
  mutate(S = max(abs_richness_glass),
         E = max(abs_evenness_glass),
         R = max(rank_glass),
         G = max(gains_glass),
         L = max(losses_glass),
         Smax = ifelse(S == abs_richness_glass, treatment_year2, 0),
         Emax = ifelse(E == abs_evenness_glass, treatment_year2, 0),
         Rmax = ifelse(R == rank_glass, treatment_year2, 0),
         Gmax = ifelse(G == gains_glass, treatment_year2, 0),
         Lmax = ifelse(L == losses_glass, treatment_year2, 0))%>%
  gather(max_metric, max_value, Smax:Lmax)%>%
  select(site_project_comm, treatment, max_metric, max_value)%>%
  filter(max_value!=0)

# ##ranking everything and then seeing when changed - not doing it this way
# rank<-max%>%
#   group_by(site_project_comm, treatment)%>%
#   filter(max_metric!="Smax")%>%
#   mutate(rank=rank(max_value, ties.method="average"))%>%
#   mutate(response_var = ifelse(max_metric=="Emax","evenness_change_abs",ifelse(max_metric=="Rmax","rank_change",ifelse(max_metric=="Gmax","gains", "losses"))))%>%
#   right_join(metrics_sig)
#
# rank_mean<-rank%>%
#   group_by(response_var)%>%
#   summarize(mrank=mean(rank), sdrank=sd(rank), n=length(response_var))%>%
#   mutate(se = sdrank/sqrt(n))

##dropping what didn't change and then ranking everything - I think this is the way to do it.
rank_sig<-max%>%
  mutate(response_var = ifelse(max_metric=="Emax","evenness_change_abs",ifelse(max_metric=="Rmax","rank_change",ifelse(max_metric=="Gmax","gains", "losses"))))%>%
  right_join(gam_sig)%>%
  group_by(site_project_comm, treatment)%>%
  filter(max_metric!="Smax")%>%
  mutate(rank=rank(max_value, ties.method="random"))

rank_table<-rank_sig%>%
  group_by(site_project_comm, treatment)%>%
  arrange(rank, .by_group=T)%>%
  mutate(metric=ifelse(max_metric=="Emax","E", ifelse(max_metric=="Gmax","G", ifelse(max_metric=="Rmax","R", ifelse(max_metric=="Lmax","L","X")))),
         rank1=ifelse(rank==1, "a", ifelse(rank==2, "b", ifelse(rank==3, "c","d"))))%>%
  select(site_project_comm, rank1, metric, treatment)%>%
  spread(rank1, metric, fill="")%>%
  mutate(order=paste(a, b, c, d, sep=""))%>%
  ungroup()%>%
  group_by(order)%>%
  summarize(num=length(order))%>%
  mutate(first=substring(order,1,1))%>%
  arrange(first, -num)%>%
  mutate(ordered=seq(1:49),
         first1=factor(first, levels=c("E", "R", "G", "L")))#%>% dropping code to order by number of community changes
  # mutate(numchang=nchar(order))%>%
  # ungroup() %>%
  # group_by(numchang)%>%
  # arrange(-num, .by_group=T)%>%
  # mutate(ordered2=seq(1:length(num)))

# rank_sig_mean<-rank_sig%>%
#   group_by(response_var)%>%
#   summarize(mrank=mean(rank), sdrank=sd(rank), n=length(response_var))%>%
#   mutate(se = sdrank/sqrt(n))

graph_headings<-rank_table%>%
  group_by(first1)%>%
  summarize(tot=sum(num),
            pct=tot/150)
chisq.test(graph_headings$tot)

parameter<-c(
  E = "Evenness changes occur first in 23%\n of experimental treatments",
  R = "Rank changes occur first in 29%\n of experimental treatments",
  G = "Gains occur first in 23%\n of experimental treatments",
  L = "Losses occur first in 24%\n of experimental treatments"
)

##plotting by what changes first
ggplot(data=rank_table, aes(x=reorder(order, ordered), y = num))+
  geom_bar(stat="identity", position = position_dodge(0.9))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Order of Community Changes")+
  ylab("Number of Experiments")+
  facet_wrap(~first1, scales="free_x", labeller=labeller(first1 = parameter))#+
  #geom_text(data=graph_headings, mapping=aes(x=Inf, y = Inf, label = tot), size= 6, hjust=1.5, vjust=1.5)

# #plotting by number of changes
# ggplot(data=rank_table, aes(x=reorder(order, ordered2), y = num))+
#   geom_bar(stat="identity", position = position_dodge(0.9))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   xlab("Order of Community Changes")+
#   ylab("Number of Experiments")+
#   facet_wrap(~numchang, scales="free_x")


#investigate gains and losses
gainloss<-rank_sig%>%
  filter(response_var=="gains"|response_var=="losses")%>%
  group_by(site_project_comm, treatment)%>%
  arrange(rank, .by_group=T)%>%
  mutate(metric=ifelse(max_metric=="Gmax","G", ifelse(max_metric=="Lmax","L","X")),
         rank1=ifelse(rank==1, "a", ifelse(rank==2, "b", ifelse(rank==3, "c","d"))))%>%
  select(site_project_comm, rank1, metric, treatment)%>%
  spread(rank1, metric, fill="")%>%
  mutate(order=paste(a, b, c, d, sep=""))%>%
  ungroup()%>%
  group_by(order)%>%
  summarise(num=length(order))
  

# ggplot(data=rank_mean, aes(x=response_var, y = mrank))+
#   geom_bar(stat="identity", position=position_dodge(0.9))+
#   geom_errorbar(aes(ymin=mrank-se, ymax=mrank+se), position=position_dodge(0.9), width=0.2)+
#   scale_x_discrete(limits=c("evenness_change_abs", "rank_change", "gains", "losses"), labels=c("Evenness","Rank","Gains","Losses"))+
#   ylab("Average Rank")+
#   xlab("Communtiy Change Metric")
# summary(aov(rank~response_var, data=rank_sig))
# ggplot(data=rank_sig_mean, aes(x=response_var, y = mrank))+
#   geom_bar(stat="identity", position=position_dodge(0.9))+
#   geom_errorbar(aes(ymin=mrank-se, ymax=mrank+se), position=position_dodge(0.9), width=0.2)+
#   scale_x_discrete(limits=c("evenness_change_abs", "rank_change", "gains", "losses"), labels=c("Evenness","Rank","Gains","Losses"))+
#   ylab("Average Rank")+
#   xlab("Communtiy Change Metric")


##THS IS MAKING THE OLD PLOT WITH GLASS'S D
##need to only select those that saw sig change.
# ggplot(data = subset(max), aes(x = max_metric, y = max_value))+
#   geom_jitter()+
#   geom_boxplot(alpha=.1) +
#   xlab("Change Metric") +
#   ylab("Treatment Year") +
#   theme(axis.text=element_text(size=12, color="black"), strip.text.x=element_text(size=12)) +
#   scale_x_discrete(limits=c("Smax","Emax","Rmax","Gmax","Lmax"),labels=c("Rich","Even","Rank","Gain","Loss"))+
#   coord_flip()

# ###testing for first year sig diff trt-controls. This is the approach we are going to use. What is the first year there is sig diff between treatments and controls.
# ####NO LONGER DOING THIS. TOO MANY T-TEST AND TOO MANY MISMATCHES BETWEEN TTEST RESULTS AND GAM ANALYSIS.
# change_test_c<-change_metrics%>%
#   filter(plot_mani==0)%>%
#   right_join(subset_studies2)
# 
# change_test<-change_metrics%>%
#   mutate(spc_yr_trt=paste(site_project_comm, treatment_year, treatment, sep="::"))%>%
#   select(-trt_type2)%>%
#   right_join(subset_studies)
# 
# spc_yr_trt_vec<-unique(change_test$spc_yr_trt)
# 
# ttests<-data.frame()
# 
# for (i in 1:length(spc_yr_trt_vec)){
#   subset<-change_test%>%
#     filter(spc_yr_trt==spc_yr_trt_vec[i])
#   
#   spc<-unique(subset$site_project_comm)
#   yr<-unique(subset$treatment_year)
#   
#   control_sub<-change_test_c%>%
#     filter(site_project_comm==spc,
#            treatment_year==yr)
#   
#   r_change<-t.test(subset$rank_change, control_sub$rank_change)$p.value
#   e_change<-ifelse(anyNA(subset$evenness_change_abs), NA, t.test(subset$evenness_change, control_sub$evenness_change)$p.value)
#   gains<-t.test(subset$gains, control_sub$gains)$p.value
#   losses<-t.test(subset$losses, control_sub$losses)$p.value
#   
#   ttest_temp <- data.frame(
#     site_project_comm = spc,
#     treatment = unique(subset$treatment),
#     treatment_year = unique(subset$treatment_year),
#     even_pval =  e_change,
#     rank_pval =  r_change,
#     gain_pval =  gains,
#     loss_pval = losses)
#   
#   ttests<-rbind(ttests, ttest_temp)
#   
# }
# 
# ttests2<-ttests%>%
#   mutate(evenness_change_abs=ifelse(even_pval<0.05, 1, 0),
#          rank_change=ifelse(rank_pval<0.05, 1, 0),
#          gains=ifelse(rank_pval<0.05, 1, 0),
#          losses=ifelse(loss_pval<0.05, 1, 0))%>%
#   select(-even_pval, -rank_pval, -gain_pval, -loss_pval)%>%
#   gather(response_var, sig, evenness_change_abs:losses)%>%
# #  filter(sig!=0)%>%
#   right_join(metrics_sig)
# 
# firstyr<-ttests2%>%
#   filter(sig!=0)%>%
#   group_by(site_project_comm, treatment, response_var)%>%
#   summarize(minyr=min(treatment_year))%>%
#   mutate(rank=rank(minyr, ties.method="random"))
# 
# rank_table2<-firstyr%>%
#   group_by(site_project_comm, treatment)%>%
#   arrange(rank, .by_group=T)%>%
#   mutate(metric=ifelse(response_var=="evenness_change_abs","E", ifelse(response_var=="gains","G", ifelse(response_var=="rank_change","R", ifelse(response_var=="losses","L","X")))),
#          rank1=ifelse(rank==1, "a", ifelse(rank==2, "b", ifelse(rank==3, "c","d"))))%>%
#   select(site_project_comm, rank1, metric, treatment)%>%
#   spread(rank1, metric, fill="")%>%
#   mutate(order=paste(a, b, c, d, sep=""))%>%
#   ungroup()%>%
#   group_by(order)%>%
#   summarize(num=length(order))%>%
#   mutate(first=substring(order,1,1))%>%
#   arrange(first, -num)%>%
#   mutate(ordered=seq(1:33))%>%
#   mutate(numchang=nchar(order))%>%
#   ungroup() %>% 
#   group_by(numchang)%>%
#   arrange(-num, .by_group=T)%>%
#   mutate(ordered2=seq(1:length(num)))
# 
# 
# ggplot(data=rank_table2, aes(x=reorder(order, ordered), y = num))+
#   geom_bar(stat="identity", position = position_dodge(0.9))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   xlab("Order of Community Changes")+
#   ylab("Number of Experiments")+
#   facet_wrap(~first, scales="free_x")
# 
# ggplot(data=rank_table2, aes(x=reorder(order, ordered2), y = num))+
#   geom_bar(stat="identity", position = position_dodge(0.9))+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   xlab("Order of Community Changes")+
#   ylab("Number of Experiments")+
#   facet_wrap(~numchang, scales="free_x")

####redoing this. first by comparing each year to the first year.
plotinfo<-change_metrics%>%
  select(site_project_comm, plot_id, treatment, plot_mani)%>%
  unique()

refyear<-read.csv("C2E/Products/CommunityChange/March2018 WG/CORRE_RAC_Refyear_Metrics_May2020.csv")%>%
  left_join(plotinfo)

refyear_trt<-refyear%>%
  right_join(subset_studies)

subset_studies2<-subset_studies%>%
  select(site_project_comm)%>%
  unique()

refyear_c<-refyear%>%
  filter(plot_mani==0)%>%
  right_join(subset_studies2)

ggplot(data=refyear_trt, aes(x=treatment_year2, y=evenness_change, color=trt_type2, group=treatment))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  facet_wrap(~site_project_comm, scales = "free")

ggplot(data=refyear_c, aes(x=treatment_year2, y=evenness_change))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  facet_wrap(~site_project_comm, scales = "free")
