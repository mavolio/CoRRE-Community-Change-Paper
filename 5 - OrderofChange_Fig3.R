################################################################################
##  This script tests whether there is an order to community change
##
##  Author: Meghan Avolio (meghan.avolio@jhu.edu)
##  Date: March 20, 2018
##  Update: May 4, 2021
################################################################################
library(tidyverse)
library(gridExtra)

#meghan's working directory
setwd("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\")
theme_set(theme_bw(12))


# read in the data --------------------------------------------------------

change_metrics <- read.csv("CoRRE_RAC_Measures.csv") %>%
  mutate(richness_change_abs = abs(richness_change),
         evenness_change_abs = abs(evenness_change))%>%
  select(-richness_change, -evenness_change)

plot_mani<-read.csv("ExperimentInformation_March2019.csv")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  select(site_project_comm, plot_mani, treatment)%>%
  unique()

subset_studies<-read.csv("Experiment_Trt_Subset.csv")

gam_sig<-read.csv("gam_metrics_sig_change_Dec2020.csv")%>%
  filter(response_var!="richness_change_abs")%>%
  right_join(subset_studies)%>%
  select(site_project_comm, treatment, response_var, sig_diff_cntrl_trt)%>%
  filter(sig_diff_cntrl_trt=="yes")

#most of experiments only see one change.
check<-gam_sig%>%
  group_by(site_project_comm, treatment)%>%
  summarize(n=length(response_var))



# Calculate Glass's D -----------------------------------------------------
#doing Glass' D and max diff
## Control data
change_control <- change_metrics %>%
  left_join(plot_mani)%>%
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
  left_join(plot_mani)%>%
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

## replace Inf with 0 in change_glass_d. I am replacing with 0 because I am ranking it all and this way it wont' rank.
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

# Creating the order of changes -------------------------------------------

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

##dropping what didn't change and then ranking everything 
rank_sig<-max%>%
  mutate(response_var = ifelse(max_metric=="Emax","evenness_change_abs",ifelse(max_metric=="Rmax","rank_change",ifelse(max_metric=="Gmax","gains", "losses"))))%>%
  right_join(gam_sig)%>%
  group_by(site_project_comm, treatment)%>%
  filter(max_metric!="Smax")%>%
  mutate(rank=rank(max_value, ties.method="random"))


#because we rank random in the above step, the results slightly differ each time. The overall findings are always the same - but might get an error on seq # on the last line of this next step.
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
  mutate(ordered=seq(1:50),
         first1=factor(first, levels=c("E", "R", "G", "L")))

# Making figure 3 ---------------------------------------------------------

graph_headings<-rank_table%>%
  group_by(first1)%>%
  summarize(tot=sum(num),
            pct=tot/150)
chisq.test(graph_headings$tot)

parameter<-c(
  E = "Evenness changes occur first in 24%\n of experimental treatments",
  R = "Rank changes occur first in 28%\n of experimental treatments",
  G = "Gains occur first in 25%\n of experimental treatments",
  L = "Losses occur first in 23%\n of experimental treatments"
)

##plotting by what changes first
fig3<-ggplot(data=rank_table, aes(x=reorder(order, ordered), y = num))+
  geom_bar(stat="identity", position = position_dodge(0.9))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("Order of Community Changes")+
  ylab("Number of Experiments")+
  facet_wrap(~first1, scales="free_x", labeller=labeller(first1 = parameter))


ggsave("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Fig3.pdf", fig3, dpi=600, units="mm", width=140, height = 90)


# investigating gains/losses ----------------------------------------------
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

