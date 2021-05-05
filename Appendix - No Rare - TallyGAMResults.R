################################################################################
##  GamOutputAnalysis.R This code analyzes the spreadsheet output by Andrew do see when communities changed.
##
##  Author: Meghan Avolio
##  Date: 2019
##  Updated: May 4, 2021
################################################################################


library(tidyverse)
library(gridExtra)
theme_set(theme_bw(12))

#meghan wd
setwd("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\")

#read in and drop sucessional treatments (those that had a big disturbance to start)
gam<-read.csv("GamSigTable_norare.csv")%>%
  rename(site_project_comm=site_proj_comm)%>%
  separate(site_project_comm, into=c("site_code","project_name","community_type"), sep="_", remove=F)

#filter the press treatments for the experiments with enough data
trt_touse<-read.csv("ExperimentInformation_March2019.csv")%>%
  filter(pulse==0, plot_mani!=0)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_")) %>% 
  select(site_project_comm, treatment, trt_type)%>%
  unique()%>%
  mutate(use=ifelse(trt_type=="N"|trt_type=="P"|trt_type=="CO2"|trt_type=="irr"|trt_type=="temp"|trt_type=="N*P"|trt_type=="mult_nutrient"|trt_type=='precip_vari', 1, 0))

info.trt2<-read.csv("SiteExperimentDetails_March2019.csv") %>%
  mutate(site_project_comm = paste(site_code, project_name, community_type, sep="_"))%>%
  select(site_project_comm, treatment, trt_type)%>%
  unique()%>%
  mutate(trt_type3=ifelse(trt_type=="drought"|trt_type=="irr"|trt_type=="N"|trt_type=="precip_vari"|trt_type=="P"|trt_type=="CO2"|trt_type=="other_resource", "Res.", ifelse(trt_type=="mow_clip"|trt_type=="temp"|trt_type=="plant_mani"|trt_type=="other_nonresource"|trt_type=="herb_removal"|trt_type=="NxN", "Non-Res.", ifelse(trt_type=="RxR"|trt_type=="RxRxR", "Mult. Res.", ifelse(trt_type=="threeway"|trt_type=="RxN", "Res.+Non-Res.", "oops")))))%>%
  select(-trt_type)

gam_touse<-gam%>%
 inner_join(trt_touse)%>%
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
        ifelse(trt_type=="N*irr*CO2", "mult_res", 999))))))))))))))))))))))))%>%
  left_join(info.trt2)%>%
  filter(response_var != "composition_change")

###adjusting for multiple comparisons
spc<-unique(gam_touse$site_project_comm)

adjp<-data.frame()

#looping through and correcting p-values

for (i in 1:length(spc)){
  
  subset<-gam_touse%>%
    filter(site_project_comm==spc[i])
  
  out<-subset%>%
    mutate(adjp=p.adjust(p_value, method="BH", n=length(site_project_comm)))
  
  adjp<-rbind(adjp, out)
  
}

adjp_sig<-adjp%>%
  rename(oldsig=sig_diff_cntrl_trt)%>%
  mutate(sig_diff_cntrl_trt=ifelse(is.na(adjp)|adjp>0.05, "no", ifelse(adjp<0.05, "yes", 999)))

#Regradless of whether the community changed, how much to the metrics change?
metrics_sig<-adjp_sig

metrics_sig_tally <- metrics_sig %>%
  group_by(response_var) %>%
  summarise(
    num_sig = length(which(sig_diff_cntrl_trt == "yes")),
    num_nonsig = length(which(sig_diff_cntrl_trt == "no"))
  )

#How many communities saw 1 aspect of change?
metrics_all<-metrics_sig%>%
  filter(sig_diff_cntrl_trt=="yes")%>%
  select(site_project_comm, treatment, sig_diff_cntrl_trt)%>%
  unique()

gam_trt<-gam_touse%>%
  select(site_project_comm, treatment, trt_type2)%>%
  unique()

1-(154/209)

#overall diff in metrics of change - NO
prop.test(x=as.matrix(metrics_sig_tally[c('num_sig', 'num_nonsig')]), alternative='two.sided')

vector_overall<-data.frame("response_var"=c("all","all"), sig=c("psig", "pnsig"), value = c(0.736, 0.263), response_var2=c("Any Change", "Any Change")) 

sig_tograph<-metrics_sig_tally%>%
  mutate(sum = num_sig+num_nonsig,
         psig = num_sig/sum,
         pnsig = num_nonsig/sum)%>%
  select(-sum, -num_sig, -num_nonsig)%>%
  gather(key = sig, value = value, -response_var) %>%
  mutate(
    response_var2 = ifelse(response_var == "evenness_change_abs", "Evenness", response_var),
    response_var2 = ifelse(response_var == "gains", "Species gains", response_var2),
    response_var2 = ifelse(response_var == "losses", "Species losses", response_var2),
    response_var2 = ifelse(response_var == "rank_change", "Rank change", response_var2),
    response_var2 = ifelse(response_var == "richness_change_abs", "Richness change", response_var2))

sig_tograph2<-rbind(vector_overall, sig_tograph)


ggplot(sig_tograph2, aes(x = response_var2, y = value, fill = sig)) +
  geom_col(width = 0.7) +
  coord_flip() +
  theme_minimal() +
  scale_fill_brewer(name = "Treatment vs. Control", labels = c("Not significant", "Significant"))+
  scale_x_discrete(limits=c("Richness change", "Species losses", "Species gains", "Rank change", "Evenness", "Any Change"), labels = c("Richness change","Species Losses","Species Gains", "Rank Change","Evenness Change","Any Change" ))+
  labs(x = "Change metric", y = "Proportion of communities") +
  theme(legend.position = "top")+
  #geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = 5.5, linetype="dashed")


