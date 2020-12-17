################################################################################
##  GamOutputAnalysis.R This code analyzes the spreadsheet output by Andrew do see when communities changed.
##
##  Author: Meghan Avolio (meghan.avolio@gmail.com)
##  Date: August 6, 2018
##  Updated: Nov 4, 2018
################################################################################


library(tidyverse)
library(gridExtra)

theme_set(theme_bw(base_size=20))


#work
setwd("C:\\Users\\mavolio2\\Dropbox\\")
#home
setwd("~/Dropbox/")

#read in and drop sucessional treatments (those that had a big disturbance to start)
gam<-read.csv("C2E/Products/CommunityChange/Summer2018_Results/gam_comparison_table_last_year.csv")%>%
  rename(site_project_comm=site_proj_comm)%>%
  separate(site_project_comm, into=c("site_code","project_name","community_type"), sep="_", remove=F)%>%
  filter(response_var != "composition_change")

###can do this anlayses for datasets 10 yrs or longer, to match what kim has
# %>%
#   filter(diff_treatment_year>9)

#filter the press treatments for the experiments with enough data
trt_touse<-read.csv("C2E/Products/CommunityChange/March2018 WG/ExperimentInformation_March2019.csv")%>%
  filter(pulse==0, plot_mani!=0)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_")) %>% 
  select(site_project_comm, treatment, trt_type)%>%
  unique()%>%
  mutate(use=ifelse(trt_type=="N"|trt_type=="P"|trt_type=="CO2"|trt_type=="irr"|trt_type=="temp"|trt_type=="N*P"|trt_type=="mult_nutrient"|trt_type=='precip_vari', 1, 0))

info.trt2<-read.csv("C2E\\Products\\CommunityChange\\March2018 WG\\ForAnalysis_allAnalysisAllDatasets_04082019.csv") %>%
  mutate(site_project_comm = paste(site_code, project_name, community_type, sep="_"))%>%
  select(site_project_comm, treatment, trt_type)%>%
  unique()%>%
  mutate(trt_type3=ifelse(trt_type=="drought"|trt_type=="irr"|trt_type=="N"|trt_type=="precip_vari"|trt_type=="P"|trt_type=="CO2"|trt_type=="other_resource", "Res.", ifelse(trt_type=="mow_clip"|trt_type=="temp"|trt_type=="plant_mani"|trt_type=="other_nonresource"|trt_type=="herb_removal"|trt_type=="NxN", "Non-Res.", ifelse(trt_type=="RxR"|trt_type=="RxRxR", "Mult. Res.", ifelse(trt_type=="threeway"|trt_type=="RxN", "Res.+Non-Res.", "oops")))))%>%
  select(-trt_type)

summary_res<-info.trt2%>%
  right_join(gam_trt)%>%
  group_by(trt_type3)%>%
  summarize(n=length(trt_type3))

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
  left_join(info.trt2)

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

##basic stats on what we are doing.
gam_exp<-gam_touse%>%
  select(site_project_comm)%>%
  unique()

gam_trt<-gam_touse%>%
  select(site_project_comm, treatment, trt_type2)%>%
  unique()

##summary of treatment for table 1
sum_trts<-gam_trt%>%
  select(site_project_comm, trt_type2)%>%
  separate(site_project_comm, into=c("site_code", "project_name", "community_type"), sep="_")%>%
  group_by(trt_type2)%>%
  summarize(n=length(trt_type2))

sum_trts2<-gam_touse%>%
  separate(site_project_comm, into=c("site_code", "project_name", "community_type"), sep="_")%>%
  select(site_code, trt_type2)%>%
  unique()%>%
  group_by(trt_type2)%>%
  summarize(n=length(trt_type2))

#write.csv(gam_trt, "C2E/Products/CommunityChange/March2018 WG/experiment_trt_subset_May2019.csv", row.names=F)

# write.csv(trt_summary, "treatment_summary.csv")

###

#no longer doing this based on those communities that changed.

# #first, how many communities saw sig differences in change between controls and treatments?
# compchange<-gam_touse%>%
#   filter(response_var == "composition_change")%>%
#   group_by(sig_diff_cntrl_trt)%>%
#   summarize(length(sig_diff_cntrl_trt))
# 
# ##did change differ depending on treatment?
# gamtrts<-gam%>%
#   left_join(trts_interactions)%>%
#   filter(response_var == "composition_change", use == 1)%>%
#   group_by(trt_type2) %>%
#   summarise(
#     num_sig = length(which(sig_diff_cntrl_trt == "yes")),
#     num_nonsig = length(which(sig_diff_cntrl_trt == "no"))
#   )%>%
#   filter(trt_type2!="N+irr"&trt_type2!="drought")
# 
# write.csv(gamtrts, "C2E/Products/CommunityChange/Summer2018_Results/chisq_comp_change_newtrts.csv")
# 
# gamtrts_toplot<-gamtrts%>%
#   mutate(sum = num_sig+num_nonsig,
#          psig = num_sig/sum,
#          pnsig = num_nonsig/sum)%>%
#   select(-sum, -num_sig, -num_nonsig)%>%
#   gather(key = sig, value = number, -trt_type2)
# 
# 
# ggplot(gamtrts_toplot, aes(x = trt_type2, y = number, fill = sig)) +
#   geom_col(width = 0.7) +
#   coord_flip() +
#   theme_minimal() +
#   scale_fill_brewer(name = "Treatment vs. Control", labels = c("Not significant", "Significant")) +
#   labs(x = "Global Change Treatment", y = "Proportion of communities") +
#   geom_hline(yintercept = 0.50)+
#   theme(legend.position = "top")+
#   annotate(geom="text", x = 1, y = 0.05, label="n = 7", size=3)+
#   annotate(geom="text", x = 2, y = 0.05, label="n = 12", size=3)+
#   annotate(geom="text", x = 3, y = 0.05, label="n = 51", size=3)+
#   annotate(geom="text", x = 4, y = 0.05, label="n = 34", size=3)+
#   annotate(geom="text", x = 5, y = 0.05, label="n = 11", size=3)
# 
# 
# #second, of those that saw change, what aspect of the community changes
# comchange_sig<-gam_touse%>%
#   filter(response_var == "composition_change" & sig_diff_cntrl_trt == "yes")%>%
#   select(site_project_comm, treatment)%>%
#   mutate(keep = "yes")
# #
# # write.csv(comchange_sig, 'C:\\Users\\megha\\Dropbox\\C2E\\Products\\CommunityChange\\Summer2018_Results\\gam_com_sig_change.csv', row.names = F )
# 
# 
# #second, of those that saw change, what aspect of the community changes
# comchange_sig_metrics<-gam_touse%>%
#   filter(response_var != "composition_change" & sig_diff_cntrl_trt == "yes")%>%
#   select(site_project_comm, treatment, response_var)%>%
#   mutate(keep = "yes")
# 
# overlap<-comchange_sig_metrics%>%
#   select(-keep)%>%
#   left_join(comchange_sig)
# 
# write.csv(comchange_sig_metrics, 'C:\\Users\\megha\\Dropbox\\C2E\\Products\\CommunityChange\\Summer2018_Results\\gam_metrics_sig_change_may2019.csv', row.names = F )
# 
# sig_only<-gam%>%
#   right_join(comchange_sig)%>%
#   filter(response_var != "composition_change")
# 
# sig_tally <- sig_only %>%
#   group_by(response_var) %>%
#   summarise(
#     num_sig = length(which(sig_diff_cntrl_trt == "yes")),
#     num_nonsig = length(which(sig_diff_cntrl_trt == "no"))
#   ) %>%
#   mutate(sum = num_sig+num_nonsig,
#          psig = num_sig/sum,
#          pnsig = num_nonsig/sum)%>%
#   select(-sum, -num_sig, -num_nonsig)%>%
#   gather(key = sig, value = value, -response_var) %>%
#   mutate(
#     response_var2 = ifelse(response_var == "evenness_change_abs", "Evenness", response_var),
#     response_var2 = ifelse(response_var == "gains", "Species gains", response_var2),
#     response_var2 = ifelse(response_var == "losses", "Species losses", response_var2),
#     response_var2 = ifelse(response_var == "rank_change", "Rank change", response_var2),
#     response_var2 = ifelse(response_var == "richness_change_abs", "Richness change", response_var2))
# 
# allSERGL<-sig_tally%>%
#   select(-response_var2)%>%
#   spread(sig, value)
# 
# write.csv(allSERGL, 'C:\\Users\\megha\\Dropbox\\C2E\\Products\\CommunityChange\\Summer2018_Results\\gam_com_sig_change_all.csv', row.names = F )
# 
# sergl<-ggplot(sig_tally, aes(x = response_var2, y = value, fill = sig)) +
#   geom_col(width = 0.7) +
#   coord_flip() +
#   theme_minimal() +
#   scale_fill_brewer(name = "Treatment vs. Control", labels = c("Not significant", "Significant")) +
#   labs(x = "Change metric", y = "Proportion of communities") +
#   theme(legend.position = "top")+
#   geom_hline(yintercept = 0.5)
# 
# ##did aspect of community change depend on the treatment?
# gamtrts_metrics<-sig_only%>%
#   left_join(trts_interactions)%>%
#   ungroup()%>%
#   filter(response_var != "composition_change", use == 1)%>%
#   group_by(response_var, trt_type2) %>%
#   summarise(
#     num_sig = length(which(sig_diff_cntrl_trt == "yes")),
#     num_nonsig = length(which(sig_diff_cntrl_trt == "no"))
#   )%>%
#   filter(trt_type2!="N+irr"&trt_type2!="drought")
# 
# write.csv(gamtrts_metrics, "C:\\Users\\megha\\Dropbox\\C2E\\Products\\CommunityChange\\Summer2018_Results\\chisq_metrics_change.csv")
# 
# tograph_metrics_trt<-gamtrts_metrics%>%
#   mutate(sum = num_sig + num_nonsig,
#          psig = num_sig/sum,
#          pnonsig = num_nonsig/sum)%>%
#   select(-num_sig, -num_nonsig, -sum)%>%
#   gather(key = sig, value = value, -trt_type2, -response_var) %>%
#   mutate(
#     response_var2 = ifelse(response_var == "evenness_change_abs", "Evenness", ifelse(response_var == "gains", "Species gains", ifelse(response_var == "losses", "Species losses", ifelse(response_var == "rank_change", "Rank change", "Richness change")))))
# 
# 
# trt_sergl<-ggplot(subset(tograph_metrics_trt, trt_type2!="P"), aes(x = response_var2, y = value, fill = sig)) +
#   geom_col(width = 0.7) +
#   coord_flip() +
#   theme_minimal() +
#   scale_fill_brewer(name = "Treatment vs. Control", labels = c("Not significant", "Significant")) +
#   labs(x = "Change metric", y = "Proportion of communities") +
#   theme(legend.position = "top")+
#   geom_hline(yintercept=0.50)+
#   facet_wrap(~trt_type2, ncol = 2)


#Regradless of whether the community changed, how much to the metrics change?
metrics_sig<-adjp_sig

write.csv(metrics_sig, 'C:\\Users\\mavolio2\\Dropbox\\C2E\\Products\\CommunityChange\\Summer2018_Results\\gam_metrics_sig_change_Dec2020.csv', row.names = F )

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



#overall diff in metrics of change - NO
prop.test(x=as.matrix(metrics_sig_tally[c('num_sig', 'num_nonsig')]), alternative='two.sided')

vector_overall<-data.frame("response_var"=c("all","all"), sig=c("psig", "pnsig"), value = c(0.7077, 0.2922), response_var2=c("Any Change", "Any Change")) 

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

overall<-
ggplot(sig_tograph2, aes(x = response_var2, y = value, fill = sig)) +
  geom_col(width = 0.7) +
  coord_flip() +
  theme_minimal() +
  scale_fill_brewer(name = "Treatment vs. Control", labels = c("Not significant", "Significant"))+
  scale_x_discrete(limits=c("Richness change", "Species losses", "Species gains", "Rank change", "Evenness", "Any Change"), labels = c("Richness change","Species Losses","Species Gains", "Rank Change","Evenness Change","Any Change" ))+
  labs(x = "Change metric", y = "Proportion of communities") +
  #theme(legend.position = "none")+
  #geom_hline(yintercept = 0.5)+
  geom_vline(xintercept = 5.5, linetype="dashed")+
  ggtitle("A)")

###how does this differ by GCD?

gamtrts_metrics<-metrics_sig%>%
  ungroup()%>%
  filter(response_var != "composition_change", use == 1)%>%
  group_by(response_var, trt_type2) %>%
  summarise(
    num_sig = length(which(sig_diff_cntrl_trt == "yes")),
    num_nonsig = length(which(sig_diff_cntrl_trt == "no"))
  )

gamtrts3_metrics<-metrics_sig%>%
  ungroup()%>%
  filter(response_var != "composition_change")%>%
  group_by(response_var, trt_type3) %>%
  summarise(
    num_sig = length(which(sig_diff_cntrl_trt == "yes")),
    num_nonsig = length(which(sig_diff_cntrl_trt == "no"))
  )%>%
  rename(trt_type2=trt_type3)


#for emily's bionomial analsis
gamtrts_metrics_em<-metrics_sig%>%
  ungroup()%>%
  filter(response_var != "composition_change", use == 1)%>%
  mutate(sigdiff=ifelse(sig_diff_cntrl_trt=="no", 0, 1))%>%
  select(site_project_comm, treatment, response_var, site_code, project_name, community_type, trt_type2, sigdiff)

#write.csv(gamtrts_metrics_em, "C2E/Products/CommunityChange/Summer2018_Results/sig_diff_by_trts_May2019.csv", row.names = F)

# # #wihtin a GCD is there a differnce?
# CO2 <- gamtrts_metrics%>%filter(trt_type2=='CO2')
# prop.test(x=as.matrix(CO2[c('num_sig', 'num_nonsig')]), alternative='two.sided')
# # 
# #N
# N <- gamtrts_metrics%>%filter(trt_type2=='N')
# prop.test(x=as.matrix(N[c('num_sig', 'num_nonsig')]), alternative='two.sided')
# 
# #P
# P <- gamtrts_metrics%>%filter(trt_type2=='P')
# prop.test(x=as.matrix(P[c('num_sig', 'num_nonsig')]), alternative='two.sided')
# 
# #irr
# irr <- gamtrts_metrics%>%filter(trt_type2=='Irrigation')
# prop.test(x=as.matrix(irr[c('num_sig', 'num_nonsig')]), alternative='two.sided')
# 
# #mult nuts
# multnuts <- gamtrts_metrics%>%filter(trt_type2=='Mult. Nuts.')
# prop.test(x=as.matrix(multnuts[c('num_sig', 'num_nonsig')]), alternative='two.sided')
# 
# #temp
# temp <- gamtrts_metrics%>%filter(trt_type2=='Temperature')
# prop.test(x=as.matrix(temp[c('num_sig', 'num_nonsig')]), alternative='two.sided')
# 
# #precip var
# pv <- gamtrts_metrics%>%filter(trt_type2=='Precip. Vari.')
# prop.test(x=as.matrix(pv[c('num_sig', 'num_nonsig')]), alternative='two.sided')
# 
# # #within a manipulation type is there a differnce?
# nonres <- gamtrts3_metrics%>%filter(trt_type2=='Non-Res.')
# prop.test(x=as.matrix(nonres[c('num_sig', 'num_nonsig')]), alternative='two.sided')
# # 
# #N
# res <- gamtrts3_metrics%>%filter(trt_type2=='Res.')
# prop.test(x=as.matrix(res[c('num_sig', 'num_nonsig')]), alternative='two.sided')
# 
# #P
# multres <- gamtrts3_metrics%>%filter(trt_type2=='Mult. Res.')
# prop.test(x=as.matrix(multres[c('num_sig', 'num_nonsig')]), alternative='two.sided')
# 
# #irr
# rn<- gamtrts3_metrics%>%filter(trt_type2=='Res.+Non-Res.')
# prop.test(x=as.matrix(rn[c('num_sig', 'num_nonsig')]), alternative='two.sided')
# 
# #mult nuts
# multnuts <- gamtrts_metrics%>%filter(trt_type2=='Mult. Nuts.')
# prop.test(x=as.matrix(multnuts[c('num_sig', 'num_nonsig')]), alternative='two.sided')

# ###now looking within communitychange
# 
rich <- gamtrts_metrics%>%filter(response_var=='richness_change_abs')
r<-prop.test(x=as.matrix(rich[c('num_sig', 'num_nonsig')]), alternative='two.sided')
p.adjust(p=r["p.value"], "BH", n=10)

even <- gamtrts_metrics%>%filter(response_var=='evenness_change_abs')
e<-prop.test(x=as.matrix(even[c('num_sig', 'num_nonsig')]), alternative='two.sided')
p.adjust(p=e["p.value"], "BH", n=10)

#rank
rank <- gamtrts_metrics%>%filter(response_var=='rank_change')
ra<-prop.test(x=as.matrix(rank[c('num_sig', 'num_nonsig')]), alternative='two.sided')
p.adjust(p=ra["p.value"], "BH", n=10)

#gain
gain <- gamtrts_metrics%>%filter(response_var=='gains')
g<-prop.test(x=as.matrix(gain[c('num_sig', 'num_nonsig')]), alternative='two.sided')
p.adjust(p=g["p.value"], "BH", n=10)

#loss
loss <- gamtrts_metrics%>%filter(response_var=='losses')
l<-prop.test(x=as.matrix(loss[c('num_sig', 'num_nonsig')]), alternative='two.sided')
p.adjust(p=l["p.value"], "BH", n=10)

# ###now looking within communitychange
# 
rich2 <- gamtrts3_metrics%>%filter(response_var=='richness_change_abs')
r2<-prop.test(x=as.matrix(rich2[c('num_sig', 'num_nonsig')]), alternative='two.sided')
p.adjust(p=r2["p.value"], "BH", n=10)

even2 <- gamtrts3_metrics%>%filter(response_var=='evenness_change_abs')
e2<-prop.test(x=as.matrix(even2[c('num_sig', 'num_nonsig')]), alternative='two.sided')
p.adjust(p=e2["p.value"], "BH", n=10)

#rank
rank2 <- gamtrts3_metrics%>%filter(response_var=='rank_change')
ra2<-prop.test(x=as.matrix(rank2[c('num_sig', 'num_nonsig')]), alternative='two.sided')
p.adjust(p=ra2["p.value"], "BH", n=10)

#gain
gain2 <- gamtrts3_metrics%>%filter(response_var=='gains')
g2<-prop.test(x=as.matrix(gain2[c('num_sig', 'num_nonsig')]), alternative='two.sided')
p.adjust(p=g2["p.value"], "BH", n=10)

#loss
loss2 <- gamtrts3_metrics%>%filter(response_var=='losses')
l2<-prop.test(x=as.matrix(loss2[c('num_sig', 'num_nonsig')]), alternative='two.sided')
p.adjust(p=l2["p.value"], "BH", n=10)


numexp_trt<-metrics_sig%>%
  ungroup()%>%
  filter(response_var != "composition_change", use == 1)%>%
  select(site_project_comm, treatment, trt_type2)%>%
  unique
         
anysig_gamtrts_metrics<-metrics_sig%>%
  ungroup()%>%
  filter(response_var != "composition_change", use == 1, sig_diff_cntrl_trt=="yes")%>%
  select(site_project_comm, treatment, sig_diff_cntrl_trt, trt_type2)%>%
  unique()%>%
  right_join(numexp_trt)%>%
  mutate(sig_diff=ifelse(is.na(sig_diff_cntrl_trt), "no", "yes"))%>%
  group_by(trt_type2) %>%
  summarise(
    num_sig = length(which(sig_diff == "yes")),
    num_nonsig = length(which(sig_diff == "no"))
  )%>%
  mutate(response_var="any_change")

numexp_trt2<-metrics_sig%>%
  filter(response_var != "composition_change")%>%
  select(site_project_comm, treatment)%>%
  left_join(info.trt2)%>%
  unique()

anysig_gamtrts3_metrics<-metrics_sig%>%
  ungroup()%>%
  filter(response_var != "composition_change", sig_diff_cntrl_trt=="yes")%>%
  select(site_project_comm, treatment, sig_diff_cntrl_trt, trt_type3)%>%
  unique()%>%
  right_join(numexp_trt2)%>%
  mutate(sig_diff=ifelse(is.na(sig_diff_cntrl_trt), "no", "yes"))%>%
  group_by(trt_type3) %>%
  summarise(
    num_sig = length(which(sig_diff == "yes")),
    num_nonsig = length(which(sig_diff == "no"))
  )%>%
  mutate(response_var="any_change")%>%
  rename(trt_type2=trt_type3)


tograph_metrics_trt<-gamtrts_metrics%>%
  bind_rows(anysig_gamtrts_metrics, gamtrts3_metrics, anysig_gamtrts3_metrics)%>%
  mutate(sum = num_sig + num_nonsig,
         psig = num_sig/sum,
         pnonsig = num_nonsig/sum)%>%
  select(-num_sig, -num_nonsig, -sum)%>%
  gather(key = sig, value = value, -trt_type2, -response_var) %>%
  mutate(group=factor(response_var, levels = c("any_change", "evenness_change_abs", "rank_change", "gains", "losses", "richness_change_abs")))%>%
  mutate(trt_type3=factor(trt_type2,levels = c("Mult. Nuts.", "P", "N", "Temperature","Precip. Vari.","Irrigation", "CO2", 
                                               "Res.+Non-Res.", "Mult. Res.", "Res.", "Non-Res.")))

responses<-c(
  evenness_change_abs = "Evenness Change", 
  gains = "Species Gains", 
  losses = "Species Losses",
  rank_change = "Rank Change",
  richness_change_abs = "Richness Change", 
  any_change = "Any Change")

##overall how many experiments saw some aspect of change.
trts<-
  ggplot(tograph_metrics_trt, aes(x = trt_type3, y = value, fill = sig)) +
  geom_col(width = 0.7) +
  coord_flip() +
  theme_minimal() + 
  scale_x_discrete(labels=c("Mult. Nuts.", "Phosphorus","Nitrogen","Temperature" , "Precip. Vari.","Irrigation","CO2", "Res.+Non-Res.","Multiple Res.","Single Res.","Non-Res." , "All GCDs"))+
  scale_fill_brewer(name = "Treatment vs. Control", labels = c("Not significant", "Significant")) +
  labs(x = "Treatment", y = "Proportion of communities") +
  theme(legend.position = "none")+
  geom_vline(xintercept=7.5, size=0.5)+
  facet_wrap(~group, labeller=labeller(group = responses), ncol = 2)+
  ggtitle("B)")

grid.arrange(overall, trts, ncol=1, heights=c(1,2))

#for esa
ggplot(subset(tograph_metrics_trt, trt_type3=="Non-Res."|trt_type3=="Res."|trt_type3=="Mult. Res."|trt_type3=="Res.+Non-Res."), aes(x = trt_type3, y = value, fill = sig)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_x_discrete(labels=c("Res.+Non-Res.","Multiple Res.","Single Res.","Non-Res."))+
  scale_fill_brewer(name = "Treatment vs. Control", labels = c("Not significant", "Significant")) +
  labs(x = "Treatment", y = "Proportion of communities") +
  theme(legend.position = "none")+
  geom_vline(xintercept=7.5, size=0.5)+
  facet_wrap(~group, labeller=labeller(group = responses), ncol = 3)

ggplot(subset(tograph_metrics_trt, trt_type3!="Non-Res."&trt_type3!="Res."&trt_type3!="Mult. Res."&trt_type3!="Res.+Non-Res."), aes(x = trt_type3, y = value, fill = sig)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_x_discrete(labels=c("Mult. Nuts.", "Phosphorus","Nitrogen","Temperature" , "Precip. Vari.","Irrigation","CO2"))+
  scale_fill_brewer(name = "Treatment vs. Control", labels = c("Not significant", "Significant")) +
  labs(x = "Treatment", y = "Proportion of communities") +
  theme(legend.position = "none")+
  geom_vline(xintercept=7.5, size=0.5)+
  facet_wrap(~group, labeller=labeller(group = responses), ncol = 3)

