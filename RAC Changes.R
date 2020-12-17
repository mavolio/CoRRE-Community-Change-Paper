################################################################################
##  RAC Changes.R: This script creates the RAC change metrics for the codyn dataset and investigates a bit overall trends.
##
##  Author: Meghan Avolio (meghan.avolio@gmail.com)
##  Date: March 19, 2018
##  Update: may 27, 2020
################################################################################

library(tidyverse)
library(gridExtra)
library(grid)
library(gtable)
library(codyn)
library(vegan)

#home
setwd("~/Dropbox/converge_diverge/datasets/LongForm/")
#work
setwd("C:\\Users\\mavolio2\\Dropbox\\converge_diverge\\datasets\\LongForm\\")

corredat<-read.csv("SpeciesRelativeAbundance_Nov2019.csv")

#gvn face - only 2 years of data so will only have one point for the dataset, therefore we are removing this dataset from these analyses.
corredat1<-corredat%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm!="GVN_FACE_0", site_project_comm!="AZI_NitPhos_0", site_project_comm!="JRN_study278_0", site_project_comm!="KNZ_GFP_4F", site_project_comm!="Saskatchewan_CCD_0", project_name!="e001", project_name!="e002", site_project_comm!="CHY_EDGE_0", site_project_comm!="HYS_EDGE_0", site_project_comm!="SGS_EDGE_0")

##several studies only have two measurments of a plot. I am dropping those plots
azi<-corredat%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_code=="AZI")%>%
  filter(plot_id!=11&plot_id!=15&plot_id!=35&plot_id!=37)

jrn<-corredat%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm=="JRN_study278_0")%>%
  filter(plot_id!=211&plot_id!=210)

knz<-corredat%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm=="KNZ_GFP_4F")%>%
  filter(plot_id!="7_1_1"&plot_id!="7_2_1")

sak<-corredat%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_project_comm=="Saskatchewan_CCD_0")%>%
  filter(plot_id!=2)

###remove extra treatments from CDR e001 and e002
cdr <- corredat%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(project_name=="e001"|project_name=="e002")%>%
  filter(treatment==1|treatment==6|treatment==8|treatment==9|treatment=='1_f_u_n'|treatment=='6_f_u_n'|treatment=='8_f_u_n'|treatment=='9_f_u_n')

##remove one of 2 pre-treatment years in edge for CHY, SGS, and HAYS
edge<-corredat%>%
  select(-X)%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  filter(site_code=="CHY"|site_code=="SGS"|site_code=="HYS"&project_name=="EDGE")%>%
  filter(calendar_year!=2012)



###final dataset to use
corredat_raw<-rbind(corredat1, azi, jrn, knz, sak, cdr, edge)


plotinfo<-corredat_raw%>%
  select(site_project_comm, calendar_year, plot_id, treatment, treatment_year)%>%
  unique()


#####CALCULATING DIVERSITY METRICs. What is the richness and evenness (measured with Evar for each plot at each time point?)

spc<-unique(corredat_raw$site_project_comm)
div_evar<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_raw%>%
    filter(site_project_comm==spc[i])
  
  out<-community_structure(subset, time.var = 'calendar_year', abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  div_evar<-rbind(div_evar, out)
}

write.csv(div_evar, "C:\\Users\\mavolio2\\Dropbox\\C2E\\Products\\CommunityChange\\March2018 WG\\CORRE_DivEvar_June2020.csv", row.names=F)

#####CALCULATING RAC changes
spc<-unique(corredat_raw$site_project_comm)
delta_rac<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_raw%>%
    filter(site_project_comm==spc[i])
  
  out<-RAC_change(subset, time.var = 'calendar_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  delta_rac<-rbind(delta_rac, out)
}

write.csv(delta_rac, "C:\\Users\\megha\\Dropbox\\converge_diverge\\datasets\\LongForm\\CORRE_RAC_Metrics_March19_allReplicates.csv")


write.csv(delta_rac, "~/Dropbox/C2E/Products/CommunityChange/March2018 WG/CORRE_RAC_Metrics_July2018.csv")

####based on treatment_year
spc<-unique(corredat_raw$site_project_comm)
delta_rac<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_raw%>%
    filter(site_project_comm==spc[i])
  
  out<-RAC_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  delta_rac<-rbind(delta_rac, out)
}
write.csv(delta_rac, "~/Dropbox/C2E/Products/CommunityChange/March2018 WG/CORRE_RAC_Metrics_July2018_trtyr.csv")
write.csv(delta_rac, "C:\\Users\\megha\\Dropbox\\C2E\\Products\\CommunityChange\\March2018 WG\\CORRE_RAC_Metrics_March2019_trtyr.csv")

###calculating multivariate changes
spc<-unique(corredat_raw$site_project_comm)
delta_mult<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_raw%>%
    filter(site_project_comm==spc[i])
  
  out<-multivariate_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id', treatment.var = "treatment")
  out$site_project_comm<-spc[i]
  
  delta_mult<-rbind(delta_mult, out)
}

write.csv(delta_mult, "C:\\Users\\megha\\Dropbox\\C2E\\Products\\CommunityChange\\March2018 WG\\CORRE_Mult_Metrics_March2019.csv")

###doing for without rare species
treatment_info<-read.csv("ExperimentInformation_March2019.csv")%>%
  dplyr::select(site_code, project_name, community_type, treatment,plot_mani)%>%
  unique()%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))

noraresp<-corredat_raw%>%
  left_join(treatment_info)%>%
  filter(plot_mani==0)%>%
  group_by(site_project_comm, genus_species)%>%
  summarize(mrelcov=mean(relcov))%>%
  filter(mrelcov>0.1)%>%
  select(-mrelcov)


corredat_norare <- corredat_raw %>%
  right_join(noraresp)


spc<-unique(corredat_norare$site_project_comm)
delta_rac_nr<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_norare%>%
    filter(site_project_comm==spc[i])
  
  out<-RAC_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  delta_rac_nr<-rbind(delta_rac_nr, out)
}

delta_rac_nr2<-delta_rac_nr%>%
  left_join(plotinfo)


write.csv(delta_rac_nr2, "CORRE_RAC_Metrics_June2020_allReplicates_norares.csv")

# ##calculating RAC changes with a reference year.
# 
# firstyear<-corredat_raw%>%
#   group_by(site_project_comm)%>%
#   summarise(yr1=min(treatment_year))%>%
#   mutate(drop=ifelse(yr1>1, 1, 0))
# 
# corredat_refyear<-corredat_raw%>%
#   left_join(firstyear)%>%
#   filter(drop==0)%>%
#   filter(site_project_comm!="dcgs_gap_0")
# 
# spc<-unique(corredat_refyear$site_project_comm)
# delta_rac_refyear<-data.frame()
# 
# for (i in 1:length(spc)){
#   subset<-corredat_refyear%>%
#     filter(site_project_comm==spc[i])
#   
#   out<-RAC_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id', reference.time = 1)
#   out$site_project_comm<-spc[i]
#   
#   delta_rac_refyear<-rbind(delta_rac_refyear, out)
# }
# 
# #first year is yr2
# corredat_refyear2<-corredat_raw%>%
#   left_join(firstyear)%>%
#   filter(yr1==2)
# 
# spc<-unique(corredat_refyear2$site_project_comm)
# delta_rac_refyear2<-data.frame()
# 
# for (i in 1:length(spc)){
#   subset<-corredat_refyear2%>%
#     filter(site_project_comm==spc[i])
#   
#   out<-RAC_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id', reference.time = 2)
#   out$site_project_comm<-spc[i]
#   
#   delta_rac_refyear2<-rbind(delta_rac_refyear2, out)
# }
# 
# #first year is yr3
# corredat_refyear3<-corredat_raw%>%
#   left_join(firstyear)%>%
#   filter(yr1==3)
# 
# spc<-unique(corredat_refyear3$site_project_comm)
# delta_rac_refyear3<-data.frame()
# 
# for (i in 1:length(spc)){
#   subset<-corredat_refyear3%>%
#     filter(site_project_comm==spc[i])
#   
#   out<-RAC_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id', reference.time = 3)
#   out$site_project_comm<-spc[i]
#   
#   delta_rac_refyear3<-rbind(delta_rac_refyear3, out)
# }
# #first year is yr4
# corredat_refyear4<-corredat_raw%>%
#   left_join(firstyear)%>%
#   filter(yr1==4)
# 
# spc<-unique(corredat_refyear4$site_project_comm)
# delta_rac_refyear4<-data.frame()
# 
# for (i in 1:length(spc)){
#   subset<-corredat_refyear4%>%
#     filter(site_project_comm==spc[i])
#   
#   out<-RAC_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id', reference.time = 4)
#   out$site_project_comm<-spc[i]
#   
#   delta_rac_refyear4<-rbind(delta_rac_refyear4, out)
# }
# #first year is yr5
# corredat_refyear5<-corredat_raw%>%
#   left_join(firstyear)%>%
#   filter(yr1==5)
# 
# spc<-unique(corredat_refyear5$site_project_comm)
# delta_rac_refyear5<-data.frame()
# 
# for (i in 1:length(spc)){
#   subset<-corredat_refyear5%>%
#     filter(site_project_comm==spc[i])
#   
#   out<-RAC_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id', reference.time = 5)
#   out$site_project_comm<-spc[i]
#   
#   delta_rac_refyear5<-rbind(delta_rac_refyear5, out)
# }
# 
# #first year is yr6
# corredat_refyear6<-corredat_raw%>%
#   left_join(firstyear)%>%
#   filter(yr1==6)
# 
# spc<-unique(corredat_refyear6$site_project_comm)
# delta_rac_refyear6<-data.frame()
# 
# for (i in 1:length(spc)){
#   subset<-corredat_refyear6%>%
#     filter(site_project_comm==spc[i])
#   
#   out<-RAC_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id', reference.time = 6)
#   out$site_project_comm<-spc[i]
#   
#   delta_rac_refyear6<-rbind(delta_rac_refyear6, out)
# }
# #first year is yr10
# corredat_refyear10<-corredat_raw%>%
#   left_join(firstyear)%>%
#   filter(yr1==10)
# 
# spc<-unique(corredat_refyear10$site_project_comm)
# delta_rac_refyear10<-data.frame()
# 
# for (i in 1:length(spc)){
#   subset<-corredat_refyear10%>%
#     filter(site_project_comm==spc[i])
#   
#   out<-RAC_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id', reference.time = 10)
#   out$site_project_comm<-spc[i]
#   
#   delta_rac_refyear10<-rbind(delta_rac_refyear10, out)
# }
# #first year is yr11
# corredat_refyear11<-corredat_raw%>%
#   left_join(firstyear)%>%
#   filter(yr1==11)
# 
# spc<-unique(corredat_refyear11$site_project_comm)
# delta_rac_refyear11<-data.frame()
# 
# for (i in 1:length(spc)){
#   subset<-corredat_refyear11%>%
#     filter(site_project_comm==spc[i])
#   
#   out<-RAC_change(subset, time.var = 'treatment_year', species.var = "genus_species", abundance.var = 'relcov', replicate.var = 'plot_id', reference.time = 11)
#   out$site_project_comm<-spc[i]
#   
#   delta_rac_refyear11<-rbind(delta_rac_refyear11, out)
# }
# 
# delta_rac_refyear_all<-delta_rac_refyear%>%
#   bind_rows(delta_rac_refyear10, delta_rac_refyear11, delta_rac_refyear2, delta_rac_refyear3, delta_rac_refyear4, delta_rac_refyear5, delta_rac_refyear6)
#   
# write.csv(delta_rac_refyear_all, "C:\\Users\\mavolio2\\Dropbox\\C2E\\Products\\CommunityChange\\March2018 WG\\CORRE_RAC_Refyear_Metrics_May2020.csv", row.names = F)
# 

###figuring out treatments
# trts<-read.csv("~/Dropbox/C2E/Products/CommunityChange/March2018 WG/treatment interactions_July2018.csv")%>%
#   select(site_code, trt_type)%>%
#   unique()%>%
#   group_by(trt_type)%>%
#   summarize(n = length(site_code))
# 
# 
# 

# ##getting the average for each treatment in a year
# 
# corre_diversity<-merge(plotinfo, diversity, by=c("site_project_comm","calendar_year","plot_id"))%>%
#   group_by(site_project_comm, calendar_year, treatment_year, treatment)%>%
#   summarize(S_diff=mean(S_diff), even_diff=mean(E_diff, na.rm=T))
# 
# corre_gainloss<-merge(plotinfo, gain_loss, by=c("site_project_comm","calendar_year","plot_id"))%>%
#   group_by(site_project_comm, calendar_year, treatment_year, treatment)%>%
#   summarize(gain=mean(appearance), loss=mean(disappearance))
# 
# corre_reordering<-merge(plotinfo, reordering, by=c("site_project_comm","calendar_year","plot_id"))%>%
#   group_by(site_project_comm, calendar_year, treatment_year, treatment)%>%
#   summarize(MRSc=mean(MRSc))
# 
# ####MERGING TO A SINGE DATASET and exporting
# 
# merge1<-merge(corre_diversity, corre_gainloss, by=c("site_project_comm","calendar_year","treatment_year","treatment"), all=T)
# merge2<-merge(merge1, corre_reordering, by=c("site_project_comm","calendar_year","treatment_year","treatment"), all=T)
# all_metrics<-merge(merge2, corre_braycurtis, by=c("site_project_comm","calendar_year","treatment"), all=T)
# 
# write.csv(all_metrics, "C:\\Users\\megha\\Dropbox\\converge_diverge\\datasets\\LongForm\\CORRE_RAC_Metrics_Oct2017_allyears_2.csv")
# 
# 
# write.csv(all_metrics, "~/Dropbox/converge_diverge/datasets/LongForm/CORRE_RAC_Metrics_Oct2017_allyears_2.csv")


# ###Getting b-C distnace of each plot to itself comparing t1 to t2.
# 
# corredat$expplot<-paste(corredat$site_project_comm, corredat$plot_id, sep="::")
# 
# exp_plot_list<-unique(corredat$expplot)
# 
# 
# #makes an empty dataframe
# bray_curtis_dissim=data.frame(site_project_comm_plot=c(), calendar_year=c(), bc_dissim=c()) 
# 
# ##calculating bray-curtis mean change and disperison differecnes
# for(i in 1:length(exp_plot_list)) {
#   
#   #subsets out each dataset
#   subset=corredat%>%
#     filter(expplot==exp_plot_list[i])%>%
#     select(site_project_comm, treatment, calendar_year, genus_species, relcov, plot_id)
#   
#   #get years
#   experiment_years<-sort(unique(subset$calendar_year))
#   
#   #transpose data
#   species=subset%>%
#     spread(genus_species, relcov, fill=0)
#   
#   #calculate bray-curtis dissimilarities
#   bc=as.matrix(vegdist(species[,5:ncol(species)], method="bray"))
#   
#   ###experiment_year is year x+1
#   bc_dis=data.frame(site_project_comm_plot=exp_plot_list[i],
#                     calendar_year=experiment_years[2:length(experiment_years)],
#                     bc_dissim=diag(bc[2:nrow(bc),1:(ncol(bc)-1)]))
#   
#   #pasting dispersions into the dataframe made for this analysis
#   bray_curtis_dissim=rbind(bc_dis, bray_curtis_dissim)  
# }
# 
# corre_braycurtis<-bray_curtis_dissim%>%
#   separate(site_project_comm_plot, into=c("site_project_comm","plot_id"), sep="::")
# 
# ###merging to a single dataset and adding treatment information
# merge1<-merge(gain_loss, diversity, by=c("site_project_comm","calendar_year","plot_id"))
# merge2<-merge(merge1, reordering,by=c("site_project_comm","calendar_year","plot_id")) 
# merge3<-merge(merge2, corre_braycurtis, by=c("site_project_comm","calendar_year","plot_id"))
# corre_all<-merge(plotinfo, merge3, by=c("site_project_comm","calendar_year","plot_id"))
# 
# write.csv(corre_all, "C:\\Users\\megha\\Dropbox\\converge_diverge\\datasets\\LongForm\\CORRE_RAC_Metrics_Oct2017_allReplicates.csv")
# 
# write.csv(corre_all, "~/Dropbox/converge_diverge/datasets/LongForm/CORRE_RAC_Metrics_Oct2017_allReplicates_2.csv")


write.csv(corre_all, "C:\\Users\\megha\\Dropbox\\converge_diverge\\datasets\\LongForm\\CORRE_RAC_Metrics_NOV2017_allReplicates.csv")

#investigating patterns of change
corredat<-read.csv("~/Dropbox/converge_diverge/datasets/LongForm/SpeciesRelativeAbundance_Oct2017.csv")
corredat$site_project_comm <- with(corredat, paste(site_code, project_name, community_type, sep="_"))
cul<-subset(corredat, site_project_comm == "CUL_Culardoch_0"& plot_id == 17 & treatment_year %in% c(2,3))

cdr<-subset(corredat, site_project_comm == "CDR_e001_A"& plot_id == 13 & treatment_year %in% c(11,12))

car<-subset(corredat, site_project_comm == "CAR_salt marsh_DisSal"& plot_id == 6.2 & treatment_year %in% c(1,2))


even<-div_eq%>%
  right_join(div_evar)%>%
  right_join(div_simp)%>%
  select(site_project_comm, calendar_year, plot_id, richness, EQ, Evar, SimpsonEvenness)%>%
  gather(metric, value, EQ:SimpsonEvenness)

ggplot(data=even, aes(x=value))+
  geom_histogram()+
  facet_wrap(~metric)

ggplot(data=even, aes(x = richness, y = value))+
  geom_point()+
  facet_wrap(~metric)

even_change<-delta_rac%>%
  left_join(even)
ggplot(data=even_change, aes(x = richness, y = evenness_change))+
  geom_point()

colnames(delta_rac)

even_test<-delta_rac%>%
  gather(metric, value, richness_change:losses)%>%
  filter(metric != "evenness_change_divrich"& metric!= 'evenness_change_multrich')

ggplot(data=even_test, aes(x = sppool, y = value))+
  geom_point()+
  facet_wrap(~metric, scales = "free")

ggplot(data=even_test, aes(x=value))+
  geom_histogram()+
  facet_wrap(~metric, scales = "free")


ggplot(data=delta_rac, aes(x = rank_change, y = gains))+
  geom_point()

jsp<-subset(corredat, site_project_comm == "JSP_GCE_0"& plot_id == 62 & treatment_year %in% c(3,4))%>%
  group_by(treatment_year)%>%
  mutate(rank = rank(-relcov, ties.method = "average"))

ggplot(data = jsp, aes(x = rank, y= relcov ))+
  geom_line()+
  facet_wrap(~treatment_year)
