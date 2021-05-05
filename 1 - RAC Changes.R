################################################################################
##This script creates the RAC change metrics for the codyn dataset and investigates a bit overall trends.
##
##  Author: Meghan Avolio (meghan.avolio@jhu.edu)
##  Date: March 19, 2018
##  Update: May 3, 2021
################################################################################

library(tidyverse)
library(codyn)


#this dataset is not public
corredat<-read.csv("C:\\Users\\mavolio2\\Dropbox\\converge_diverge\\datasets\\LongForm\\SpeciesRelativeAbundance_Nov2019.csv")

# Cleaning CoRRE dataset --------------------------------------------------


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


# Calculating richness and evenness  --------------------------------------

spc<-unique(corredat_raw$site_project_comm)
rich_evar<-data.frame()

for (i in 1:length(spc)){
  subset<-corredat_raw%>%
    filter(site_project_comm==spc[i])
  
  out<-community_structure(subset, time.var = 'calendar_year', abundance.var = 'relcov', replicate.var = 'plot_id')
  out$site_project_comm<-spc[i]
  
  rich_evar<-rbind(rich_evar, out)
}

write.csv(rich_evar, "C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\CORRE_RichEvar.csv", row.names=F)

# calculating RAC changes using codyn -------------------------------------
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


dat<-delta_rac%>%
  left_join(plotinfo)

write.csv(dat, "C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\CoRRE_RAC_Measures.csv", row.names=F)



###doing for without rare species
treatment_info<-read.csv("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\ExperimentInformation_March2019.csv")%>%
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


write.csv(delta_rac_nr2, "C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\CORRE_RAC_Measures_norares.csv", row.names = F)



# Assessing temporal sampling bias -----------------------------------------

dat<-read.csv("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\CoRRE_RAC_Measures.csv")
plot_mani<-read.csv("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\ExperimentInformation_March2019.csv")%>%
  mutate(site_project_comm=paste(site_code, project_name, community_type, sep="_"))%>%
  select(site_project_comm, plot_mani, treatment)%>%
  unique()

dat<-dat%>%
  left_join(plot_mani)

#are there differences between the pre-treatment and first year of data for experiments with pre-treatment data?

pretrtdata<-dat%>%
  filter(treatment_year==0)%>%
  filter(site_project_comm %in% c("JSP_GCE_0", "KNZ_pplots_0", "KUFS_E6_type1", "KUFS_E6_type2", "NWT_snow_0", "PIE_TIDE_0", "SEV_EDGE_EB", "SEV_EDGE_EG"))


pvals<-data.frame()

sites<-unique(pretrtdata$site_project_comm)

for (i in 1:length(sites)){
  
  subset<-pretrtdata%>%
    filter(site_project_comm==sites[i])%>%
    filter(plot_mani!=0)
  
  subsetc<-pretrtdata%>%
    filter(site_project_comm==sites[i])%>%
    filter(plot_mani==0)
  
  trt<-unique(subset$treatment)
    
  for (j in 1:length(trt)){
    subsett<-subset%>%
      filter(treatment==trt[j])
    
    combine<-subsetc%>%
      bind_rows(subsett)
    
    rich<-summary(aov(abs(richness_change)~treatment, data=combine))
    outr<-rich[[1]]$"Pr(>F)"[1]
    
    even<-summary(aov(abs(evenness_change)~treatment, data=combine))
    oute<-even[[1]]$"Pr(>F)"[1]
    
    
    rank<-summary(aov(rank_change~treatment, data=combine))
    outra<-rank[[1]]$"Pr(>F)"[1]
    
    
    gain<-summary(aov(gains~treatment, data=combine))
    outg<-gain[[1]]$"Pr(>F)"[1]
    
    
    loss<-summary(aov(losses~treatment, data=combine))
    outl<-loss[[1]]$"Pr(>F)"[1]
    
    output<-data.frame(
      site_project_comm=unique(subset$site_project_comm),
      treatment=unique(subsett$treatment),
      rich=outr,
      even=oute,
      rank=outra,
      gain=outg,
      loss=outl
    )
    pvals<-pvals%>%
      bind_rows(output)
    }

}

adjust<-pvals%>%
  gather(measure, pval, rich:loss)%>%
  group_by(site_project_comm)%>%
  mutate(adj=p.adjust(pval, "BH"))

#only 6 out of 240 are sig. 


# Checking whether data sets that start late are problematic --------------


#When data collection began after year one did we miss the community change?

using<-read.csv("C:\\Users\\mavolio2\\Dropbox\\Manuscripts\\C2E- Community change\\Manuscript\\Submit EL\\Revision\\Final submission\\Datafiles\\Experiment_Trt_Subset.csv")%>%
  select(site_project_comm)%>%
  unique()%>%
  mutate(use=1)

samplinglag<-dat%>%
  select(site_project_comm, treatment_year, treatment_year2)%>%
  unique()%>%
  filter(treatment_year!=0)%>%
  group_by(site_project_comm)%>%
  mutate(min=min(treatment_year))%>%
  filter(treatment_year==min)%>%
  right_join(using)

#for these experiments I checked whether we say sig gam differences. We did for all but SEV_Nfert.

