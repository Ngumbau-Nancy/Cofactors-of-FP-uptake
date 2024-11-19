setwd("/Users/integ/Dropbox/Dama/Longitudinal")
rm (list = ls())
#Running packages
library(tidyverse)
library(haven)
library(magrittr)
library(forcats)
library(dplyr)
library(scales)
library(ggplot2)
library(ggthemes)
library(wesanderson)
library(lubridate)
library(summarytools)
library(readxl)


#Loading the datasets
Baseline<-read_excel("C:/Users/integ/Dropbox/Dama/Longitudinal/cofactors_baseline.xlsx")
fpdata_sae_ltfp<-read_excel("C:/Users/integ/Dropbox/Dama/Longitudinal/fpdata_sae_ltfp.xlsx")

table(fpdata$repeat_preg)
table(fpdata_sae$repeat_preg)
table(fpdata_sae_ltfp$repeat_preg)

summary(age_enr_cof)
table(fpdata_sae_ltfp$sa_resumesex)

#participants to censor in between visits
df_wide<- df_wide%>%
  mutate(total1 = case_when(total<'1'~'0',
                            total>='1'~'1'))

df_wide1<-df_wide%>%
  subset(total1=='1')

df_wide2<-df_wide1%>%
  subset(month9=='0')
duplicated_values <- df_wide2 [duplicated(df_wide2 $record_id), ]
print(duplicated_values)

#dropped at week6
df_wide2<- df_wide2%>%
  mutate(week6_censor = ifelse(rowSums(.[,c(4:5)]==0)>1, 'yes', 'no'))
df_wide2<- df_wide2%>%
  mutate(week6_censor = ifelse(rowSums(.[,c("weeks14","months6")]==0)>1, 'yes', 'no'))
dropped_wk6<- df_wide2%>%
  subset(week6_censor=='yes')

#dropped at week14
df_wide_14wks<- df_wide2%>%
  subset(weeks14=='1')
df_wide_14wks<- df_wide_14wks%>%
  mutate(week14_censor = ifelse(df_wide_14wks$months6==0, 'yes', 'no'))
df_wide_14wks1<- df_wide_14wks%>%
  subset(week14_censor=='yes')

#dropped at month6
dropped_month6<-df_wide2%>%
  subset(months6=='1')

#came for all visits but did not initiate
df_month9<- df_wide%>%
  subset(total=='4')

#maternal deaths
mat<-fpdata_sae_ltfp%>%
  filter(sae_maternal_death_cof=="1")
mat1<-mat%>%
  select(record_id,clt_ptid,clt_visit,sae_maternal_death_cof)

#molar preg
molar<-fpdata_sae_ltfp%>%
  filter(sae_molarpreg_cof=="1")

molar1<-molar%>%
  select(record_id,clt_ptid,clt_visit,sae_molarpreg_cof)

table(fpdata_sae_ltfp$clt_visit,fpdata_sae_ltfp$sae_maternal_death_cof)
maternal_death<-fpdata_sae_ltfp%>%
  filter(sae_maternal_death_cof=="1")
maternal_death<-maternal_death%>%
  group_by(record_id) %>%
  summarise(first_maternal_death= clt_visit[which.max(sae_maternal_death_cof)])
summarytools::freq(maternal_death$first_maternal_death)

#According to study visits  
Baseline<-fpdata_sae_ltfp%>%
  filter(redcap_event_name =="enrollment_arm_1"|redcap_event_name=="enrollment_arm_2")

#pregnancy intention
#edit sa_pregcircumstothsp
table(fpdata_sae_ltfp$sa_pregcircumst, fpdata_sae_ltfp$redcap_event_name)
fpdata_sae_ltfp <-fpdata_sae_ltfp %>%
  mutate(other_pregcircumst=sa_pregcircumstothsp)

#replace empty cells in other_pregcircumst with 999
fpdata_sae_ltfp$other_pregcircumst <- replace(fpdata_sae_ltfp$other_pregcircumst, fpdata_sae_ltfp$other_pregcircumst == "", 999)

#replace 'child died' with NA, 999 with NA and anything else with No
fpdata_sae_ltfp$other_pregcircumst<- ifelse(fpdata_sae_ltfp$other_pregcircumst == "Child died", NA,
                                            ifelse(fpdata_sae_ltfp$other_pregcircumst == 999, NA, 
                                                   ifelse(is.na(fpdata_sae_ltfp$other_pregcircumst), NA, "No")))


table(fpdata$sa_pregcircumst)
table(fpdata_sae_ltfp$sa_pregcircumst)
table(fpdata_sae_ltfp$other_pregcircumst)

#Pregnancy intention

#new dataframe with blank preg intention
livebirth <- fpdata_sae_ltfp %>% group_by(record_id) %>% tally() %>% select(record_id)
livebirth$sa_pregcircumst <- NA
livebirth$sa_pregcircumstothsp <- NA


## pts with data at six weeks
six_wk_pp <- fpdata_sae_ltfp %>% 
  filter(redcap_event_name=="fup_6_wk_pp_arm_1"|redcap_event_name=="fup_6_wk_pp_arm_2")
temp <- six_wk_pp %>% select(record_id, sa_pregcircumst, sa_pregcircumstothsp) #just find record id and answer to preg intention q
temp <- temp %>% filter(!is.na(sa_pregcircumst)) #2290 women answered at 6 weeks
livebirth <- left_join(livebirth, temp, by=c("record_id")) %>% 
  mutate(sa_pregcircumst = coalesce(sa_pregcircumst.x, sa_pregcircumst.y),
         sa_pregcircumstothsp = coalesce(sa_pregcircumstothsp.x, sa_pregcircumstothsp.y)) %>% 
  select(record_id, sa_pregcircumst, sa_pregcircumstothsp)
table(livebirth$sa_pregcircumst, useNA="always")

## pts with missing at 6 weeks, but answered at 14 weeks
still_missing <- livebirth %>% filter(is.na(sa_pregcircumst))
fortin_wk_pp <- fpdata_sae_ltfp %>% 
  filter(redcap_event_name=="fup_14_wk_pp_arm_1"|redcap_event_name=="fup_14_wks_pp_arm_2") %>% 
  filter(record_id %in% still_missing$record_id)
temp <- fortin_wk_pp %>% select(record_id, sa_pregcircumst, sa_pregcircumstothsp) #just find record id and answer to preg intention q
temp <- temp %>% filter(!is.na(sa_pregcircumst)) #2290 women answered at 6 weeks
livebirth <- left_join(livebirth, temp, by=c("record_id")) %>% 
  mutate(sa_pregcircumst = coalesce(sa_pregcircumst.x, sa_pregcircumst.y),
         sa_pregcircumstothsp = coalesce(sa_pregcircumstothsp.x, sa_pregcircumstothsp.y)) %>% 
  select(record_id, sa_pregcircumst, sa_pregcircumstothsp)
table(livebirth$sa_pregcircumst, useNA="always")

## pts with missing at 6 weeks and 14 weeks, but answered at 6 months
still_missing <- livebirth %>% filter(is.na(sa_pregcircumst))
six_mo_pp <- fpdata_sae_ltfp %>% 
  filter(redcap_event_name=="fup_6_mo_pp_arm_1"|redcap_event_name=="fup_6_mo_pp_arm_2") %>% 
  filter(record_id %in% still_missing$record_id)
temp <- six_mo_pp %>% select(record_id, sa_pregcircumst, sa_pregcircumstothsp) #just find record id and answer to preg intention q
temp <- temp %>% filter(!is.na(sa_pregcircumst)) #2290 women answered at 6 weeks
livebirth <- left_join(livebirth, temp, by=c("record_id")) %>% 
  mutate(sa_pregcircumst = coalesce(sa_pregcircumst.x, sa_pregcircumst.y),
         sa_pregcircumstothsp = coalesce(sa_pregcircumstothsp.x, sa_pregcircumstothsp.y)) %>% 
  select(record_id, sa_pregcircumst, sa_pregcircumstothsp)
table(livebirth$sa_pregcircumst, useNA="always")

## pts with missing at 6 weeks and 14 weeks and 6 months, but answered at 9 months
still_missing <- livebirth %>% filter(is.na(sa_pregcircumst))
nine_mo_pp <- fpdata_sae_ltfp %>% 
  filter(redcap_event_name=="fup_9_mo_pp_arm_2"|redcap_event_name=="fup9_mo_pp_arm_1") %>% 
  filter(record_id %in% still_missing$record_id)
temp <- nine_mo_pp %>% select(record_id, sa_pregcircumst, sa_pregcircumstothsp) #just find record id and answer to preg intention q
temp <- temp %>% filter(!is.na(sa_pregcircumst)) #2290 women answered at 6 weeks
livebirth <- left_join(livebirth, temp, by=c("record_id")) %>% 
  mutate(sa_pregcircumst = coalesce(sa_pregcircumst.x, sa_pregcircumst.y),
         sa_pregcircumstothsp = coalesce(sa_pregcircumstothsp.x, sa_pregcircumstothsp.y)) %>% 
  select(record_id, sa_pregcircumst, sa_pregcircumstothsp)
table(livebirth$sa_pregcircumst, useNA="always")

table(livebirth$sa_pregcircumstothsp)

livebirth <- livebirth %>% 
  mutate(sa_pregcircumst = ifelse(sa_pregcircumst==99, 5, sa_pregcircumst))
table(livebirth$sa_pregcircumst)
livebirth$sa_pregcircumst<-as.factor(livebirth$sa_pregcircumst)
livebirth<-livebirth%>% 
  mutate(intention_pregnancy=recode(sa_pregcircumst,
                                    "1"="Intended", 
                                    "2"="Ambivalent",
                                    "3"="Ambivalent",
                                    "4"="Ambivalent",
                                    "5"="No"))
table(livebirth$preg_intent)

#leftjoin the pregnancy intention
livebirth_clean <- livebirth %>% select(record_id, intention_pregnancy)
fpdata_sae_ltfp<-fpdata_sae_ltfp%>%
  left_join((livebirth_clean), by='record_id')
Baseline<-Baseline%>%
  left_join((livebirth_clean), by='record_id')


###merge total_combine_ord with baseline
summarytools::freq(Baseline$intention_pregnancy)

fpdata_sae_ltfp$age_enr_cof<-round(fpdata_sae_ltfp$age_enr_cof, digits = 1)
table(fpdata_sae_ltfp$redcap_event_name)

six_wk_pp <- fpdata_sae_ltfp %>% 
  filter(redcap_event_name=="fup_6_wk_pp_arm_1"|redcap_event_name=="fup_6_wk_pp_arm_2")
fortin_wk_pp <- fpdata_sae_ltfp %>% 
  filter(redcap_event_name=="fup_14_wk_pp_arm_1"|redcap_event_name=="fup_14_wks_pp_arm_2")
six_mnth_pp<- fpdata_sae_ltfp%>% 
  filter(redcap_event_name=="fup_6_mo_pp_arm_1"|redcap_event_name=="fup_6_mo_pp_arm_2")
nine_mnth_fup <- fpdata_sae_ltfp%>% 
  filter(redcap_event_name=="fup9_mo_pp_arm_1"|redcap_event_name=="fup_9_mo_pp_arm_2")
trans_to_pnc <- fpdata_sae_ltfp%>% 
  filter(redcap_event_name=="transition_to_pnc_arm_1"|redcap_event_name=="transition_to_pnc_arm_2")

#table1
fpdata_sae_ltfp$age_enr_cof<-round(fpdata_sae_ltfp$age_enr_cof, digits = 1)
table(fpdata_sae_ltfp$redcap_event_name, fpdata_sae_ltfp$sa_resumesex)
table(fpdata_sae_ltfp$modern_fp, fpdata_sae_ltfp$redcap_event_name)
table(fpdata_sae_ltfp$Facility)

write_xlsx(Baseline, "C:/Users/integ/Dropbox/Dama/Longitudinal/cofactors_baseline.xlsx")
###BEGIN ANALYSIS
missing_age<-Baseline%>%
  filter(is.na(age_groups_cof))

summary(Baseline$age_enr_cof)
summarytools::freq(Baseline$age_groups_cof)
summarytools::freq(Baseline$participant_education1)
summarytools::freq(Baseline$highrisk_nascop_enr_cof)
summarytools::freq(Baseline$highrisk_pintye_gt6_enr_cof)
summarytools::freq(Baseline$employment_enr_cof)
summarytools::freq(Baseline$mos_sss_lt72_enr_cof)
summarytools::freq(Baseline$hits_gt10_enr_cof)
summarytools::freq(Baseline$phq2_gt3_enr_cof)

summarytools::freq(Baseline$total_anc_attended_visits_gt4_cof)
summarytools::freq(Baseline$parity)
##number of living children
living_child<-Baseline%>%filter(parity=="Multiparous")
summarytools::freq(living_child$number_living)
summarytools::freq(living_child$male_child)
summarytools::freq(Baseline$hx_miscarriage)
summarytools::freq(Baseline$hx_stillbirth)

liv_age<-living_child%>%filter(number_living!="No living child")
summarytools::freq(living_child$childage_groups)
summarytools::freq(Baseline$intention_pregnancy)
#summarytools::freq(Baseline$pre_pregnant_enr_cof)

summarytools::freq(Baseline$pc_current)
part<-Baseline%>%filter(pc_current=="Yes")
summary(part$age_of_partner)
summarytools::freq(part$pc_hiv)
summarytools::freq(Baseline$realpc_education)
summarytools::freq(Baseline$realpc_residence)
summarytools::freq(Baseline$realpc_financial)


summarytools::freq(Baseline$sa_resume_time)

median_age <- Baseline %>%
  filter(parity=="Multiparous")
summary(median_age$real_age)
summary(Baseline$first_response_time)


table(Baseline$pc_current, Baseline$pc_financial)

#Family planning methods
implant <- fpdata_sae_ltfp %>% 
  filter(sa_nonpermbc___4=="1")
implant<-implant %>%
  group_by(record_id) %>%
  summarise(first_imp= clt_visit[which.max(sa_nonpermbc___4)])
summarytools::freq(implant$first_imp)

#Condoms
condoms <- fpdata_sae_ltfp %>% 
  filter(sa_nonpermbc___6=="1")
condoms<-condoms %>%
  group_by(record_id) %>%
  summarise(first_condom= clt_visit[which.max(sa_nonpermbc___6)])
summarytools::freq(condoms$first_condom)

#Injectables
injectables <- fpdata_sae_ltfp %>% 
  filter(sa_nonpermbc___2=="1")
injectables<-injectables %>%
  group_by(record_id) %>%
  summarise(first_injectable= clt_visit[which.max(sa_nonpermbc___2)])
summarytools::freq(injectables$first_injectable)

#oral contraception
ocps <- fpdata_sae_ltfp %>% 
  filter(sa_nonpermbc___1=="1")
ocps <- ocps %>%
  group_by(record_id) %>%
  summarise(first_ocp= clt_visit[which.max(sa_nonpermbc___1)])
summarytools::freq(ocps$first_ocp)

#IUCD
iucd <- fpdata_sae_ltfp %>% 
  filter(sa_nonpermbc___3=="1")
iucd <- iucd %>%
  group_by(record_id) %>%
  summarise(first_iucd= clt_visit[which.max(sa_nonpermbc___3)])
summarytools::freq(iucd$first_iucd)


#ever taken modern FP
modern <- fpdata_sae_ltfp %>% 
  filter(modern_fp=="1")
modern<-modern %>%
  group_by(record_id) %>%
  summarise(first_FP= clt_visit[which.max(modern_fp)])
summarytools::freq(modern$first_FP)


fpdata_sae_ltfp$clt_attended<-as.numeric(fpdata_sae_ltfp$clt_attended)
onefp <- fpdata_sae_ltfp %>%
  filter(clt_attended == "1")
onefp <- onefp %>%
  group_by(record_id) %>%
  summarise(first_onefp = clt_visit[which.max(clt_attended)])
summarytools::freq(onefp$first_onefp)

table(modern$first_FP)
modern<-modern%>%
  mutate(status_FP=1)

#resume sex

table(fpdata_sae_ltfp$sa_resumesex)
resume <- fpdata_sae_ltfp%>% 
  filter(sa_resumesex=="1")
resume<-resume %>%
  group_by(record_id) %>%
  summarise(first_resume = clt_visit[which.max(sa_resumesex)]) 
summarytools::freq(resume$first_resume)

summary(resume$first_resume)

#modern1<-fpdata%>%
#select(record_id,clt_visit,modern_fp)

modern_c<-fpdata_sae_ltfp%>%
  select(record_id,clt_visit,modern_fp,sa_resumesex, sa_nonpermbc___7,
         sa_nonpermbc___8, sa_nonpermbc___9, sa_nonpermbc___10, 
         sa_nonpermbc___11, sa_nonpermbc___12, sa_nonpermbc___13)

table(modern_c$clt_visit)
table(modern$first_FP)

#merge datasets
modernfp_kaplan<-modern_c %>%
  left_join((modern),by='record_id')

table(modernfp_kaplan$clt_visit)
table(modernfp_kaplan$status_FP)

#replace NA in status_FP with zero
modernfp_kaplan <- modernfp_kaplan %>%
  mutate(status_FP=ifelse(is.na(status_FP),0,status_FP))

modernfp_kaplan<-modernfp_kaplan %>%
  mutate(visit=case_when(status_FP==modern_fp~'1',
                         status_FP!=modern_fp~'0'))
table(modernfp_kaplan$visit)
class(modernfp_kaplan$clt_visit)
table(modernfp_kaplan$clt_visit)

class(modernfp_kaplan$first_FP)
modernfp_kaplan$clt_visit<-as.numeric(as.character(modernfp_kaplan$clt_visit))
modernfp_kaplan$first_FP<-as.numeric(as.character(modernfp_kaplan$first_FP))

modernfp_kaplan<-modernfp_kaplan %>%
  mutate(censored_fp=case_when(status_FP=='1'&modern_fp!='1'~'0',
                               status_FP=='1'&modern_fp=='1'~'1'))

modernfp_kaplan<-modernfp_kaplan %>%
  mutate(censored_fp=case_when(status_FP=='1'&modern_fp!='1'~'0',
                               status_FP=='1'&modern_fp=='1'~'1'))

modernfp_kaplan$visit<-ifelse(modernfp_kaplan$clt_visit==modernfp_kaplan$first_FP,"Yes","No")
table(modernfp_kaplan$visit)

#create fp use
modernfp_kaplan <- modernfp_kaplan %>%
  mutate(fp_use=case_when(censored_fp=='1'& visit=='Yes'~'1',
                          censored_fp!='1'& visit=='Yes'~'0',
                          censored_fp!='1'& visit!='Yes'~'0',
                          censored_fp=='1'& visit!='Yes'~'0'))

#replace na with zero
modernfp_kaplan <- modernfp_kaplan %>%
  mutate(fp_use=ifelse(is.na(fp_use),0,fp_use))

table(modernfp_kaplan$clt_visit)
table(modernfp_kaplan$fp_use)
class(modernfp_kaplan$fp_use)

##order data
#change fp_use from character to numeric to be able to order and drop duplicates
class(modernfp_kaplan$fp_use)
class(modernfp_kaplan$modern_fp)


# kaplan_ord<- modernfp_kaplan[order(modernfp_kaplan[,'record_id'],-modernfp_kaplan[,'fp_use']),]
kaplan_ord <- modernfp_kaplan %>%
  arrange(record_id, desc(fp_use))
table(kaplan_ord $clt_visit)
kaplan_order<-kaplan_ord[ !duplicated(kaplan_ord$record_id), ]
table(kaplan_order$clt_visit)

table(kaplan_ord$first_FP,kaplan_ord$fp_use)
library(survival)

#convert first_FP to numeric
kaplan_order$first_FP<-as.numeric(kaplan_order$first_FP)
table(kaplan_order$first_FP)


#length(kaplan_order$clt_visit)
#Surv(kaplan_order$clt_visit,kaplan_order$fp_use)
##survfit(Surv(kaplan_order$clt_visit,kaplan_order$fp_use)~1)
#summary(survfit(Surv(kaplan_order$clt_visit,kaplan_order$fp_use)~1))
#fit2<-plot(survfit(Surv(kaplan_order$clt_visit,as.numeric(kaplan_order$fp_use))~0,conf.type="none"),xlab="Visit",xlim=c(100,105, by=100),ylab="FP uptake")
#fit2

# adding suffix _1 to column names in kaplan_order to make them diffrent
colnames(kaplan_order) <- paste(colnames(kaplan_order),"ord",sep="_")
kaplan_order<-kaplan_order%>%
  rename("record_id"="record_id_ord")
kaplan_order<-kaplan_order%>%
  rename("fp_use"="fp_use_ord")


### *repeat pregnancy*
modern_preg<-fpdata%>%
  select(record_id,clt_visit,modern_fp)
table(modern_c$clt_visit)
table(modern$first_FP)

#merge datasets
modernfp_kaplan_preg<-modern_preg %>%
  left_join((modern),by='record_id')


#replace NA in status_FP with zero
modernfp_kaplan_preg <- modernfp_kaplan_preg %>%
  mutate(status_FP=ifelse(is.na(status_FP),0,status_FP))

modernfp_kaplan_preg<-modernfp_kaplan_preg %>%
  mutate(visit=case_when(status_FP==modern_fp~'1',
                         status_FP!=modern_fp~'0'))

modernfp_kaplan_preg$clt_visit<-as.numeric(as.character(modernfp_kaplan_preg$clt_visit))
modernfp_kaplan_preg$first_FP<-as.numeric(as.character(modernfp_kaplan_preg$first_FP))

modernfp_kaplan_preg<-modernfp_kaplan_preg %>%
  mutate(censored_fp=case_when(status_FP=='1'&modern_fp!='1'~'0',
                               status_FP=='1'&modern_fp=='1'~'1'))

modernfp_kaplan_preg<-modernfp_kaplan_preg %>%
  mutate(censored_fp=case_when(status_FP=='1'&modern_fp!='1'~'0',
                               status_FP=='1'&modern_fp=='1'~'1'))

modernfp_kaplan_preg$visit<-ifelse(modernfp_kaplan_preg$clt_visit==modernfp_kaplan_preg$first_FP,"Yes","No")


#create fp use
modernfp_kaplan_preg <- modernfp_kaplan_preg %>%
  mutate(fp_use=case_when(censored_fp=='1'& visit=='Yes'~'1',
                          censored_fp!='1'& visit=='Yes'~'0',
                          censored_fp!='1'& visit!='Yes'~'0',
                          censored_fp=='1'& visit!='Yes'~'0'))

#replace na with zero
modernfp_kaplan_preg <- modernfp_kaplan_preg %>%
  mutate(fp_use=ifelse(is.na(fp_use),0,fp_use))


##order data
kaplan_ord_preg <- modernfp_kaplan_preg %>%
  arrange(record_id, desc(fp_use))
kaplan_ord_preg<-kaplan_ord_preg[ !duplicated(kaplan_ord_preg$record_id), ]

#convert first_FP to numeric
kaplan_ord_preg$first_FP<-as.numeric(kaplan_ord_preg$first_FP)


colnames(kaplan_ord_preg) <- paste(colnames(kaplan_ord_preg),"ord",sep="_")
kaplan_ord_preg<-kaplan_ord_preg%>%
  rename("record_id"="record_id_ord")
kaplan_ord_preg<-kaplan_ord_preg%>%
  rename("fp_use"="fp_use_ord")


table(fpdata$repeat_preg)

repeat_preg_1<-fpdata|>
  dplyr::select(record_id,clt_visit,repeat_preg)|>
  filter(repeat_preg==1)|>
  rename(clt_visit_preg=clt_visit)

summarytools::freq(repeat_preg_1$repeat_preg)

modern_fp_1<-kaplan_ord_preg|>
  dplyr::select(record_id,first_FP_ord,fp_use)



modern_fp_preg<-modern_fp_1|>left_join(repeat_preg_1, by=c("record_id"))|>
  mutate(fp_preg=case_when(fp_use=="1"&repeat_preg=="1"~"1",
                           fp_use=="1"&repeat_preg=="0"~"0",
                           fp_use=="0"&repeat_preg=="1"~"0",
                           fp_use=="0"&repeat_preg=="0"~"0"))
summarytools::freq(modern_fp_preg$repeat_preg)
summarytools::freq(modern_fp_preg$first_FP_ord)
summarytools::freq(modern_fp_preg$fp_preg)

modern_fp_preg_1<-modern_fp_preg|>
  filter(fp_preg=="1")
duplicated_fp_reg <- modern_fp_preg_1[duplicated(modern_fp_preg_1$record_id), ]

###person time
survival_data_fp_time<-survival_data_fp|>
  select(record_id, visit1)
table(survival_data_fp_time$visit1)
survival_data_fp_time$visit1<-as.numeric(survival_data_fp_time$visit1)

#write_xlsx(survival_data_fp_time, "/Users/damariskimonge/Dropbox/Dama/Longitudinal/preg data.xlsx")

b<-sum(survival_data_fp_time$visit1)

###check if time pregnant is later than time reported fp use

preg_fp_check<-nancy_prima_longitudinal_071122|>
  filter(record_id==13747057309)|>
  dplyr::select(record_id,redcap_event_name,io_secondpreg,modern_fp)

#write_xlsx(modern_fp_preg_1,"/Users/damariskimonge/Dropbox/Dama/Longitudinal/modern_fp_preg_1.xlsx")

class(Baseline$clt_visit)
Baseline$clt_visit<-as.numeric(as.character(Baseline$clt_visit))

survival_data<-Baseline%>%
  left_join((kaplan_order), by='record_id')

table(kaplan_order$clt_visit_ord,kaplan_order$fp_use)
#survival_data_fp<-survival_data%>%
#filter(fp_use=='1')

library(ggpubr)
library(survminer)

# Fit survival data using the Kaplan-Meier method
#1.Outcome variable
#rename clt_visit.y to Visit
survival_data_fp<-survival_data%>%
  rename("visit_clean"="clt_visit_ord")
table(survival_data_fp$visit_clean)

table(survival_data_fp$repeat_preg)
glimpse(survival_data_fp)
#recode
survival_data_fp$visit_clean<-as.factor(survival_data_fp$visit_clean)
survival_data_fp<-survival_data_fp%>%
  mutate(visit1=recode(visit_clean,
                       "101"="6",
                       "102"="14",
                       "103"="24",
                       "104"="36"))
table(survival_data_fp$visit1)
table(survival_data_fp$visit_clean)
class(survival_data_fp$visit1)
class(survival_data_fp$fp_use)


#visit 6 censoring
table(survival_data_fp$visit1,survival_data_fp$fp_use)
survival_data_fp$visit1 <- as.factor(survival_data_fp$visit1)
survival_data_fp$visit1[survival_data_fp$record_id %in% c(13476002909,13608172409,13608172809,13608173209,13653213009,
                                                          13668252109,13668261209,13668267409,13747062709,13953517409,
                                                          13953519909,14022609509,14022613609,14022619309,14022623709,
                                                          14036668509,14036673609,14063720309,14063720509,14080771709,
                                                          14130800509,14164854709,14164856109,14164856409,14164856809,
                                                          14164861809,14164864609,14165905109,14175970609)& survival_data_fp$fp_use == 0] <- 6

table(survival_data_fp$visit1,survival_data_fp$fp_use)

#week14 censoring
survival_data_fp$visit1[survival_data_fp$record_id %in% c(13507107409,13608171509,13608173309,13608174709,13608174809,
                                                          13608175009,13747052209,13747069009,13841470309,13953518409,
                                                          14022609609,14022622709,14022623009,14036654809,14036671809,
                                                          14036673009,14036674509,14063715009,14063715309,14063719909,
                                                          14063720809,14063721009,14063721209,14063721309,14080769409,
                                                          14130800109,14164850809,14164855009,14164856309,14164857009,
                                                          14164867609,14175970209,14175971209)& survival_data_fp$fp_use == 0] <- 14
table(survival_data_fp$visit1,survival_data_fp$fp_use)


#month6 censoring
survival_data_fp$visit1[survival_data_fp$record_id %in% c(13507122909,13608168309,13608171409,13608172209,13608172609,
                                                          13608174109,13668262909,13668268109,13668270209,13668270509,
                                                          13668270809,13668272709,13668272809,13668273709,13760307809,
                                                          13760311409,13798412809,13798415209,13841470509,13987563809,
                                                          13987568409,13987572609,14022623409,14036670509,14036671709,
                                                          14036672209,14036672709,14063719509,14063719709,14080759809,
                                                          14080761009,14130804209,14164857809,14164859109,14164870209,
                                                          14175961209,14175966609,14175969609,14175970309,14175972209)& survival_data_fp$fp_use == 0] <- 24

table(survival_data_fp$visit1,survival_data_fp$fp_use)


#fp non-modern methods by visit
table(survival_data_fp$visit1,survival_data_fp$sa_nonpermbc___7_ord)#Abstinence
table(survival_data_fp$visit1,survival_data_fp$sa_nonpermbc___8_ord) #lactation (lactational amenorrhea method;LAM)
table(survival_data_fp$visit1,survival_data_fp$sa_nonpermbc___9_ord)#Herbal methods
table(survival_data_fp$visit1,survival_data_fp$sa_nonpermbc___10_ord)#Standard days method (periodic abstinence)
table(survival_data_fp$visit1,survival_data_fp$sa_nonpermbc___11_ord)#Withdrawal
table(survival_data_fp$visit1,survival_data_fp$sa_nonpermbc___12_ord)#Rhythm/calendar method
table(survival_data_fp$visit1,survival_data_fp$sa_nonpermbc___13_ord)#Emergency contraception


##replace 0 (enrolment) values in visit1 with 36 so as to censor at month9
#participants who came for all the visits but did not initiate fp use
table(survival_data_fp$visit1)
survival_data_fp$visit1[survival_data_fp$visit1 == 0] <- 36
table(survival_data_fp$visit1,survival_data_fp$fp_use)
table(survival_data_fp$fp_use)

table(survival_data_fp$Facility)

summary(survival_data_fp$visit1)

###median time test
fp_resume<-survival_data_fp|>
  select(record_id,first_response_time,fp_use,visit1,sa_resume_time)|>
  mutate(visit2=visit1)|>
  filter(!is.na(first_response_time))
fp_resume$visit2<-as.numeric(as.character(fp_resume$visit2))
summary(fp_resume$visit2)

fp_resume_1<-fp_resume|>
  filter(fp_use=="1")


table(fp_resume$visit2,fp_resume$fp_use)

summarytools::freq(fp_resume_1$visit2)

table(survival_data_fp$first_response_time)

summarytools::freq(survival_data_fp$fp_use)



#K-M and cox regression analysis
table(survival_data_fp$visit1)
survival_data_fp$visit1<-as.numeric(as.character(survival_data_fp$visit1))
survival_data_fp$fp_use<-as.numeric(as.character(survival_data_fp$fp_use))
surv_fp1<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_fp1
#summary
fp1 <- survfit(surv_fp1 ~ 1, data = survival_data_fp)
summary(fp1)
#survival_data_fp2<-data.frame(survival_data_fp2)
#plot
survival_data_fp_cens<-survival_data_fp%>%
  filter(record_id=='14175970609')

ggsurvplot(fp1, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette ="turquoise4", size=1,
           xlim=c(0,40),break.x.by=6,
           surv.median.line = "hv", conf.type="plain",conf.int = 0.95,
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Family planning use",
           xlab = "Time to FP (weeks)", size=1,fun = "event",
           ylab="Probability", pval = FALSE,pval.coord = c(10, 0.03),fontsize=3,
           censor.shape = "O", censor.size = 5)

#1.continuous age
surv_age_cont <- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_age_cont
#summary
cox_fit_age_cont <- coxph(surv_age_cont ~ age_enr_cof, data = survival_data_fp)
summary(cox_fit_age_cont)
cox_fit_age_cont_clus <- coxph(surv_age_cont ~ age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_age_cont_clus)

#2.age groups
#rename age groups
survival_data_fp<-survival_data_fp%>%
  rename(Age=age_groups_cof)

surv_age <- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_age
#summary
fit_age <- survfit(surv_age ~ Age, data = survival_data_fp)
summary(fit_age)

table(survival_data_fp$Age)
survival_data_fp$Age <- factor(survival_data_fp$Age, levels = c("15 - 19years",
                                                                "20 - 24years",
                                                                "More than 24 years"))
survival_data_fp$Age = relevel(survival_data_fp$Age, ref = "More than 24 years")

cox_fit_age <- coxph(surv_age ~ Age, data = survival_data_fp)
summary(cox_fit_age)
cox_fit_age_clus <- coxph(surv_age ~ Age+cluster(Facility), data = survival_data_fp)
summary(cox_fit_age_clus)

#plot
names(fit_age$strata) <- gsub("Age=", "", names(fit_age$strata))
ggsurvplot(fit_age, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800","red"), size=1,
           xlim=c(0,40),break.x.by=6,
           surv.median.line = "hv", conf.type="plain",conf.int = 0.95,
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Age",
           xlab = "Time to FP (weeks)", size=1,fun = "event",
           ylab="Probability", pval = TRUE,pval.coord = c(10, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)


#3.Marital relationship
#rename marital relationship
survival_data_fp<-survival_data_fp%>%
  rename(Marital_relationship=mar_relationship_cof)

surv_mar <- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_mar
#summary
fit_mar <- survfit(surv_mar ~ Marital_relationship, data = survival_data_fp)
summary(fit_mar)
summarytools::freq(survival_data_fp$Marital_relationship)

#plot
names(fit_mar$strata) <- gsub("Marital_relationship=", "", names(fit_mar$strata))
ggsurvplot(fit_mar, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800","red"), size=1,
           xlim=c(0,40),break.x.by=6,
           surv.median.line = "hv", conf.type="plain",conf.int = 0.95,
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Relationship",
           xlab = "Time to FP (weeks)", size=1,fun = "event",
           ylab="Probability", pval = TRUE,pval.coord = c(10, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

#cox regression
table(survival_data_fp$Marital_relationship)
survival_data_fp$Marital_relationship = relevel(survival_data_fp$Marital_relationship, ref = "Steady relationship")
cox_fit_mar <- coxph(surv_mar ~ Marital_relationship, data = survival_data_fp)
summary(cox_fit_mar)
cox_fit_mar_clus <- coxph(surv_mar ~ Marital_relationship+cluster(Facility), data = survival_data_fp)
summary(cox_fit_mar_clus)

#SENSITIVITY ANALYSIS
#Adjusting for having a partner at this time as a mediator
cox_fit_mar <- coxph(surv_mar ~ Marital_relationship*pc_current, data = survival_data_fp)
summary(cox_fit_mar)

#Continuation
#Adjusting for age as a confounder
# cox_fit_mar_adj <- coxph(surv_mar ~ Marital_relationship+age_enr_cof, data = survival_data_fp)
# summary(cox_fit_mar_adj)
cox_fit_mar_adj_clus <- coxph(surv_mar ~ Marital_relationship+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_mar_adj_clus)

cox_fit_mar <- coxph(surv_mar ~ Marital_relationship*pc_residence_enr_cof, data = survival_data_fp)
summary(cox_fit_mar)

#4.had partner
#rename employed variable
survival_data_fp<-survival_data_fp%>%
  rename(Had_partner=pc_current)

surv_part <- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_part
#summary
fit_part <- survfit(surv_part ~ Had_partner, data = survival_data_fp)
summary(fit_part)

#plot
names(fit_part$strata) <- gsub("Had_partner=", "", names(fit_part$strata))
ggsurvplot(fit_part, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor", size = 1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Had partner",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=4,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$Had_partner)
table(survival_data_fp$Had_partner)
survival_data_fp$Had_partner = relevel(survival_data_fp$Had_partner, ref = "Yes")

cox_fit_part <- coxph(surv_part ~ Had_partner, data = survival_data_fp)
summary(cox_fit_part)
cox_fit_part_clus <- coxph(surv_part ~ Had_partner+cluster(Facility), data = survival_data_fp)
summary(cox_fit_part_clus)
#Adjusting for age as a confounder
cox_fit_part_adj <- coxph(surv_part ~ Had_partner+age_enr_cof, data = survival_data_fp)
summary(cox_fit_part_adj)
cox_fit_part_adj_clus <- coxph(surv_part ~ Had_partner+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_part_adj_clus)


#5. Residing with partner
survival_data_fp<-survival_data_fp%>%
  rename(Reside_partner=realpc_residence)

surv_res <- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_res
#summary
fit_res <- survfit(surv_res ~ Reside_partner, data = survival_data_fp)
summary(fit_res)



#plot
names(fit_res$strata) <- gsub("residing_with_partner=", "", names(fit_res$strata))
ggsurvplot(fit_res, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor", size = 1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Resided with partner",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

# class(had_partner_overall$residing_with_partner)
# had_partner_overall$residing_with_partner<-as.factor(had_partner_overall$residing_with_partner)
# table(had_partner_overall$residing_with_partner)
table(survival_data_fp$Reside_partner)
survival_data_fp$Reside_partner <- as.factor(survival_data_fp$Reside_partner)
survival_data_fp$Reside_partner = relevel(survival_data_fp$Reside_partner, ref = "Yes")

# had_partner_overall$residing_with_partner = relevel(had_partner_overall$residing_with_partner, ref = "Yes")
cox_fit_res  <- coxph(surv_res ~ Reside_partner, data = survival_data_fp)
summary(cox_fit_res)
cox_fit_res_adj_clus  <- coxph(surv_res ~ Reside_partner+cluster(Facility), data = survival_data_fp)
summary(cox_fit_res_adj_clus)
#Adjusting for age as a confounder
cox_fit_res_adj <- coxph(surv_res ~ Reside_partner+age_enr_cof, data = survival_data_fp)
summary(cox_fit_res_adj)
cox_fit_res_adj_clus <- coxph(surv_res ~ Reside_partner+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_res_adj_clus)

#6.Education level
surv_edu <- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_edu
#summary
fit_edu <- survfit(surv_edu ~ participant_education1, data = survival_data_fp)
summary(fit_edu)

#plot
names(fit_edu$strata) <- gsub("participant_education1=", "", names(fit_edu$strata))
ggsurvplot(fit_edu, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Education level",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$participant_education1)
table(survival_data_fp$participant_education1)

survival_data_fp$participant_education1 = relevel(survival_data_fp$participant_education1, ref = "Completed basic education")

cox_fit_edu <- coxph(surv_edu ~ participant_education1, data = survival_data_fp)
summary(cox_fit_edu)
cox_fit_edu_clus <- coxph(surv_edu ~ participant_education1+cluster(Facility), data = survival_data_fp)
summary(cox_fit_edu_clus)
#Adjusting for age as a confounder
cox_fit_edu_adj <- coxph(surv_edu ~ participant_education1+age_enr_cof, data = survival_data_fp)
summary(cox_fit_edu_adj)
cox_fit_edu_adj_clus <- coxph(surv_edu ~ participant_education1+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_edu_adj_clus)

#7.High risk at enrolment by nascop
survival_data_fp<-survival_data_fp%>%
  mutate(HIV_risk=recode(highrisk_nascop_enr_cof,
                         "0"="Low risk",
                         "1"="High risk"))
survival_data_fp<-survival_data_fp%>%
  mutate(HIV_risk_pintye=recode(highrisk_pintye_gt6_enr_cof,
                                "0"="Low risk",
                                "1"="High risk"))
table(survival_data_fp$HIV_risk)
summarytools::freq(survival_data_fp$HIV_risk_pintye)
surv_risk<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_risk
#summary
fit_risk <- survfit(surv_risk ~ HIV_risk, data = survival_data_fp)
summary(fit_risk)

#plot
names(fit_risk$strata) <- gsub("HIV_risk=", "", names(fit_risk$strata))
ggsurvplot(fit_risk, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "HIV risk",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=4,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$HIV_risk)
table(survival_data_fp$HIV_risk)
survival_data_fp$HIV_risk<-as.factor(survival_data_fp$HIV_risk)
survival_data_fp$HIV_risk = relevel(survival_data_fp$HIV_risk, ref = "Low risk")

cox_fit_risk <- coxph(surv_risk ~ HIV_risk, data = survival_data_fp)
summary(cox_fit_risk)
cox_fit_risk_clus <- coxph(surv_risk ~ HIV_risk+cluster(Facility), data = survival_data_fp)
summary(cox_fit_risk_clus)

#Adjusting for age as a confounder
cox_fit_risk_adj <- coxph(surv_risk ~ HIV_risk+age_enr_cof, data = survival_data_fp)
summary(cox_fit_risk_adj)
cox_fit_risk_adj_clus <- coxph(surv_risk ~ HIV_risk+age_enr_cof, data = survival_data_fp)
summary(cox_fit_risk_adj_clus)

#8.High risk at enrolment by Pintye
surv_riskpint<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_riskpint
#summary
fit_riskpint <- survfit(surv_riskpint ~ HIV_risk_pintye, data = survival_data_fp)
summary(fit_riskpint)

#plot
names(fit_riskpint$strata) <- gsub("HIV_risk_pintye=", "", names(fit_riskpint$strata))
ggsurvplot(fit_riskpint, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "HIV risk (Pintye)",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$HIV_risk_pintye)
table(survival_data_fp$HIV_risk_pintye)
survival_data_fp$HIV_risk_pintye<-as.factor(survival_data_fp$HIV_risk_pintye)
survival_data_fp$HIV_risk_pintye = relevel(survival_data_fp$HIV_risk_pintye, ref = "Low risk")

cox_fit_riskpin <- coxph(surv_risk ~ HIV_risk_pintye, data = survival_data_fp)
summary(cox_fit_riskpin)
cox_fit_riskpin_clus <- coxph(surv_risk ~ HIV_risk_pintye+cluster(Facility), data = survival_data_fp)
summary(cox_fit_riskpin_clus)

#Adjusting for age as a confounder
cox_fit_riskpin_adj <- coxph(surv_risk ~ HIV_risk_pintye+age_enr_cof, data = survival_data_fp)
summary(cox_fit_riskpin_adj)
cox_fit_riskpin_adj_clus <- coxph(surv_risk ~ HIV_risk_pintye+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_riskpin_adj_clus)

#9.employed
#rename employed variable
survival_data_fp<-survival_data_fp%>%
  rename(employed=employment_enr_cof)

surv_employ <- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_employ
#summary
fit_employ <- survfit(surv_employ ~ employed, data = survival_data_fp)
summary(fit_employ)

#plot
names(fit_employ$strata) <- gsub("employed=", "", names(fit_employ$strata))
ggsurvplot(fit_employ, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Employment",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$employed)
table(survival_data_fp$employed)
survival_data_fp$employed<-as.factor(survival_data_fp$employed)
survival_data_fp$employed = relevel(survival_data_fp$employed, ref = "No")
cox_fit_employ <- coxph(surv_employ ~ employed, data = survival_data_fp)
summary(cox_fit_employ)
cox_fit_employ_clus <- coxph(surv_employ ~ employed+cluster(Facility), data = survival_data_fp)
summary(cox_fit_employ_clus)

#Adjusting for age as a confounder
cox_fit_employ_adj <- coxph(surv_employ ~ employed+age_enr_cof, data = survival_data_fp)
summary(cox_fit_employ_adj)
cox_fit_employ_adj_clus <- coxph(surv_employ ~ employed+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_employ_adj_clus)

#10. low social support
table(survival_data_fp$mos_sss_lt72_enr_cof)
#rename variable
survival_data_fp<-survival_data_fp%>% 
  rename(low_social=mos_sss_lt72_enr_cof)
surv_low<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_low
#summary
fit_low <- survfit(surv_low ~ low_social, data = survival_data_fp)
summary(fit_low)

#plot
names(fit_low$strata) <- gsub("low_social=", "", names(fit_low$strata))
ggsurvplot(fit_low, low = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Low social support",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

table(survival_data_fp$low_social)
survival_data_fp$low_social = relevel(survival_data_fp$low_social, ref = "High social support")

cox_fit_low  <- coxph(surv_low ~ low_social, data = survival_data_fp)
summary(cox_fit_low)
cox_fit_low_clus  <- coxph(surv_low ~ low_social+cluster(Facility), data = survival_data_fp)
summary(cox_fit_low_clus)


#Adjusting for age as a confounder
cox_fit_low_adj <- coxph(surv_low ~ low_social+age_enr_cof, data = survival_data_fp)
summary(cox_fit_low_adj)
cox_fit_low_adj_clus <- coxph(surv_low ~ low_social+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_low_adj_clus)


#11.IPV
survival_data_fp<-survival_data_fp%>%
  rename(IPV=hits_gt10_enr_cof)

surv_IPV <- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_IPV
#summary
fit_IPV <- survfit(surv_IPV ~ IPV, data = survival_data_fp)
summary(fit_IPV)

#plot
names(fit_IPV$strata) <- gsub("IPV=", "", names(fit_IPV$strata))
ggsurvplot(fit_IPV, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "IPV",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$IPV)
table(survival_data_fp$IPV)
survival_data_fp$IPV = relevel(survival_data_fp$IPV, ref = "No hx of IPV")

cox_fit_IPV <- coxph(surv_IPV ~ IPV, data = survival_data_fp)
summary(cox_fit_IPV)

cox_fit_IPV_clus <- coxph(surv_IPV ~ IPV+cluster(Facility), data = survival_data_fp)

summary(cox_fit_IPV_clus)

#Adjusting for age as a confounder
cox_fit_IPV_adj <- coxph(surv_IPV ~ IPV+age_enr_cof, data = survival_data_fp)
summary(cox_fit_IPV_adj)
cox_fit_IPV_adj_clus <- coxph(surv_IPV ~ IPV+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_IPV_adj_clus)

#12.Depression symptoms (PHQ2) (phq2_gt3_enr)
#rename variable
survival_data_fp<-survival_data_fp%>%
  rename(phq=phq2_gt3_enr_cof)
surv_phq<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_phq
#summary
fit_phq <- survfit(surv_phq ~ phq, data = survival_data_fp)
summary(fit_phq)

class(survival_data_fp$phq)
table(survival_data_fp$phq)
survival_data_fp$phq = relevel(survival_data_fp$phq, ref = "No symptoms of depression")

#plot
names(fit_phq$strata) <- gsub("phq=", "", names(fit_phq$strata))
ggsurvplot(fit_phq, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Depression symptoms",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

cox_fit_phq  <- coxph(surv_phq ~ phq, data = survival_data_fp)
summary(cox_fit_phq)
cox_fit_phq_clus  <- coxph(surv_phq ~ phq+cluster(Facility), data = survival_data_fp)
summary(cox_fit_phq_clus)
#Adjusting for age as a confounder
cox_fit_phq_adj <- coxph(surv_phq ~ phq+age_enr_cof, data = survival_data_fp)
summary(cox_fit_phq_adj)
cox_fit_phq_adj_clus <- coxph(surv_phq ~ phq+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_phq_adj_clus)

#13.parity
table(survival_data_fp$parity)
surv_parity<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_parity
#summary
fit_parity <- survfit(surv_parity ~ parity, data = survival_data_fp)
summary(fit_parity)

#plot
names(fit_parity$strata) <- gsub("parity=", "", names(fit_parity$strata))
ggsurvplot(fit_parity, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Parity",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$parity)
survival_data_fp$parity<-as.factor(survival_data_fp$parity)
table(survival_data_fp$parity)
survival_data_fp$parity = relevel(survival_data_fp$parity, ref = "Multiparous")

cox_fit_parity  <- coxph(surv_parity ~ parity, data = survival_data_fp)
summary(cox_fit_parity)
cox_fit_parity_clus  <- coxph(surv_parity ~ parity+cluster(Facility), data = survival_data_fp)
summary(cox_fit_parity_clus)

#Adjusting for age as a confounder
cox_fit_parity_adj <- coxph(surv_parity ~ parity+age_enr_cof, data = survival_data_fp)
summary(cox_fit_parity_adj)
cox_fit_parity_adj_clus <- coxph(surv_parity ~ parity+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_parity_adj_clus)

#14.Number of living children
survival_data_fp<-survival_data_fp%>%
  rename(Number_Living=number_living)

surv_num <- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_num
#summary
fit_num <- survfit(surv_num ~ Number_Living, data = survival_data_fp)
summary(fit_num)

table(survival_data_fp$Number_Living)
summarytools::freq(survival_data_fp$Number_Living)
survival_data_fp$Number_Living <- factor(survival_data_fp$Number_Living, levels = c("No living child",
                                                                                    "Less than or equal to 4 children",
                                                                                    "More than 4 children"))
survival_data_fp$Number_Living = relevel(survival_data_fp$Number_Living, ref = "More than 4 children")

#plot
names(fit_num$strata) <- gsub("Number_Living=", "", names(fit_num$strata))
ggsurvplot(fit_num, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800","red"), size=1,
           xlim=c(0,40),break.x.by=6,
           surv.median.line = "hv", conf.type="plain",conf.int = 0.95,
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Number of living children",
           xlab = "Time to FP (weeks)", size=1,fun = "event",
           ylab="Probability", pval = TRUE,pval.coord = c(10, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

cox_fit_num <- coxph(surv_num ~ Number_Living, data = survival_data_fp)
summary(cox_fit_num)

cox_fit_num_clus <- coxph(surv_num ~ Number_Living+cluster(Facility), data = survival_data_fp)
summary(cox_fit_num_clus)
#Adjusting for age as a confounder
cox_fit_num_adj <- coxph(surv_num ~ Number_Living+age_enr_cof, data = survival_data_fp)
summary(cox_fit_num_adj)
cox_fit_num_adj_clus <- coxph(surv_num ~ Number_Living+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_num_adj_clus)

#15. history of either miscarriage or stillbirth pre_miscarriage_stillbirth_enr
table(survival_data_fp$pre_miscarriage_stillbirth_enr_cof)
#rename variable
survival_data_fp<-survival_data_fp%>%
  rename(miscarriage_stillbirth=hx_mis_stillbirth)
surv_miss_still<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_miss_still
#summary
fit_miss_still <- survfit(surv_miss_still ~ miscarriage_stillbirth, data = survival_data_fp)
summary(fit_miss_still)

#plot
names(fit_miss_still$strata) <- gsub("miscarriage_stillbirth=", "", names(fit_miss_still$strata))
ggsurvplot(fit_miss_still, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "History of miscarriage/stillbirth",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=4,
           censor.shape = "O", censor.size = 5)
table(survival_data_fp$visit1,survival_data_fp$miscarriage_stillbirth)
class(survival_data_fp$miscarriage_stillbirth)
survival_data_fp$miscarriage_stillbirth<-as.factor(survival_data_fp$miscarriage_stillbirth)
table(survival_data_fp$miscarriage_stillbirth)
survival_data_fp$miscarriage_stillbirth = relevel(survival_data_fp$miscarriage_stillbirth, ref = "No miscarriage/stillbirth")
cox_fit_miss_still  <- coxph(surv_miss_still ~ miscarriage_stillbirth, data = survival_data_fp)
summary(cox_fit_miss_still)
cox_fit_miss_still_clus  <- coxph(surv_miss_still ~ miscarriage_stillbirth+cluster(Facility), data = survival_data_fp)
summary(cox_fit_miss_still_clus)

#Adjusting for age as a confounder
cox_fit_miss_still_adj <- coxph(surv_miss_still ~ miscarriage_stillbirth+age_enr_cof, data = survival_data_fp)
summary(cox_fit_miss_still_adj)
cox_fit_miss_still_adj_clus <- coxph(surv_miss_still ~ miscarriage_stillbirth+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_miss_still_adj_clus)

#16. history of miscarriage
#rename variable
survival_data_fp<-survival_data_fp%>%
  rename(previous_miscarriage=hx_miscarriage)
surv_miss<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_miss
#summary
fit_miss <- survfit(surv_miss ~ previous_miscarriage, data = survival_data_fp)
summary(fit_miss)

#plot
names(fit_miss$strata) <- gsub("previous_miscarriage=", "", names(fit_miss$strata))
ggsurvplot(fit_miss, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "History of miscarriage",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.5,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$previous_miscarriage)
survival_data_fp$previous_miscarriage<-as.factor(survival_data_fp$previous_miscarriage)
table(survival_data_fp$previous_miscarriage)
survival_data_fp$previous_miscarriage = relevel(survival_data_fp$previous_miscarriage, ref = "No miscarriage")
cox_fit_miss  <- coxph(surv_miss ~ previous_miscarriage, data = survival_data_fp)
summary(cox_fit_miss)
cox_fit_miss_clus  <- coxph(surv_miss ~ previous_miscarriage+cluster(Facility), data = survival_data_fp)
summary(cox_fit_miss_clus)

#Adjusting for age as a confounder
cox_fit_miss_adj <- coxph(surv_miss ~ previous_miscarriage+age_enr_cof, data = survival_data_fp)
summary(cox_fit_miss_adj)
cox_fit_miss_adj <- coxph(surv_miss ~ previous_miscarriage+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_miss_adj)

#17. history of stillbirth
table(survival_data_fp$pre_stillbirth_enr_cof)
#rename variable
survival_data_fp<-survival_data_fp%>%
  rename(history_stillbirth=hx_stillbirth)
surv_still<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_still
#summary
fit_still <- survfit(surv_still ~ history_stillbirth, data = survival_data_fp)
summary(fit_still)
class(survival_data_fp$history_stillbirth)

#plot
names(fit_still$strata) <- gsub("history_stillbirth=", "", names(fit_still$strata))
ggsurvplot(fit_still, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Stillbirth",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.3,
           censor.shape = "O", censor.size = 5)

survival_data_fp$history_stillbirth<-as.factor(survival_data_fp$history_stillbirth)
table(survival_data_fp$history_stillbirth)
survival_data_fp$history_stillbirth = relevel(survival_data_fp$history_stillbirth, ref = "No stillbirth")
cox_fit_still  <- coxph(surv_still ~ history_stillbirth, data = survival_data_fp)

summary(cox_fit_still)
cox_fit_still_clus  <- coxph(surv_still ~ history_stillbirth+cluster(Facility), data = survival_data_fp)
summary(cox_fit_still_clus)

#Adjusting for age as a confounder
cox_fit_still_adj <- coxph(surv_still ~ history_stillbirth+age_enr_cof, data = survival_data_fp)
summary(cox_fit_still_adj)
cox_fit_still_adj_clus <- coxph(surv_still ~ history_stillbirth+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_still_adj_clus)

#18.age of youngest child
survival_data_fp<-survival_data_fp%>%
  rename(age_youngest=childage_groups)
surv_child_age<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_child_age
#summary
fit_child_age <- survfit(surv_child_age ~ age_youngest, data = survival_data_fp)
summary(fit_child_age)
class(survival_data_fp$history_stillbirth)


#plot
names(fit_child_age$strata) <- gsub("age_young_child=", "", names(fit_child_age$strata))
ggsurvplot(fit_child_age, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800", "red", "black"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "age of youngest child",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.3,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$age_youngest)
table(survival_data_fp$age_youngest)
survival_data_fp$age_youngest = relevel(survival_data_fp$age_youngest, ref = "2-4.9years")

cox_fit_child_age  <- coxph(surv_child_age ~ age_youngest, data = survival_data_fp)
summary(cox_fit_child_age)
cox_fit_child_age_clus  <- coxph(surv_child_age ~ age_youngest+cluster(Facility), data = survival_data_fp)
summary(cox_fit_child_age_clus)
#Adjusting for age as a confounder
cox_fit_child_age_adj <- coxph(surv_child_age ~ age_youngest+age_enr_cof, data = survival_data_fp)
summary(cox_fit_child_age_adj)
cox_fit_child_age_adj_clus <- coxph(surv_child_age ~ age_youngest+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_child_age_adj_clus)

#18.pregnancy intention
table(survival_data_fp$intention_pregnancy)
#rename variable
survival_data_fp<-survival_data_fp%>%
  rename(intention_pregnancy=intention_pregnancy)
surv_preg<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_preg
#summary
fit_preg <- survfit(surv_preg ~ intention_pregnancy, data = survival_data_fp)
summary(fit_preg)

#plot
names(fit_preg$strata) <- gsub("pregnancy_intention=", "", names(fit_preg$strata))
ggsurvplot(fit_preg, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Pregnancy intention",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.3,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$intention_pregnancy)
survival_data_fp$intention_pregnancy<-as.factor(survival_data_fp$intention_pregnancy)
table(survival_data_fp$intention_pregnancy)
survival_data_fp$intention_pregnancy = relevel(survival_data_fp$intention_pregnancy, ref = "Intended")

cox_fit_preg  <- coxph(surv_preg ~ intention_pregnancy, data = survival_data_fp)
summary(cox_fit_preg)
cox_fit_preg_clus  <- coxph(surv_preg ~ intention_pregnancy+cluster(Facility), data = survival_data_fp)
summary(cox_fit_preg_clus)

#Adjusting for age as a confounder
cox_fit_preg_adj <- coxph(surv_preg ~ intention_pregnancy+age_enr_cof, data = survival_data_fp)
summary(cox_fit_preg_adj)
cox_fit_preg_adj_clus <- coxph(surv_preg ~ intention_pregnancy+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_preg_adj_clus)

#19. Any male child
table(survival_data_fp$male_child)
#rename variable
survival_data_fp<-survival_data_fp%>% 
  rename(anymale_child=male_child)
surv_male<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_male
#summary
fit_male <- survfit(surv_male ~ anymale_child, data = survival_data_fp)
summary(fit_male)

#plot
names(fit_male$strata) <- gsub("anymale_child=", "", names(fit_male$strata))
ggsurvplot(fit_male, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Any male child",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

table(survival_data_fp$anymale_child)
survival_data_fp$anymale_child<-as.factor(survival_data_fp$anymale_child)
survival_data_fp$anymale_child = relevel(survival_data_fp$anymale_child, ref = "No")

cox_fit_male  <- coxph(surv_male ~ anymale_child, data = survival_data_fp)
summary(cox_fit_male)
cox_fit_male_clus  <- coxph(surv_male ~ anymale_child+cluster(Facility), data = survival_data_fp)
summary(cox_fit_male_clus)
#Adjusting for age as a confounder
cox_fit_male_adj <- coxph(surv_male ~ anymale_child+age_enr_cof, data = survival_data_fp)
summary(cox_fit_male_adj)
cox_fit_male_adj_clus <- coxph(surv_male ~ anymale_child+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_male_adj_clus)

#20.partner HIV status at enrolment partnerhiv_enr
survival_data_fp<-survival_data_fp%>% 
  rename(partner_HIV_status=pc_hiv)
surv_parthiv<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_parthiv
#summary
fit_parthiv <- survfit(surv_parthiv ~ partner_HIV_status, data = survival_data_fp)
summary(fit_parthiv)


#plot
names(fit_parthiv$strata) <- gsub("partner_HIV_status=", "", names(fit_parthiv$strata))
ggsurvplot(fit_parthiv, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800","black"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Partner HIV status",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.5,
           censor.shape = "O", censor.size = 5)

survival_data_fp$partner_HIV_status<-as.factor(survival_data_fp$partner_HIV_status)
table(survival_data_fp$partner_HIV_status)
survival_data_fp$partner_HIV_status <- factor(survival_data_fp$partner_HIV_status, levels = c("HIV negative",
                                                                                              "HIV positive",
                                                                                              "Unknown"))
survival_data_fp$partner_HIV_status = relevel(survival_data_fp$partner_HIV_status, ref = "HIV negative")
cox_fit_parthiv  <- coxph(surv_parthiv ~ partner_HIV_status, data = survival_data_fp)
summary(cox_fit_parthiv)
cox_fit_parthiv_clus  <- coxph(surv_parthiv ~ partner_HIV_status+cluster(Facility), data = survival_data_fp)
summary(cox_fit_parthiv_clus)

#Adjusting for age as a confounder
cox_fit_parthiv_adj <- coxph(surv_parthiv ~ partner_HIV_status+age_enr_cof, data = survival_data_fp)
summary(cox_fit_parthiv_adj)
cox_fit_parthiv_adj_clus <- coxph(surv_parthiv ~ partner_HIV_status+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_parthiv_adj_clus)

#21.level of partner education
survival_data_fp<-survival_data_fp%>% 
  rename(partner_education=realpc_education)
surv_partner_ed<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_partner_ed

#summary
fit_part_edu <- survfit(surv_partner_ed ~ partner_education, data = survival_data_fp)
summary(fit_part_edu)


#plot
names(fit_part_edu$strata) <- gsub("partner_education=", "", names(fit_part_edu$strata))
ggsurvplot(fit_part_edu, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Partner education",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$partner_education)
table(survival_data_fp$partner_education)
survival_data_fp$partner_education <- as.factor(survival_data_fp$partner_education)
survival_data_fp$partner_education = relevel(survival_data_fp$partner_education, ref = "Completed basic education")

cox_fit_part_edu  <- coxph(surv_partner_ed ~ partner_education, data = survival_data_fp)
summary(cox_fit_part_edu)
cox_fit_part_edu_clus  <- coxph(surv_partner_ed ~ partner_education+cluster(Facility), data = survival_data_fp)
summary(cox_fit_part_edu_clus)

#Adjusting for age as a confounder
cox_fit_part_edu_adj <- coxph(surv_partner_ed ~ partner_education+age_enr_cof, data = survival_data_fp)
summary(cox_fit_part_edu_adj)
cox_fit_part_edu_adj_clus <- coxph(surv_partner_ed ~ partner_education+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_part_edu_adj_clus)

#22.Partner financial support 

survival_data_fp<-survival_data_fp%>% 
  rename(Financial_support=realpc_financial)
surv_fin<- Surv(survival_data_fp$visit1,survival_data_fp$fp_use)
surv_fin

fit_fin <- survfit(surv_fin~ Financial_support, data = survival_data_fp)
summary(fit_fin)

#plot
names(fit_fin$strata) <- gsub("Financial_support=", "", names(fit_fin$strata))
ggsurvplot(fit_fin, data = survival_data_fp,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Financial support",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp$Financial_support)
table(survival_data_fp$Financial_support)
survival_data_fp$Financial_support <- as.factor(survival_data_fp$Financial_support)
survival_data_fp$Financial_support = relevel(survival_data_fp$Financial_support, ref = "Yes")

cox_fit_fin  <- coxph(surv_fin ~ Financial_support, data = survival_data_fp)
summary(cox_fit_fin)
cox_fit_fin_clus  <- coxph(surv_fin ~ Financial_support+cluster(Facility), data = survival_data_fp)
summary(cox_fit_fin_clus)

#Adjusting for age as a confounder
cox_fit_fin_adj <- coxph(surv_fin ~ Financial_support+age_enr_cof, data = survival_data_fp)
summary(cox_fit_fin_adj)
cox_fit_fin_adj_clus <- coxph(surv_fin ~ Financial_support+age_enr_cof+cluster(Facility), data = survival_data_fp)
summary(cox_fit_fin_adj_clus)


# subanalysis among pregnant women who resumed sex

##### Testing for difference in median time to resumption and fp initiation
clean_revised_resume_sex<- fpdata_sae_ltfp %>% 
  filter(sa_resumesex=="1")
table(clean_revised_resume_sex$sa_resumesex)


fp_resume<-survival_data_fp|>
  select(record_id,first_response_time,visit1,sa_resume_time)|>
  mutate(visit2=visit1)|>
  filter(!is.na(first_response_time))

summary(fp_resume$first_response_time)
summary(fp_resume$visit2)
kruskal.test(fp_resume$first_response_time,fp_resume$visit2)



survival_data_resume_join<-survival_data_fp|>
  select(-c(sa_resume_time,first_response_time))

fp_resume_surv<-fp_resume|>
  left_join(survival_data_resume_join, by="record_id")|>
  mutate(visit_resume=recode(sa_resume_time,
                             "6 weeks"="6","14 weeks"="14", "24 weeks"="24", "36 weeks"="36"))|>
  mutate(visit_resume=as.numeric(as.character(visit_resume)))

table(fp_resume_surv$fp_use,fp_resume_surv$sa_resume_time)
table(fp_resume_surv$visit_resume,fp_resume_surv$sa_resume_time)

dim(fp_resume_surv)
###


#####RESUME SEX

resume_sub<-resume%>%
  mutate(resume_sex_indicator="resume sex")

resume_sub<-resume_sub%>%
  rename(clt_visit=first_resume)

sex_resumption <- resume_sub %>% 
  left_join(Baseline, by='record_id')

# adding suffix _ord
colnames(kaplan_ord) <- paste(colnames(kaplan_ord),"ord",sep="_")
kaplan_ord<-kaplan_ord%>%
  rename(record_id=record_id_ord)

survival_resume <-sex_resumption%>% 
  left_join(kaplan_order,by ='record_id')

table(survival_resume$sa_resumesex_ord)
table(survival_resume$pc_current)

# Fit survival data using the Kaplan-Meier method
#1.Outcome variable
#rename clt_visit.y to Visit
survival_resume_fp<-survival_resume%>%
  rename("visit_clean"="clt_visit_ord")

glimpse(survival_data_fp)
#recode
survival_resume_fp$visit_clean<-as.factor(survival_resume_fp$visit_clean)
survival_resume_fp<-survival_resume_fp%>%
  mutate(visit1=recode(visit_clean,
                       "101"="6",
                       "102"="14",
                       "103"="24",
                       "104"="36"))
class(survival_resume_fp$visit1)

class(survival_resume_fp$fp_use)

#visit 6 censoring
table(survival_resume_fp$visit1,survival_resume_fp$fp_use)
survival_resume_fp$visit1[survival_resume_fp$record_id %in% c(13476002909,13608172409,13608172809,13608173209,13653213009,
                                                              13668252109,13668261209,13668267409,13747062709,13953517409,
                                                              13953519909,14022609509,14022613609,14022619309,14022623709,
                                                              14036668509,14036673609,14063720309,14063720509,14080771709,
                                                              14130800509,14164854709,14164856109,14164856409,14164856809,
                                                              14164861809,14164864609,14165905109,14175970609)& survival_resume_fp$fp_use == 0] <- 6
table(survival_resume_fp$visit1,survival_resume_fp$fp_use)
table(survival_resume_fp$sa_resumesex_ord)

#week14 censoring
survival_resume_fp$visit1[survival_resume_fp$record_id %in% c(13507107409,13608171509,13608173309,13608174709,13608174809,
                                                              13608175009,13747052209,13747069009,13841470309,13953518409,
                                                              14022609609,14022622709,14022623009,14036654809,14036671809,
                                                              14036673009,14036674509,14063715009,14063715309,14063719909,
                                                              14063720809,14063721009,14063721209,14063721309,14080769409,
                                                              14130800109,14164850809,14164855009,14164856309,14164857009,
                                                              14164867609,14175970209,14175971209)& survival_resume_fp$fp_use == 0] <- 14
table(survival_resume_fp$visit1,survival_resume_fp$fp_use)

#month6 censoring
survival_resume_fp$visit1[survival_resume_fp$record_id %in% c(13507122909,13608168309,13608171409,13608172209,13608172609,
                                                              13608174109,13668262909,13668268109,13668270209,13668270509,
                                                              13668270809,13668272709,13668272809,13668273709,13760307809,
                                                              13760311409,13798412809,13798415209,13841470509,13987563809,
                                                              13987568409,13987572609,14022623409,14036670509,14036671709,
                                                              14036672209,14036672709,14063719509,14063719709,14080759809,
                                                              14080761009,14130804209,14164857809,14164859109,14164870209,
                                                              14175961209,14175966609,14175969609,14175970309,14175972209)& survival_resume_fp$fp_use == 0] <- 24

table(survival_resume_fp$visit1,survival_resume_fp$fp_use)

##replace 0 (enrolment) values in visit1 with 36 so as to censor at month9
#participants who came for all the visits but did not initiate fp use
table(survival_resume_fp$visit1)
survival_resume_fp$visit1[survival_resume_fp$visit1 == 0] <- 36
table(survival_resume_fp$visit1,survival_resume_fp$fp_use)
table(survival_resume_fp$fp_use)

table(survival_resume_fp$Facility)


#K-M and cox regression analysis
table(survival_resume_fp$visit1)
class(survival_resume_fp$visit1)
survival_resume_fp$visit1<-as.numeric(as.character(survival_resume_fp$visit1))
survival_resume_fp$fp_use<-as.numeric(as.character(survival_resume_fp$fp_use))
surv_fp_resume<- Surv(survival_resume_fp$visit1,survival_resume_fp$fp_use)
surv_fp_resume
#summary
fp_resume <- survfit(surv_fp_resume ~ 1, data = survival_resume_fp)
summary(fp_resume)

table(survival_resume_fp$resume_sex_indicator)
table(survival_resume_fp$pc_current)
table(survival_resume_fp$pc_current,survival_resume_fp$resume_sex_indicator)

table(survival_resume_fp$sa_resumesex_ord,survival_resume_fp$fp_use)

survival_data_fp_resume=survival_resume_fp
#KM and cox regression analysis
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(fp_use_resume=fp_use)
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(visit1_resume=visit1)
table(survival_data_fp_resume$fp_use_resume)

table(survival_data_fp_resume$visit1_resume)
survival_data_fp_resume$visit1_resume<-as.numeric(as.character(survival_data_fp_resume$visit1_resume))
survival_data_fp_resume$fp_use_resume<-as.numeric(as.character(survival_data_fp_resume$fp_use_resume))

surv_fp1_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_fp1_resume
#summary
fp1_resume <- survfit(surv_fp1_resume ~ 1, data = survival_data_fp_resume)
summary(fp1_resume)

ggsurvplot(fp1_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette ="turquoise4", size=1,
           xlim=c(0,40),break.x.by=6,
           surv.median.line = "hv", conf.type="plain",conf.int = 0.95,
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Family planning use among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun = "event",
           ylab="Probability", pval = FALSE,pval.coord = c(10, 0.03),fontsize=3,
           censor.shape = "O", censor.size = 5)

#1. continuous age
surv_age_cont_resume <- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_age_cont_resume
#summary
cox_fit_age_cont_resume <- coxph(surv_age_cont_resume ~ age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_age_cont_resume)


#2.age groups
#rename age groups
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(Age_resume=age_groups_cof)

surv_age_resume <- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_age_resume
#summary
fit_age_resume <- survfit(surv_age_resume ~ Age_resume, data = survival_data_fp_resume)
summary(fit_age_resume)

table(survival_data_fp_resume$Age_resume)
survival_data_fp_resume$Age_resume <- factor(survival_data_fp_resume$Age_resume, levels = c("15 - 19years",
                                                                                            "20 - 24years",
                                                                                            "More than 24 years"))
survival_data_fp_resume$Age_resume = relevel(survival_data_fp_resume$Age_resume, ref = "More than 24 years")

#plot
names(fit_age_resume$strata) <- gsub("Age_resume=", "", names(fit_age_resume$strata))
ggsurvplot(fit_age_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800","red"), size=1,
           xlim=c(0,40),break.x.by=6,
           surv.median.line = "hv", conf.type="plain",conf.int = 0.95,
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Age",
           xlab = "Time to FP among women who resumed sex (weeks)", size=1,fun = "event",
           ylab="Probability", pval = TRUE,pval.coord = c(10, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

cox_fit_age_resume <- coxph(surv_age_resume ~ Age_resume, data = survival_data_fp_resume)
summary(cox_fit_age_resume)

#3.Marital relationship
#rename marital relationship
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(Marital_relationship_resume=mar_relationship_cof)

surv_mar_resume <- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_mar_resume
#summary
fit_mar_resume <- survfit(surv_mar_resume ~ Marital_relationship_resume, data = survival_data_fp_resume)
summary(fit_mar_resume)

#plot
names(fit_mar_resume$strata) <- gsub("Marital_relationship_resume=", "", names(fit_mar_resume$strata))
ggsurvplot(fit_mar_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800","red"), size=1,
           xlim=c(0,40),break.x.by=6,
           surv.median.line = "hv", conf.type="plain",conf.int = 0.95,
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Marital relationship",
           xlab = "Time to FP among women who resumed sex (weeks)", size=1,fun = "event",
           ylab="Probability", pval = TRUE,pval.coord = c(10, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

summarytools::freq(survival_data_fp_resume$age_enr_cof)

#cox regression
survival_data_fp_resume$Marital_relationship_resume = relevel(survival_data_fp_resume$Marital_relationship_resume, ref = "Steady relationship")

cox_fit_mar_resume <- coxph(surv_mar_resume ~ Marital_relationship_resume, data = survival_data_fp_resume)
summary(cox_fit_mar_resume)
cox_fit_mar_resume_clus <- coxph(surv_mar_resume ~ Marital_relationship_resume+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_mar_resume_clus)

#Adjusting for age as a confounder
cox_fit_mar_resume_adj <- coxph(surv_mar_resume ~ Marital_relationship_resume+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_mar_resume_adj)
cox_fit_mar_resume_adj_clus <- coxph(surv_mar_resume ~ Marital_relationship_resume+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_mar_resume_adj_clus)


#4.had partner
#rename employed variable
#4.had partner
#rename employed variable
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(Had_partner=pc_current)
table(survival_data_fp_resume$Had_partner)

surv_part_resume <- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_part_resume
#summary
fit_part_resume <- survfit(surv_part_resume ~ Had_partner, data = survival_data_fp_resume)
summary(fit_part_resume)


#plot
names(fit_part_resume$strata) <- gsub("Had_partner=", "", names(fit_part_resume$strata))
ggsurvplot(fit_part_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Had partner",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=4,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp_resume$Had_partner)
table(survival_data_fp_resume$Had_partner_resume)
survival_data_fp_resume$Had_partner = relevel(survival_data_fp_resume$Had_partner, ref = "Yes")

cox_fit_part_resume <- coxph(surv_part_resume ~ Had_partner, data = survival_data_fp_resume)
summary(cox_fit_part_resume)
cox_fit_part_clus_resume <- coxph(surv_part_resume ~ Had_partner+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_part_clus_resume)
#Adjusting for age as a confounder
cox_fit_part_resume_adj <- coxph(surv_part_resume ~ Had_partner+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_part_resume_adj)
cox_fit_part_resume_adj_clus <- coxph(surv_part_resume ~ Had_partner+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_part_resume_adj_clus)

#5.residing with partner
#subset those with partner
had_partner_resume<-survival_data_fp_resume%>%
  filter(Had_partner=='Yes')
had_partner_resume<-had_partner_resume%>%
  rename(residing_with_partner=pc_residence_enr_cof)
class(had_partner_resume$fp_use_resume)
surv_res_resume <- Surv(had_partner_resume$visit1_resume,had_partner_resume$fp_use_resume)
surv_res_resume
#summary
fit_res_resume <- survfit(surv_res_resume~ residing_with_partner, data = had_partner_resume)
summary(fit_res_resume)

#plot
names(fit_res_resume$strata) <- gsub("residing_with_partner=", "", names(fit_res_resume$strata))
ggsurvplot(fit_res_resume, data = had_partner_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Resided with partner among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

class(had_partner_resume$residing_with_partner)
had_partner_resume$residing_with_partner<-as.factor(had_partner_resume$residing_with_partner)
table(had_partner_resume$residing_with_partner)
had_partner_resume$residing_with_partner = relevel(had_partner_resume$residing_with_partner, ref = "Yes")
cox_fit_res_resume  <- coxph(surv_res_resume ~ residing_with_partner, data = had_partner_resume)
summary(cox_fit_res_resume)
cox_fit_res_resume_clus  <- coxph(surv_res_resume ~ residing_with_partner+cluster(Facility), data = had_partner_resume)
summary(cox_fit_res_resume_clus)

#Adjusting for age as a confounder
cox_fit_res_resume_adj <- coxph(surv_res_resume ~ residing_with_partner+age_enr_cof, data = had_partner_resume)
summary(cox_fit_res_resume_adj)
cox_fit_res_resume_adj_clus <- coxph(surv_res_resume ~ residing_with_partner+age_enr_cof+cluster(Facility), data = had_partner_resume)
summary(cox_fit_res_resume_adj_clus)


#6.Education level
surv_edu_resume <- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_edu_resume
#summary
fit_edu_resume <- survfit(surv_edu_resume ~ participant_education1, data = survival_data_fp_resume)
summary(fit_edu_resume)

#plot
names(fit_edu_resume$strata) <- gsub("participant_education1=", "", names(fit_edu_resume$strata))
ggsurvplot(fit_edu_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Education level among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp_resume$participant_education1)
table(survival_data_fp_resume$participant_education1)

survival_data_fp_resume$participant_education1 = relevel(survival_data_fp_resume$participant_education1, ref = "Completed basic education")

cox_fit_edu_resume <- coxph(surv_edu_resume ~ participant_education1, data = survival_data_fp_resume)
summary(cox_fit_edu_resume)

#Adjusting for age as a confounder
cox_fit_edu_resume_adj <- coxph(surv_edu_resume ~ participant_education1+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_edu_resume_adj)

cox_fit_edu_resume_clus <- coxph(surv_edu_resume ~ participant_education1+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_edu_resume_clus)

#Adjusting for age as a confounder
cox_fit_edu_resume_adj_clus <- coxph(surv_edu_resume ~ participant_education1+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_edu_resume_adj_clus)

#7.High risk at enrolment by nascop
survival_data_fp_resume<-survival_data_fp_resume%>%
  mutate(HIV_risk_resume=recode(highrisk_nascop_enr_cof,
                                "0"="Low risk",
                                "1"="High risk"))
survival_data_fp_resume<-survival_data_fp_resume%>%
  mutate(HIV_risk_pintye_resume=recode(highrisk_pintye_gt6_enr_cof,
                                       "0"="Low risk",
                                       "1"="High risk"))
table(survival_data_fp_resume$HIV_risk_resume)
summarytools::freq(survival_data_fp_resume$HIV_risk_pintye_resume)
surv_risk_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_risk_resume
#summary
fit_risk_resume <- survfit(surv_risk_resume ~ HIV_risk_resume, data = survival_data_fp_resume)
summary(fit_risk_resume)

#plot
names(fit_risk_resume$strata) <- gsub("HIV_risk_resume=", "", names(fit_risk_resume$strata))
ggsurvplot(fit_risk_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "HIV risk among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=4,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp_resume$HIV_risk_resume)
table(survival_data_fp_resume$HIV_risk_resume)
survival_data_fp_resume$HIV_risk_resume<-as.factor(survival_data_fp_resume$HIV_risk_resume)
survival_data_fp_resume$HIV_risk_resume = relevel(survival_data_fp_resume$HIV_risk_resume, ref = "Low risk")

cox_fit_risk_resume <- coxph(surv_risk_resume ~ HIV_risk_resume, data = survival_data_fp_resume)
summary(cox_fit_risk_resume)

#Adjusting for age as a confounder
cox_fit_risk_resume_adj <- coxph(surv_risk_resume ~ HIV_risk_resume+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_risk_resume_adj)

cox_fit_risk_resume_clus <- coxph(surv_risk_resume ~ HIV_risk_resume+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_risk_resume_clus)
cox_fit_risk_resume_adj_clus <- coxph(surv_risk_resume ~ HIV_risk_resume+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_risk_resume_adj_clus)

#8.High risk at enrolment by Pintye
surv_riskpint_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_riskpint_resume
#summary
fit_riskpint_resume <- survfit(surv_riskpint_resume ~ HIV_risk_pintye_resume, data = survival_data_fp_resume)
summary(fit_riskpint_resume)

#plot
names(fit_riskpint_resume$strata) <- gsub("HIV_risk_pintye_resume=", "", names(fit_riskpint_resume$strata))
ggsurvplot(fit_riskpint_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "HIV risk (Pintye) among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp_resume$HIV_risk_pintye_resume)
table(survival_data_fp_resume$HIV_risk_pintye_resume)
survival_data_fp_resume$HIV_risk_pintye_resume<-as.factor(survival_data_fp_resume$HIV_risk_pintye_resume)
survival_data_fp_resume$HIV_risk_pintye_resume = relevel(survival_data_fp_resume$HIV_risk_pintye_resume, ref = "Low risk")

cox_fit_riskpin_resume <- coxph(surv_riskpint_resume ~ HIV_risk_pintye_resume, data = survival_data_fp_resume)
summary(cox_fit_riskpin_resume)

#Adjusting for age as a confounder
cox_fit_riskpin_resume_adj <- coxph(surv_riskpint_resume ~ HIV_risk_pintye_resume+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_riskpin_resume_adj)
#clustering
cox_fit_riskpin_resume_clus <- coxph(surv_riskpint_resume ~ HIV_risk_pintye_resume+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_riskpin_resume_clus)
cox_fit_riskpin_resume_adj_clus <- coxph(surv_riskpint_resume ~ HIV_risk_pintye_resume+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_riskpin_resume_adj_clus)

#9.employed
#rename employed variable
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(employed_resume=employment_enr_cof)

surv_employ_resume <- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_employ_resume
#summary
fit_employ_resume <- survfit(surv_employ_resume ~ employed_resume, data = survival_data_fp_resume)
summary(fit_employ_resume)

#plot
names(fit_employ_resume$strata) <- gsub("employed_resume=", "", names(fit_employ_resume$strata))
ggsurvplot(fit_employ_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Employment among women who have resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp_resume$employed_resume)
table(survival_data_fp_resume$employed_resume)
survival_data_fp_resume$employed_resume<-as.factor(survival_data_fp_resume$employed_resume)
survival_data_fp_resume$employed_resume = relevel(survival_data_fp_resume$employed_resume, ref = "No")
cox_fit_employ_resume <- coxph(surv_employ_resume ~ employed_resume, data = survival_data_fp_resume)
summary(cox_fit_employ_resume)

#Adjusting for age as a confounder
cox_fit_employ_resume_adj <- coxph(surv_employ_resume ~ employed_resume+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_employ_resume_adj)
#cluster
cox_fit_employ_resume_clus <- coxph(surv_employ_resume ~ employed_resume+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_employ_resume_clus)
cox_fit_employ_resume_adj_clus <- coxph(surv_employ_resume ~ employed_resume+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_employ_resume_adj_clus)

#10. Low social support
table(survival_data_fp_resume$mos_sss_lt72_enr_cof)
#rename variable
survival_data_fp_resume<-survival_data_fp_resume%>% 
  rename(low_social_resume=mos_sss_lt72_enr_cof)
surv_low_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_low_resume
#summary
fit_low_resume <- survfit(surv_low_resume ~ low_social_resume, data = survival_data_fp_resume)
summary(fit_low_resume)

#plot
names(fit_low_resume$strata) <- gsub("low_social_resume=", "", names(fit_low_resume$strata))
ggsurvplot(fit_low_resume, low = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Low social support among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

table(survival_data_fp_resume$low_social_resume)
survival_data_fp_resume$low_social_resume = relevel(survival_data_fp_resume$low_social_resume, ref = "High social support")

cox_fit_low_resume  <- coxph(surv_low_resume ~ low_social_resume, data = survival_data_fp_resume)
summary(cox_fit_low_resume)

#Adjusting for age as a confounder
cox_fit_low_resume_adj <- coxph(surv_low_resume ~ low_social_resume+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_low_resume_adj)

#cluster
cox_fit_low_resume_clus  <- coxph(surv_low_resume ~ low_social_resume+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_low_resume_clus)
cox_fit_low_resume_adj_clus <- coxph(surv_low_resume ~ low_social_resume+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_low_resume_adj_clus)

#11. IPV
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(IPV_resume=hits_gt10_enr_cof)

surv_IPV_resume <- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_IPV_resume
#summary
fit_IPV_resume <- survfit(surv_IPV_resume ~ IPV_resume, data = survival_data_fp_resume)
summary(fit_IPV_resume)

#plot
names(fit_IPV_resume$strata) <- gsub("IPV_resume=", "", names(fit_IPV_resume$strata))
ggsurvplot(fit_IPV_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "IPV among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp_resume$IPV_resume)
table(survival_data_fp_resume$IPV_resume)
survival_data_fp_resume$IPV_resume = relevel(survival_data_fp_resume$IPV_resume, ref = "No hx of IPV")

cox_fit_IPV_resume <- coxph(surv_IPV_resume ~ IPV_resume, data = survival_data_fp_resume)
summary(cox_fit_IPV_resume)

#Adjusting for age as a confounder
cox_fit_IPV_resume_adj <- coxph(surv_IPV_resume ~ IPV_resume+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_IPV_resume_adj)

#clustering
cox_fit_IPV_resume_clus <- coxph(surv_IPV_resume ~ IPV_resume+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_IPV_resume_clus)
cox_fit_IPV_resume_adj_clus <- coxph(surv_IPV_resume ~ IPV_resume+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_IPV_resume_adj_clus)


#12. Depression symptoms (PHQ2) (phq2_gt3_enr)
#rename variable
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(phq_resume=phq2_gt3_enr_cof)
surv_phq_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_phq_resume
#summary
fit_phq_resume <- survfit(surv_phq_resume ~ phq_resume, data = survival_data_fp_resume)
summary(fit_phq_resume)

class(survival_data_fp_resume$phq_resume)
table(survival_data_fp_resume$phq_resume)
survival_data_fp_resume$phq_resume = relevel(survival_data_fp_resume$phq_resume, ref = "No symptoms of depression")
#plot
names(fit_phq_resume$strata) <- gsub("phq_resume=", "", names(fit_phq_resume$strata))
ggsurvplot(fit_phq_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Depression symptoms among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

cox_fit_phq_resume  <- coxph(surv_phq_resume ~ phq_resume, data = survival_data_fp_resume)
summary(cox_fit_phq_resume) 
#Adjusting for age as a confounder
cox_fit_phq_resume_adj <- coxph(surv_phq_resume ~ phq_resume+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_phq_resume_adj)

#cluster
cox_fit_phq_resume_clus  <- coxph(surv_phq_resume ~ phq_resume+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_phq_resume_clus)
cox_fit_phq_resume_adj_clus <- coxph(surv_phq_resume ~ phq_resume+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_phq_resume_adj_clus)

#13. Parity
table(survival_data_fp_resume$parity)
surv_parity_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_parity_resume
#summary
fit_parity_resume <- survfit(surv_parity_resume ~ parity, data = survival_data_fp_resume)
summary(fit_parity_resume)

#plot
names(fit_parity_resume$strata) <- gsub("parity=", "", names(fit_parity_resume$strata))
ggsurvplot(fit_parity_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Parity among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp_resume$parity)
survival_data_fp_resume$parity<-as.factor(survival_data_fp_resume$parity)
table(survival_data_fp_resume$parity)
survival_data_fp_resume$parity = relevel(survival_data_fp_resume$parity, ref = "Multiparous")

cox_fit_parity_resume  <- coxph(surv_parity_resume ~ parity, data = survival_data_fp_resume)
summary(cox_fit_parity_resume)

#Adjusting for age as a confounder
cox_fit_parity_resume_adj <- coxph(surv_parity_resume ~ parity+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_parity_resume_adj)
##clustering
cox_fit_parity_resume_clus  <- coxph(surv_parity_resume ~ parity+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_parity_resume_clus)
cox_fit_parity_resume_adj_clus <- coxph(surv_parity_resume ~ parity+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_parity_resume_adj_clus)

#14.Number of living children
surv_num_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_num_resume
#summary
fit_num_resume <- survfit(surv_num_resume ~ number_living, data = survival_data_fp_resume)
summary(fit_num_resume)

survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(Number_Living=number_living)

surv_num <- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_num
#summary
fit_num <- survfit(surv_num ~ Number_Living, data = survival_data_fp_resume)
summary(fit_num)

table(survival_data_fp_resume$Number_Living)
summarytools::freq(survival_data_fp_resume$Number_Living)
survival_data_fp_resume$Number_Living <- factor(survival_data_fp_resume$Number_Living, levels = c("No living child",
                                                                                                  "Fewer than 4 children",
                                                                                                  "More than 4 children"))
survival_data_fp_resume$Number_Living = relevel(survival_data_fp_resume$Number_Living, ref = "More than 4 children")

#plot
names(fit_num$strata) <- gsub("Number_Living=", "", names(fit_num$strata))
ggsurvplot(fit_num, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800","red"), size=1,
           xlim=c(0,40),break.x.by=6,
           surv.median.line = "hv", conf.type="plain",conf.int = 0.95,
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Number of living children",
           xlab = "Time to FP (weeks)", size=1,fun = "event",
           ylab="Probability", pval = TRUE,pval.coord = c(10, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

cox_fit_num_resume <- coxph(surv_num ~ Number_Living, data = survival_data_fp_resume)
summary(cox_fit_num_resume)

#Adjusting for age as a confounder
cox_fit_num_resume_adj <- coxph(surv_num ~ Number_Living+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_num_resume_adj)

#clustering
cox_fit_num_resume_clus <- coxph(surv_num ~ Number_Living+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_num_resume_clus)
cox_fit_num_adj_clus <- coxph(surv_num ~ Number_Living+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_num_adj_clus)

#15. History of either miscarriage or stillbirth pre_miscarriage_stillbirth_enr
table(survival_data_fp_resume$pre_miscarriage_stillbirth_enr_cof)
#rename variable
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(miscarriage_stillbirth_resume=pre_miscarriage_stillbirth_enr_cof)
surv_miss_still_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_miss_still_resume
#summary
fit_miss_still_resume <- survfit(surv_miss_still_resume ~ miscarriage_stillbirth_resume, data = survival_data_fp_resume)
summary(fit_miss_still_resume)

#plot
names(fit_miss_still_resume$strata) <- gsub("miscarriage_stillbirth_resume=", "", names(fit_miss_still_resume$strata))
ggsurvplot(fit_miss_still_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "History of miscarriage/stillbirth among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=4,
           censor.shape = "O", censor.size = 5)
table(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$miscarriage_stillbirth_resume)

class(survival_data_fp_resume$miscarriage_stillbirth_resume)
survival_data_fp_resume$miscarriage_stillbirth_resume<-as.factor(survival_data_fp_resume$miscarriage_stillbirth_resume)
table(survival_data_fp_resume$miscarriage_stillbirth_resume)
survival_data_fp_resume$miscarriage_stillbirth_resume = relevel(survival_data_fp_resume$miscarriage_stillbirth_resume, ref = "No")
cox_fit_miss_still_resume  <- coxph(surv_miss_still_resume ~ miscarriage_stillbirth_resume, data = survival_data_fp_resume)
summary(cox_fit_miss_still_resume)

#Adjusting for age as a confounder
cox_fit_miss_still_resume_adj <- coxph(surv_miss_still_resume ~ miscarriage_stillbirth_resume+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_miss_still_resume_adj)
#clustering
cox_fit_miss_still_resume_clus  <- coxph(surv_miss_still_resume ~ miscarriage_stillbirth_resume+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_miss_still_resume_clus)
cox_fit_miss_still_resume_adj_clus <- coxph(surv_miss_still_resume ~ miscarriage_stillbirth_resume+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_miss_still_resume_adj_clus)

#16. history of miscarriage
table(survival_data_fp_resume$pre_miscarriage_enr_cof)
#rename variable
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(previous_miscarriage_resume=pre_miscarriage_enr_cof)
surv_miss_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_miss_resume
#summary
fit_miss_resume <- survfit(surv_miss_resume ~ previous_miscarriage_resume, data = survival_data_fp_resume)
summary(fit_miss_resume)

#plot
names(fit_miss_resume$strata) <- gsub("previous_miscarriage_resume=", "", names(fit_miss_resume$strata))
ggsurvplot(fit_miss_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "History of miscarriage among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.5,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp_resume$previous_miscarriage_resume)
survival_data_fp_resume$previous_miscarriage_resume<-as.factor(survival_data_fp_resume$previous_miscarriage_resume)
table(survival_data_fp_resume$previous_miscarriage_resume)
survival_data_fp_resume$previous_miscarriage_resume = relevel(survival_data_fp_resume$previous_miscarriage_resume, ref = "No")
cox_fit_miss_resume  <- coxph(surv_miss_resume ~ previous_miscarriage_resume, data = survival_data_fp_resume)
summary(cox_fit_miss_resume)
#Adjusting for age as a confounder
cox_fit_miss_resume_adj <- coxph(surv_miss_resume ~ previous_miscarriage_resume+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_miss_resume_adj)

#clustering
cox_fit_miss_resume_clus  <- coxph(surv_miss_resume ~ previous_miscarriage_resume+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_miss_resume_clus)
cox_fit_miss_resume_adj <- coxph(surv_miss_resume ~ previous_miscarriage_resume+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_miss_resume_adj)

#17. history of stillbirth
table(survival_data_fp_resume$pre_stillbirth_enr_cof)
#rename variable
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(history_stillbirth_resume=pre_stillbirth_enr_cof)
surv_still_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_still_resume
#summary
fit_still_resume <- survfit(surv_still_resume ~ history_stillbirth_resume, data = survival_data_fp_resume)
summary(fit_still)
class(survival_data_fp_resume$history_stillbirth_resume)
survival_data_fp_resume$history_stillbirth_resume<-as.factor(survival_data_fp_resume$history_stillbirth_resume)
table(survival_data_fp_resume$history_stillbirth_resume)
survival_data_fp_resume$history_stillbirth_resume = relevel(survival_data_fp_resume$history_stillbirth_resume, ref = "No")
cox_fit_still_resume  <- coxph(surv_still_resume ~ history_stillbirth_resume, data = survival_data_fp_resume)
summary(cox_fit_still_resume)

#Adjusting for age as a confounder
cox_fit_still_resume_adj <- coxph(surv_still_resume ~ history_stillbirth_resume+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_still_resume_adj)

#clustering
cox_fit_still_resume_clus  <- coxph(surv_still_resume ~ history_stillbirth_resume+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_still_resume_clus)
cox_fit_still_resume_adj_clus <- coxph(surv_still_resume ~ history_stillbirth_resume+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_still_resume_adj_clus)

#plot
names(fit_still_resume$strata) <- gsub("history_stillbirth_resume=", "", names(fit_still_resume$strata))
ggsurvplot(fit_still_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Stillbirth among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.3,
           censor.shape = "O", censor.size = 5)

#18. age of youngest child
table(survival_data_fp_resume$childage_groups)
class(survival_data_fp_resume$childage_groups)
#complete child age
child_age_complete_resume<-survival_data_fp_resume%>%
  filter(childage_groups=="Below 2years"|childage_groups=="2-4.9years"|childage_groups=="5 and above")
summarytools::freq(child_age_complete_resume$childage_groups)
table(child_age_complete_resume$childage_groups)
#rename variable
child_age_complete_resume<-child_age_complete_resume%>%
  rename(age_young_child=childage_groups)
surv_young_resume<- Surv(child_age_complete_resume$visit1_resume,child_age_complete_resume$fp_use_resume)
surv_young_resume


#summary
fit_young_resume <- survfit(surv_young_resume ~ age_young_child, data = child_age_complete_resume)
summary(fit_young_resume)

class(child_age_complete_resume$age_young_child)
child_age_complete_resume$age_young_child<-as.factor(child_age_complete_resume$age_young_child)
table(child_age_complete_resume$age_young_child)
child_age_complete_resume$age_young_child = relevel(child_age_complete_resume$age_young_child, ref = "2-4.9years")

cox_fit_young_resume  <- coxph(surv_young_resume ~ age_young_child, data = child_age_complete_resume)
summary(cox_fit_young_resume)

#plot
names(fit_young_resume$strata) <- gsub("age_young_child=", "", names(fit_young_resume$strata))
ggsurvplot(fit_young_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800", "red", "black"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "age of youngest child among women who resume sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.3,
           censor.shape = "O", censor.size = 5)

#Adjusting for age as a confounder
cox_fit_young_resume_adj <- coxph(surv_young_resume ~ age_young_child+age_enr_cof, data = child_age_complete_resume)
summary(cox_fit_young_resume_adj)
#clustering
cox_fit_young_resume_clus  <- coxph(surv_young_resume ~ age_young_child+cluster(Facility), data = child_age_complete_resume)
summary(cox_fit_young_resume_clus)
cox_fit_young_resume_adj_clus <- coxph(surv_young_resume ~ age_young_child+age_enr_cof+cluster(Facility), data = child_age_complete_resume)
summary(cox_fit_young_resume_adj_clus)

#19.pregnancy intention
table(survival_data_fp_resume$intention_pregnancy)
#rename variable
survival_data_fp_resume<-survival_data_fp_resume%>%
  rename(intention_pregnancy=intention_pregnancy)
surv_preg_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_preg_resume
#summary
fit_preg_resume <- survfit(surv_preg_resume ~ intention_pregnancy, data = survival_data_fp_resume)
summary(fit_preg_resume)

#plot
names(fit_preg_resume$strata) <- gsub("pregnancy_intention=", "", names(fit_preg_resume$strata))
ggsurvplot(fit_preg_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Pregnancy intention among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.3,
           censor.shape = "O", censor.size = 5)

class(survival_data_fp_resume$intention_pregnancy)
survival_data_fp_resume$intention_pregnancy<-as.factor(survival_data_fp_resume$intention_pregnancy)
table(survival_data_fp_resume$intention_pregnancy)
survival_data_fp_resume$intention_pregnancy = relevel(survival_data_fp_resume$intention_pregnancy, ref = "Intended")

cox_fit_preg_resume  <- coxph(surv_preg_resume ~ intention_pregnancy, data = survival_data_fp_resume)
summary(cox_fit_preg_resume)

#Adjusting for age as a confounder
cox_fit_preg_resume_adj <- coxph(surv_preg_resume ~ intention_pregnancy+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_preg_resume_adj)
#clustering
cox_fit_preg_resume_clus  <- coxph(surv_preg_resume ~ intention_pregnancy+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_preg_resume_clus)
cox_fit_preg_resume_adj_clus <- coxph(surv_preg_resume ~ intention_pregnancy+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_preg_resume_adj_clus)


#20. Any male child
table(survival_data_fp_resume$male_child)
#rename variable
survival_data_fp_resume<-survival_data_fp_resume%>% 
  rename(anymale_child=male_child)
surv_male_resume<- Surv(survival_data_fp_resume$visit1_resume,survival_data_fp_resume$fp_use_resume)
surv_male_resume
#summary
fit_male_resume <- survfit(surv_male_resume ~ anymale_child, data = survival_data_fp_resume)
summary(fit_male_resume)

#plot
names(fit_male_resume$strata) <- gsub("anymale_child=", "", names(fit_male_resume$strata))
ggsurvplot(fit_male_resume, data = survival_data_fp_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Any male child among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

table(survival_data_fp_resume$anymale_child)
survival_data_fp_resume$anymale_child<-as.factor(survival_data_fp_resume$anymale_child)
survival_data_fp_resume$anymale_child = relevel(survival_data_fp_resume$anymale_child, ref = "No")

cox_fit_male_resume  <- coxph(surv_male_resume ~ anymale_child, data = survival_data_fp_resume)
summary(cox_fit_male_resume)

#Adjusting for age as a confounder
cox_fit_male_resume_adj <- coxph(surv_male_resume ~ anymale_child+age_enr_cof, data = survival_data_fp_resume)
summary(cox_fit_male_resume_adj)

#clustering
cox_fit_male_resume_clus  <- coxph(surv_male_resume ~ anymale_child+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_male_resume_clus)
cox_fit_male_resume_adj_clus <- coxph(surv_male_resume ~ anymale_child+age_enr_cof+cluster(Facility), data = survival_data_fp_resume)
summary(cox_fit_male_resume_adj_clus)

#21. partner HIV status at enrolment partnerhiv_enr
#rename variable
summarytools::freq(had_partner_resume$partnerhiv_enr_cof)
had_partner_resume<-had_partner_resume%>%
  rename(partner_HIV_status_resume=partnerhiv_enr_cof)

patner_hiv_complete_resume<-had_partner_resume%>%
  filter(partner_HIV_status_resume=="HIV negative"|partner_HIV_status_resume=="HIV positive"|partner_HIV_status_resume=="Unknown")
summarytools::freq(patner_hiv_complete_resume$partner_HIV_status_resume)

surv_parthiv_resume<- Surv(patner_hiv_complete_resume$visit1_resume,patner_hiv_complete_resume$fp_use_resume)
surv_parthiv_resume
#summary
fit_parthiv_resume <- survfit(surv_parthiv_resume ~ partner_HIV_status_resume, data = patner_hiv_complete_resume)
summary(fit_parthiv_resume)

#plot
names(fit_parthiv_resume$strata) <- gsub("partner_HIV_status_resume=", "", names(fit_parthiv_resume$strata))
ggsurvplot(fit_parthiv_resume, data = patner_hiv_complete_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800","black"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Partner HIV status among women who resume sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.5,
           censor.shape = "O", censor.size = 5)
class(patner_hiv_complete_resume$partner_HIV_status_resume)
patner_hiv_complete_resume$partner_HIV_status_resume<-as.factor(patner_hiv_complete_resume$partner_HIV_status_resume)
table(patner_hiv_complete_resume$partner_HIV_status_resume)
patner_hiv_complete_resume$partner_HIV_status_resume <- factor(patner_hiv_complete_resume$partner_HIV_status_resume, levels = c("HIV negative",
                                                                                                                                "HIV positive",
                                                                                                                                "Unknown"))
patner_hiv_complete_resume$partner_HIV_status_resume = relevel(patner_hiv_complete_resume$partner_HIV_status_resume, ref = "HIV negative")
cox_fit_parthiv_resume  <- coxph(surv_parthiv_resume ~ partner_HIV_status_resume, data = patner_hiv_complete_resume)
summary(cox_fit_parthiv_resume)

#Adjusting for age as a confounder
cox_fit_parthiv_resume_adj <- coxph(surv_parthiv_resume ~ partner_HIV_status_resume+age_enr_cof, data = patner_hiv_complete_resume)
summary(cox_fit_parthiv_resume_adj)

#clustering
cox_fit_parthiv_resume_clus  <- coxph(surv_parthiv_resume ~ partner_HIV_status_resume+cluster(Facility), data = patner_hiv_complete_resume)
summary(cox_fit_parthiv_resume_clus)
cox_fit_parthiv_resume_adj_clus <- coxph(surv_parthiv_resume ~ partner_HIV_status_resume+age_enr_cof+cluster(Facility), data = patner_hiv_complete_resume)
summary(cox_fit_parthiv_resume_adj_clus)


#22. level of partner education
table(had_partner_resume$partner_education)
patner_edu_complete_resume<-had_partner_resume%>%
  filter(partner_education=="Not completed basic school"|partner_education=="Completed basic school")
summarytools::freq(patner_edu_complete_resume$partner_education)
#summary
surv_partner_ed_resume <- Surv(patner_edu_complete_resume$visit1_resume,patner_edu_complete_resume$fp_use_resume)
surv_partner_ed_resume
fit_part_edu_resume <- survfit(surv_partner_ed_resume ~ partner_education, data = patner_edu_complete_resume)
summary(fit_part_edu_resume)

#plot
names(fit_part_edu_resume$strata) <- gsub("partner_education=", "", names(fit_part_edu_resume$strata))
ggsurvplot(fit_part_edu_resume, data = patner_edu_complete_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Partner education among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3.4,
           censor.shape = "O", censor.size = 5)

class(patner_edu_complete_resume$partner_education)
table(patner_edu_complete_resume$partner_education)
patner_edu_complete_resume$partner_education = relevel(patner_edu_complete_resume$partner_education, ref = "Completed basic school")

cox_fit_part_edu_resume  <- coxph(surv_partner_ed_resume ~ partner_education, data = patner_edu_complete_resume)
summary(cox_fit_part_edu_resume)

#Adjusting for age as a confounder
cox_fit_part_edu_resume_adj <- coxph(surv_partner_ed_resume ~ partner_education+age_enr_cof, data = patner_edu_complete_resume)
summary(cox_fit_part_edu_resume_adj)

#cluster
cox_fit_part_edu_resume_clus  <- coxph(surv_partner_ed_resume ~ partner_education+cluster(Facility), data =patner_edu_complete_resume)
summary(cox_fit_part_edu_resume_clus)
cox_fit_part_edu_resume_adj_clus <- coxph(surv_partner_ed_resume ~ partner_education+age_enr_cof+cluster(Facility), data = patner_edu_complete_resume)
summary(cox_fit_part_edu_resume_adj_clus)

#23.Partner financial support 
table(had_partner_resume$pc_financial)
#rename employed variable
had_partner_resume<-had_partner_resume%>%
  rename(Financial_support=pc_financial)
#recode
had_partner_resume<-had_partner_resume%>%
  mutate(Financial_support=recode(Financial_support,
                                  "0"="No financial support",
                                  "1"="Partner provided financial support"))

surv_fin_resume <- Surv(had_partner_resume$visit1_resume,had_partner_resume$fp_use_resume)
surv_fin_resume
#summary
fit_fin_resume <- survfit(surv_fin_resume~ Financial_support, data = had_partner_resume)
summary(fit_fin_resume)

#plot
names(fit_fin_resume$strata) <- gsub("Financial_support=", "", names(fit_fin_resume$strata))
ggsurvplot(fit_fin_resume, data = had_partner_resume,
           tables.y.text = FALSE,
           risk.table = "nrisk_cumcensor",size=1,
           palette =c("turquoise4", "blue4","#E7B800"), size=1,
           xlim=c(0,40),break.time.by=6,
           surv.median.line="hv", conf.type="plain",conf.int = 0.95,
           #ggtheme = theme_survminer(),
           ggtheme = theme_classic(),
           tables.theme = theme_cleantable(),
           legend.title = "Financial support among women who resumed sex",
           xlab = "Time to FP (weeks)", size=1,fun="event",
           ylab="Probability", pval = TRUE, pval.coord = c(7, 1),fontsize=3,
           censor.shape = "O", censor.size = 5)

class(had_partner_resume$Financial_support)
table(had_partner_resume$Financial_support)
had_partner_resume$Financial_support = relevel(had_partner_resume$Financial_support, ref = "Yes")

cox_fit_fin_resume  <- coxph(surv_fin_resume ~ Financial_support, data = had_partner_resume)
summary(cox_fit_fin_resume)

#Adjusting for age as a confounder
cox_fit_fin_resume_adj <- coxph(surv_fin_resume ~ Financial_support+age_enr_cof, data = had_partner_resume)
summary(cox_fit_fin_resume_adj)

#clustering
cox_fit_fin_resume_clus  <- coxph(surv_fin_resume ~ Financial_support+cluster(Facility), data = had_partner_resume)
summary(cox_fit_fin_resume_clus)
cox_fit_fin_resume_adj_clus <- coxph(surv_fin_resume ~ Financial_support+age_enr_cof+cluster(Facility), data = had_partner_resume)
summary(cox_fit_fin_resume_adj_clus)


################ *END OF SUB-ANALYSIS*


##Forest plot
# Load necessary libraries
# Load necessary libraries


# Prepare the data

data <- data.frame(
  Group = c(
    "Currently without a partner", 
    "Not residing with partner", 
    "Not completed primary school", 
    "High HIV risk by NASCOP", 
    "Has regular employment", 
    "Low social support", 
    "Had IPV (High HITS score ≥10)", 
    "Had signs of moderate-to-severe depression", 
    "Primigravida", 
    "No living child", 
    "Four children and below", 
    "Had history of miscarriage/stillbirth",
    "Had history of miscarriage",  
    "Had history of stillbirth", 
    "Youngest child below 2 years", 
    "Youngest child 5 years and above", 
    "Pregnancy not intended", 
    "Pregnancy ambivalent", 
    "Any male child", 
    "Partner HIV status positive", 
    "Partner HIV status unknown", 
    "Partner not completed primary school", 
    "No provision of financial support from partner"
  ),
  est = c(
    0.69, 
    0.67, 
    0.86, 
    0.94, 
    1.06, 
    0.88, 
    0.98, 
    0.97, 
    0.71, 
    0.99, 
    1.27,
    0.82,
    0.86,  
    0.75, 
    0.87, 
    0.96, 
    0.83, 
    0.79, 
    0.99, 
    1.02, 
    1.04, 
    1.10, 
    0.69
  ),
  lower_CI = c(
    0.58, 
    0.62, 
    0.79, 
    0.85, 
    0.93, 
    0.80, 
    0.87, 
    0.86, 
    0.64, 
    0.78, 
    1.05,
    0.75,
    0.76, 
    0.59, 
    0.77, 
    0.88, 
    0.71, 
    0.71, 
    0.91, 
    0.84, 
    0.95, 
    0.97, 
    0.59
  ),
  upper_CI = c(
    0.82, 
    0.72, 
    0.94, 
    1.04, 
    1.20, 
    0.97, 
    1.11, 
    1.10, 
    0.81, 
    1.26, 
    1.53, 
    0.89,
    0.97, 
    0.96, 
    0.98, 
    1.05, 
    0.98, 
    0.89, 
    1.08, 
    1.22, 
    1.13, 
    1.26, 
    0.79
  )
)

# Set the factor levels for the Group column in the specified order
data$Group <- factor(data$Group, levels = c(
  "No provision of financial support from partner", 
  "Partner not completed primary school", 
  "Partner HIV status unknown", 
  "Partner HIV status positive", 
  "Any male child", 
  "Pregnancy ambivalent", 
  "Pregnancy not intended", 
  "Youngest child 5 years and above", 
  "Youngest child below 2 years", 
  "Had history of stillbirth", 
  "Had history of miscarriage", 
  "Had history of miscarriage/stillbirth",
  "Four children and below", 
  "No living child", 
  "Primigravida", 
  "Had signs of moderate-to-severe depression", 
  "Had IPV (High HITS score ≥10)", 
  "Low social support", 
  "Has regular employment", 
  "High HIV risk by NASCOP", 
  "Not completed primary school", 
  "Not residing with partner", 
  "Currently without a partner"
))

data$se <- (log(data$upper_CI) - log(data$est))/1.96

# Add a blank column for the forest plot to display CI.
# Adjust the column width with space, and increase the number of spaces below 
# to have a larger area to draw the CI. 
data$` ` <- paste(rep(" ", 20), collapse = " ")

data$`HR (95% CI)` <- ifelse(is.na(data$se), "",
                             sprintf("%.2f (%.2f to %.2f)",
                                     data$est, data$lower_CI, data$upper_CI))

# data$Group <- ifelse(is.na(data$HR), 
#                          data$Group,
#                       paste0("   ", data$Group))

tm_ov <- forest_theme(base_size = 10,
                      ci_pch = 15,
                      ci_col = "blue",
                      ci_fill = "black",
                      ci_alpha = 0.8,
                      ci_lty = 1,
                      ci_lwd = 1.0,
                      refline_lwd = gpar(lwd = 1, lty = "dashed", col = "grey20"),
                      vertline_lwd = 1,
                      vertline_lty = "dashed",
                      vertline_col = "grey20",
                      # Table cell padding, width 4 and heights 3
                      core = list(fg_params=list(hjust = 1, x = 0.9),
                                  bg_params=list(fill = c("white"))),
                      colhead=list(fg_params=list(hjust=0.5, x=0.5)))


p_overall <- forest(data[,c(1,6,7)],
                    est = data$est,
                    lower = data$lower_CI, 
                    upper = data$upper_CI,
                    sizes = data$se*1.5,
                    ci_column = 2,
                    ci_color = "blue",
                    ref_line = 1,
                    xlim = c(0, 3),
                    ticks_at = c(0.5, 1, 1.5, 2),
                    title = "Forest plots of cofactors of earlier 
        uptake of modern postpartum family planning methods 
              Overall population",
                    theme = tm_ov)


###sex resumption

# Create the updated data frame with new values
data1 <- data.frame(
  Group = c(
    "Currently without a partner", 
    "Not residing with partner", 
    "Not completed primary school", 
    "High HIV risk by NASCOP", 
    "Has regular employment", 
    "Low social support", 
    "Had IPV (High HITS score ≥10)", 
    "Had signs of moderate-to-severe depression", 
    "Primigravida", 
    "No living child", 
    "Four children and below", 
    "Had history of miscarriage/stillbirth",
    "Had history of miscarriage",  
    "Had history of stillbirth", 
    "Youngest child below 2 years", 
    "Youngest child 5 years and above", 
    "Pregnancy not intended", 
    "Pregnancy ambivalent", 
    "Any male child", 
    "Partner HIV status positive", 
    "Partner HIV status unknown", 
    "Partner not completed primary school", 
    "No provision of financial support from partner"
  ),
  est = c(
    0.90, 
    0.74, 
    0.83, 
    0.92, 
    1.06, 
    0.88, 
    1.01, 
    0.97, 
    0.82, 
    0.97, 
    1.22, 
    0.80,
    0.85,  
    0.71, 
    0.89, 
    1.02, 
    0.93, 
    0.87, 
    1.00, 
    1.05, 
    1.03, 
    1.08, 
    0.79
  ),
  lower_CI = c(
    0.74, 
    0.68, 
    0.76, 
    0.83, 
    0.92, 
    0.80, 
    0.89, 
    0.86, 
    0.71, 
    0.80, 
    1.04,
    0.74,
    0.75, 
    0.55, 
    0.79, 
    0.92, 
    0.80, 
    0.78, 
    0.93, 
    0.85, 
    0.93, 
    0.93, 
    0.71
  ),
  upper_CI = c(
    1.09, 
    0.81, 
    0.91, 
    1.03, 
    1.20, 
    0.97, 
    1.14, 
    1.10, 
    0.94, 
    1.17, 
    1.43,
    0.87,
    0.96, 
    0.92, 
    1.00, 
    1.13, 
    1.08, 
    0.96, 
    1.09, 
    1.28, 
    1.13, 
    1.25, 
    0.87
  )
)

# Set the factor levels for the Group column in the specified order
data1$Group <- factor(data1$Group, levels = c(
  "No provision of financial support from partner", 
  "Partner not completed primary school", 
  "Partner HIV status unknown", 
  "Partner HIV status positive", 
  "Any male child", 
  "Pregnancy ambivalent", 
  "Pregnancy not intended", 
  "Youngest child 5 years and above", 
  "Youngest child below 2 years", 
  "Had history of stillbirth", 
  "Had history of miscarriage",  
  "Had history of miscarriage/stillbirth",
  "Four children and below", 
  "No living child", 
  "Primigravida", 
  "Had signs of moderate-to-severe depression", 
  "Had IPV (High HITS score ≥10)", 
  "Low social support", 
  "Has regular employment", 
  "High HIV risk by NASCOP", 
  "Not completed primary school", 
  "Not residing with partner", 
  "Currently without a partner"
))

data1$se <- (log(data1$upper_CI) - log(data1$est))/1.96

# Add a blank column for the forest plot to display CI.
# Adjust the column width with space, and increase the number of spaces below 
# to have a larger area to draw the CI. 
data1$` ` <- paste(rep(" ", 20), collapse = " ")

data1$`HR (95% CI)` <- ifelse(is.na(data1$se), "",
                              sprintf("%.2f (%.2f to %.2f)",
                                      data1$est, data1$lower_CI, data1$upper_CI))

# data1$Group <- ifelse(is.na(data1$HR), 
#                          data1$Group,
#                       paste0("   ", data1$Group))

tm <- forest_theme(base_size = 10,
                   ci_pch = 15,
                   ci_col = "blue",
                   ci_fill = "black",
                   ci_alpha = 0.8,
                   ci_lty = 1,
                   ci_lwd = 1.0,
                   refline_lwd = gpar(lwd = 1, lty = "dashed", col = "grey20"),
                   vertline_lwd = 1,
                   vertline_lty = "dashed",
                   vertline_col = "grey20",
                   # Table cell padding, width 4 and heights 3
                   core = list(fg_params=list(hjust = 1, x = 0.9),
                               bg_params=list(fill = c("white"))),
                   colhead=list(fg_params=list(hjust=0.5, x=0.5)))


p_1 <- forest(data1[,c(1,6,7)],
              est = data1$est,
              lower = data1$lower_CI, 
              upper = data1$upper_CI,
              sizes = data1$se*1.5,
              ci_column = 2,
              ci_color = "blue",
              ref_line = 1,
              xlim = c(0, 3),
              ticks_at = c(0.5, 1, 1.5, 2),
              title = "Women who resumed sex",
              theme = tm)



comb_cof<-cowplot::plot_grid(p_overall, p_1, nrow = 1,
                             rel_widths=4.2, rel_heights=0.5)

ggsave("comb_cof.png", plot = comb_cof, width = 25, height = 10,bg="white")





