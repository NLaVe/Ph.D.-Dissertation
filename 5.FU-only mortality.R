
### Follow up ###

#Clear things we won't use to ease computation
#{#remove( aux,aux2,aux3,aux4,aux5,Charlson_AUX,Cohort_t,Cohort1,Cohort1_BL,Control,Control_b,Hosp_cases_AUX,ICD10,keep_aux,
#        Long_care_AUX,Long_care_AUX_AUX,Matching,output,Participants,Participants_db,periods,Phy_cases_AUX,Phy_cases_AUX2,Prescription_AUX,
#        reduced_data,Rehab_AUX,scored_data,Treat,Work_perm_AUX,Work_temp_AUX)}

#Create FU times
ifelse(match_check=="yes",fu_t<-c(-2),
{ifelse(Cohort=="2006-2009",
       ifelse(ass_periods=="long",
              fu_t<-c(0.5013699,1,2,3,4,5,6,7),#8)
              fu_t<-c(0.5013699,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8)),
       ifelse(ass_periods=="long",
              fu_t<-c(0.5013699,1,2,3,4,5,5),
              fu_t<-c(0.5013699,1,1.5,2,2.5,3,3.5,4,4.5,5)))})

#Create outputs from survival all FU periods
outs_all<-matrix(data=NA, nrow = length(fu_t)+1, ncol=18*2)

for(j in c(1:length(fu_t))){

  #create FU period
  fu<-fu_t[[j]] #(In years)
  ifelse(j==1,"nada",fu_p<-fu_t[[j-1]])
  
  Cohort1_yfu<-FU_DB

# interval(START_fu,START_fu+365*fu) #Period of assessment to use in each calculation

###Start###
#ifelse(j==0,"nada",Insured_days <- readRDS("Bases/Insured_days.rds"))
Insured_days_AUX<-as.data.table(unique(Insured_days[FS_Versicherte %in% Cohort1_yfu$FS_Versicherte,c("FS_Versicherte","FS_Datum_Ereignis","last_insurance","begin_insurance","Versichertentage_Anz")]))
Insured_days_AUX<-merge(Insured_days_AUX,Cohort1_yfu[,c("FS_Versicherte_p","FS_Versicherte","START_fu")],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
Insured_days_AUX<-unique(Insured_days_AUX[,c("FS_Versicherte_p","FS_Datum_Ereignis","last_insurance","begin_insurance","Versichertentage_Anz","START_fu")])
Insured_days_AUX[,last_in_d_y:=FS_Datum_Ereignis+Versichertentage_Anz]
ifelse(j==1,
Insured_days_AUX<-Insured_days_AUX[FS_Datum_Ereignis %within% interval(START_fu,START_fu+365*fu)|last_in_d_y %within% interval(START_fu,START_fu+365*fu)|(last_in_d_y>START_fu+365*fu & FS_Datum_Ereignis<START_fu)],
ifelse(j!=7,
Insured_days_AUX<-Insured_days_AUX[FS_Datum_Ereignis %within% interval(START_fu+365*(fu_p)+1,START_fu+365*fu)|last_in_d_y %within% interval(START_fu+365*(fu_p)+1,START_fu+365*fu)|(last_in_d_y>START_fu+365*fu & FS_Datum_Ereignis<START_fu+365*(fu_p)+1)],
Insured_days_AUX<-Insured_days_AUX[FS_Datum_Ereignis %within% interval(START_fu+365*(0.5013699)+1,START_fu+365*fu)|last_in_d_y %within% interval(START_fu+365*(0.5013699)+1,START_fu+365*fu)|(last_in_d_y>START_fu+365*fu & FS_Datum_Ereignis<START_fu+365*(0.5013699)+1)]))
Insured_days_AUX<-Insured_days_AUX[,days_insured:=sum(Versichertentage_Anz, na.rm = T), by=FS_Versicherte_p]
Insured_days_AUX<-Insured_days_AUX[,last_in_d_y:=max(last_in_d_y),by=FS_Versicherte_p]
Insured_days_AUX<-Insured_days_AUX[last_insurance<last_in_d_y,last_insurance:=last_in_d_y]
Insured_days_AUX<-unique(Insured_days_AUX[,c("FS_Versicherte_p","begin_insurance","last_insurance","days_insured")])
Cohort1_yfu<-merge(Cohort1_yfu,Insured_days_AUX,by.x = "FS_Versicherte_p", by.y = "FS_Versicherte_p",all.x = TRUE)
##remove(Insured_days_AUX,Insured_days)

### Add basic(time invariant) variables to cohort
Cohort1_yfu<-merge(Cohort1_yfu,Insurees[,c('sex','FS_Versicherte_Todesdatum','FS_Versicherte_Geburtsdatum','FS_Krankenkassen','FS_Versicherte')],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

## Add variable period living in Kinzigtal
Cohort1_yfu<-merge(Cohort1_yfu,Time_inK_AUX,by.x = "FS_Versicherte", by.y = "PS_Versicherter_ID",all.x = TRUE)

#add variable death
Cohort1_yfu<-Cohort1_yfu[,death:=0]
Cohort1_yfu$FS_Versicherte_Todesdatum<-as.Date(Cohort1_yfu$FS_Versicherte_Todesdatum, "%Y-%m-%d")
ifelse(j==1,
Cohort1_yfu<-Cohort1_yfu[FS_Versicherte_Todesdatum %within% interval(START_fu,START_fu+365*fu),death:=1],
ifelse(j!=7,
Cohort1_yfu<-Cohort1_yfu[FS_Versicherte_Todesdatum %within% interval(START_fu+365*(fu_p)+1,START_fu+365*fu),death:=1],
Cohort1_yfu<-Cohort1_yfu[FS_Versicherte_Todesdatum %within% interval(START_fu+365*(0.5013699)+1,START_fu+365*fu),death:=1]))

# Add variable left region or insurance (attrition)
Cohort1_yfu<-Cohort1_yfu[,att:=0]
Cohort1_yfu$last_insurance<-as.Date(Cohort1_yfu$last_insurance, "%Y-%m-%d")
ifelse(j==1,
Cohort1_yfu<-Cohort1_yfu[last_insurance %within% interval(START_fu,START_fu+365*fu) & death==0,att:=1],
ifelse(j!=7,
Cohort1_yfu<-Cohort1_yfu[last_insurance %within% interval(START_fu+365*(fu_p)+1,START_fu+365*fu) & death==0,att:=1],
Cohort1_yfu<-Cohort1_yfu[last_insurance %within% interval(START_fu+365*(0.5013699)+1,START_fu+365*fu) & death==0,att:=1]))

# Create days of follow up variable
Cohort1_yfu<-Cohort1_yfu[death==0 & att==0,FU:=(START_fu+365*fu)-(START_fu)]
Cohort1_yfu<-Cohort1_yfu[att==1,FU:=last_insurance-(START_fu)]
Cohort1_yfu<-Cohort1_yfu[death==1,FU:=FS_Versicherte_Todesdatum-(START_fu)]
Cohort1_yfu$FU<-as.numeric(Cohort1_yfu$FU)

#  # Add program participation in period or before.
Program_AUX<-merge(Program,Cohort1_yfu[,c("FS_Versicherte","FS_Versicherte_p","START_fu")],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
Program_AUX<-Program_AUX[FS_Ereignis_Datum_Beginn <= START_fu+365*fu]
Cohort1_yfu<-Cohort1_yfu[,Program:=0]
Cohort1_yfu<-Cohort1_yfu[FS_Versicherte %in% Program_AUX$FS_Versicherte,Program:=1]

####

# Long-term care levels by German long-term care insurance (0-4) + age at the start of
Long_care_AUX<-merge(Long_care,Cohort1_yfu[,c("FS_Versicherte","FS_Versicherte_p","START_fu","FS_Versicherte_Geburtsdatum")],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
Long_care_AUX<-unique(Long_care_AUX)
Long_care_AUX$beginn<-as.Date(Long_care_AUX$FS_Ereignis_Datum_Beginn, "%Y-%m-%d")
Long_care_AUX$ende<-as.Date(Long_care_AUX$FS_Ereignis_Datum_Ende, "%Y-%m-%d")

ifelse(match_check=="no",
       {Long_care_AUX<-Long_care_AUX[beginn <= START_fu+365*fu & ende  >= START_fu]
       Long_care_AUX<-Long_care_AUX[ende>START_fu+365*fu,ende:=START_fu+365*fu]
       Long_care_AUX<-Long_care_AUX[beginn<START_fu,beginn:=START_fu]},
       
       {Long_care_AUX<-Long_care_AUX[beginn <= START_fu & ende  >= START_fu+365*fu]
       Long_care_AUX<-Long_care_AUX[ende>START_fu,ende:=START_fu]
       Long_care_AUX<-Long_care_AUX[beginn<START_fu+365*fu,beginn:=START_fu+365*fu]})

Long_care_AUX<-Long_care_AUX[,AUX:=1]
Long_care_AUX<-Long_care_AUX[,N_LC:=sum(AUX,na.rm = TRUE),by=FS_Versicherte_p]
Long_care_AUX<-Long_care_AUX[,LoS_LC:=ende-beginn+1,by=ID]
Long_care_AUX<-unique(Long_care_AUX[,-c('ID')])
Long_care_AUX<-Long_care_AUX[,LoS_LC:=sum(LoS_LC),by=FS_Versicherte_p]
auxLC<-as.data.table(cbind(c('0','1','2','3','4','5',"H"),c('0','1','1','2','3','3','4')))
auxLC<-auxLC %>% rename(pflegestufe =V1)
auxLC<-auxLC %>% rename(LongCare =V2)
Long_care_AUX<-merge(Long_care_AUX,auxLC,by.x = "pflegestufe", by.y = "pflegestufe",all.x = TRUE,allow.cartesian=TRUE)
Long_care_AUX$LongCare<-as.numeric(Long_care_AUX$LongCare)
Long_care_AUX<-Long_care_AUX[,Mean_LC:=mean(LongCare,na.rm = TRUE),by=FS_Versicherte]
Long_care_AUX<-Long_care_AUX[,Mean_LC:=round(Mean_LC,digits=0)]
Long_care_AUX$FS_Versicherte_Geburtsdatum<-as.Date(Long_care_AUX$FS_Versicherte_Geburtsdatum, "%Y-%m-%d")
Long_care_AUX$FS_Ereignis_Datum_Beginn<-as.Date(Long_care_AUX$FS_Ereignis_Datum_Beginn, "%Y-%m-%d")
Long_care_AUX<-Long_care_AUX[Mean_LC>=1,age_LC:=(FS_Ereignis_Datum_Beginn - FS_Versicherte_Geburtsdatum)/365]
Long_care_AUX$age_LC<-as.numeric(Long_care_AUX$age_LC)
Long_care_AUX<-Long_care_AUX[LongCare==1,age_LC_1:=(FS_Ereignis_Datum_Beginn - FS_Versicherte_Geburtsdatum)/365]
Long_care_AUX$age_LC_1<-as.numeric(Long_care_AUX$age_LC_1)
Long_care_AUX<-Long_care_AUX[LongCare==2,age_LC_2:=(FS_Ereignis_Datum_Beginn - FS_Versicherte_Geburtsdatum)/365]
Long_care_AUX$age_LC_2<-as.numeric(Long_care_AUX$age_LC_2)
Long_care_AUX<-Long_care_AUX[LongCare==3,age_LC_3:=(FS_Ereignis_Datum_Beginn - FS_Versicherte_Geburtsdatum)/365]
Long_care_AUX$age_LC_3<-as.numeric(Long_care_AUX$age_LC_3)
Long_care_AUX<-Long_care_AUX[LongCare==4,age_LC_4:=(FS_Ereignis_Datum_Beginn - FS_Versicherte_Geburtsdatum)/365]
Long_care_AUX$age_LC_4<-as.numeric(Long_care_AUX$age_LC_4)
Long_care_AUX<-Long_care_AUX[,age_LC:=min(age_LC,na.rm = TRUE),by=FS_Versicherte]
Long_care_AUX<-Long_care_AUX[,age_LC_1:=min(age_LC_1,na.rm = TRUE),by=FS_Versicherte]
Long_care_AUX<-Long_care_AUX[,age_LC_2:=min(age_LC_2,na.rm = TRUE),by=FS_Versicherte]
Long_care_AUX<-Long_care_AUX[,age_LC_3:=min(age_LC_3,na.rm = TRUE),by=FS_Versicherte]
Long_care_AUX<-Long_care_AUX[,age_LC_4:=min(age_LC_4,na.rm = TRUE),by=FS_Versicherte]
Long_care_AUX_AUX<-unique(Long_care_AUX[,c("FS_Versicherte_p","age_LC","age_LC_1","age_LC_2","age_LC_3","age_LC_4")])
Long_care_AUX_AUX<-unique(Long_care_AUX_AUX)
Long_care_AUX_AUX<-Long_care_AUX_AUX[age_LC<65,age_LC:=NA]
Long_care_AUX_AUX<-Long_care_AUX_AUX[age_LC_1<65,age_LC_1:=NA]
Long_care_AUX_AUX<-Long_care_AUX_AUX[age_LC_2<65,age_LC_2:=NA]
Long_care_AUX_AUX<-Long_care_AUX_AUX[age_LC_3<65,age_LC_3:=NA]
Long_care_AUX_AUX<-Long_care_AUX_AUX[age_LC_4<65,age_LC_4:=NA]
Long_care_AUX_AUX<-do.call(data.table,lapply(Long_care_AUX_AUX, function(x) replace(x, is.infinite(x),NA)))
Cohort1_yfu<-merge(Cohort1_yfu,Long_care_AUX_AUX,by.x = "FS_Versicherte_p", by.y = "FS_Versicherte_p",all.x = TRUE)

# Add LE at birth and YPLLG
LE <- as.data.table(read_excel("E:/Nicolas - OptiMedis/Ablage_Nicolas/GK comparison/S31-Lebenserwartung-Geburt-Geschlecht-ab-1871_xls.xls"))
Cohort1_yfu<-Cohort1_yfu[,year_g:=year(FS_Versicherte_Geburtsdatum)]
Cohort1_yfu<-merge(Cohort1_yfu,LE,by.x = "year_g", by.y = "year",all.x = TRUE, allow.cartesian = T)
Cohort1_yfu$FS_Versicherte_Geburtsdatum<-as.Date(Cohort1_yfu$FS_Versicherte_Geburtsdatum, "%Y-%m-%d")
Cohort1_yfu$FS_Versicherte_Todesdatum<-as.Date(Cohort1_yfu$FS_Versicherte_Todesdatum, "%Y-%m-%d")
Cohort1_yfu<-Cohort1_yfu[death == 1,age_atod := (FS_Versicherte_Todesdatum-FS_Versicherte_Geburtsdatum)/365]
Cohort1_yfu<-Cohort1_yfu[sex=='w' & death == 1,YPLLG := Women - age_atod]
Cohort1_yfu<-Cohort1_yfu[sex=='m' & death == 1,YPLLG := Men - age_atod]

#take NA out of numeric vars
Cohort1_yfu<-as.data.frame(Cohort1_yfu)
Cohort1_yfu[c('FU','days_insured','sex')][is.na(Cohort1_yfu[c('FU','days_insured','sex')])] <- 0
Cohort1_yfu<-as.data.table(Cohort1_yfu)

#Create numeric var for sex.
Cohort1_yfu<-Cohort1_yfu[,sex_d:=0]
Cohort1_yfu<-Cohort1_yfu[sex=="w",sex_d:=1]

# Create cumulative deaths
Survival<-Cohort1_yfu[,c('FS_Versicherte','FU','days_insured','death','treat','FS_Versicherte_Geburtsdatum','YPLLG','age_atod','sex',"weights","age_LC","age_LC_1","age_LC_2","age_LC_3","age_LC_4")]
Survival$YPLLG<-as.numeric(Survival$YPLLG)
Survival$age_atod<-as.numeric(Survival$age_atod)

#Create outputs from survival
outs<-matrix(data=NA, nrow = 18, ncol=3)
outs[1,1] <- "Treat"
outs[1,2] <- "Control"

outs[2,1] <- sum(Survival[treat==1,death*weights])
outs[3,1] <- sum(Survival[treat==1,death*weights],na.rm = T)/(sum(Survival[treat==1,c('weights')]))
outs[4,1] <- 1-as.numeric(outs[3,1])
outs[5,1] <- sum(Survival[treat==1,YPLLG*weights],na.rm = T)/(sum(Survival[treat==1 & is.na(age_atod)==FALSE,c('weights')]))
outs[6,1] <- sum(Survival[treat==1,age_atod*weights], na.rm = T)/(sum(Survival[treat==1 & is.na(age_atod)==FALSE,c('weights')]))
outs[7,1] <- sd(Survival[treat==1,YPLLG*weights],na.rm = T)
outs[8,1] <- sd(Survival[treat==1,age_atod*weights], na.rm = T)
outs[9,1] <- sum(Survival[treat==1,age_LC*weights], na.rm = T)/(sum(Survival[treat==1 & is.na(age_LC)==FALSE,c('weights')]))
outs[10,1] <- sd(Survival[treat==1,age_LC*weights], na.rm = T)
outs[11,1] <- sum(Survival[treat==1,age_LC_1*weights], na.rm = T)/(sum(Survival[treat==1 & is.na(age_LC_1)==FALSE,c('weights')]))
outs[12,1] <- sd(Survival[treat==1,age_LC_1*weights], na.rm = T)
outs[13,1] <- sum(Survival[treat==1,age_LC_2*weights], na.rm = T)/(sum(Survival[treat==1 & is.na(age_LC_2)==FALSE,c('weights')]))
outs[14,1] <- sd(Survival[treat==1,age_LC_2*weights], na.rm = T)
outs[15,1] <- sum(Survival[treat==1,age_LC_3*weights], na.rm = T)/(sum(Survival[treat==1 & is.na(age_LC_3)==FALSE,c('weights')]))
outs[16,1] <- sd(Survival[treat==1,age_LC_3*weights], na.rm = T)
outs[17,1] <- sum(Survival[treat==1,age_LC_4*weights], na.rm = T)/(sum(Survival[treat==1 & is.na(age_LC_4)==FALSE,c('weights')]))
outs[18,1] <- sd(Survival[treat==1,age_LC_4*weights], na.rm = T)

outs[2,2] <- sum(Survival[treat==0,death*weights],na.rm = T)
outs[3,2] <- sum(Survival[treat==0,death*weights],na.rm = T)/(sum(Survival[treat==0,c('weights')]))
outs[4,2] <- 1-as.numeric(outs[3,2])
outs[5,2] <- sum(Survival[treat==0,YPLLG*weights],na.rm = T)/(sum(Survival[treat==0 & is.na(age_atod)==FALSE,c('weights')]))
outs[6,2] <- sum(Survival[treat==0,age_atod*weights], na.rm = T)/(sum(Survival[treat==0 & is.na(age_atod)==FALSE,c('weights')]))
outs[7,2] <- sd(Survival[treat==0,YPLLG*weights],na.rm = T)
outs[8,2] <- sd(Survival[treat==0,age_atod*weights], na.rm = T)
outs[9,2] <- sum(Survival[treat==0,age_LC*weights], na.rm = T)/(sum(Survival[treat==0 & is.na(age_LC)==FALSE,c('weights')]))
outs[10,2] <- sd(Survival[treat==0,age_LC*weights], na.rm = T)
outs[11,2] <- sum(Survival[treat==0,age_LC_1*weights], na.rm = T)/(sum(Survival[treat==0 & is.na(age_LC_1)==FALSE,c('weights')]))
outs[12,2] <- sd(Survival[treat==0,age_LC_1*weights], na.rm = T)
outs[13,2] <- sum(Survival[treat==0,age_LC_2*weights], na.rm = T)/(sum(Survival[treat==0 & is.na(age_LC_2)==FALSE,c('weights')]))
outs[14,2] <- sd(Survival[treat==0,age_LC_2*weights], na.rm = T)
outs[15,2] <- sum(Survival[treat==0,age_LC_3*weights], na.rm = T)/(sum(Survival[treat==0 & is.na(age_LC_3)==FALSE,c('weights')]))
outs[16,2] <- sd(Survival[treat==0,age_LC_3*weights], na.rm = T)
outs[17,2] <- sum(Survival[treat==0,age_LC_4*weights], na.rm = T)/(sum(Survival[treat==0 & is.na(age_LC_4)==FALSE,c('weights')]))
outs[18,2] <- sd(Survival[treat==0,age_LC_4*weights], na.rm = T)

outs[1,3] <- "Group"
outs[2,3] <- "Deaths"
outs[3,3] <- "Percentage"
outs[4,3] <- "Survival %"
outs[5,3] <- "YPLLG"
outs[6,3] <- "Age at the time of death"
outs[7,3] <- "YPLLG-sd"
outs[8,3] <- "Age at the time of death-sd"
outs[9,3] <- 'Age at the start of long-term care'
outs[10,3] <- 'Age at the start of long-term care - sd'
outs[11,3] <- 'Age at the start of long-term care 1'
outs[12,3] <- 'Age at the start of long-term care 1 - sd'
outs[13,3] <- 'Age at the start of long-term care 2'
outs[14,3] <- 'Age at the start of long-term care 2 - sd'
outs[15,3] <- 'Age at the start of long-term care 3'
outs[16,3] <- 'Age at the start of long-term care 3 - sd'
outs[17,3] <- 'Age at the start of long-term care 4'
outs[18,3] <- 'Age at the start of long-term care 4 - sd'

#Create outputs from survival all FU periods
outs_all[1,]<-cbind(outs[,3],outs[,3])
outs_all[1+j,]<-cbind(outs[,1],outs[,2])
}

outs_all

##Survival Analysis
library(survival)
library(survminer)
library(dplyr)

ifelse(Match_method=="PSM",{
  Survival$treat <- factor(Survival$treat, 
                           levels = c("0", "1"), 
                           labels = c("PSM_Control", "PSM_Treatment"))},{
  Survival$treat <- factor(Survival$treat, 
                           levels = c("0", "1"), 
                           labels = c("EB_Control(w)", "EB_Treatment"))})

Survival <- Survival %>% rename(Group = treat)

Survival$Program <- factor(Survival$Program, 
                           levels = c("0", "1"), 
                           labels = c("N", "Y"))
#censoring first 183 days
Survival<-Survival[FU>182]

# Fit survival data using the Kaplan-Meier method
surv_object <- Surv(time = Survival$FU, event = Survival$death)

#The next step is to fit the Kaplan-Meier curves.
fit1 <- survfit(surv_object ~ Group, data = Survival)
print(fit1, print.rmean=TRUE)


#examine the corresponding survival curve by passing the survival object to the ggsurvplot function
graph<-ggsurvplot(fit1, data = Survival, pval = TRUE, ylim = c(0.93, 1), xlim =c(0,1825))
#ylim = c(0.75, 1)

##
require(survey)
svyDesignObject<- svydesign(id=~1,weights=Survival$weights,data=Survival)
svyKm <- svykm(surv_object ~ Group,design=svyDesignObject,se=F)
##
graph3 <-jskm::svyjskm(svyKm,xlim=c(0,1825),ylim = c(0.92, 1)
                       ,dashed = TRUE)
#add CI
#lines(fit1,conf.int=T,mark.time=F,col='green')

# Examine predictive value of residual disease status
#fit2 <- survfit(surv_object ~ age_g, data = Survival)
#ggsurvplot(fit2, data = Survival, pval = TRUE)

# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ Group, weights = weights, robust = TRUE,
                   data = Survival)
graph2<-ggforest(fit.coxph, data = Survival)
summary(fit.coxph)
# + age_g + Program

## Check weights in Control group
 check_weights<-(Cohort1_yfu[treat==0,c('FS_Versicherte', 'FS_Versicherte_p', 'weights')])
 check_weights<-check_weights[,total_weight:=sum(weights),by = 'FS_Versicherte']
 check_weights$total_weight_g <- cut(check_weights$total_weight, breaks=c(0,1,2,3,4), right = FALSE)
table(check_weights$total_weight_g)

