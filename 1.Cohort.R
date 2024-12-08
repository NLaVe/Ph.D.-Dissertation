
#Packages####
{
  library(data.table)
  library(dplyr)
  library(RODBCDBI)
  library(gmodels)
  library(odbc)
  library(DBI)
  library(lubridate)
  library(tidyr)
  library(readxl)
  library(stddiff)
  library(matrixStats)
}
#Data####
#Hosp_cases <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Hosp_cases.rds")
#Hosp_ICD <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Hosp_ICD.rds")
#Hosp_ICD2 <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Hosp_ICD2.rds")
#Insured_days <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Insured_days.rds")
#Insurees <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Insurees.rds")
#Long_care <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Long_care.rds")
#Phy_cases <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Phy_cases.rds")
#Phy_ICD <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Phy_ICD.rds")
#Phy_ICD2 <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Phy_ICD2.rds")
#Prescription <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Prescription.rds")
#Rehab <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Rehab.rds")
#Work_perm <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Work_perm.rds")
#Work_temp <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Work_temp.rds")
#Time_inK <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Time_inK.rds")
#Participants_db <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Participants_db.rds")
#Charlson <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Charlson.rds")
#Program <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Program.rds")
#Cost <- readRDS("O:/Ablagen/Ablage_Nicolas/GK comparison/Bases/Cost.rds")
## Missing: 1. date of retirement

## Create cohort: had insured days from start cohort to end cohort.

ifelse(Cohort=="2006-2009",
        {begin<-"20090101"
         end<-"20101231"},
        {begin<-"20060101"
         end<-"20131231"})
interval<-interval(ymd(begin),ymd(end)) #Period of assessment


###Start###
Cohort1<-as.data.table(unique(Insured_days[FS_Datum_Ereignis %within% interval,c("FS_Versicherte","FS_Datum_Ereignis","last_insurance","begin_insurance","Versichertentage_Anz")]))
Cohort1<-Cohort1[,days_insured:=sum(Versichertentage_Anz, na.rm = T), by=FS_Versicherte]
Cohort1<-Cohort1[,last_in_d_y:=FS_Datum_Ereignis+Versichertentage_Anz]
Cohort1<-Cohort1[,last_in_d_y:=max(last_in_d_y),by=FS_Versicherte]
Cohort1<-Cohort1[last_insurance<last_in_d_y,last_insurance:=last_in_d_y]
Cohort1<-unique(Cohort1[,c("FS_Versicherte","begin_insurance","last_insurance","days_insured")])

### Add basic(time invariant) variables to cohort
Cohort1<-merge(Cohort1,Insurees,by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

## Add variable period living in Kinzigtal
Time_inK$scd_start<-as.Date(Time_inK$scd_start, "%Y-%m-%d")
Time_inK$scd_end<-as.Date(Time_inK$scd_end, "%Y-%m-%d")
Time_inK<-Time_inK[,beginn:=min(scd_start),by=PS_Versicherter_ID]
Time_inK<-Time_inK[,ende:=max(scd_end),by=PS_Versicherter_ID]
Time_inK_AUX<-unique(Time_inK[,c("PS_Versicherter_ID","beginn","ende")])
Cohort1<-merge(Cohort1,Time_inK_AUX,by.x = "FS_Versicherte", by.y = "PS_Versicherter_ID",all.x = TRUE)

## Add date of GK participation
Participants<-Participants_db[,GK_begin:=as.Date(FS_Ereignis_Datum_Beginn, "%Y-%m-%d")]
Participants<-Participants[,GK_end:=as.Date(FS_Ereignis_Datum_Ende, "%Y-%m-%d")]
Participants<-Participants[,GK_begin:=min(GK_begin),by = FS_Versicherte]
Participants<-Participants[,GK_end:=max(GK_end),by = FS_Versicherte]
Participants<-unique(Participants[,c("FS_Versicherte","GK_begin","GK_end")])
Participants<-Participants[,GK_begin:=min(GK_begin),by = "FS_Versicherte"]
Participants<-Participants[,GK_end:=max(GK_end),by = "FS_Versicherte"]
Participants<-unique(Participants[,c("FS_Versicherte","GK_begin","GK_end")])
Cohort1<-merge(Cohort1,Participants,by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

##Add dummy Variable of GK participation
Cohort1<-Cohort1[,GK_part:=0]
Cohort1$GK_begin<-as.Date(Cohort1$GK_begin,"%Y-%m-%d")
Cohort1<-Cohort1[GK_begin <= ymd(end),GK_part:=1]
nrow(Cohort1[GK_part==1])

## Add program participation
Program_AUX<-Program[,P_begin:=as.Date(FS_Ereignis_Datum_Beginn, "%Y-%m-%d")]
Program_AUX<-Program_AUX[,P_end:=as.Date(FS_Ereignis_Datum_Ende, "%Y-%m-%d")]
Program_AUX<-Program_AUX[,P_begin:=min(P_begin),by = FS_Versicherte]
Program_AUX<-Program_AUX[,P_end:=max(P_end),by = FS_Versicherte]
Program_AUX<-unique(Program_AUX[,c("FS_Versicherte","P_begin","P_end")])
Program_AUX<-Program_AUX[,P_begin:=min(P_begin),by = "FS_Versicherte"]
Program_AUX<-Program_AUX[,P_end:=max(P_end),by = "FS_Versicherte"]
Program_AUX<-unique(Program_AUX[,c("FS_Versicherte","P_begin","P_end")])
Cohort1<-merge(Cohort1,Program_AUX,by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

##Add dummy Variable of program participation
Cohort1<-Cohort1[,P_part:=0]
Cohort1$P_begin<-as.Date(Cohort1$P_begin,"%Y-%m-%d")
Cohort1<-Cohort1[P_begin <= ymd(end),P_part:=1]
nrow(Cohort1[P_part==1])

###Change name of sex variable
Insurees<-Insurees[,sex:=Vers_Geschlecht]
Insurees$Vers_Geschlecht<-NULL
Cohort1<-Cohort1[,sex:=Vers_Geschlecht]
Cohort1$Vers_Geschlecht<-NULL

