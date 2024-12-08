
### Base line ###
  Cohort1_BL<-Cohort1
  Cohort1_BL$GK_begin<-as.Date(Cohort1_BL$GK_begin, "%Y-%m-%d")
  Cohort1_BL$GK_end<-as.Date(Cohort1_BL$GK_end, "%Y-%m-%d")
  Cohort1_BL$P_begin<-as.Date(Cohort1_BL$P_begin, "%Y-%m-%d")
  Cohort1_BL$P_end<-as.Date(Cohort1_BL$P_end, "%Y-%m-%d")
  
  # keep only treatment
  Cohort1_BL<-Cohort1_BL[get(treat)==1]
  
  #Create period
  ifelse(Cohort=="2006-2009",
    {start_BL<-c('20080101','20090101')#c('20040101','20050101','20060101','20070101')
     end_BL<-c('20091231','20101231')#c('20051231','20061231','20071231','20081231')
     period<-c(1:2)},
    {start_BL<-c('20040101','20050101','20060101','20070101','20080101','20090101','20100101','20110101')
    end_BL<-c('20051231','20061231','20071231','20081231','20091231','20101231','20111231','20121231')
    period<-c(1:8)})
  
  periods<-as.data.table(cbind(start_BL,end_BL,period))
  periods$period<-as.double(periods$period)
  
  # Add period
  Cohort1_BL<-Cohort1_BL[,period:=0]
  ifelse(treat=="GK_part" ,Cohort1_BL<-Cohort1_BL[,period:=year(GK_begin)-2005],#change to 2005
  Cohort1_BL<-Cohort1_BL[,period:=year(P_begin)-2005])#change to 2005
  
  #create BL for cohort.
  Cohort1_BL<-merge(Cohort1_BL,periods,by.x = "period",by.y = "period", all.x = TRUE, allow.cartesian=TRUE)
  
  #Add BL interval
  
  # Add variable death.
  fu<--1 #-1=baseline
  Cohort1_BL<-Cohort1_BL[,death:=0]
  Cohort1_BL$FS_Versicherte_Todesdatum<-as.Date(Cohort1_BL$FS_Versicherte_Todesdatum, "%Y-%m-%d")
  Cohort1_BL<-Cohort1_BL[FS_Versicherte_Todesdatum %within% interval(ymd(start_BL),ymd(end_BL)),death:=1]
  
  # Add variable left region or insurance (attrition)
  Cohort1_BL<-Cohort1_BL[,att:=0]
  Cohort1_BL$last_insurance<-as.Date(Cohort1_BL$last_insurance, "%Y-%m-%d")
  Cohort1_BL<-Cohort1_BL[last_insurance %within% interval(ymd(start_BL),ymd(end_BL)),att:=1]
  
  # add variable late integration into region, inverse attrition
  Cohort1_BL<-Cohort1_BL[,att_i:=0]
  Cohort1_BL$begin_insurance<-as.Date(Cohort1_BL$begin_insurance, "%Y-%m-%d")
  Cohort1_BL<-Cohort1_BL[begin_insurance %within% interval(ymd(start_BL),ymd(end_BL)),att_i:=1]
  
  # add variable join same year he started to be insured
  Cohort1_BL<-Cohort1_BL[,sy_int:=0]
  Cohort1_BL<-Cohort1_BL[begin_insurance>ymd(end_BL),sy_int:=1]
  
  # Create days of follow up variable
  Cohort1_BL<-Cohort1_BL[death==0 & att==0,FU:=ymd(end_BL)-ymd(start_BL)]
  Cohort1_BL<-Cohort1_BL[att==1,FU:=last_insurance-ymd(start_BL)]
  Cohort1_BL<-Cohort1_BL[death==1,FU:=FS_Versicherte_Todesdatum-ymd(start_BL)]
  Cohort1_BL<-Cohort1_BL[att_i==1,FU:=ymd(end_BL)-begin_insurance]
  Cohort1_BL<-Cohort1_BL[sy_int==1,FU:=0]
  Cohort1_BL$FU<-as.numeric(Cohort1_BL$FU)
  
  # Only keep ppl that was in the region, alive at the moment of intervention. (Did not died or left during baseline period)
  # and had 90% of all possible insured days in the baseline year.
  Cohort1_BL<-Cohort1_BL[death!=1]
  Cohort1_BL<-Cohort1_BL[att!=1]
  Cohort1_BL<-Cohort1_BL[FU>=0.9*365*2]
  
  #Cohort1_BL<-Cohort1_BL[FU<0.9*365*2] # for group comparison of people without all info in baseline
  
  # Count the dropouts
  Cohort1_BL<-Cohort1_BL[,att2:=0]
  ifelse(Cohort=="2006-2009",
         Cohort1_BL<-Cohort1_BL[last_insurance<=(ymd(end_BL)+8*365) & FS_Versicherte_Todesdatum > (ymd(end_BL)+8*365),att2:=1],
         Cohort1_BL<-Cohort1_BL[last_insurance<=(ymd(end_BL)+6*365) & FS_Versicherte_Todesdatum > (ymd(end_BL)+6*365),att2:=1])
  nrow(Cohort1_BL[att2==1])
  
  # Create number of physician visits & specialist visits
  Phy_cases_AUX<-merge(Phy_cases,Cohort1_BL[,c("FS_Versicherte","start_BL","end_BL")],by.x = "versicherter", by.y = "FS_Versicherte",all.x = TRUE, allow.cartesian=TRUE)
  Phy_cases_AUX$leistungsendedatum<-as.Date(Phy_cases_AUX$leistungsendedatum, "%Y-%m-%d")
  Phy_cases_AUX$P_begin<-as.Date(Phy_cases_AUX$P_begin, "%Y-%m-%d")
  Phy_cases_AUX<-Phy_cases_AUX[leistungsendedatum %within% interval(ymd(start_BL),ymd(end_BL))]
  Phy_cases_AUX2<-as.data.table(Phy_cases_AUX %>% group_by(versicherter, id_arzt_fall) %>% summarize(count=n()))
  Phy_cases_AUX2<-Phy_cases_AUX2[,AUX:=1]
  Phy_cases_AUX2<-Phy_cases_AUX2[,N_phy:=sum(AUX,na.rm=T),by=versicherter]
  Phy_cases_AUX<-Phy_cases_AUX[Leistungspartner=="LP KINZ",Partner:=1]
  Phy_cases_AUX<-Phy_cases_AUX[Leistungspartner!="LP KINZ",NonPartner:=1]
  Phy_cases_AUX2<-merge(unique(Phy_cases_AUX[,c('id_arzt_fall','Partner','NonPartner','fachgruppe','erbringer_ik')]), Phy_cases_AUX2, by.x = "id_arzt_fall", by.y = 'id_arzt_fall', all.y = TRUE, allow.cartesian=TRUE)
  Phy_cases_AUX2<-Phy_cases_AUX2[fachgruppe!="Allgemeinmedizin" & fachgruppe!="Unbekannt",AUX2:=1]
  Phy_cases_AUX2<-Phy_cases_AUX2[,N_spec:=sum(AUX2,na.rm=T),by=versicherter]
  Phy_cases_AUX2<-Phy_cases_AUX2[,N_cases_partner:=sum(Partner,na.rm=T),by=versicherter]
  Phy_cases_AUX2<-Phy_cases_AUX2[,N_cases_NONpartner:=sum(NonPartner,na.rm=T),by=versicherter]
  Phy_cases_AUX2<-Phy_cases_AUX2[,pp_partner:=N_cases_partner/(N_cases_partner+N_cases_NONpartner),by=versicherter]
  Phy_cases_AUX2<-unique(Phy_cases_AUX2[,c('versicherter','N_phy','pp_partner','N_spec')])
  Cohort1_BL<-merge(Cohort1_BL,unique(Phy_cases_AUX2[,c("versicherter","N_phy","pp_partner",'N_spec')]),by.x = "FS_Versicherte", by.y = "versicherter",all.x = TRUE)
  
  # Create hospital visits and length of stay
  Hosp_cases_AUX<-merge(Hosp_cases,Cohort1_BL[,c("FS_Versicherte","start_BL","end_BL")],by.x = "versicherter", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
  Hosp_cases_AUX<-unique(Hosp_cases_AUX)
  Hosp_cases_AUX$beginn<-as.Date(Hosp_cases_AUX$beginn, "%Y-%m-%d")
  Hosp_cases_AUX$ende<-as.Date(Hosp_cases_AUX$ende, "%Y-%m-%d")
  Hosp_cases_AUX<-Hosp_cases_AUX[is.na(ende)==TRUE,ende:=beginn]
  Hosp_cases_AUX<-Hosp_cases_AUX[ymd(beginn) <= ymd(end_BL) & ymd(ende) >= ymd(start_BL)]
  Hosp_cases_AUX<-Hosp_cases_AUX[ende>ymd(end_BL),ende:=ymd(end_BL)]
  Hosp_cases_AUX<-Hosp_cases_AUX[beginn<ymd(start_BL),beginn:=ymd(start_BL)]
  Hosp_cases_AUX<-Hosp_cases_AUX[,AUX:=1]
  Hosp_cases_AUX<-Hosp_cases_AUX[,N_Hosp:=sum(AUX,na.rm = TRUE),by=versicherter]
  Hosp_cases_AUX<-Hosp_cases_AUX[,LoS:=ende-beginn+1,by=ID]
  Hosp_cases_AUX<-Hosp_cases_AUX[,S_LoS:=sum(LoS,na.rm = TRUE),by=versicherter]
  Cohort1_BL<-merge(Cohort1_BL,unique(Hosp_cases_AUX[,c("versicherter","N_Hosp","S_LoS")]),by.x = "FS_Versicherte", by.y = "versicherter",all.x = TRUE)
  
  # Days of in-patient rehabilitation
  Rehab_AUX<-merge(Rehab,Cohort1_BL[,c("FS_Versicherte","start_BL","end_BL")],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
  Rehab_AUX<-unique(Rehab_AUX)
  Rehab_AUX$beginn<-as.Date(Rehab_AUX$FS_Ereignis_Datum_Beginn, "%Y-%m-%d")
  Rehab_AUX$ende<-as.Date(Rehab_AUX$FS_Ereignis_Datum_Ende, "%Y-%m-%d")
  Rehab_AUX<-Rehab_AUX[ymd(beginn) <= ymd(end_BL) & ymd(ende) >= ymd(start_BL)]
  Rehab_AUX<-Rehab_AUX[ende>ymd(end_BL),ende:=ymd(end_BL)]
  Rehab_AUX<-Rehab_AUX[beginn<ymd(start_BL),beginn:=ymd(start_BL)]
  Rehab_AUX<-Rehab_AUX[,AUX:=1]
  Rehab_AUX<-Rehab_AUX[,N_Rehab:=sum(AUX,na.rm = TRUE),by=FS_Versicherte]
  Rehab_AUX<-Rehab_AUX[,LoS_rehab:=ende-beginn+1,by=FS_RehaKuren_ID]
  Rehab_AUX<-Rehab_AUX[,S_LoS_rehab:=sum(LoS_rehab,na.rm = TRUE),by=FS_Versicherte]
  Cohort1_BL<-merge(Cohort1_BL,unique(Rehab_AUX[,c("FS_Versicherte","N_Rehab","S_LoS_rehab")]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)
  
  # N of drug prescriptions
  Prescription_AUX<-merge(Prescription,Cohort1_BL[,c("FS_Versicherte","start_BL","end_BL")],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
  Prescription_AUX$FS_Verordnung_Datum<-as.Date(Prescription_AUX$FS_Verordnung_Datum, "%Y-%m-%d")
  Prescription_AUX<-Prescription_AUX[FS_Verordnung_Datum %within% interval(ymd(start_BL),ymd(end_BL))]
  Prescription_AUX<-Prescription_AUX[,AUX:=1]
  Prescription_AUX<-Prescription_AUX[,N_pres:=sum(AUX,na.rm = TRUE),by=FS_Versicherte]
  Cohort1_BL<-merge(Cohort1_BL,unique(Prescription_AUX[,c("FS_Versicherte","N_pres")]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)
  
  # Days of temporary incapacity for work
  # PW minor error: 1.1.2005-1.1.2005 is actually 1 day, but here is zero
  Work_temp_AUX<-merge(Work_temp,Cohort1_BL[,c("FS_Versicherte","start_BL","end_BL")],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
  Work_temp_AUX<-unique(Work_temp_AUX)
  Work_temp_AUX$beginn<-as.Date(Work_temp_AUX$FS_Ereignis_Datum_Beginn, "%Y-%m-%d")
  Work_temp_AUX$ende<-as.Date(Work_temp_AUX$FS_Ereignis_Datum_Ende, "%Y-%m-%d")
  Work_temp_AUX<-Work_temp_AUX[ymd(beginn) <= ymd(end_BL) & ymd(ende) >= ymd(start_BL)]
  Work_temp_AUX<-Work_temp_AUX[ende>ymd(end_BL),ende:=ymd(end_BL)]
  Work_temp_AUX<-Work_temp_AUX[beginn<ymd(start_BL),beginn:=ymd(start_BL)]
  Work_temp_AUX<-Work_temp_AUX[,AUX:=1]
  Work_temp_AUX<-Work_temp_AUX[,N_WT:=sum(AUX,na.rm = TRUE),by=FS_Versicherte]
  
  # Pw, this was wrong
  Work_temp_AUX<-Work_temp_AUX[,LoS_WT:=ende-beginn+1,by=FS_Arbeitsunfaehigkeiten_ID]
  Work_temp_AUX<-Work_temp_AUX[,S_LoS_WT:=sum(LoS_WT,na.rm = TRUE),by=FS_Versicherte]
  Cohort1_BL<-merge(Cohort1_BL,unique(Work_temp_AUX[,c("FS_Versicherte","S_LoS_WT")]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)
  
  # Days of permanent incapacity for work
  Work_perm_AUX<-merge(Work_perm,Cohort1_BL[,c("FS_Versicherte","start_BL","end_BL")],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
  Work_perm_AUX<-unique(Work_perm_AUX)
  Work_perm_AUX$beginn<-as.Date(Work_perm_AUX$FS_Ereignis_Datum_Beginn, "%Y-%m-%d")
  Work_perm_AUX$ende<-as.Date(Work_perm_AUX$FS_Ereignis_Datum_Ende, "%Y-%m-%d")
  Work_perm_AUX<-Work_perm_AUX[ymd(beginn) <= ymd(end_BL) & ymd(ende) >= ymd(start_BL)]
  Work_perm_AUX<-Work_perm_AUX[ende>ymd(end_BL),ende:=ymd(end_BL)]
  Work_perm_AUX<-Work_perm_AUX[beginn<ymd(start_BL),beginn:=ymd(start_BL)]
  Work_perm_AUX<-Work_perm_AUX[,AUX:=1]
  Work_perm_AUX<-Work_perm_AUX[,N_WP:=sum(AUX,na.rm = TRUE),by=FS_Versicherte]
  Work_perm_AUX<-Work_perm_AUX[,LoS_WP:=ende-beginn+1,by=ID]
  Work_perm_AUX<-Work_perm_AUX[,S_LoS_WP:=sum(LoS_WP,na.rm = TRUE),by=FS_Versicherte]
  Cohort1_BL<-merge(Cohort1_BL,unique(Work_perm_AUX[,c("FS_Versicherte","S_LoS_WP")]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)
  
  # Long-term care levels by German long-term care insurance (0-4) + days of long term care
  Long_care_AUX<-merge(Long_care,Cohort1_BL[,c("FS_Versicherte","start_BL","end_BL",'FS_Versicherte_Geburtsdatum')],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
  Long_care_AUX<-unique(Long_care_AUX)
  Long_care_AUX$beginn<-as.Date(Long_care_AUX$FS_Ereignis_Datum_Beginn, "%Y-%m-%d")
  Long_care_AUX$ende<-as.Date(Long_care_AUX$FS_Ereignis_Datum_Ende, "%Y-%m-%d")
  Long_care_AUX<-Long_care_AUX[ymd(beginn) <= ymd(end_BL) & ymd(ende) >= ymd(start_BL)]
  Long_care_AUX<-Long_care_AUX[ende>ymd(end_BL),ende:=ymd(end_BL)]
  Long_care_AUX<-Long_care_AUX[beginn<ymd(start_BL),beginn:=ymd(start_BL)]
  Long_care_AUX<-Long_care_AUX[,AUX:=1]
  Long_care_AUX<-Long_care_AUX[,N_LC:=sum(AUX,na.rm = TRUE),by=FS_Versicherte]
  Long_care_AUX<-Long_care_AUX[,LoS_LC:=ende-beginn+1,by=ID]
  Long_care_AUX<-unique(Long_care_AUX[,-c('ID')])
  Long_care_AUX<-Long_care_AUX[,LoS_LC:=sum(LoS_LC),by=FS_Versicherte]
  auxLC<-as.data.table(cbind(c('0','1','2','3','4','5',"H"),c('0','1','1','2','3','3','4')))
  auxLC<-auxLC %>% rename(pflegestufe =V1)
  auxLC<-auxLC %>% rename(LongCare =V2)
  Long_care_AUX<-merge(Long_care_AUX,auxLC,by.x = "pflegestufe", by.y = "pflegestufe",all.x = TRUE,allow.cartesian=TRUE)
  Long_care_AUX$LongCare<-as.numeric(Long_care_AUX$LongCare)
  Long_care_AUX<-Long_care_AUX[,Mean_LC:=max(LongCare,na.rm = TRUE),by=FS_Versicherte]
  Long_care_AUX<-Long_care_AUX[,Mean_LC:=round(Mean_LC,digits=0)]
  Long_care_AUX$FS_Versicherte_Geburtsdatum<-as.Date(Long_care_AUX$FS_Versicherte_Geburtsdatum, "%Y-%m-%d")
  Long_care_AUX$FS_Ereignis_Datum_Beginn<-as.Date(Long_care_AUX$FS_Ereignis_Datum_Beginn, "%Y-%m-%d")
  Long_care_AUX<-Long_care_AUX[,age_LC:=(FS_Ereignis_Datum_Beginn - FS_Versicherte_Geburtsdatum)/365]
  Long_care_AUX<-Long_care_AUX[,age_LC:=min(age_LC,na.rm = TRUE),by=FS_Versicherte]
  Long_care_AUX_AUX<-unique(Long_care_AUX[,c("FS_Versicherte","Mean_LC")])
  Long_care_AUX_AUX<-unique(Long_care_AUX_AUX)
  Cohort1_BL<-merge(Cohort1_BL,Long_care_AUX_AUX,by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)
  
  # Dummy of presence of a medication in an ATC (all classes A01-V10)
  '%ni%' <- function(x,y)!('%in%'(x,y))
  library(stringr)
  library(fastDummies)
  Prescription_AUX<-Prescription_AUX[,WIDO_ATC_12:= substr(WIDO_ATC_123, 1, 3)]
  Prescription_AUX<-Prescription_AUX[,WIDO_ATC_1:= substr(WIDO_ATC_12, 1, 1)]
  Prescription_AUX<-Prescription_AUX[WIDO_ATC_12 %in% c('C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09', 'C10'),
                                     ATC:=WIDO_ATC_12]
  Prescription_AUX<-Prescription_AUX[WIDO_ATC_12 %ni% c('C01', 'C02', 'C03', 'C04', 'C05', 'C06', 'C07', 'C08', 'C09', 'C10'),
                                     ATC:=WIDO_ATC_1]
  
  ifelse(Prescription_ATC==1, {aux0<-unique(Prescription_AUX[,c('FS_Versicherte','WIDO_ATC_12')])
  aux00<-dummy_cols(Prescription_AUX$ATC)
  aux00$.data<-NULL
  aux00$.data_unbekannt<-NULL
  aux00<-cbind(Prescription_AUX$FS_Versicherte,aux00)
  aux00<-aux00 %>% rename(FS_Versicherte =`Prescription_AUX$FS_Versicherte`)
  aux00<-as.data.table(aux00)
  keep00<-as.vector(colnames(aux00))
  names(aux00) <- paste0(names(aux00), "_P")
  aux00<-aux00[ , lapply(.SD, sum), by = FS_Versicherte_P]
  aux00[aux00 == 0] <- NA
  ifelse(treat=="GK_part",
         cond10 <- sapply(aux00, function(col) sum(!is.na(col)) > 100),
         cond10 <- sapply(aux00, function(col) sum(!is.na(col)) > 10))
  aux00<-as.data.frame(aux00)
  aux00<-aux00[,cond10,drop=F]
  Cohort1_BL<-merge(Cohort1_BL,aux00,by.x = "FS_Versicherte", by.y = "FS_Versicherte_P",all.x = TRUE)
  aux00<-as.data.table(aux00)
  keep2<-as.vector(colnames(aux00[,-c("FS_Versicherte_P")]))
  },
  "No prescriptions for matching")
  
  # Dummy of presence of an outpatient or in-hospital diagnosis all ICD diagnosis groups A00-Z99;
  #except diagnosis groups with less than 100 persons with an event in the intervention or control group in the diagnosis group concerned)
  
  #Hosp & Phy
  ifelse(icd10==1,{
    auxICD="gruppen_nr"
    PhyICDaux<-Phy_ICD2
    HospICDaux<-Hosp_ICD2
    a<-0.2},
    {auxICD="desease_nr"
    PhyICDaux<-Phy_ICD
    HospICDaux<-Hosp_ICD
    a<-0.09})
  
  Hosp_Phy_cases<-rbind(PhyICDaux[,c(1,2,4)],
                        HospICDaux[,c(1,2,3)])
  Hosp_Phy_cases$leistungsendedatum<-as.Date(Hosp_Phy_cases$leistungsendedatum, "%Y-%m-%d")
  Hosp_Phy_cases<-merge(Hosp_Phy_cases,Cohort1_BL[,c("FS_Versicherte","start_BL","end_BL")],by.x = "versicherter", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
  Hosp_Phy_cases<-unique(Hosp_Phy_cases[leistungsendedatum %within% interval(ymd(start_BL),ymd(end_BL)),c(1,3)])

  aux<-dummy_cols(Hosp_Phy_cases[,2])
  aux[,1]<-NULL
  aux$desease_nr_NA<-NULL
  aux<-cbind(Hosp_Phy_cases$versicherter,aux)
  # PW hier git es Fehler in R 3.4
  aux<-aux %>% rename(versicherter =V1)
  
  aux<-as.data.table(aux)
  aux<-aux[ , lapply(.SD, sum), by = versicherter]
  aux[aux == 0] <- NA
  ifelse(treat=="GK_part",
  cond1 <- sapply(aux, function(col) sum(!is.na(col)) > 100),
  cond1 <- sapply(aux, function(col) sum(!is.na(col)) > 10))
  aux<-as.data.frame(aux)
  aux<-aux[,cond1,drop=F]
  aux[is.na(aux)] <- 0
  keep<-as.vector(colnames(aux))
    
  Cohort1_BL<-merge(Cohort1_BL,aux,by.x = "FS_Versicherte", by.y = "versicherter",all.x = TRUE)
    
  
  # Add last Charlson score
  Charlson_AUX<-merge(Charlson,Cohort1_BL[,c("FS_Versicherte","start_BL","end_BL")],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
  Charlson_AUX$FS_Ereignis_Datum_Ende<-as.Date(Charlson_AUX$FS_Ereignis_Datum_Ende, "%Y-%m-%d")
  Charlson_AUX<-Charlson_AUX[FS_Ereignis_Datum_Ende %within% interval(ymd(start_BL),ymd(end_BL))]
  Charlson_AUX<-Charlson_AUX[,c("CharlsonScore"):= list(mean(CharlsonScore,na.rm = TRUE)), by=FS_Versicherte]
  Charlson_AUX<-unique(Charlson_AUX[,c("FS_Versicherte","CharlsonScore")])
  Cohort1_BL<-merge(Cohort1_BL,unique(Charlson_AUX[,c("FS_Versicherte","CharlsonScore")]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)
  
  #add Cost and contribution
  ifelse(Cost_in_matching==0,"nada",{
  Cost_AUX<-merge(Cost,Cohort1_BL[,c("FS_Versicherte","start_BL","end_BL")],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
  Cost_AUX$FS_Ereignis_Datum<-as.Date(Cost_AUX$FS_Ereignis_Datum, "%Y-%m-%d")
  Cost_AUX<-Cost_AUX[FS_Ereignis_Datum %within% interval(ymd(start_BL),ymd(end_BL))]
  Cost_AUX<-Cost_AUX[,Total_cost:=sum(RSA_Istkosten_unbereinigt_nur_ausgleichsfaehige_Kosten,na.rm = TRUE),by=FS_Versicherte]
  Cost_AUX<-Cost_AUX[,Total_contribution:=sum(RSA_Normkosten_unbereinigt,na.rm = TRUE),by=FS_Versicherte]
  Cohort1_BL<-merge(Cohort1_BL,unique(Cost_AUX[,c("FS_Versicherte","Total_cost",'Total_contribution')]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)
  })
  
  
  # Add insurance status
  
  # Add mean FORTA score
  
  # Create dummies of prevalence of CD(heart; diabetes; COPD; CKD)
  
  # Clean base
  Cohort1_BL$FS_Versicherte_Geburtsdatum<-as.Date(Cohort1_BL$FS_Versicherte_Geburtsdatum, "%Y-%m-%d")
  Cohort1_BL<-Cohort1_BL[,age:=(ymd(end_BL)-FS_Versicherte_Geburtsdatum)/365]
  Cohort1_BL<-Cohort1_BL[death==1,age:=(FS_Versicherte_Todesdatum-FS_Versicherte_Geburtsdatum)/365]
  ifelse(Cost_in_matching==0,{
  ifelse(Prescription_ATC==1,
         vars<-append(c("FS_Versicherte","period","GK_part","P_part","age","FS_Krankenkassen","sex","N_phy","N_spec","N_Hosp","S_LoS","S_LoS_rehab","N_pres","CharlsonScore","FU","Mean_LC","S_LoS_WP","S_LoS_WT"),append(keep,keep2)),
         vars<-append(c("FS_Versicherte","period","GK_part","P_part","age","FS_Krankenkassen","sex","N_phy","N_spec","N_Hosp","S_LoS","S_LoS_rehab","N_pres","CharlsonScore","FU","Mean_LC","S_LoS_WP","S_LoS_WT"),keep))
  },{
  ifelse(Prescription_ATC==1,
         vars<-append(c("FS_Versicherte","period","GK_part","P_part","age","Total_cost",'Total_contribution',"FS_Krankenkassen","sex","N_phy","N_spec","N_Hosp","S_LoS","S_LoS_rehab","N_pres","CharlsonScore","FU","Mean_LC","S_LoS_WP","S_LoS_WT"),append(keep,keep2)),
         vars<-append(c("FS_Versicherte","period","GK_part","P_part","age","Total_cost",'Total_contribution',"FS_Krankenkassen","sex","N_phy","N_spec","N_Hosp","S_LoS","S_LoS_rehab","N_pres","CharlsonScore","FU","Mean_LC","S_LoS_WP","S_LoS_WT"),keep))
  })
  '%ni%' <- function(x,y)!('%in%'(x,y))
  vars<-vars[vars %ni% c("versicherter")]
  Cohort1_BL = subset(Cohort1_BL, select = vars )
  
