###Start###
#create time of assessment & #Create list to hold all sample-periods
# PW 2005- 2008 ? nicht 2006-2009 ?
ifelse(Cohort=="2006-2009",
  {start<-c('20080101','20090101')#c('20040101','20050101','20060101','20070101')
  end<-c('20091231','20101231')#c('20051231','20061231','20071231','20081231')
  period<-c(1:2)
  periods<-as.data.table(cbind(start,end,period))
  Control<-vector("list", length = 2)
  n<-2},
  
  {start<-c('20040101','20050101','20060101','20070101','20080101','20090101','20100101','20110101')
  end<-c('20051231','20061231','20071231','20081231','20091231','20101231','20111231','20121231')
  period<-c(1:8)
  periods<-as.data.table(cbind(start,end,period))
  Control<-vector("list", length = 8)
  n<-8})

#start loop (shoudl be to )

for(i in c(1:n)){

#create period 
begin_t<-ymd(periods[period==i,c("start")])
end_t<-ymd(periods[period==i,c("end")])
interval_t<-interval(begin_t,end_t) #Period of assessment

Cohort_t<-as.data.table(unique(Insured_days[FS_Datum_Ereignis %within% interval_t,c("FS_Versicherte","FS_Datum_Ereignis","last_insurance","begin_insurance","Versichertentage_Anz")]))
Cohort_t<-Cohort_t[,days_insured:=sum(Versichertentage_Anz, na.rm = T), by=FS_Versicherte]
Cohort_t<-Cohort_t[,last_in_d_y:=FS_Datum_Ereignis+Versichertentage_Anz]
Cohort_t<-Cohort_t[,last_in_d_y:=max(last_in_d_y),by=FS_Versicherte]
Cohort_t<-Cohort_t[last_insurance<last_in_d_y,last_insurance:=last_in_d_y]
Cohort_t<-unique(Cohort_t[,c("FS_Versicherte","begin_insurance","last_insurance","days_insured")])

## Add basic(time invariant) variables to cohort
Cohort_t<-merge(Cohort_t,Insurees,by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

# Add variable period living in Kinzigtal
Cohort_t<-merge(Cohort_t,Time_inK_AUX,by.x = "FS_Versicherte", by.y = "PS_Versicherter_ID",all.x = TRUE)

# Keep ppl living in kizigtal in the period of ass (at any point)
Cohort_t<-Cohort_t[begin_t < ende]

# Add date of GK participation
Cohort_t<-merge(Cohort_t,Participants,by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

#Add dummy Variable of GK participation (at any point in time)
Cohort_t<-Cohort_t[,GK_part:=0]
Cohort_t<-Cohort_t[!is.na(GK_begin), GK_part:=1]

#keep ppl that did not participate in GK
Cohort_t<-Cohort_t[GK_part!=1]

ifelse(treat=="GK_part","GKvsC",
{# keep ppl taht did not participate in any special program
Cohort_t<-merge(Cohort_t,Program_AUX,by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)
Cohort_t<-Cohort_t[,P_part:=0]
Cohort_t<-Cohort_t[!is.na(P_begin), P_part:=1]
Cohort_t<-Cohort_t[P_part!=1]})

### Particular period ###

# Fix last insurance when ppl died.
Cohort_t$FS_Versicherte_Todesdatum<-as.Date(Cohort_t$FS_Versicherte_Todesdatum, "%Y-%m-%d")
Cohort_t<-Cohort_t[FS_Versicherte_Todesdatum<'9999-01-01' & last_insurance %within% interval(FS_Versicherte_Todesdatum-45,FS_Versicherte_Todesdatum+45)
                   ,last_insurance:=FS_Versicherte_Todesdatum]

# Add variable death.
Cohort_t<-Cohort_t[,death:=0]
Cohort_t$FS_Versicherte_Todesdatum<-as.Date(Cohort_t$FS_Versicherte_Todesdatum, "%Y-%m-%d")
Cohort_t<-Cohort_t[FS_Versicherte_Todesdatum<=end_t,death:=1]

# Add variable left region or insurance (attrition)
Cohort_t<-Cohort_t[,att:=0]
Cohort_t<-Cohort_t[last_insurance<=end_t,att:=1]

# add variable late integration into region, inverse attrition
Cohort_t<-Cohort_t[,att_i:=0]
Cohort_t$begin_insurance<-as.Date(Cohort_t$begin_insurance, "%Y-%m-%d")
Cohort_t<-Cohort_t[begin_insurance>begin_t ,att_i:=1]

# Create days of follow up variable
Cohort_t<-Cohort_t[death==0 & att==0,FU:=end_t-begin_t]
Cohort_t<-Cohort_t[att==1,FU:=last_insurance-begin_t]
Cohort_t<-Cohort_t[death==1,FU:=FS_Versicherte_Todesdatum-begin_t]
Cohort_t<-Cohort_t[att_i==1,FU:=end_t-begin_insurance]
Cohort_t$FU<-as.numeric(Cohort_t$FU)

# Only keep ppl that was in the region, alive at the moment of intervention. (Did not died or left during baseline period)
# and had 90% of all possible insured days in the baseline year.
Cohort_t<-Cohort_t[death!=1]
Cohort_t<-Cohort_t[att!=1]
Cohort_t<-Cohort_t[days_insured>=0.9*365]
      
      # Also, ppl that dont have data in the period of follow up must go. (Until 6-8 more years)
      # Add variable left region or insurance in FU period (attrition2)
      Cohort_t<-Cohort_t[,att2:=0]
      ifelse(Cohort=="2006-2009",
      Cohort_t<-Cohort_t[last_insurance<=(end_t+8*365) & FS_Versicherte_Todesdatum > (end_t+8*365),att2:=1],
      Cohort_t<-Cohort_t[last_insurance<=(end_t+6*365) & FS_Versicherte_Todesdatum > (end_t+6*365),att2:=1])
      nrow(Cohort_t[att2==1])
      Cohort_t<-Cohort_t[att2!=1]
      

# Create number of physician visits
      
      # Create number of physician visits
      Phy_cases_AUX<-Phy_cases
      Phy_cases_AUX$leistungsendedatum<-as.Date(Phy_cases_AUX$leistungsendedatum, "%Y-%m-%d")
      Phy_cases_AUX<-Phy_cases_AUX[leistungsendedatum %within% interval_t]
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
      Cohort_t<-merge(Cohort_t,unique(Phy_cases_AUX2[,c("versicherter","N_phy","pp_partner",'N_spec')]),by.x = "FS_Versicherte", by.y = "versicherter",all.x = TRUE)
      
# keep only patients with less than 49% visits to doc integrate.
Cohort_t<-Cohort_t[is.na(pp_partner)==TRUE,pp_partner:=0]
Cohort_t<-Cohort_t[pp_partner<=0.49]
#50, 80, 10 and 30

# Create hospital visits and length of stay
Hosp_cases_AUX<-Hosp_cases
Hosp_cases_AUX<-unique(Hosp_cases_AUX)
Hosp_cases_AUX$beginn<-as.Date(Hosp_cases_AUX$beginn, "%Y-%m-%d")
Hosp_cases_AUX$ende<-as.Date(Hosp_cases_AUX$ende, "%Y-%m-%d")
Hosp_cases_AUX<-Hosp_cases_AUX[ymd(beginn) <= end_t & ymd(ende) >= begin_t]
Hosp_cases_AUX<-Hosp_cases_AUX[ende>end_t,ende:=end_t]
Hosp_cases_AUX<-Hosp_cases_AUX[beginn<begin_t,beginn:=begin_t]
Hosp_cases_AUX<-Hosp_cases_AUX[,AUX:=1]
Hosp_cases_AUX<-Hosp_cases_AUX[,N_Hosp:=sum(AUX,na.rm = TRUE),by=versicherter]
Hosp_cases_AUX<-Hosp_cases_AUX[,LoS:=ende-beginn+1,by=ID]
Hosp_cases_AUX<-Hosp_cases_AUX[,S_LoS:=sum(LoS,na.rm = TRUE),by=versicherter]
Cohort_t<-merge(Cohort_t,unique(Hosp_cases_AUX[,c("versicherter","N_Hosp","S_LoS")]),by.x = "FS_Versicherte", by.y = "versicherter",all.x = TRUE)

# Days of in-patient rehabilitation
Rehab_AUX<-Rehab
Rehab_AUX<-unique(Rehab_AUX)
Rehab_AUX$beginn<-as.Date(Rehab_AUX$FS_Ereignis_Datum_Beginn, "%Y-%m-%d")
Rehab_AUX$ende<-as.Date(Rehab_AUX$FS_Ereignis_Datum_Ende, "%Y-%m-%d")
Rehab_AUX<-Rehab_AUX[ymd(beginn) <= end_t & ymd(ende) >= begin_t]
Rehab_AUX<-Rehab_AUX[ende>end_t,ende:=end_t]
Rehab_AUX<-Rehab_AUX[beginn<begin_t,beginn:=begin_t]
Rehab_AUX<-Rehab_AUX[,AUX:=1]
Rehab_AUX<-Rehab_AUX[,N_Rehab:=sum(AUX,na.rm = TRUE),by=FS_Versicherte]
Rehab_AUX<-Rehab_AUX[,LoS_rehab:=ende-beginn+1,by=FS_RehaKuren_ID]
Rehab_AUX<-Rehab_AUX[,S_LoS_rehab:=sum(LoS_rehab,na.rm = TRUE),by=FS_Versicherte]
Cohort_t<-merge(Cohort_t,unique(Rehab_AUX[,c("FS_Versicherte","N_Rehab","S_LoS_rehab")]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

# N of drug prescriptions
Prescription_AUX<-Prescription
Prescription_AUX$FS_Verordnung_Datum<-as.Date(Prescription_AUX$FS_Verordnung_Datum, "%Y-%m-%d")
Prescription_AUX<-Prescription_AUX[FS_Verordnung_Datum %within% interval_t]
Prescription_AUX<-Prescription_AUX[,AUX:=1]
Prescription_AUX<-Prescription_AUX[,N_pres:=sum(AUX,na.rm = TRUE),by=FS_Versicherte]
Cohort_t<-merge(Cohort_t,unique(Prescription_AUX[,c("FS_Versicherte","N_pres")]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

# Days of temporary incapacity for work
Work_temp_AUX<-Work_temp
Work_temp_AUX<-unique(Work_temp_AUX)
Work_temp_AUX$beginn<-as.Date(Work_temp_AUX$FS_Ereignis_Datum_Beginn, "%Y-%m-%d")
Work_temp_AUX$ende<-as.Date(Work_temp_AUX$FS_Ereignis_Datum_Ende, "%Y-%m-%d")
Work_temp_AUX<-Work_temp_AUX[ymd(beginn) <= end_t & ymd(ende) >= begin_t]
Work_temp_AUX<-Work_temp_AUX[ende>end_t,ende:=end_t]
Work_temp_AUX<-Work_temp_AUX[beginn<begin_t,beginn:=begin_t]
Work_temp_AUX<-Work_temp_AUX[,AUX:=1]
Work_temp_AUX<-Work_temp_AUX[,N_WT:=sum(AUX,na.rm = TRUE),by=FS_Versicherte]
Work_temp_AUX<-Work_temp_AUX[,LoS_WT:=ende-beginn+1,by=FS_Arbeitsunfaehigkeiten_ID]
Work_temp_AUX<-Work_temp_AUX[,S_LoS_WT:=sum(LoS_WT,na.rm = TRUE),by=FS_Versicherte]
Cohort_t<-merge(Cohort_t,unique(Work_temp_AUX[,c("FS_Versicherte","S_LoS_WT")]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

# Days of permanent incapacity for work
Work_perm_AUX<-Work_perm
Work_perm_AUX<-unique(Work_perm_AUX)
Work_perm_AUX$beginn<-as.Date(Work_perm_AUX$FS_Ereignis_Datum_Beginn, "%Y-%m-%d")
Work_perm_AUX$ende<-as.Date(Work_perm_AUX$FS_Ereignis_Datum_Ende, "%Y-%m-%d")
Work_perm_AUX<-Work_perm_AUX[ymd(beginn) <= end_t & ymd(ende) >= begin_t]
Work_perm_AUX<-Work_perm_AUX[ende>end_t,ende:=end_t]
Work_perm_AUX<-Work_perm_AUX[beginn<begin_t,beginn:=begin_t]
Work_perm_AUX<-Work_perm_AUX[,AUX:=1]
Work_perm_AUX<-Work_perm_AUX[,N_WP:=sum(AUX,na.rm = TRUE),by=FS_Versicherte]
Work_perm_AUX<-Work_perm_AUX[,LoS_WP:=ende-beginn+1,by=ID]
Work_perm_AUX<-Work_perm_AUX[,S_LoS_WP:=sum(LoS_WP,na.rm = TRUE),by=FS_Versicherte]
Cohort_t<-merge(Cohort_t,unique(Work_perm_AUX[,c("FS_Versicherte","S_LoS_WP")]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

# Long-term care levels by German long-term care insurance (0-4)
Long_care_AUX<-merge(Long_care,Cohort1[,c("FS_Versicherte",'FS_Versicherte_Geburtsdatum')],by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE,allow.cartesian=TRUE)
Long_care_AUX<-unique(Long_care_AUX)
Long_care_AUX$beginn<-as.Date(Long_care_AUX$FS_Ereignis_Datum_Beginn, "%Y-%m-%d")
Long_care_AUX$ende<-as.Date(Long_care_AUX$FS_Ereignis_Datum_Ende, "%Y-%m-%d")
Long_care_AUX<-Long_care_AUX[ymd(beginn) <= end_t & ymd(ende) >= begin_t]
Long_care_AUX<-Long_care_AUX[ende>end_t,ende:=end_t]
Long_care_AUX<-Long_care_AUX[beginn<begin_t,beginn:=begin_t]
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
Cohort_t<-merge(Cohort_t,Long_care_AUX_AUX,by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

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

ifelse(Prescription_ATC==1,{aux0C<-unique(Prescription_AUX[,c('FS_Versicherte','ATC')])
keep_aux00<-as.data.table(keep00)
keep_aux00<-keep_aux00[keep00!="FS_Versicherte"]
keep_aux00$codes<-sub('.*_', '', keep_aux00$keep)
aux0C<-aux0C[ATC %ni% keep_aux00$codes, ATC:=0]
aux00C<-dummy_cols(aux0C$ATC)
aux00C$.data<-NULL
aux00C$.data_0<-NULL
aux00C$.data_unbekannt<-NULL
aux00C<-cbind(aux0C$FS_Versicherte,aux00C)
aux00C<-aux00C %>% rename(FS_Versicherte =`aux0C$FS_Versicherte`)
aux00C<-as.data.table(aux00C)
aux00C<-aux00C[ , lapply(.SD, sum), by = FS_Versicherte]
names(aux00C) <- paste0(names(aux00C), "_P")
Cohort_t<-merge(Cohort_t,aux00C,by.x = "FS_Versicherte", by.y = "FS_Versicherte_P",all.x = TRUE)
},
"No prescriptions for matching")

# Dummy of presence of an outpatient or in-hospital diagnosis all ICD diagnosis groups A00-Z99;
#except diagnosis groups with less than 100 persons with an event in the intervention or control group in the diagnosis group concerned)

# Hosp & Phy
#Hosp & Phy
Hosp_Phy_cases<-rbind(PhyICDaux[,c(1,2,4)],
                      HospICDaux[,c(1,2,3)])
Hosp_Phy_cases$leistungsendedatum<-as.Date(Hosp_Phy_cases$leistungsendedatum, "%Y-%m-%d")
Hosp_Phy_cases<-unique(Hosp_Phy_cases[leistungsendedatum %within% interval_t,c(1,3)])

'%ni%' <- function(x,y)!('%in%'(x,y))
keep_aux<-as.data.table(keep)
library(stringr)
keep_aux$codes<-sub('.*_', '', keep_aux$keep)
ifelse(icd10==1,{
Hosp_Phy_cases<-Hosp_Phy_cases[gruppen_nr %ni% keep_aux$codes, gruppen_nr:= 0]},
Hosp_Phy_cases<-Hosp_Phy_cases[desease_nr %ni% keep_aux$codes, desease_nr:= 0])
aux2<-dummy_cols(Hosp_Phy_cases[,2])
aux2[,1]<-NULL
aux2<-cbind(Hosp_Phy_cases$versicherter,aux2)
aux2<-aux2 %>% rename(versicherter =V1)
aux2<-as.data.table(aux2)
aux2<-aux2[ , lapply(.SD, sum), by = versicherter] 

Cohort_t<-merge(Cohort_t,aux2,by.x = "FS_Versicherte", by.y = "versicherter",all.x = TRUE)


# Add last Charlson score
Charlson_AUX<-Charlson
Charlson_AUX$FS_Ereignis_Datum_Ende<-as.Date(Charlson_AUX$FS_Ereignis_Datum_Ende, "%Y-%m-%d")
Charlson_AUX<-Charlson_AUX[FS_Ereignis_Datum_Ende %within% interval_t]
Charlson_AUX<-Charlson_AUX[,c("CharlsonScore"):= list(mean(CharlsonScore, na.rm = T)), by=FS_Versicherte]
Charlson_AUX<-unique(Charlson_AUX[,c("FS_Versicherte","CharlsonScore")])
Cohort_t<-merge(Cohort_t,unique(Charlson_AUX[,c("FS_Versicherte","CharlsonScore")]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)

#add Cost and contribution
ifelse(Cost_in_matching==0,"nada",{
  Cost_AUX<-Cost
  Cost_AUX$FS_Ereignis_Datum<-as.Date(Cost_AUX$FS_Ereignis_Datum, "%Y-%m-%d")
  Cost_AUX<-Cost_AUX[FS_Ereignis_Datum %within% interval_t]
  Cost_AUX<-Cost_AUX[,Total_cost:=sum(RSA_Istkosten_unbereinigt_nur_ausgleichsfaehige_Kosten,na.rm = TRUE),by=FS_Versicherte]
  Cost_AUX<-Cost_AUX[,Total_contribution:=sum(RSA_Normkosten_unbereinigt,na.rm = TRUE),by=FS_Versicherte]
  Cohort_t<-merge(Cohort_t,unique(Cost_AUX[,c("FS_Versicherte","Total_cost",'Total_contribution')]),by.x = "FS_Versicherte", by.y = "FS_Versicherte",all.x = TRUE)
})

#Add mean FORTA score

# Create dummies of prevalence of CD(heart; diabetes; COPD; CKD)

#Clear variables for mathching; add perdiod variable, age
Cohort_t$period<-i
Cohort_t$period<-as.character(Cohort_t$period)
Cohort_t<-merge(Cohort_t,periods,by.x = "period", by.y = "period",all.x = TRUE)
Cohort_t$end<-ymd(Cohort_t$end)
Cohort_t$FS_Versicherte_Geburtsdatum<-as.Date(Cohort_t$FS_Versicherte_Geburtsdatum, "%Y-%m-%d")
Cohort_t<-Cohort_t[,age:=(end_t-FS_Versicherte_Geburtsdatum)/365]
Cohort_t<-Cohort_t[death==1,age:=(FS_Versicherte_Todesdatum-FS_Versicherte_Geburtsdatum)/365]
for(z in vars){
  ifelse(z %in% colnames(Cohort_t),'',Cohort_t[,get("z"):=0])
}


Cohort_t = subset(Cohort_t, select = vars )
### Save run ###
Control[[i]]<-Cohort_t

# end loop
}

#Count number of unique potential controls

nopc <- rbind(Control[[1]],Control[[2]],Control[[3]],Control[[4]],Control[[5]],
              Control[[6]],Control[[7]],Control[[8]])
length(unique(nopc$FS_Versicherte))
