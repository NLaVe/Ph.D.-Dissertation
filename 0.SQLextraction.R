{
  library(data.table)
  library(dplyr)
  library(xlsx)
  library(RODBCDBI)
  library(gmodels)
  library(odbc)
  library(DBI)
  library(lubridate)
  library(tidyr)
  library(readxl)

}
#Data####
{
  myconn <- dbConnect(odbc::odbc(),
                      .connection_string = 'driver={SQL Server};server=OM-SQL-01;database=DWH_olap;trusted_connection=true')
  ##Programs
  {
    
    Program <- as.data.table(dbGetQuery(myconn," SELECT distinct [FS_Versicherte]
                                        --,[FS_IV_Programm]
                                        ,[FS_Ereignis_Datum_Beginn]
                                        ,[FS_Ereignis_Datum_Ende]
                                        FROM [DWH_olap].[dbo].[T_FACT_Versicherter_iv_Programmzeiten]
                                        where FS_Krankenkassen in (3,4)
                                        order by FS_Versicherte, FS_Ereignis_Datum_Beginn")
    )}
  #Charlson / FORTA
  {
    
    Charlson <- as.data.table(dbGetQuery(myconn,"SELECT [FS_Versicherte]
                                         ,[FS_Ereignis_Datum_Ende]
                                         ,[Ten_Year_Survial_Rate]
                                         ,[CharlsonScore]
                                         FROM [DWH_olap].[dbo].[T_FACT_CharlsonScore]
                                         where FS_Krankenkassen in (3,4)
                                         order by FS_Versicherte, FS_Ereignis_Datum_Ende"))
    
  }
  ##Insurees
  {
    
    # PW KK, added Versicherter_ID_Name for debugging
    Insurees <- as.data.table(dbGetQuery(myconn, "SELECT [FS_Versicherte]
                                         ,[Versicherte_Anz]
                                         ,[FS_Krankenkassen]
                                         ,[FS_Versicherte_Geburtsdatum]
                                         ,[FS_Versicherte_Todesdatum]
                                         ,[FS_Hausarzt_OM_IK]
                                         --,[FS_Hausarzt_OM_PLZ]
                                         ,[FS_Vertrauensarzt_IK]
                                         --,[FS_Vertrauensarzt_PLZ]
                                         --,[FS_TimeUtility]
                                         ,[FS_AGG]
                                         --,[FS_Quartal_Kassenwechsel]
                                         ,b.[Vers_Geschlecht]
                                         ,Versicherter_ID_Name
                                         FROM [DWH_olap].[dbo].[T_FACT_Versicherte] AS a
                                         inner JOIN [DWH_olap].[dbo].[T_DIM_Versicherte] AS b
                                         ON a.[FS_Versicherte] = b.[PS_Versicherter_ID]
                                         where FS_Krankenkassen in (3,4)
                                         order by fs_versicherte"))
  }
  #list of GK patients participants:
  {
    # PW lots of duplicates, because FS_Ereignis_Datum_Tage] is missing, added distinct, Log_duplikat removed
    Participants_db <- as.data.table(dbGetQuery(myconn, "SELECT distinct [FS_Versicherte]
                                                ,[FS_Ereignis_Datum_Beginn]
                                                ,[FS_Ereignis_Datum_Ende]
                                                ,[FS_Krankenkassen]
                                                FROM [DWH_olap].[dbo].[T_FACT_Versicherter_integrierte_versorgungszeiten]
                                                where [log_Duplikat]=0
                                                AND FS_Krankenkassen in (3,4)
                                                order by FS_Versicherte,FS_Ereignis_Datum_Beginn")
    )}
  #GK insruees time in Kinzigtal:
  {
    # PW added KK
    Time_inK <- as.data.table(dbGetQuery(myconn, "SELECT [SCD_Jahr_Key]
                                                 ,[scd_start]
                                                 ,[scd_end]
                                                 ,scd.[PS_Versicherter_ID]
                                                 ,[VersZeitStatusDetails]
                                                 FROM [DWH_olap].[dbo].[T_DIM_Versicherte_SCD_Jahr] SCD
												                         inner join T_DIM_Versicherte DV
												                         on dv.PS_Versicherter_ID = scd.PS_Versicherter_ID
                                                 where [VersZeitStatusDetails]!='nicht versichert'
                                                 and WohnOrtRegion = 'Kinzigtal'
												                         and DV.Vers_Krankenkasse in ('AOKBW',  'LKKBW')
                                                  order by PS_Versicherter_ID, scd_start ")
    )
  } 
  #Insured days
  {
    # PW Alternative: werte pro Jahr
    
    Insured_days <- as.data.table(dbGetQuery(myconn,"SELECT [FS_Versicherte]
    ,  cast(cast(year([FS_Datum_Ereignis]) as varchar) as date) [FS_Datum_Ereignis]
    ,sum([Versichertentage_Anz]) [Versichertentage_Anz]
    FROM [DWH_olap].[dbo].[T_FACT_Versichertentage]
    Where [FS_Krankenkassen] in( 3 ,4 )
    group by [FS_Versicherte]
    ,year([FS_Datum_Ereignis])
	order by FS_Versicherte, FS_Datum_Ereignis"))
    
    Insured_days$FS_Datum_Ereignis<-as.Date(Insured_days$FS_Datum_Ereignis, "%Y-%m-%d", tz = "UTC")
    Insured_days<-Insured_days[,last_insurance:=max(FS_Datum_Ereignis), by = "FS_Versicherte"]
    Insured_days<-Insured_days[,begin_insurance:=min(FS_Datum_Ereignis), by = "FS_Versicherte"]
  }
  ##Contribution
  {
    # PW KK aufgenommen, where umgebaut, NUll-Werte raus Summe erstellt
    Contribution_Morbi <- dbGetQuery(myconn,"SELECT [FS_Ereignis_Datum]
                                            ,[FS_Versicherte]
                                           ,sum([RSA_Normkosten_unbereinigt]) [RSA_Normkosten_unbereinigt]
                                           FROM [DWH_olap].[dbo].[T_FACT_RSA_Verbund]
                                           WHERE 
									                         FS_Krankenkassen in (3,4)
									                          and FS_RSA_Szenario=300 
                                           AND FS_RSA_Version_Leistungsbereich in ( 1, 2, 3, 54, 55, 56, 57, 59, 31, 33, 34, 35, 36, 37, 39)
			                        						 and RSA_Normkosten_unbereinigt is not null
                        									 group by 
                        									 [FS_Ereignis_Datum] ,[FS_Versicherte]
                        									 order by FS_Versicherte, FS_Ereignis_Datum
                        									  ")
  }
  ##Costs
  {
    # PW diverse Anpassungen: nur ben?tigte Spalten, KK rein, Summe, keine NUllwerte
    Cost <- as.data.table(dbGetQuery(myconn,"SELECT [FS_Ereignis_Datum]
                                     ,[FS_Versicherte]
                                     ,sum([RSA_Istkosten_unbereinigt_nur_ausgleichsfaehige_Kosten]) [RSA_Istkosten_unbereinigt_nur_ausgleichsfaehige_Kosten]
                                     FROM [DWH_olap].[dbo].[T_FACT_RSA_Verbund]
                                     WHERE FS_RSA_Szenario=300
                  									 AND FS_Krankenkassen in ( 3, 4)
                  									 AND [RSA_Istkosten_unbereinigt_nur_ausgleichsfaehige_Kosten] is not null
                                     AND (
                  									    FS_RSA_Version_Leistungsbereich in( 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 31, 32, 33, 34, 35, 36, 37, 38, 39)
                  										  OR 
                  										  FS_RSA_Version_Leistungsbereich in (1,2,3,54,55,56,57,59)
                  										)
                      							group by [FS_Ereignis_Datum]
                                           ,[FS_Versicherte]
                  									order by 
                                     [FS_Versicherte], [FS_Ereignis_Datum]")
    )}
  
  Cost$RSA_Normkosten_unbereinigt<-0
  Contribution_Morbi$RSA_Istkosten_unbereinigt_nur_ausgleichsfaehige_Kosten<-0
  Cost<-rbind(Cost,Contribution_Morbi)
  remove(Contribution_Morbi)
  
  #Physician cases
  {
    Phy_cases <- as.data.table(dbGetQuery(myconn,"SELECT [erbringer_ik]
                                          ,[id_arzt_fall]
                                          ,[versicherter]
                                          ,[leistungsendedatum]
                                          ,b.[fachuntergruppe]
                                          ,b.[fachgruppe]
                                          ,[Leistungspartner]
                                          ,[inanspruchnahme]
                                          ,v.krankenkasse
                                          ,[BSNR_1]
                                          FROM [DWH_Core].[dbo].[arzt_fall] AS a
                                          LEFT JOIN [DWH_olap].[dbo].[T_DIM_Leistungserbringer] AS b
                                          ON a.erbringer_ik = b.[PS_Leistungserbringer]
                                          inner JOIN [DWH_core].[dbo].[versicherter] AS v
                                          ON a.[versicherter] = v.id
                    										  where v.krankenkasse in (3,4)
                   										  order by versicherter , leistungsendedatum ;"))
    
    
    # ICD
    
    Phy_ICD <- as.data.table(dbGetQuery(myconn,"
    SELECT 
      [versicherter]
      ,cast('01-01-' + cast(year([leistungsendedatum]) as varchar) as date)  [leistungsendedatum]
      ,v.krankenkasse
      ,[gruppen_nr]
	    , count(*) as ICD_Count
      FROM [DWH_Core].[dbo].[arzt_fall] AS a
      inner JOIN [DWH_core].[dbo].[versicherter] AS v
      ON a.[versicherter] = v.id
      left JOIN [DWH_core].[dbo].[arzt_diagnose] AS d
      ON  a.id_arzt_fall = d.arzt_fall
      inner join [DWH_core].[dbo].k_icd10_aktuell  icd  on icd.code = [diagnose_code] 
      where v.krankenkasse in (3,4)
      and [diagnose_sicherheit] = 'G'
      group by 
      [versicherter]
      ,cast('01-01-' + cast(year([leistungsendedatum]) as varchar) as date)  
      ,v.krankenkasse
      ,[gruppen_nr];")) 
    
  }
  #Hospital cases
  {
    
    Hosp_cases <- as.data.table(dbGetQuery(myconn,"SELECT min(a.[id]) ID
    ,[versicherter]
    ,[beginn]
    ,[ende]
    FROM [DWH_core].[dbo].[krankenhaus_fall] AS a
    inner JOIN [DWH_core].[dbo].[versicherter] AS v
    ON a.[versicherter] = v.id
    WHERE [log_duplikat] = 0
    and v.krankenkasse in (3,4)
    group by [versicherter]
    ,[beginn]
    ,[ende]
    order by versicherter, beginn;" ))
    
    Hosp_ICD <- as.data.table(dbGetQuery(myconn,"
    SELECT 
    [versicherter]
    ,cast('01-01-' + cast(year([ende]) as varchar) as date)  [leistungsendedatum]
    ,gruppen_nr
    , count(*) ICD_Count
    FROM [DWH_core].[dbo].[krankenhaus_fall] AS a
    LEFT JOIN [DWH_core].[dbo].[krankenhaus_fall_diagnose] AS b
    ON a.[id] = b.[kh_fall]
    inner JOIN [DWH_core].[dbo].[versicherter] AS v
    ON a.[versicherter] = v.id
    inner join dwh_core..k_icd10_aktuell  icd  on icd.code = b.[diagnose_code] 
    WHERE [log_duplikat] = 0
    and v.krankenkasse in (3,4)
    and diagnose_typ = 5
    group by
    [versicherter]
    ,cast('01-01-' + cast(year([ende]) as varchar) as date)  
    ,gruppen_nr
    order by versicherter, cast('01-01-' + cast(year([ende]) as varchar) as date) ;" )) 
    
    
  }
  ## Diagnosis
  # duplicates because of many catalog-versions. changed to view k_icd10_aktuell
  {

    ICD10 <- as.data.table(dbGetQuery(myconn,"SELECT gruppen_nr as [code]
                                      FROM [DWH_core].[dbo].[k_icd10_aktuell]"))
  }
  ## N of drug prescriptions
  {

    # new Version with ATC
    Prescription <- as.data.table(dbGetQuery(myconn,"SELECT 
    [FS_Versicherte]
    ,cast('01-01-' + cast(year([FS_Verordnung_Datum]) as varchar) as date)  [FS_Verordnung_Datum]
    ,WIDO_ATC_123
    , count(*) ATC_Count
    FROM [DWH_olap].[dbo].[T_FACT_Verordnungen]
    inner join dwh_olap..T_DIM_Arzneimittel_WIDO wido on wido.PS_WIDO = FS_Arzneimittel_WIDO
    WHERE [Verordnungen_Anz] = 1
    and WIDO_ATC_123 <> 'unbekannt'
    AND [FS_Krankenkassen]  in( 3, 4)
    group by [FS_Versicherte]
    ,cast('01-01-' + cast(year([FS_Verordnung_Datum]) as varchar) as date)
    ,WIDO_ATC_123
    order by fs_versicherte, FS_Verordnung_Datum"))

  }
  ## Days of in-patient rehabilitation
  {
    
    Rehab <- as.data.table(dbGetQuery(myconn,"SELECT distinct [FS_Ereignis_Datum_Beginn]
                                    ,[FS_Ereignis_Datum_Ende]
                                    ,[FS_Versicherte]
                                    ,[FS_RehaKuren_ID]
                                    FROM [DWH_olap].[dbo].[T_FACT_RehaKuren_Faelle]
									                  where FS_Krankenkassen in (3,4)
                                      order by fs_versicherte, FS_Ereignis_Datum_Beginn ")
    )}
  ## Days of temporary incapacity for work
  {
    Work_temp <- as.data.table(dbGetQuery(myconn,"SELECT min ([FS_Arbeitsunfaehigkeiten_ID]) [FS_Arbeitsunfaehigkeiten_ID],
                                                [FS_Versicherte]
                                                ,[FS_Ereignis_Datum_Beginn]
                                                ,[FS_Ereignis_Datum_Ende]
                                                , sum(AU_Dauer) AU_Dauer
                                            FROM [DWH_olap].[dbo].[T_FACT_Arbeitsunfaehigkeiten]
                                            WHERE [log_duplikat]='0'
                                          and FS_Krankenkassen in (3, 4)
                                          group by [FS_Versicherte],[FS_Ereignis_Datum_Beginn],[FS_Ereignis_Datum_Ende]
                                          order by fs_versicherte,FS_Ereignis_Datum_Beginn ")
    )}
  ## Days of permanent incapacity for work
  {
    
    # PW Dubletten entfernt
    Work_perm <- as.data.table(dbGetQuery(myconn,"SELECT min([ID]) ID,
                                                [FS_Versicherte]
                                                ,[FS_Ereignis_Datum_Beginn]
                                                ,[FS_Ereignis_Datum_Ende]
                                                FROM [DWH_olap].[dbo].[T_FACT_Versicherter_Erwerbsunfaehigkeitenzeiten]
                                                WHERE [log_duplikat]='0'
                                          and FS_Krankenkassen in (3, 4)
                                          group by [FS_Versicherte]
                                                ,[FS_Ereignis_Datum_Beginn]
                                                ,[FS_Ereignis_Datum_Ende]
                                          order by fs_Versicherte, FS_Ereignis_Datum_Beginn")
    )}
  ## Long term care by German long-term care insurance (0-4)
  {
    Long_care <- as.data.table(dbGetQuery(myconn,"SELECT distinct min(a.[ID]) ID,
                                                  [FS_Versicherte]
                                                  ,[FS_Ereignis_Datum_Beginn]
                                                  ,[FS_Ereignis_Datum_Ende]
                                                  ,a.[pflegestufe]
                                            	    ,b.[pflegestufe_name]
                                                  FROM [DWH_olap].[dbo].[T_FACT_Versicherter_Pflegestufenzeiten]  AS a
                                                  LEFT JOIN [DWH_olap].[dbo].[T_DIM_Pflegestufe] AS b
                                                  ON a.[pflegestufe]= b.[kuerzel]
                                                  WHERE a.[log_duplikat]='0'
                                                  AND fs_krankenkassen in (3,4)
                                                  AND b.id  in (1,2,3,4,5,13)
                                          group by [FS_Versicherte]
                                                  ,[FS_Ereignis_Datum_Beginn]
                                                  ,[FS_Ereignis_Datum_Ende]
                                                  ,a.[pflegestufe]
                                            	    ,b.[pflegestufe_name]
                                          order by fs_versicherte, FS_Ereignis_Datum_Beginn")
    )}
}