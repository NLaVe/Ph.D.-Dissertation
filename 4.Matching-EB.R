# Create database
Treat<-Cohort1_BL
Control_b<-Control[[1]]
ifelse(Cohort=="2006-2009",m<-2,m<-8)
for(u in c(2:m)){Control_b<-rbind(Control_b,Control[[u]])}


Matching<-as.data.table(rbind(Treat,Control_b))

# Create age groups and Charlson groups
Matching$age<-as.numeric(Matching$age)
Matching$Agegroup <- cut(Matching$age, breaks=c(0, 18, 21, 24, 27, 30, 33, 36, 39, 42, 45, 48, 51, 54, 57, 60, 63, 66, 69, 72, 75, 78, 81, 84, 87, 90, 93, 96, 99, 110), right = FALSE)
table(Matching$Agegroup, useNA="always")

Matching$CharlsonScore<-as.numeric(Matching$CharlsonScore)
table(Matching$CharlsonScore, useNA="always")
Matching<-Matching[is.na(CharlsonScore),CharlsonScore:=0]
Matching$Charlsongroup <- cut(Matching$CharlsonScore, breaks=c(0, 2, 4, 6, 8, 10, 20), right = FALSE)


ifelse(Match_method=="PSM",
{
####PSM####
## Create periods unique to exact matching vars exact= c('age+-2', "sex","Charlson+-1","Krankenkasse","Insurance satatus")

#First 2-3 numbers is period; first numer is an aux 5.
Matching$period_c<-as.character(Matching$period)
Matching<-Matching[nchar(period_c)==1,period_c:=paste0("0",period_c)]
Matching<-Matching[,period_c:=paste0("5",period_c)]

#4-5 numbers are age group:
Matching<-Matching[Agegroup=="[0,18)",period_c:=paste0(period_c,"01")]
Matching<-Matching[Agegroup=='[18,21)',period_c:=paste0(period_c,"02")]
Matching<-Matching[Agegroup=='[21,24)',period_c:=paste0(period_c,"03")]
Matching<-Matching[Agegroup=='[24,27)',period_c:=paste0(period_c,"04")]
Matching<-Matching[Agegroup=='[27,30)',period_c:=paste0(period_c,"05")]
Matching<-Matching[Agegroup=='[30,33)',period_c:=paste0(period_c,"06")]
Matching<-Matching[Agegroup=='[33,36)',period_c:=paste0(period_c,"07")]
Matching<-Matching[Agegroup=='[36,39)',period_c:=paste0(period_c,"08")]
Matching<-Matching[Agegroup=='[39,42)',period_c:=paste0(period_c,"09")]
Matching<-Matching[Agegroup=='[42,45)',period_c:=paste0(period_c,"10")]
Matching<-Matching[Agegroup=='[45,48)',period_c:=paste0(period_c,"11")]
Matching<-Matching[Agegroup=='[48,51)',period_c:=paste0(period_c,"12")]
Matching<-Matching[Agegroup=='[51,54)',period_c:=paste0(period_c,"13")]
Matching<-Matching[Agegroup=='[54,57)',period_c:=paste0(period_c,"14")]
Matching<-Matching[Agegroup=='[57,60)',period_c:=paste0(period_c,"15")]
Matching<-Matching[Agegroup=='[60,63)',period_c:=paste0(period_c,"16")]
Matching<-Matching[Agegroup=='[63,66)',period_c:=paste0(period_c,"17")]
Matching<-Matching[Agegroup=='[66,69)',period_c:=paste0(period_c,"18")]
Matching<-Matching[Agegroup=='[69,72)',period_c:=paste0(period_c,"19")]
Matching<-Matching[Agegroup=='[72,75)',period_c:=paste0(period_c,"20")]
Matching<-Matching[Agegroup=='[75,78)',period_c:=paste0(period_c,"21")]
Matching<-Matching[Agegroup=='[78,81)',period_c:=paste0(period_c,"22")]
Matching<-Matching[Agegroup=='[81,84)',period_c:=paste0(period_c,"23")]
Matching<-Matching[Agegroup=='[84,87)',period_c:=paste0(period_c,"24")]
Matching<-Matching[Agegroup=='[87,90)',period_c:=paste0(period_c,"25")]
Matching<-Matching[Agegroup=='[90,93)',period_c:=paste0(period_c,"26")]
Matching<-Matching[Agegroup=='[93,96)',period_c:=paste0(period_c,"27")]
Matching<-Matching[Agegroup=='[96,99)',period_c:=paste0(period_c,"28")]
Matching<-Matching[Agegroup=='[99,110)',period_c:=paste0(period_c,"29")]


# 6-7 number are Charlson group
ifelse(exact_Charlson==1,{
Matching<-Matching[Charlsongroup=='[0,2)',period_c:=paste0(period_c,"01")]
Matching<-Matching[Charlsongroup=='[2,4)',period_c:=paste0(period_c,"02")]
Matching<-Matching[Charlsongroup=='[4,6)',period_c:=paste0(period_c,"03")]
Matching<-Matching[Charlsongroup=='[6,8)',period_c:=paste0(period_c,"04")]
Matching<-Matching[Charlsongroup=='[8,10)',period_c:=paste0(period_c,"05")]
Matching<-Matching[Charlsongroup=='[10,20)',period_c:=paste0(period_c,"06")]
Matching<-Matching[is.na(Charlsongroup)==TRUE,period_c:=paste0(period_c,"07")]},
{Matching<-Matching[,period_c:=paste0(period_c,"00")]})

# 8-9 number is sex
table(Matching$sex, useNA="always")
Matching<-Matching[sex=="w",period_c:=paste0(period_c,"01")]
Matching<-Matching[sex=="m",period_c:=paste0(period_c,"02")]

#10-11 number is kankenkasse
table(Matching$FS_Krankenkassen, useNA="always")
Matching<-Matching[FS_Krankenkassen=="3",period_c:=paste0(period_c,"01")]
Matching<-Matching[FS_Krankenkassen=="4",period_c:=paste0(period_c,"02")]
Matching<-Matching[FS_Krankenkassen=="8",period_c:=paste0(period_c,"02")]

# last aux number (5) to not mess with entry period
Matching<-Matching[,period_c:=paste0(period_c,"05")]

#clean database
Matching<-as.data.table(Matching)
ifelse(exact_Charlson==1,
       Matching<-Matching[,-c('period','age',"Agegroup","Charlsongroup",'CharlsonScore',"sex",'Agegroup','FS_Krankenkassen')],
       Matching<-Matching[,-c('period','age',"Agegroup","Charlsongroup","sex",'Agegroup','FS_Krankenkassen')])

# Check a tabulation per period to see if there are enough ppl in each period
aux3<-as.data.table(table(Matching$period_c))

#create entry period for treated
Matching$period_c<-as.numeric(Matching$period_c)
Matching<-Matching[,entry:=period_c+1] #because we only have data of the baseline

# package
library(rollmatch)

# Trim the data
reduced_data <- reduce_data(data = Matching, treat = paste(treat),
                            tm = "period_c", entry = "entry",
                            id = "FS_Versicherte", lookback = 1)

#Given that the number comes from claims and taht they are correctly calculated (na.rm=T), we change NAs for 0's
reduced_data<-as.data.frame(reduced_data)
ifelse(Cost_in_matching==1,
reduced_data[c('Total_cost','Total_contribution','N_phy', 'N_spec', 'N_Hosp', 'S_LoS', 'S_LoS_rehab', 'N_pres', 'Mean_LC', 'S_LoS_WP', 'S_LoS_WT')][is.na(reduced_data[c('Total_cost','Total_contribution','N_phy', 'N_spec', 'N_Hosp', 'S_LoS', 'S_LoS_rehab', 'N_pres', 'Mean_LC', 'S_LoS_WP', 'S_LoS_WT')])] <- 0,
reduced_data[c('N_phy', 'N_spec', 'N_Hosp', 'S_LoS', 'S_LoS_rehab', 'N_pres', 'Mean_LC', 'S_LoS_WP', 'S_LoS_WT')][is.na(reduced_data[c('N_phy', 'N_spec', 'N_Hosp', 'S_LoS', 'S_LoS_rehab', 'N_pres', 'Mean_LC', 'S_LoS_WP', 'S_LoS_WT')])] <- 0)

ifelse(Prescription_ATC==1,reduced_data[,append(keep[keep %ni% c("versicherter")],keep2)][is.na(reduced_data[,append(keep[keep %ni% c("versicherter")],keep2)])] <- 0,
       reduced_data[,keep[keep %ni% c("versicherter")]][is.na(reduced_data[,keep[keep %ni% c("versicherter")]])] <- 0)


#Score the data
reduced_data<-as.data.table(reduced_data)
if(treat=="GK_part")
       {fm<-reformulate(colnames(reduced_data[,-c("FS_Versicherte","period_c","entry","P_part")]), response="GK_part")} else
       {fm<-reformulate(colnames(reduced_data[,-c("FS_Versicherte","period_c","entry","GK_part")]), response="P_part")}
vars2 <- all.vars(fm)
scored_data <- score_data(reduced_data = reduced_data,
                          model_type = "logistic", match_on = "pscore",
                          fm = fm, treat = paste(treat),
                          tm = "period_c", entry = "entry", id = "FS_Versicherte")

# Output
output <- rollmatch(scored_data, data=Matching, treat = paste(treat),
                    tm = "period_c", entry = "entry", id = "FS_Versicherte",
                    vars = vars2, lookback = 1, alpha = a,
                    standard_deviation = "average", num_matches = 1,
                    replacement = FALSE)

#caliper "A maximal difference of 0.01  or 0.1(reviewer bmj) (0.2 to 1.9 standard deviations)"
#-> "The caliper width is calculated as the alpha multiplied by the pooled standard deviation of the propensity scores or the logit of the propensity scores - depending on the value of match_on."
sd(output$scores)*a
sd(scored_data$score)*a


matched<-output$matched_data
output$summary
scores<-output$scores
unmatched<-Cohort1_BL[FS_Versicherte %ni% matched$treat_id,1:18]
yesmatched<-Cohort1_BL[FS_Versicherte %in% matched$treat_id,1:18]

Matching_check<-merge(Matching,matched[,c('control_id','treat_id')],by.x = 'FS_Versicherte', by.y='treat_id', all.x = TRUE)
Matching_check<-Matching_check[is.na(control_id)==FALSE | FS_Versicherte %in% control_id]
Matching_check$period_c<-as.character(Matching_check$period_c)

# Database for follow up
aux4<-as.data.table(matched[,c("control_id","period_c")])
aux4<-aux4[,treat:=0]
aux4<-aux4 %>% rename(FS_Versicherte=control_id)

aux5<-as.data.table(matched[,c("treat_id","period_c")])
aux5<-aux5[,treat:=1]
aux5<-aux5 %>% rename(FS_Versicherte=treat_id)
FU_DB<-rbind(aux4,aux5)

#Create numeric period and starting point of FU as date
FU_DB<-FU_DB[,period:=substr(period_c, 2,3)]
FU_DB$period<-as.numeric(FU_DB$period)
periods$period<-as.numeric(periods$period)
FU_DB<-merge(FU_DB,periods,by.x = "period", by.y = "period", all.x = T)
FU_DB<-merge(FU_DB,Cohort1[,c('FS_Versicherte','GK_begin','P_begin')],by.x = "FS_Versicherte", by.y = "FS_Versicherte", all.x = T)
FU_DB<-FU_DB[treat==0, START_fu:=ymd(end)]
FU_DB$START_fu<-as.Date(FU_DB$START_fu, "%Y-%m-%d")
FU_DB$GK_begin<-as.Date(FU_DB$GK_begin, "%Y-%m-%d")
FU_DB$P_begin<-as.Date(FU_DB$P_begin, "%Y-%m-%d")
ifelse(treat=="GK_part",
       FU_DB<-FU_DB[treat==1, START_fu:=GK_begin],
       FU_DB<-FU_DB[treat==1, START_fu:=P_begin])
FU_DB<-FU_DB[,c("FS_Versicherte","START_fu","treat")]
FU_DB<-merge(FU_DB,matched[,c('treat_id','control_id')],by.x = "FS_Versicherte", by.y = "treat_id", all.x = T)
  FU_DB<-merge(FU_DB,matched[,c('treat_id','control_id')],by.x = "FS_Versicherte", by.y = "control_id", all.x = T)
  FU_DB<-merge(FU_DB,Cohort1[,c('FS_Versicherte','GK_begin')],by.x = "treat_id", by.y = "FS_Versicherte", all.x = T)
  FU_DB<-FU_DB[treat==0, START_fu:=GK_begin]
  FU_DB<-FU_DB[,c('FS_Versicherte','treat','START_fu','control_id')]
  FU_DB<-FU_DB[,weights:=1]
  FU_DB<-FU_DB[,FS_Versicherte_p:=paste(FS_Versicherte,"_",period)]
},
{
  ###Entropy Balancing###
  library(ebal)
  
  #Create matrix for the reweighting
  X<-Matching[,-c('Agegroup','Charlsongroup')]
  #Given that the number comes from claims and taht they are correctly calculated (na.rm=T), we change NAs for 0's
  X<-as.data.frame(X)
  X[c('N_phy', 'N_spec', 'N_Hosp', 'S_LoS', 'S_LoS_rehab', 'N_pres', 'Mean_LC', 'S_LoS_WP', 'S_LoS_WT')][is.na(X[c('N_phy', 'N_spec', 'N_Hosp', 'S_LoS', 'S_LoS_rehab', 'N_pres', 'Mean_LC', 'S_LoS_WP', 'S_LoS_WT')])] <- 0
  ifelse(Prescription_ATC==1,X[,append(keep[keep %ni% c("versicherter")],keep2)][is.na(X[,append(keep[keep %ni% c("versicherter")],keep2)])] <- 0,
         X[,keep[keep %ni% c("versicherter")]][is.na(X[,keep[keep %ni% c("versicherter")]])] <- 0)
  
  #transform everything to numeric variables except FS_Versicherter, that should be transformed into unique period-versicherter
  X<-as.data.table(X)
  X<-X[,FS_Versicherte_p:=paste(FS_Versicherte,"_",period)]
  X$FS_Versicherte<-NULL
  aux7<-dummy_cols(X$period)
  ifelse(Cohort=="2006-2013",
  aux7<-aux7 %>% rename(period_1 =.data_1,
                        period_2 =.data_2,
                        period_3 =.data_3,
                        period_4 =.data_4,
                        period_5 =.data_5,
                        period_6 =.data_6,
                        period_7 =.data_7,
                        period_8 =.data_8),
  aux7<-aux7 %>% rename(period_1 =.data_1,
                        period_2 =.data_2,
                        period_3 =.data_3,
                        period_4 =.data_4))
  aux7<-as.data.table(aux7)
  X<-cbind(X[,-c('period')],aux7[,-c('.data')])
  
  aux8<-dummy_cols(X$FS_Krankenkassen)
  aux8<-aux8 %>% rename(krankenkasse_1 =.data_3,
                        krankenkasse_2 =.data_4)
  aux8<-as.data.table(aux8)
  X<-cbind(X[,-c('FS_Krankenkassen')],aux8[,-c('.data')])
  
  X<-X[,sex_d:=0]
  X<-X[sex=="w",sex_d:=1]
  X$sex<-NULL
  X$S_LoS<-as.numeric(X$S_LoS)
  X$S_LoS_rehab<-as.numeric(X$S_LoS_rehab)
  X$S_LoS_WP<-as.numeric(X$S_LoS_WP)
  X$S_LoS_WT<-as.numeric(X$S_LoS_WT)
  X$FS_Versicherte_p<-as.character(X$FS_Versicherte_p)
  
  #Eliminate one dummy for avoiding collinear variables.
  if(Cohort=="2006-2013") {X$period_8<-NULL} else {X$period_4<-NULL}
  X$krankenkasse_2<-NULL
  
  #add squares of Charlson, age to also balance their variances
  X<-X[,age2:=age*age]
  X<-X[,Charlson2:=CharlsonScore*CharlsonScore]
  
  #Check for collinear columns
  qr<-qr(X[,-c('GK_part','P_part','FS_Versicherte_p')])
  remove<-caret::findLinearCombos(X[get(treat)==0,-c('GK_part','P_part','FS_Versicherte_p')])
  
  #treatment vector
  ifelse(treat=="GK_part",
         treatment<-as.vector(X[,c(GK_part)]),
         treatment<-as.vector(X[,c(P_part)]))
  
  #create entropy balanced weights
  eb.out<-ebalance(treatment, X[,-c('GK_part','P_part','FS_Versicherte_p')], base.weight = NULL,
                   norm.constant = NULL, coefs = NULL,
                   max.iterations = 2000,
                   constraint.tolerance = 0.5, print.level = 0)
  
  #Create numeric period and starting point of FU as date
  control_weights<-cbind(X[get(treat)==0,c('FS_Versicherte_p')],eb.out$w)
  names(control_weights)[2] <- "weights"
  treatment_weights<-X[get(treat)==1,weights:=1]
  treatment_weights<-treatment_weights[get(treat)==1,c('FS_Versicherte_p',"weights")]
  FU_DB<-rbind(control_weights,treatment_weights)
  FU_DB<-merge(FU_DB,X[,c('FS_Versicherte_p','GK_part','P_part',
                          'period_1','period_2','period_3','period_4',
                          'period_5','period_6','period_7','krankenkasse_1')],by.x = "FS_Versicherte_p", by.y = "FS_Versicherte_p", all.x = T)
  FU_DB<-FU_DB[,period:=sub(".*_ ", "", FS_Versicherte_p)]
  FU_DB$period<-as.character(FU_DB$period)
  FU_DB<-merge(FU_DB,periods,by.x = "period", by.y = "period", all.x = T)
  FU_DB<-FU_DB[, START_fu:=ymd(end)]
  FU_DB<-FU_DB[, START_fu:=START_fu+(183)]
  FU_DB<-FU_DB[, treat:=0]
  ifelse(treat=="GK_part",
         FU_DB<-FU_DB[GK_part==1, treat:=1],
         FU_DB<-FU_DB[P_part==1, treat:=1])
  FU_DB<-FU_DB[,c('FS_Versicherte_p','treat','START_fu','weights',
                  'period_1','period_2','period_3','period_4',
                  'period_5','period_6','period_7','krankenkasse_1')]
  FU_DB$START_fu<-as.Date(FU_DB$START_fu, "%Y-%m-%d")
  FU_DB<-FU_DB[,FS_Versicherte:=sub("\\_.*", "", FS_Versicherte_p)]
  FU_DB$FS_Versicherte<-as.numeric(FU_DB$FS_Versicherte)
  ##FU will start at the start of the next year to the year of signing up.
  
  #check balanced sample
  sum(FU_DB[treat==0,c('weights')])
  sum(FU_DB[treat==1,c('weights')])
  
}) 
  
