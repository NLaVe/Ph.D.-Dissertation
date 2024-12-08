#Select Cohort
Cohort<-"2006-2013" #"2006-2009" OR  "2006-2013"

#Select treatment
treat<-"GK_part" #P_part; GK_part

#Select if prescriptions should be part of matching process
Prescription_ATC<-0

#set up period of assessment length
ass_periods<-"long" # "long" or "short". Long = 1 year; Short = 0.5 years)

#Define sex (for mortality analysis)
sex<-"all" #all, "male","female"

#Define follow up for "other indicators"
FU_otherind<-1 #1=all - dead or alive; 2 = Only living and living matched pair; 3 = Living regardless matched pair

#Select matching method
Match_method<-"PSM" # "PSM" OR "EB" (Propensity Score Matching; Entropy Balancing)

#Check matching?
match_check<-"no" #'yes' or 'no'
match_check_before<-"no" #'yes' or 'no'

#Charlson score in the exact matching?
exact_Charlson<-1 #0 or 1

#Add Cost & contribution to Matching?
Cost_in_matching<-0 #0 or 1

#ICD code or disease group ?
icd10<-1 #0 or 1

#Run everything
setwd("E:/Nicolas - OptiMedis/Ablage_Nicolas/GK comparison")
source("1.Cohort.R")
source("2.BL.R")
source("3.Control_group.R")
source("4.Matching.R")
source("5.FU-only mortality.R")

##Matching
#check match (only if match_check=="yes")
#In the matched results we check for the "Std. Mean diff" to be lower than 0.2, and less is better. If they are we say the model is balanced.
table3 #EB
table4 #PSM
write.csv(table3, file = paste0(paste(treat,Cohort,Prescription_ATC,FU_otherind,ass_periods,Match_method,exact_Charlson,Cost_in_matching,icd10,match_check_before),"table3.csv"))
write.csv(table4, file = paste0(paste(treat,Cohort,Prescription_ATC,FU_otherind,ass_periods,Match_method,exact_Charlson,Cost_in_matching,icd10),"table4.csv"))

###5.plot to see the balance of the samples.

#Matched vs unmatched & scored data
write.csv(yesmatched,paste0(paste(treat,Cohort,Prescription_ATC,icd10),"_yesmatched.csv"))
write.csv(unmatched,paste0(paste(treat,Cohort,Prescription_ATC,icd10),"_unmatched.csv"))
write.csv(scored_data,paste0(paste(treat,Cohort,Prescription_ATC,icd10),"_scored_data.csv"))

##Mortality
write.csv(outs_all, file = paste0(paste(treat,Cohort,Prescription_ATC,sex,ass_periods,Match_method,icd10),"_Mortality.csv"))

#Kaplan-Meyer curve
jpeg(paste0(paste(treat,Cohort,sex,Match_method,icd10),"_Kaplan-Meyer.jpg"))
graph
dev.off()

#WEighted Kaplan-Meyer curve
jpeg(paste0(paste(treat,Cohort,sex,Match_method,icd10),"_Kaplan-Meyer-Weighted.jpeg"))
graph3
dev.off()

ggplot2::ggsave("graph3.jpeg")

# Hazard ratio
jpeg(paste0(paste(treat,Cohort,sex,Match_method,icd10),"_Hazard-Ratio.jpg"))
graph2
dev.off()
summary(fit.coxph)

# Cox regression
summary(fit.coxph)

##Other Indicators
write.csv(other_indicators, file = paste0(paste(treat,Cohort,Prescription_ATC,FU_otherind,ass_periods,Match_method,exact_Charlson,Cost_in_matching,icd10),"_Other_indicators.csv"))

#Propensity score histograms
#Propensity score histograms
A<-as.numeric(scored_data[GK_part==1,score])
B<-as.numeric(scored_data[GK_part==0,score])

b <- (min(A) - 0.00001) # Set the minimum for the breakpoints
e <- max(B) # Set the maximum for the breakpoints
ax <- pretty(b:e, n = 1000) # Make a neat vector for the breakpoints

hgA <- hist(A, plot = FALSE) # Save first histogram data
hgB <- hist(B, plot = FALSE) # Save 2nd histogram data

plot(hgB, col = "#0000FF20") # Add 2nd histogram using different color
plot(hgA, col = "#FF000090", add = TRUE) # Plot 1st histogram using a transparent color

##for combined graph
ifelse(Match_method=="PSM",{Survival_PSM<-Survival},{Survival_EB<-Survival})
Survival<-rbind(Survival_EB,Survival_PSM)


