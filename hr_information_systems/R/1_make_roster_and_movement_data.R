# 1_make_ending_roster_data_and_movement_data.R
# Bennet Voorhees (bevo_data_science [at] me [dot] com)

# The purpose of this script is to build four datasets based on 
# 0_make_employee_data_bank.R for a fictitious company (WidgetCorp)- 
# a starting headcount roster, hires, terminations, and an ending headcount roster. 

# Headcount and movement types are parameterized. 

# Outputs are in CSV and feather formats

#### SETUP ####

library(data.table)
library(randomNames)
library(stringr)
library(feather)

set.seed(12)

#### PARAMETERS ####

starting_roster_effective_date<-'2018-12-31'

number_of_months_to_elapse<-12 #from previous report's effective date

ending_roster_effective_date<-ending_roster_effective_date

starting_headcount<-550


voluntary_attrition_rate<-0.072

involuntary_attrition_rate<-0.015

hire_rate<-0.08

internal_transfer_rate<-0.1

promotions_rate<-0.1

#### DATA GENERATION ####

## generate starting roster
sampling_data<-readRDS('./hr_information_systems/R/employee_data_for_sampling.RDS')

starting_roster<-sampling_data[`Employee Number`%in%sample(sampling_data$`Employee Number`,starting_headcount)]
setDT(starting_roster)

# add in effective date and tenure date for starting roster
starting_roster[,`Tenure Date`:=as.Date(starting_roster_effective_date)-(Tenure*365.25)]
starting_roster[,`Report Effective Date`:=as.Date(starting_roster_effective_date)]

## generate voluntary attrition
# to do - make this feature/formula based for student analysis

vol_term_emps<-sample(starting_roster[,`Employee Number`],
                      size=starting_roster[,.N]*voluntary_attrition_rate)

vol_terms_records<-starting_roster[`Employee Number`%in%vol_term_emps]
vol_terms_records[,`Action Type`:='Termination']
vol_terms_records[,`Voluntary / Involuntary`:='Voluntary']
vol_terms_records[,`Action Date`:=sample(seq(as.Date(starting_roster_effective_date), as.Date(starting_roster_effective_date)+(number_of_months_to_elapse/12)*365.25, by="day"), 1),`Email Address`]
vol_terms_records[,`Report Effective Date`:=NULL]

ending_roster<-starting_roster[!`Employee Number`%in%vol_term_emps]

## generate involuntary attrition
# to do - make this feature/formula based for student analysis

invol_term_emps<-sample(ending_roster[,`Employee Number`],
                           size=starting_roster[,.N]*involuntary_attrition_rate)

invol_terms_records<-starting_roster[`Employee Number`%in%invol_term_emps]
invol_terms_records[,`Action Type`:='Termination']
invol_terms_records[,`Voluntary / Involuntary`:='Involuntary']
invol_terms_records[,`Action Date`:=sample(seq(as.Date(starting_roster_effective_date), as.Date(starting_roster_effective_date)+(number_of_months_to_elapse/12)*365.25, by="day"), 1),`Email Address`]
invol_terms_records[,`Report Effective Date`:=NULL]

ending_roster<-ending_roster[!`Employee Number`%in%invol_term_emps]

## generate hire data
# to do - make this feature/formula based for student analysis

hires_records<-sampling_data[`Employee Number`%in%sample(setdiff(sampling_data[,`Employee Number`],
                                                                 starting_roster[,`Employee Number`]),round(starting_headcount*hire_rate))]

hires_records[,`Action Type`:='Hire']
hires_records[,`Tenure Date`:=sample(seq(as.Date(starting_roster_effective_date), as.Date(starting_roster_effective_date)+(number_of_months_to_elapse/12)*365.25, by="day"), 1),`Email Address`]
hires_records[,Tenure:=NULL]

## generate internal transfers data
# to do - make this feature/formula based for student analysis

empids_for_elibible_for_transfer<-starting_roster[!`Employee Number`%in%invol_term_emps & !`Employee Number`%in%vol_term_emps,`Employee Number`]

transferers<-sample(empids_for_elibible_for_transfer,round(starting_headcount*internal_transfer_rate))

departments_list<-ending_roster[,unique(Department)]

for (e in transferers){
  previous_department<-starting_roster[`Employee Number`==e,Department]
  new_department<-sample(departments_list[departments_list!=previous_department],1)
  ending_roster[`Employee Number`==e,Department:=new_department]
}

## generate promotions data
# to do - make this feature/formula based for student analysis

empids_for_elibible_for_promotions<-empids_for_elibible_for_transfer[!empids_for_elibible_for_transfer%in%starting_roster[`Job Level`=='Executive',`Employee Number`]]
  
promotees<-sample(empids_for_elibible_for_promotions,round(starting_headcount*promotions_rate))

job_level_list<-as.numeric(ending_roster[,unique(`Job Level`)])
names(job_level_list)<-ending_roster[,unique(`Job Level`)]

for (e in promotees){
  previous_job_level<-as.numeric(starting_roster[`Employee Number`==e,`Job Level`])
  new_job_level<-names(job_level_list[previous_job_level+1])
  ending_roster[`Employee Number`==e,`Job Level`:=new_job_level]
}

## generate finalized headcount roster data

ending_roster[,`Pay Rate`:=`Pay Rate`*1.025]
ending_roster[,`Base Comp`:=`Base Comp`*1.025]

ending_roster[,Tenure:=Tenure+number_of_months_to_elapse/12]

ending_roster<-rbind(ending_roster,hires_records,fill=T)

ending_roster[,`Action Type`:=NULL]
ending_roster[,`Report Effective Date`:=as.Date(ending_roster_effective_date)]

ending_roster[is.na(Tenure),Tenure:=as.numeric((`Report Effective Date`-`Tenure Date`))/365.25]

#### EXPORT DATASETS ####

#starting headcount
starting_hc_filename<-paste0('widgetcorp_workforce_detail_roster_',starting_roster_effective_date)

write.csv(starting_roster,paste0('./hr_information_systems/data/',starting_hc_filename,'.csv'),row.names=F)
write_feather(starting_roster,paste0('./hr_information_systems/data/',starting_hc_filename,'.feather'))

# hires

hires_filename<-paste0('widgetcorp_hires_',starting_roster_effective_date,'_to_',ending_roster_effective_date)

write.csv(hires_records,paste0('./hr_information_systems/data/',hires_filename,'.csv'),row.names=F)
write_feather(hires_records,paste0('./hr_information_systems/data/',hires_filename,'.feather'))

# terminations

terms<-rbind(vol_terms_records,invol_terms_records)

terms_filename<-paste0('widgetcorp_terms_',starting_roster_effective_date,'_to_',ending_roster_effective_date)

write.csv(terms,paste0('./hr_information_systems/data/',terms_filename,'.csv'),row.names=F)
write_feather(terms,paste0('./hr_information_systems/data/',terms_filename,'.feather'))

#ending headcount
ending_hc_filename<-paste0('widgetcorp_workforce_detail_roster_',ending_roster_effective_date)

write.csv(ending_roster,paste0('./hr_information_systems/data/',ending_hc_filename,'.csv'),row.names=F)
write_feather(ending_roster,paste0('./hr_information_systems/data/',ending_hc_filename,'.feather'))
