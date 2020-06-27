# 0_make_starting_roster_data.R
# Bennet Voorhees (bevo_data_science [at] me [dot] com)
# The purpose of this script is to make a fictitious starting dataset 
# for a fictitious company (WidgetCorp). 

# It is parameterized to set the number of employees and to set the effective date of the 
# roster. 
# It outputs two datasets for student learning: an employee roster and a separted
# file of employee education levels. Outputs are in CSV and feather formats

#### SETUP ####

library(data.table)
library(randomNames)
library(stringr)
library(feather)

set.seed(12)

#### PARAMETERS ####

#declare number of employees
no_employees<-550

#declare effective date for report
effective_date<-as.Date('2020-01-31')

#### DATA GENERATION ####

#create random names for employees
#note the randomNames package returns gender and ethnicity data 
roster_data=data.table(randomNames(n = no_employees,return.complete.data = T))
setnames(roster_data,'ethnicity','Ethnicity')

#create email addresses for each employee
roster_data[,`Email Address`:=paste0(tolower(first_name),'.',tolower(last_name),'@widgetcorp.com')]

#create random department data with errors in spelling for students to clean
departments<-c(rep('Sales',16),
               rep('Saless',3),
               rep('HR',4),
               rep('Finance',4),
               rep('Complaince',2),
               rep('Legal',2),
               rep('Executive',4),
               rep('Information Technology and Information Seucrity',5),
               rep('IT/IS',3),
               rep('Admin Offices',4),
               rep('Marketing',12),
               rep('Engineering',13),
               rep('Design',8),
               rep('Operations',10),
               rep('Procurement',10))

roster_data[,Department:=sample(departments,size=no_employees,replace = T)]

#create random job levels with a traditional hierarchy
job_levels<-c(rep('VP',1),
              rep('Director',2),
              rep('Manager',2),
              rep('Associate',3),
              rep('Analyst',4))

job_levels<-factor(job_levels,levels=c('Analyst','Associate','Manager','Director','VP'),ordered = T)

roster_data[,`Job Level`:=sample(job_levels,size = .N,replace=T),Department]

#create random tenure distributions based on job level
roster_data[,Tenure:=as.numeric(`Job Level`)*1+rnorm(n=1,mean=3,sd=1.2),`Email Address`]

#create tenure_dates - with executives as original employees/founders
roster_data[,`Report Effective Date`:=effective_date]

roster_data[,`Tenure Date`:=`Report Effective Date`-Tenure*365.25]
roster_data[,min_tenure_date:=min(`Tenure Date`)]

roster_data[Department=='Executive',`Tenure Date`:=min_tenure_date-180]
roster_data[Department=='Executive',Tenure:=as.numeric(`Report Effective Date`-`Tenure Date`)/365.25]
roster_data[Department=='Executive',`Job Level`:='Executive']

#recode randomNames' gender data from numeric to character
#to do: include options for non-binary and non-provided gender information
setnames(roster_data,'gender','gender_code')
roster_data[gender_code==0, Gender:='Male']
roster_data[gender_code==1, Gender:='Female']

#create random employee IDs in order of tenure
roster_data[order(`Tenure Date`),`Employee Number`:=str_pad(1:.N,8,pad='0')]

#create random salary information based on tenure and job level
roster_data[,`Base Comp`:=30000+(Tenure^.5)*5000+as.numeric(`Job Level`)*10000+rnorm(n=1,mean=7000,sd=3000),`Email Address`]

#give executives a bump above everyone else
roster_data[Department=='Executive',`Base Comp`:=`Base Comp`*1.75]

#introduce pay equity issues for student analysis
roster_data[Gender=='Male',`Base Comp`:=`Base Comp`*rnorm(1,mean=1.3,sd=.2),`Email Address`]
roster_data[,`Pay Rate`:=round(`Base Comp`/52/40,2)]

#remove columns used for data construction purposes
roster_data[,gender_code:=NULL]
roster_data[,min_tenure_date:=NULL]

#rename columns
setnames(roster_data,c('first_name','last_name'),c('First Name','Last Name'))

#reorder columns
setcolorder(roster_data,c('Employee Number','First Name','Last Name','Email Address','Department','Job Level','Tenure','Tenure Date','Pay Rate','Base Comp','Gender','Ethnicity'))

#sort data
setorderv(roster_data,c('Department','Employee Number'))

# make a separate education dataset for students to practice mereges/lookups
education<-c(rep('High School Diploma',10),
             rep('Master',5),
             rep('Some College',10),
             rep('Bachelor',5),
             rep('PhD',2))

education_data<-data.table(`Employee Number`=roster_data[,`Employee Number`])
education_data[,Education:=sample(education,size = .N,replace=T)]

#### DATA EXPORT ####

write.csv(roster_data,paste0('./hr_information_systems/data/widgetcorp_detail_roster_',effective_date,'.csv'), row.names = F)
write_feather(roster_data,paste0('./hr_information_systems/data/widgetcorp_detail_roster_',effective_date,'.feather'))

write.csv(education_data,'./hr_information_systems/data/widgetcorp_worker_education.csv',row.names = F)
write_feather(education_data,'./hr_information_systems/data/widgetcorp_worker_education.feather')