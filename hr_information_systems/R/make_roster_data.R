library(data.table)
library(randomNames)
library(stringr)

set.seed(12)
core_data=data.table(randomNames(267,return.complete.data = T))

#create email address
core_data[,`Email Address`:=paste0(tolower(first_name),'.',tolower(last_name),'@widgetcorp.com')]


#create random department data
departments<-c(rep('Sales',44),
               rep('Saless',9),
               rep('HR',12),
               rep('Finance',11),
               rep('Complaince',6),
               rep('Legal',4),
               rep('Executive',10),
               rep('Information Technology and Information Seucrity',13),
               rep('IT/IS',9),
               rep('Admin Offices',10),
               rep('Marketing',32),
               rep('Engineering',36),
               rep('Design',21),
               rep('Operations',27),
               rep('Procurement',23))

core_data[,Department:=sample(departments)]

#create random Job levels
job_levels<-c(rep('VP',1),
              rep('Director',2),
              rep('Manager',2),
              rep('Associate',3),
              rep('Analyst',4))

job_levels<-factor(job_levels,levels=c('Analyst','Associate','Manager','Director','VP'),ordered = T)

core_data[,`Job Level`:=sample(job_levels,size = .N,replace=T),Department]

#create random tenure distributions
core_data[,Tenure:=as.numeric(`Job Level`)*1+rnorm(n=1,mean=1,sd=1.2),`Email Address`]

#create tenure_dates
core_data[,`Tenure Date`:=Sys.Date()-Tenure*365.25]
core_data[,min_tenure_date:=min(`Tenure Date`)]

core_data[Department=='Executive',`Tenure Date`:=min_tenure_date-180]
core_data[Department=='Executive',`Job Level`:='Executive']

#clean gender data
setnames(core_data,'gender','gender_code')
core_data[gender_code==0, Gender:='Male']
core_data[gender_code==1, Gender:='Female']

#create random employee ID
core_data[order(`Tenure Date`),`Employee Number`:=str_pad(1:.N,8,pad='0')]

#create random salary information
core_data[,base_comp:=30000+(Tenure^.5)*5000+as.numeric(`Job Level`)*10000+rnorm(n=1,mean=7000,sd=3000),`Email Address`]

core_data[Department=='Executive',base_comp:=base_comp*1.75]

core_data[Gender=='Male',base_comp:=base_comp*rnorm(1,mean=1.3,sd=.2),`Email Address`]

core_data[,`Pay Rate`:=base_comp/52/40]

#clean data
core_data[,gender_code:=NULL]
core_data[,base_comp:=NULL]

#clean ethnicity data
core_data[,ethnicity:=NULL]
core_data[,Gender:=NULL]
core_data[,Tenure:=NULL]
core_data[,min_tenure_date:=NULL]

#rename Columns
setnames(core_data,c('first_name','last_name'),c('First Name','Last Name'))
setcolorder(core_data,c('Employee Number','First Name','Last Name','Email Address'))
setorderv(core_data,c('Department','Employee Number'))

saveRDS(core_data,'./RDS/headcount.RDS')

write.csv(core_data,'./headcount.csv',row.names = F)

# make a separate education column

education<-c(rep('High School Diploma',10),
               rep('Master',5),
               rep('Some College',10),
               rep('Bachelor',5),
               rep('PhD',2))

education_dat<-data.table(`Employee Number`=core_data[,`Employee Number`])
education_dat[,Education:=sample(education,size = .N,replace=T)]

write.csv(education_dat,'highest_education_completed.csv',row.names = F)