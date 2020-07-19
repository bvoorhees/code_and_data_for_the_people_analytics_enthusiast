library(data.table)
library(randomNames)
library(stringr)
library(feather)

set.seed(12)

core_data<-read_feather('./hr_information_systems/data/widgetcorp_workforce_detail_roster_2018-12-31.feather')
setDT(core_data)

survey<-data.table(`Employee Number`=core_data[,`Employee Number`])

even_distrib<-c(rep(1,20),
                rep(2,20),
                rep(3,20),
                rep(4,20),
                rep(5,20))

skew_v_low<-c(rep(1,30),
              rep(2,30),
              rep(3,20),
              rep(4,10),
              rep(5,10))

skew_low<-c(rep(1,20),
            rep(2,30),
            rep(3,30),
            rep(4,10),
            rep(5,10))

skew_high<-c(rep(1,10),
             rep(2,10),
             rep(3,30),
             rep(4,30),
             rep(5,20))

skew_v_high<-c(rep(1,10),
               rep(2,10),
               rep(3,20),
               rep(4,30),
               rep(5,30))

for (dept in core_data[,unique(Department)]){
  assign(paste0(dept,'_emplids'),core_data[Department==dept,`Employee Number`])
}

#"I am proud to work for widgetcorp"
survey[,Q1:=sample(skew_high,size = .N,replace = T)]

#"I would recommend WidgetCorp as a great place to work"
survey[,Q2:=Q1+rbinom(.N,1,1/2)]

#"I see myself still working at company in two years' time
emplid_new_tenure<-core_data[`Report Effective Date`-`Tenure Date`<365*2,`Employee Number`]

core_data[,comp_thresh:=mean(`Pay Rate`)-sd(`Pay Rate`),.(`Job Level`,Department)]
emplid_low_comp<-core_data[comp_thresh>=`Pay Rate`,`Employee Number`]

survey[`Employee Number`%in%emplid_new_tenure,Q3:=sample(skew_low,size = .N,replace = T),`Employee Number`]
survey[`Employee Number`%in%emplid_low_comp,Q3:=sample(skew_v_low,size = .N,replace = T),`Employee Number`]
survey[is.na(Q3),Q3:=Q2+rbinom(.N,1,1/2)]

#Widgetcorp motivates me to go beyond what I would in a similar role elsewhere
survey[,Q4:=Q3+rbinom(.N,1,1/2)]

#The leaders at WidgetCorp have communicated a vision that motivates me”
survey[`Employee Number`%in%Design_emplids,Q5:=sample(skew_v_low,size= .N, replace=T),`Employee Number`]
survey[,Q5:=sample(skew_high,size= .N, replace=T),`Employee Number`]

# “I have access to the things I need to do my job well”
survey[,Q6:=Q5+rbinom(.N,4,1/3)]

# “I know what I need to do to be successful in my role”
survey[,Q7:=abs(Q5-rbinom(.N,10,1/2))]

# I believe there are good career opportunities for me at this company"
survey[,Q8:=Q3+rbinom(.N,1,1/2)]

#Randomly boot out some responses to make response rate 58%
question_vars<-grep('Q',x=names(survey),value = T)
for (v in question_vars){
  survey[get(v)<0,c(v):=abs(get(v))]
  survey[get(v)==0,c(v):=1]
  survey[get(v)>5,c(v):=5]
}

survey<-survey[!sample(nrow(survey),size = 42)]

write.csv(survey,'./employee_listening/data/survey.csv',row.names = F)

write_feather(survey,'./employee_listening/data/survey.feather')
