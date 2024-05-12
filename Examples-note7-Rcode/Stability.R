library(lavaan)

## input data

stability <- read.table("~/Downloads/NEOMA-LISREL/Longitudinal/Stability.txt",header = T)

####### stability1a.spl #######
model_1a <- '
              ## Measurement models
              
              Alien67 =~ ANOMIA67 + POWERL67 
              Alien71 =~ ANOMIA71 + POWERL71 
              Ses =~ EDUC + SEI
              
              ## Regression
              
              Alien71 ~ Alien67 + Ses
              Alien67 ~ Ses
             
'

fit1 <- cfa(data =stability, model = model_1a,std.lv=T)
summary(fit1,standardized=TRUE, rsquare=T,fit.measures=T)


####### stability2a.spl #######
model_2a <- '
              ## Measurement models

              Alien67 =~ ANOMIA67 + POWERL67 
              Alien71 =~ ANOMIA71 + POWERL71 
              Ses =~ EDUC + SEI
              
              ## Regression
              
              Alien71 ~ Alien67 + Ses
              Alien67 ~ Ses
              
              ## correlated error
              
              ANOMIA67 ~~ ANOMIA71 
              POWERL67 ~~ POWERL71 
              
'

fit2 <- cfa(data =stability, model = model_2a,std.lv=T,likelihood = "wishart")
summary(fit2,standardized=TRUE, rsquare=T,fit.measures=T)
lavInspect(fit2, what = "cor.lv")

####### stability3a.spl #######
model_3a <- '
              ## Measurement models
              
              Alien67 =~ ANOMIA67 + c*POWERL67 
              Alien71 =~ ANOMIA71 + c*POWERL71 
              Ses =~ EDUC + SEI
              
              ## add intercepts and constrains
              
              ANOMIA67 ~ a*1
              ANOMIA71 ~ a*1
              POWERL67 ~ b*1
              POWERL71 ~ b*1
 
              ## Regression
               
              Alien71 ~ 1+Alien67 + Ses
              Alien67 ~ Ses 
              
              ## correlated error
              
              ANOMIA67 ~~ ANOMIA71 
              POWERL67 ~~ POWERL71 
'

fit3 <- cfa(data = stability, model = model_3a,likelihood = "wishart")

summary(fit3,standardized=TRUE,rsquare=T,fit.measures=T)
lavInspect(fit3, what = "cov.lv")








