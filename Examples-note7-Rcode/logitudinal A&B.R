
##### longitudinalA: 
library(lavaan)
library(readr)
urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note7-Rcode/longitudinal.COR"
dat = read_lines(urlfile)

cormat = getCov(dat[1:12], names = c("MATH9", "SCI9", "SS9", "READ9",
                                     "SCATV9", "SCATQ9", "MATH7", "SCI7",
                                     "SS7", "READ7", "SCATV7", "SCATQ7" ))


std_split = as.numeric(unlist(strsplit(dat[13:14], " ")))
std = as.numeric(na.omit(std_split))

model_A <- '
            Qant9 =~ SCATQ9 + MATH9 + SCI9 + SS9 
            Verb9 =~ SCATV9 + READ9 
            Qant7 =~ SCATQ7 + MATH7 + SCI7 + SS7 
            Verb7 =~ SCATV7 + READ7 
            
            Qant9 ~ Qant7 
            Verb9 ~ Verb7
            
            SCATV9 ~~ SCATV7
  
'


fit_A <- lavaan::sem(model_A, sample.cov = cormat,
                     sample.nobs = 383,
                     likelihood = "wishart")

summary(fit_A, standardized=TRUE,rsquare=T,nd=4)

## Path diagrams
require(semPlot)
semPaths(fit_A, whatLabels = "est",
         sizeMan = 8, edge.label.cex = 0.75,
         style = "ram",sizeMan2 = 4,sizeLat = 4,
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 4)


##if don't add "SCATV9 ~~ SCATV7"
##lavaan WARNING: covariance matrix of latent variables is not positive definite;
##highly correlated factors 


model_B <- '
            Qant9 =~ SCATQ9 + MATH9 + SCI9 + SS9 
            Verb9 =~ SCATV9 + SCI9 + SS9 + READ9 
            Qant7 =~ SCATQ7 + MATH7 + SCI7 + SS7 
            Verb7 =~ SCATV7 + SCI7 + SS7 + READ7
            

            Qant9 ~ Qant7
            Qant7 ~ Verb7 

           # Verb9 ~~ Verb7 
           
            ### set the Error Covariance of Verb9 and Qant9 Free 
            
            MATH9 ~~ MATH7
            SCI9 ~~ SCI7 
            SS9 ~~ SS7
            READ9 ~~ READ7
            SCATV9 ~~ SCATV7 
            SCATQ9 ~~ SCATQ7
            
'

fit_B <- lavaan::cfa(model_B, sample.cov = cormat,
                     sample.nobs = 383, 
                     likelihood = "wishart")

summary(fit_B, standardized=TRUE,rsquare=T)
semPaths(fit_B, whatLabels = "est",
         sizeMan = 5, edge.label.cex = 0.75,
         style = "lisrel",sizeLat = 5,
         nCharNodes = 0, nCharEdges = 0,
         rotation = 3,levels = c(1,3.5,5,7))




