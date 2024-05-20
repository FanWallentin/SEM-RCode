library(lavaan)

urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note8-Rcode/LONGITUDINALUSA.csv"
longitudinal <- read.csv(urlfile)
longitudinal <- longitudinal[,1:12]

model <-
"
          Efficac1 =~ NOSAY1 + VOTING1 + COMPLEX1 + NOCARE1
          Respons1 =~ INTERES1 + NOCARE1 + TOUCH1
          Efficac2 =~ NOSAY2 + VOTING2 + COMPLEX2 + NOCARE2
          Respons2 =~ INTERES2 + NOCARE2 + TOUCH2
          
          Efficac2 ~ Efficac1
          Respons2 ~ Respons1
          
          
          NOSAY1 ~~ NOSAY2 
          COMPLEX1 ~~ COMPLEX2 
          NOCARE1 ~~ NOCARE2  
          TOUCH1 ~~ TOUCH2 
          INTERES1 ~~ INTERES2
          VOTING1 ~~  VOTING2  
          Efficac2 ~~ Respons2       

"
fit1  = cfa(model, longitudinal, estimator = "WLSMV",ordered = T)
summary(fit1,fit.measures = T,rsquare = T,standardized = T)






