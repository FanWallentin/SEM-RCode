 
### SIMPLEX Model for GPA Model 1         
library(lavaan)
library(tidyverse)

urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note7-Rcode/humphreys.csv"
dat = read.csv(urlfile)
dat <- dat[,1:10]

model <- ' 

  #### "=~" : measurement model
            gpa1 =~ 1*GPA1
            gpa2 =~ 1*GPA2
            gpa3 =~ 1*GPA3
            gpa4 =~ 1*GPA4
            gpa5 =~ 1*GPA5
            gpa6 =~ 1*GPA6
            gpa7 =~ 1*GPA7
            gpa8 =~ 1*GPA8
          
   ### Regression
            gpa2 ~ gpa1
            gpa3 ~ gpa2
            gpa4 ~ gpa3
            gpa5 ~ gpa4
            gpa6 ~ gpa5
            gpa7 ~ gpa6
            gpa8 ~ gpa7
            
            
  ### Variance          
            GPA1 ~~ v1*GPA1
            GPA2 ~~ v1*GPA2
            GPA3 ~~ v3*GPA3
            GPA4 ~~ v4*GPA4
            GPA5 ~~ v5*GPA5
            GPA6 ~~ v6*GPA6
            GPA7 ~~ v2*GPA7
            GPA8 ~~ v2*GPA8
            
      
'

GPA.out <- lavaan(model, data = dat,auto.var = TRUE,likelihood = "wishart")
summary(GPA.out, fit.measures=TRUE, rsquare=T,standardized=T)

## Path diagrams
require(semPlot)
semPaths(GPA.out, whatLabels = "est",
         sizeMan = 8, edge.label.cex = 0.75,
         style = "ram",sizeMan2 = 4,sizeLat = 4,
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree",rotation = 2)


###### GPA: no constrains (GPA1 = GPA2, GPA7 = GPA8)


model.no <- ' 

  #### "=~" : measurement model
            gpa1 =~ 1*GPA1
            gpa2 =~ 1*GPA2
            gpa3 =~ 1*GPA3
            gpa4 =~ 1*GPA4
            gpa5 =~ 1*GPA5
            gpa6 =~ 1*GPA6
            gpa7 =~ 1*GPA7
            gpa8 =~ 1*GPA8
          
    ### Regression
            gpa2 ~ gpa1
            gpa3 ~ gpa2
            gpa4 ~ gpa3
            gpa5 ~ gpa4
            gpa6 ~ gpa5
            gpa7 ~ gpa6
            gpa8 ~ gpa7
            
     ### Variance
            GPA1 ~~ v1*GPA1
            GPA2 ~~ v2*GPA2
            GPA3 ~~ v3*GPA3
            GPA4 ~~ v4*GPA4
            GPA5 ~~ v5*GPA5
            GPA6 ~~ v6*GPA6
            GPA7 ~~ v7*GPA7
            GPA8 ~~ v8*GPA8
            
      
'

GPAno.out <- lavaan(model.no, data = dat,auto.var = TRUE,likelihood = "wishart")
summary(GPAno.out, fit.measures=TRUE, rsquare=T,standardized=T)

##### OUTPUT: WARNING: Could not compute standard errors! The information matrix could
#####         not be inverted. This may be a symptom that the model is not identified.


semPaths(GPAno.out, whatLabels = "est",
         sizeMan = 8, edge.label.cex = 0.75,
         style = "ram",sizeMan2 = 4,sizeLat = 4,
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree",rotation = 2)





