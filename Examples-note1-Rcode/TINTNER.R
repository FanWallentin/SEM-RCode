
# Estimate the model using instrumental variables

library(lavaan)

lower <- '
3792.439
164.169   115.218
554.762    33.217   119.409
166.905   -24.385    44.721    62.252
379.754    38.651    56.171   -16.020    71.886'

covmat <- getCov(lower,names = c("x1","x2","x3","y1","y2"))

#### Regress Y1 on Y2 and X1 with X1-X3 as Instrumental Variables
model1 <- ' ## 23
        #### Regression model
             y1 ~ y2 + x1 
             y2 ~ x1 + x2 + x3
             x1 ~ x2 +x3

        #### Correlated variables
             y1 ~~ y2 + x1
          
         '
model1 <- ' ## 12
        #### Regression model
             y1 ~ y2 + x1 
             y2 ~ x1 + x2 
             x1 ~ x2

        #### Correlated variables
             y1 ~~ y2
          
         '

model1 <- ' ## 13
        #### Regression model
             y1 ~ y2 + x1 
             y2 ~ x1 + x3 
             x1 ~ x3 

        #### Correlated variables
             y1 ~~ y2
          
         '

model1 <- ' ## 123
        #### Regression model
             y1 ~ y2 + x1
             y2 ~ x1 +x2 + x3
             x1  ~ x3
             #x1 ~ x2 + x3
             

        #### Correlated variables
             y1 ~~y2 + x3 + x2 + x1
         '
fit1 <- lavaan::cfa(model1,sample.cov = covmat,
                    sample.nobs = 23, 
                    likelihood = "wishart",
                    fixed.x=F,std.lv = T)

summary(fit1, standardized=TRUE,rsquare=T,fit.measures=T)

#### Regress Y1 on Y2 X2 and X3 with X1-X3 as Instrumental Variables

model2 <- ' 
             y1 ~ y2 + x2 + x3
             y2 ~ x1 + x2 + x3
             x1 ~ x2 + x3
             y1 ~~ y2

         '
fit2 <- lavaan::cfa(model2,sample.cov = covmat,
                   sample.nobs = 23,
                   likelihood = "wishart",
                   fixed.x=F,std.lv = T)
summary(fit2, standardized=TRUE,rsquare=T,fit.measures=T)

fitMeasures(fit2, fit.measures = c("chisq", "rmsea"))

