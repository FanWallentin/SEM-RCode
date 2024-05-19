library(lavaan)
americanvalue <- read.csv("~/Desktop/PhD-SU/SEM/ordinal/Americanvalue.csv")
americanvalue <- americanvalue[,1:6]

model <- '
            Economic =~ privtown + govtresp + compete
            Moral =~ homosex + abortion + govtresp + euthanas

'

fit1  = cfa(model, americanvalue, estimator = "WLSMVS",ordered = T)
summary(fit1,fit.measures = T,rsquare = T)
