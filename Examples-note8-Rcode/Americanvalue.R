library(lavaan)

urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note8-Rcode/Americanvalue.csv"
americanvalue <- read.csv(urlfile)
americanvalue <- americanvalue[,1:6]

model <- '
            Economic =~ privtown + govtresp + compete
            Moral =~ homosex + abortion + govtresp + euthanas

'

fit1  = cfa(model, americanvalue, estimator = "WLSMVS",ordered = T)
summary(fit1,fit.measures = T,rsquare = T)
