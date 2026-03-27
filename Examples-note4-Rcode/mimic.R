
##### Modified Model for Performance and Satisfaction
library(lavaan)
urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note4-Rcode/mimic.COR"
lower_corr <- readLines(urlfile)
corrmat <- getCov(lower_corr,names = c("INCOME", "OCCUPATI", "EDUCATIO", "CHURCHAT", "MEMBERSH", "FRIENDS"))

#### Mimic 1

model_1 <- '
          Sparticp =~ CHURCHAT + MEMBERSH + FRIENDS
          Sparticp ~ INCOME
          Sparticp ~ OCCUPATI
          Sparticp ~ EDUCATIO
       
        
 '
fit_1 <- lavaan::cfa(model_1, sample.cov = corrmat, sample.nobs = 530,
                     likelihood = "wishart",std.lv = F)
summary(fit_1, standardized=TRUE,rsquare=T,fit.measures=T)
fitmeasures(fit_1)
semPaths(fit_1, whatLabels = "est",
         sizeMan = 8, edge.label.cex = 0.75,
         style = "ram",sizeMan2 = 5,
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 2)

#### Mimic 2

model_2 <- '
          Sparticp =~ CHURCHAT + MEMBERSH + FRIENDS
          Sstatus =~ INCOME + OCCUPATI + EDUCATIO
          Sparticp ~ Sstatus

       
        
 '
fit_2 <- lavaan::cfa(model_2, sample.cov = corrmat, sample.nobs = 530,
                     likelihood = "wishart",std.lv = F)
summary(fit_2, standardized=TRUE,rsquare=T,fit.measures=T)
fitmeasures(fit_2)
semPaths(fit_2, whatLabels = "est",
         sizeMan = 8, edge.label.cex = 0.75,
         style = "ram",sizeMan2 = 5,
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 2)
