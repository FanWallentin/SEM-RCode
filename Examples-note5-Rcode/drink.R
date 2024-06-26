
#### Drinking and Driving

library(lavaan)
library(tidyverse)

urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note5-Rcode/drinkdata.csv"
drink <- read.csv(urlfile)
drink <- drink[,c(1:16)]
drink_std <- drink %>% mutate_at(colnames(drink), ~(scale(.) %>% as.vector))

######## drink11a

model11a <- '
             Attitude =~ X1 + X2 + X3 + X4 + X5
'
fit11a <- lavaan::cfa(model11a,data=drink_std,
                      likelihood = "wishart",std.lv=T,estimator="MLR")
summary(fit11a,standardized=TRUE,rsquare=T,fit.measures=T)

## Path diagrams
require(semPlot)
path = semPaths(fit11a, whatLabels = "est",
                sizeMan = 8, edge.label.cex = 0.75,
                style = "ram",sizeMan2 = 5,
                nCharNodes = 0, nCharEdges = 0,
                layout = "tree2",rotation = 4)



######## drink12a
cormat = cor(drink)
model12a <- '
             Attitude =~ X1 + X2 + X3 + X4 + X5
             Norms =~ X6 + X7 + X8
'
fit12a <- lavaan::cfa(model12a,sample.cov = cormat, sample.nobs = 756,
                      likelihood = "wishart",std.lv=T)
summary(fit12a,standardized=TRUE,rsquare=T,fit.measures=T)
semPaths(fit12a, whatLabels = "est",
         sizeMan = 10, edge.label.cex = 0.75,
         style = "ram",
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 4)

######## drink13a
model13a <- '
             Attitude =~ X1 + X2 + X3 + X4 + X5
             Norms =~ X6 + X7 + X8
             Control =~ X9 + X10 + X11 + X12
'
fit13a <- lavaan::cfa(model13a, data = drink_std,
                      likelihood = "wishart",std.lv=T)
summary(fit13a,standardized=TRUE,rsquare=T,fit.measures=T)
semPaths(fit13a, whatLabels = "est",
         sizeMan = 8, edge.label.cex = 0.75,
         style = "ram",sizeMan2 = 4,
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 4)

######## drink14a

model14a <- '
             Intention =~ Y1 + Y2
             Behavior =~ Y3 + Y4
             Attitude =~ X1 + X2 + X3 + X4 + X5
             Norms =~ X6 + X7 + X8
             Control =~ X9 + X10 + X11 + X12
             
'

fit14a <- lavaan::cfa(model14a, data = drink_std,
                      likelihood = "wishart",std.lv=T)
summary(fit14a,standardized=TRUE,rsquare=T,fit.measures=T)
semPaths(fit14a, whatLabels = "est",
         sizeMan = 8, edge.label.cex = 0.75,
         style = "ram",sizeMan2 = 4, 
         nCharNodes = 0, nCharEdges = 0,
         layout = "circle")

######## drink15a

model15a <- '
             Intention =~ Y1 + Y2
             Behavior =~ Y3 + Y4
             Attitude =~ X1 + X2 + X3 + X4 + X5
             Norms =~ X6 + X7 + X8
             Control =~ X9 + X10 + X11 + X12
             
             Intention ~ Attitude + Norms + Control
             Behavior ~ Intention
'
             
fit15a <- lavaan::cfa(model15a, data = drink_std,
                      likelihood = "wishart",std.lv=T)
summary(fit15a,rsquare=T,standardized=TRUE)             
             
semPaths(fit15a, whatLabels = "est",
         sizeMan = 8, edge.label.cex = 0.75,
         style = "ram",sizeMan2 = 4,
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 4)






