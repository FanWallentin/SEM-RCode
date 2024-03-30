library("lavaan")
library("tidyverse")

urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note3-Rcode/npv.csv"
npv <- read.csv(urlfile)
npv <- npv[,c(1:9)]

colnames(npv)

##### npv1: Nine Psychological Variables - A Confirmatory Factor Analysis by Maximum Likelihood


model_npv1 <- '
          Visual =~ VISPERC + CUBES + LOZENGES + SCCAPS
          Verbal =~ PARCOMP + SENCOMP + WORDMEAN
          Speed =~ ADDITION + COUNTDOT + SCCAPS
          
          '
corr_npv1 = cor(npv)

fit_npv1 = lavaan::cfa(model_npv1,sample.cov = corr_npv1,sample.nobs = 145,std.lv=TRUE,
               likelihood = "wishart")


summary(fit_npv1, standardized=TRUE,rsquare = T,fit.measures=T)

## Path diagram
require(semPlot)
path = semPaths(fit_npv1, whatLabels = "est",
                sizeMan = 8, edge.label.cex = 0.75,
                style = "ram",sizeMan2 = 5,
                nCharNodes = 0, nCharEdges = 0,
                layout = "tree2",rotation = 4)


##### npv2 & 2a: Estimation of the NPV Model by Robust Maximum Likelihood

model_npv2 <- '
          Visual =~ VISPERC + CUBES + LOZENGES + SCCAPS
          Verbal =~ PARCOMP + SENCOMP + WORDMEAN
          Speed =~ ADDITION + COUNTDOT + SCCAPS
          
          '

npv_std <- npv %>% mutate_all(~(scale(.) %>% as.vector))
fit_npv2 = lavaan::cfa(model_npv2,data = npv_std,std.lv=TRUE,
                       likelihood = "wishart",estimator = "MLR")
summary(fit_npv2, standardized=TRUE,rsquare = T,fit.measures=T)
path = semPaths(fit_npv2, whatLabels = "est",
                sizeMan = 8, edge.label.cex = 0.75,
                style = "ram",sizeMan2 = 5,
                nCharNodes = 0, nCharEdges = 0,
                layout = "tree2",rotation = 4)



##### npv3 & 4: Robust Diagonally Weighted Least Squares

model_npv4  <- '
          Visual =~ VISPERC + CUBES + LOZENGES
          Verbal =~ PARCOMP + SENCOMP + WORDMEAN
          Speed =~ ADDITION + COUNTDOT + SCCAPS
          
          '
#For the DWLS, lavaan also provides ‘robust’ variants: WLSM, WLSMVS, WLSMV
fit_npv4 = cfa(model_npv4,data = npv_std,std.lv=TRUE, estimator = "wlsmvs")
summary(fit_npv4, standardized=TRUE,rsquare = T)
path = semPaths(fit_npv4, whatLabels = "est",
                sizeMan = 8, edge.label.cex = 0.75,
                style = "ram",sizeMan2 = 5,
                nCharNodes = 0, nCharEdges = 0,
                layout = "tree2",rotation = 4)

