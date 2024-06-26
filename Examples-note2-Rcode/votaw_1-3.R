library("lavaan")
###### votaw #######

lower <- '
          25.0704
          12.4363     28.2021
          11.7257      9.2281     22.7390
          20.7510     11.9732     12.0692     21.8707

'

covmat <- getCov(lower,names = c("ORIGPRT1", "WRITCOPY", "CARBCOPY", "ORIGPRT2"))

model <- '
          Ability =~ ORIGPRT1 + WRITCOPY + CARBCOPY + ORIGPRT2
'
fit <- lavaan::sem(model, sample.cov = covmat, sample.nobs = 126,
             likelihood = "wishart",fixed.x=F,std.lv=T)
summary(fit, standardized=TRUE,rsquare=T, fit.measures = TRUE)
fitmeasures(fit)

## Path diagram
require(semPlot)
path = semPaths(fit, whatLabels = "est",
                sizeMan = 10, edge.label.cex = 0.75,
                style = "ram",
                nCharNodes = 0, nCharEdges = 0,
                layout = "tree2",rotation = 4)



##### votaw1 ######

model1 <- '
          Ability =~ a*ORIGPRT1 + a*WRITCOPY + a*CARBCOPY + a*ORIGPRT2
          
          WRITCOPY ~~ b*WRITCOPY
          CARBCOPY ~~ b*CARBCOPY
          ORIGPRT2 ~~ b*ORIGPRT2
          ORIGPRT1 ~~ b*ORIGPRT1
          
          
'

fit1 <- lavaan::sem(model1, sample.cov = covmat, sample.nobs = 126,
                   likelihood = "wishart",fixed.x=F,std.lv=T)
summary(fit1, standardized=TRUE,rsquare=T,fit.measures = TRUE)

semPaths(fit1, whatLabels = "est",
                sizeMan = 10, edge.label.cex = 0.75,
                style = "ram",
                nCharNodes = 0, nCharEdges = 0,
                layout = "tree2",rotation = 4)

####### votaw2 #############

model2 <- '
          Ability =~ a*ORIGPRT1 + a*WRITCOPY + a*CARBCOPY + a*ORIGPRT2
'

fit2 <- lavaan::sem(model2, sample.cov = covmat, sample.nobs = 126,
                    likelihood = "wishart",std.lv=T)
summary(fit2, standardized=TRUE,rsquare=T,fit.measures = TRUE)

semPaths(fit2, whatLabels = "est",
         sizeMan = 10, edge.label.cex = 0.75,
         style = "ram",
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 4)













