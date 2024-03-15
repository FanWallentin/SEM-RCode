##Prediction of Grade Averages
require(lavaan)

urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note1-Rcode/Predict.DAT"
PredictG <- read.table(urlfile, header=F, sep= " ")

colnames(PredictG) = c("GRAVEREQ", "GRAVELEC","KNOWLEDG", "IQPREVYR", "EDMOTIV")
sapply(PredictG,class)

Cov = cov(PredictG)
names(Cov) = c("GRAVEREQ", "GRAVELEC","KNOWLEDG", "IQPREVYR", "EDMOTIV")


## Error terms are not correlated
model1 <- '
       
         GRAVEREQ ~ KNOWLEDG + IQPREVYR + EDMOTIV
         GRAVELEC ~ KNOWLEDG + IQPREVYR + EDMOTIV
         GRAVEREQ ~~ 0*GRAVELEC
'
fit1 = lavaan::sem(model1, sample.cov = Cov, sample.nobs = 2000 ,likelihood = "wishart",
                   fixed.x=F,std.lv = T)
summary(fit1, standardized=TRUE,rsquare=T,fit.measures = TRUE)
fitMeasures(fit1) ## AIC = 2t - 2ln(L)


## Path diagram
require(semPlot)
require(semptools)
m <- matrix(c(NA,"KNOWLEDG",     NA,  "GRAVEREQ",
              NA,NA,  NA,   NA,
              "IQPREVYR",NA,  NA, NA,
              NA, NA,    NA,"GRAVELEC",
              NA,"EDMOTIV",   NA,  NA), byrow = TRUE, 5, 4)

path = semPaths(fit1, whatLabels = "est",
         sizeMan = 10, edge.label.cex = 0.75,
         style = "ram",
         nCharNodes = 0, nCharEdges = 0,
        layout = m)


## Error term are correlated

model2 <- '
       
         GRAVEREQ ~ KNOWLEDG + IQPREVYR + EDMOTIV
         GRAVELEC ~ KNOWLEDG + IQPREVYR + EDMOTIV
         GRAVEREQ ~~ GRAVELEC
'
fit2 = lavaan::cfa(model2,data = PredictG, likelihood = "wishart",
                   std.lv=T,fixed.x=F)
#fitMeasures(fit2) 
summary(fit2, standardized=TRUE,rsquare=T,fit.measures = TRUE)

semPaths(fit2, whatLabels = "est",
         sizeMan = 10, edge.label.cex = 0.75,
         style = "ram",
         nCharNodes = 0, nCharEdges = 0,
         layout = m)
