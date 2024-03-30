
library(lavaan)
urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note3-Rcode/npv.csv"
npv <- read.csv(urlfile, header=T, sep= ",")
npv = npv[,c(1:9)]



fit_vari <- efa(data = npv, nfactors = 3, 
                rotation = "varimax",output = "efa") 
#3: three eigenvalues are freater than 1
## rotation =  varimax,  promax, 

summary(fit_vari,fit.measures = T)
fit_vari$loadings
