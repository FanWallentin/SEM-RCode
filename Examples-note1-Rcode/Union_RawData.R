
######## Input: Raw data

library(lavaan)
#### Union A
urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note1-Rcode/union.dat"
union <- read.table(urlfile)

colnames(union)=c("Y1","Y2","Y3","X1","X2")
modelB <- '
           Y1 ~ X2
           Y2 ~ X2 + Y1
           Y3 ~ X1 +Y1 +Y2

'
fit_B <- lavaan::cfa(modelB,data = union,likelihood = "wishart",
                     fixed.x=F,std.lv = T)
summary(fit_B, fit.measures = T, rsquare = T)

## Path diagram

require(semPlot)
semPaths(fit_B, whatLabels = "est",
         sizeMan = 10, edge.label.cex = 0.75,
         style = "ram",
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 4,curve=2.3,node.width = 1)



