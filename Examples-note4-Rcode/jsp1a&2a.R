library(lavaan)

urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note4-Rcode/jsp_aggregated.csv"
jsp = read.csv(urlfile)
jsp <- jsp[,c(1:6)]
jsp[jsp==-9] = NA


model1a <- '

             Math =~  math1 + math2 + math3
             Eng =~  eng1 + eng2 + eng3

'
fit1 <- lavaan::cfa(model1a, data=jsp,missing="FIML", 
               likelihood = "wishart",
               std.lv=T)
summary(fit1,standardized=TRUE,rsquare=T)

## Path diagrams
require(semPlot)
path = semPaths(fit1, whatLabels = "est",
                sizeMan = 8, edge.label.cex = 0.75,
                style = "ram",sizeMan2 = 5,
                nCharNodes = 0, nCharEdges = 0,
                layout = "tree2",rotation = 4)



model2a <- '

             Math =~  math1 + math2 + math3
             Eng =~  eng1 + eng2 + eng3
             
             math1 ~~ eng1
             math2 ~~ math1
             math3 ~~ math2
             
             eng2 ~~ eng1
             eng3 ~~ eng2
          
'
fit2 <- lavaan::cfa(model2a, data=jsp,missing="FIML", 
                    likelihood = "wishart",
                    std.lv=T)
summary(fit2,standardized=TRUE,rsquare=T)

## Path diagrams
path = semPaths(fit2, whatLabels = "est",
                sizeMan = 8, edge.label.cex = 0.75,
                style = "ram",sizeMan2 = 5,
                nCharNodes = 0, nCharEdges = 0,
                layout = "tree2",rotation = 4)


