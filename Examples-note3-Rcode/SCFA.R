
###CFA Analyze correlation - STEP 1
lower <- '
            1.00
            0.78 1.00
            0.80 0.77 1.00
            0.56 0.51 0.48 1.00
            0.52 0.51 0.46 0.78 1.00
            0.59 0.51 0.51 0.80 0.79 1.00
            0.16 0.15 0.17 0.14 0.18 0.16 1.00
            0.19 0.13 0.18 0.14 0.16 0.16 0.81 1.00
            0.12 0.17 0.17 0.17 0.10 0.16 0.75 0.80 1.00
            0.16 0.13 0.17 0.15 0.16 0.18 0.56 0.52 0.50 1.00
            0.16 0.14 0.18 0.15 0.16 0.18 0.51 0.58 0.51 0.81 1.00
            0.16 0.15 0.14 0.16 0.16 0.14 0.52 0.57 0.52 0.80 0.79 1.00
'

cormat <- getCov(lower,names=c(paste("P", seq(1:3), sep = ""),paste("C", seq(1:3), sep = ""),paste("E", seq(1:3), sep = ""),paste("S", seq(1:3), sep = "")))

model <- '
          Ps =~ P1 + P2 + P3
          Cr =~ C1 + C2 + C3
          Ee =~ E1 + E2 + E3
          Ss =~ S1 + S2 + S3
          
          '
fit <- lavaan::cfa(model, sample.cov = cormat, sample.nobs = 275,
                   likelihood = "wishart", std.lv=T)
summary(fit,rsquare=T,standardized=TRUE)

###Second Order CFA Analyze correlation - STEP 2
model2 <- '
          Ps =~ P1 + P2 + P3
          Cr =~ C1 + C2 + C3
          Ee =~ E1 + E2 + E3
          Ss =~ S1 + S2 + S3
          
          Pf =~ Ps + Cr
          Ef =~ Ee + Ss
          
          '
fit2 <- lavaan::cfa(model2, sample.cov = cormat, sample.nobs = 275,
                   likelihood = "wishart", std.lv=T)
summary(fit2,rsquare=T,standardized=TRUE)
require(semPlot)
semPaths(fit2, whatLabels = "Std.all",
         sizeMan = 10, edge.label.cex = 0.75,
         style = "ram",
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation =4)

