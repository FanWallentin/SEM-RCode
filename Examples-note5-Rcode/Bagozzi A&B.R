
##### Modified Model for Performance and Satisfaction
library(lavaan)

urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note5-Rcode/Bagizzi.DAT"
bagizzi <- readLines(urlfile)
lower_corr <- bagizzi[1:8]
corrmat <- getCov(lower_corr,names = c("PERFORMM", "JBSATIS1", "JBSATIS2", 
                                       "ACHMOT1", "ACHMOT2", 
                                       "T_S_S_E1", "T_S_S_E2", "VERBINTM"))
std <- as.numeric(unlist(strsplit(bagizzi[9], " ")))

#### BagozziA

model_A <- '
          Perform =~ PERFORMM
          Jobsatis =~ JBSATIS1 +JBSATIS2
          Achmot =~ ACHMOT1 + ACHMOT2 
          T_s_s_e =~ T_S_S_E1 + T_S_S_E2
          Verbint =~ VERBINTM
          
          Perform ~ T_s_s_e
          Jobsatis ~ Perform + Achmot + Verbint
          
          PERFORMM  ~~ PERFORMM  
          #VERBINTM  ~~ VERBINTM
        
 '
fit_A <- lavaan::cfa(model_A, sample.cov = corrmat, sample.nobs = 122,
           likelihood = "wishart",std.lv = T)
summary(fit_A, standardized=TRUE,rsquare=T,fit.measures=T)
fitmeasures(fit_A)

## Path diagrams
require(semPlot)
path = semPaths(fit_A, whatLabels = "est",
                sizeMan = 8, edge.label.cex = 0.75,
                style = "ram",sizeMan2 = 5,
                nCharNodes = 0, nCharEdges = 0,
                layout = "tree2",rotation = 4)

######## BagozziB

model_B <- '
          Perform =~ PERFORMM
          Jobsatis =~ JBSATIS1 + JBSATIS2
          Achmot =~ ACHMOT1 + ACHMOT2 
          T_s_s_e =~ T_S_S_E1 + T_S_S_E2
          Verbint =~ VERBINTM
          
          Perform ~ T_s_s_e
          Jobsatis ~ Perform + Achmot + Verbint
          
          PERFORMM ~~ 0 * PERFORMM
          VERBINTM ~~ 1.998 * VERBINTM
          
          
          
 '
fit_B <- sem(model_B, sample.cov = corrmat, sample.nobs = 122,
              likelihood = "wishart")
summary(fit_B, standardized=TRUE,rsquare=T,fit.measures=T)
## Path diagrams
require(semPlot)
path = semPaths(fit_B, whatLabels = "est",
                sizeMan = 8, edge.label.cex = 0.75,
                style = "ram",sizeMan2 = 5,
                nCharNodes = 0, nCharEdges = 0,
                layout = "tree2",rotation = 4)




