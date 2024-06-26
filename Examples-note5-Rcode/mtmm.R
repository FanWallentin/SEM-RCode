
library(lavaan)
library(semPlot)
cor2cov <- function(R, S) {
  sweep(sweep(R, 1, S, "*"), 2, S, "*")
}

urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note5-Rcode/mtmm.cm"
mtmm <- readLines(urlfile)
std <- c(3.61, 3.66, 3.59, 2.94, 3.03, 2.85, 2.22, 2.42, 2.04)

cormat <- getCov(mtmm[c(1:9)],names = c("PARI", "SZTI", "SZDI", "PARC", 
                                     "SZTC", "SZDC", "PARO", "SZTO", "SZDO"))

covmat <- cor2cov(cormat,std)

####### mtmm1-brown:
##MTMM - CFA SPECIFICATION correlated traits and uncorrelated methods
model <- '

  # Define the measurement model for the observed variables
  PARANOID =~ PARI + PARC + PARO
  SCHIZOTYP =~ SZTI + SZTC + SZTO
  SCHIZOID =~ SZDI + SZDC + SZDO
  
  INVENTRY =~ PARI + SZTI + SZDI
  INTERVW =~ PARC + SZTC + SZDC
  OBSERVE =~ PARO + SZTO + SZDO

  # Specify the uncorrelated latent variables
  PARANOID ~~ 0*INVENTRY
  PARANOID ~~ 0*INTERVW
  PARANOID ~~ 0*OBSERVE
  
  SCHIZOTYP ~~ 0*INVENTRY
  SCHIZOTYP ~~ 0*INTERVW
  SCHIZOTYP ~~ 0*OBSERVE
  
  SCHIZOID ~~ 0*INVENTRY
  SCHIZOID ~~ 0*INTERVW
  SCHIZOID ~~ 0*OBSERVE
  
  INVENTRY ~~ 0*INTERVW
  INTERVW ~~ 0*OBSERVE
  INVENTRY ~~ 0*OBSERVE
'

fit <- lavaan::sem(model, sample.cov = covmat, sample.nobs = 500, 
                   std.lv = T,likelihood = "wishart")

# Print the summary of the model
summary(fit,standardized=TRUE,rsquare=T,fit.measures = TRUE)
semPaths(fit, whatLabels = "est",
         sizeMan = 4, edge.label.cex = 0.75,
         style = "ram",
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 4,fixedStyle = c( "white","0"))

#### MTMM - CFA SPECIFICATION correlated traits and methods
#### mtmm-browm
model <- '

  # Define the measurement model for the observed variables
  PARANOID =~ PARI + PARC + PARO
  SCHIZOTYP =~ SZTI + SZTC + SZTO
  SCHIZOID =~ SZDI + SZDC 
  
  INVENTRY =~ PARI + SZTI + SZDI
  INTERVW =~ PARC + SZTC + SZDC
  OBSERVE =~ PARO + SZTO 

  # Specify the uncorrelated latent variables
  PARANOID ~~ 0*INVENTRY
  PARANOID ~~ 0*INTERVW
  PARANOID ~~ 0*OBSERVE
  
  SCHIZOTYP ~~ 0*INVENTRY
  SCHIZOTYP ~~ 0*INTERVW
  SCHIZOTYP ~~ 0*OBSERVE
  
  SCHIZOID ~~ 0*INVENTRY
  SCHIZOID ~~ 0*INTERVW
  SCHIZOID ~~ 0*OBSERVE
'

fit <- lavaan::cfa(model, sample.cov = covmat, sample.nobs = 500, 
                   std.lv = T)

# Print the summary of the model
summary(fit,standardized=TRUE,rsquare=T)
semPaths(fit, whatLabels = "est",
         sizeMan = 4, edge.label.cex = 0.75,
         style = "ram",
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 4,fixedStyle = c( "white","0"))


###MTMM - CFA SPECIFICATION correlated traits and uncorrelated methods
### mtmmcfa-brown
model3 <- '

  # Define the measurement model for the observed variables
  PARANOID =~ PARI + PARC + PARO
  SCHIZOTYP =~ SZTI + SZTC + SZTO
  SCHIZOID =~ SZDI + SZDC + SZDO
  
  PARI ~~ SZTI 
  PARI ~~ SZDI 
  SZDI ~~ SZTI
  
  PARC ~~ SZTC
  PARC ~~ SZDC 
  SZDC ~~ SZTC 
  
  PARO ~~ SZTO 
  PARO ~~ SZDO 
  SZDO ~~ SZTO


'

fit <- lavaan::cfa(model3, sample.cov = cormat, sample.nobs = 500, 
                   std.lv = T,likelihood = "wishart")
summary(fit,standardized=TRUE,rsquare=T)
semPaths(fit, whatLabels = "est",
         sizeMan = 5, edge.label.cex = 0.4,
         style = "ram",sizeLat = 6,
         nCharNodes = 0, nCharEdges = 0,
         rotation = 2,fixedStyle = c( "white","0"))



