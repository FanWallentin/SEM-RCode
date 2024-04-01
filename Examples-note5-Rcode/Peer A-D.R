library(lavaan)
library(tidyverse)

urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note5-Rcode/peerinfluences.csv"
peer <- read.csv(urlfile)
peer <- peer[,c(1:10)]

######## PeersA

model_A <- '
           Reambitn =~ REOCCASP + REEDASP
           Bfambitn =~ BFOCCASP + BFEDASP
           
           Reambitn ~ Bfambitn + REPARASP + REINTGCE + RESOCIEC + BFSOCIEC 
           Bfambitn ~ Reambitn + RESOCIEC + BFSOCIEC + BFINTGCE + BFPARASP
'

fit_A <- lavaan::sem(model_A, data = peer,
           likelihood = "wishart")
summary(fit_A, standardized=TRUE,rsquare=T,fit.measures = T)
semPaths(fit_A, whatLabels = "est",
         sizeMan = 4, edge.label.cex = 0.5,
         style = "ram",curvature =2,
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 1,asize = 0.5)


######### Peer B
peer_std <- peer %>% mutate_all(~(scale(.) %>% as.vector))

model_B <- '
           Reambitn =~ REOCCASP + REEDASP
           Bfambitn =~ BFOCCASP + BFEDASP
           
           Reambitn ~ a*Bfambitn + REPARASP + REINTGCE + RESOCIEC + BFSOCIEC 
           Bfambitn ~ a*Reambitn + RESOCIEC + BFSOCIEC + BFINTGCE + BFPARASP
           
           Reambitn ~~ 0 * Bfambitn
           
           
'

fit_B <- lavaan::sem(model_B, data = peer_std,likelihood = "wishart")
summary(fit_B, standardized=TRUE,rsquare=T,fit.measures = T)

semPaths(fit_B, whatLabels = "est",
         sizeMan = 4, edge.label.cex = 0.5,
         style = "ram",curvature =2,
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 1,asize = 0.5)


######### Peer C

model_C <- '
           Reambitn =~ 1*REOCCASP + f*REEDASP
           Bfambitn =~ 1*BFOCCASP + f*BFEDASP
           
           Reambitn ~ a*Bfambitn + b*REPARASP + c*REINTGCE + d*RESOCIEC + e*BFSOCIEC 
           Bfambitn ~ a*Reambitn + e*RESOCIEC + d*BFSOCIEC + c*BFINTGCE + b*BFPARASP
           
           
'

fit_C <- sem(model_C, data = peer,likelihood = "wishart")
summary(fit_C, standardized=TRUE,rsquare=T)

semPaths(fit_C, whatLabels = "est",
         sizeMan = 4, edge.label.cex = 0.5,
         style = "ram",curvature =2,
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 1,asize = 0.5)



######### Peer D

model_D <- '
           Reambitn =~ REOCCASP + f*REEDASP
           Bfambitn =~ BFOCCASP + f*BFEDASP
           
           Reambitn ~ a*Bfambitn + b*REPARASP + c*REINTGCE + d*RESOCIEC + e*BFSOCIEC 
           Bfambitn ~ a*Reambitn + e*RESOCIEC + d*BFSOCIEC + c*BFINTGCE + b*BFPARASP
           
           Reambitn ~~ v1*Reambitn
           Bfambitn ~~ v1*Bfambitn
           REOCCASP ~~ v2*REOCCASP
           BFOCCASP ~~ v2*BFOCCASP
           REEDASP ~~ v3*REEDASP
           BFEDASP ~~ v3*BFEDASP
'

fit_D <- sem(model_D, data = peer,likelihood = "wishart")
summary(fit_D, standardized=TRUE,rsquare=T)

semPaths(fit_D, whatLabels = "est",
         sizeMan = 4, edge.label.cex = 0.5,
         style = "ram",curvature =2,
         nCharNodes = 0, nCharEdges = 0,
         layout = "tree2",rotation = 1,asize = 0.5)








