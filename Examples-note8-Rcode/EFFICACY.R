library(lavaan)

urlfile="https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note8-Rcode/EFFICACY.csv"
efficacy <-  read.csv(urlfile)
efficacy <- efficacy[,1:6]

model1 <- 
"
            Efficacy =~ NOSAY + VOTING + COMPLEX + NOCARE + TOUCH + INTEREST

"
run1  = cfa(model1, efficacy, estimator = "WLSMV",ordered = T, std.lv=T)
summary(run1,fit.measures = T,rsquare = T,standardized = T)

model2 <- 
"
            Efficacy =~ NOSAY + VOTING + COMPLEX
            RESPONS =~ NOCARE + TOUCH + INTEREST

"
run2= cfa(model2, efficacy, estimator = "WLSMV",ordered = T,std.lv=T)
summary(run2,fit.measures = T,rsquare = T,standardized = T)

