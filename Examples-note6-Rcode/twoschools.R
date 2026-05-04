library(lavaan)

urlfile1 <- "https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note6-Rcode/paster.csv"
urlfile2 <- "https://raw.github.com/FanWallentin/SEM-RCode/main/Examples-note6-Rcode/grantwhite.csv"


paster <- read.csv(urlfile1)
grantwhite <- read.csv(urlfile2)

paster <- paster[,1:9]
grantwhite <- grantwhite[,1:9]

paster$school <- "Paster"
grantwhite$school   <- "GrantWhite"

data_all <- rbind(paster, grantwhite)

model1 <- '
  # Measurement model
  Visual =~ 1*VISPERC + CUBES + LOZENGES + SCCAPS
  Verbal =~ 1*PARCOMP + SENCOMP + WORDMEAN
  Speed  =~ 1*ADDITION + COUNTDOT + SCCAPS

  # Latent covariances
  Visual ~~ Verbal
  Visual ~~ Speed
  Verbal ~~ Speed
'
### Baseline model
fit10 <- cfa(model1,
                  data = data_all,
                  group = "school")

## Testing factorial invariance
fit1 <- cfa(model1,
             data = data_all,
             group = "school",
             group.equal = c("loadings"),likelihood = "wishart")

summary(fit10, fit.measures=TRUE, standardized=TRUE)
summary(fit1, fit.measures=TRUE, standardized=TRUE)


########### twoschools2 ###########

## Testing measurement intercepts invariance
fit2 <- cfa(model1,
             data = data_all,
             group = "school",
             group.equal = c("loadings", "intercepts", "means"), likelihood="wishart")

summary(fit2, fit.measures=TRUE, standardized=TRUE)


########### twoschools3 ###########

### Testing latent mean difference
fit3 <- lavaan::cfa(model1,
              data = data_all,
              group = "school",
              group.equal = c("loadings","intercepts"),
              control = list(rel.tol = 1e-8))


summary(fit3, fit.measures=TRUE, standardized=TRUE)

### Likelihood ratio test for model differences

lavTestLRT(fit10, fit1)
lavTestLRT(fit1,fit2)
lavTestLRT(fit2,fit3)





