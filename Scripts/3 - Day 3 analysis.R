rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

preSurvey <- read.spss("Data/Pre-Election - Processed.sav", to.data.frame = TRUE)
postSurvey <- read.spss("Data/Post-Election - Processed.sav", to.data.frame = TRUE)


##===Pre-Election survey===

#We'll run a regression on the pre-election survey, setting an uninformative prior
#Then use this to set the prior for post-election

#--Define model and data--

jags_model.preLin <- "model{
    for(i in 1:N){
        y[i] ~ dnorm(mu[i], tau)
        mu[i] <- Beta0 + Beta1 * x1[i] + Beta2 * x2[i]
    }

    Beta0 ~ dnorm(2.5, 0.2)
    Beta1 ~ dnorm(0.0, 0.2)        
    Beta2 ~ dnorm(0.0, 0.2)

    tau ~ dgamma(.001, .001)
}"

write(jags_model.preLin, "jags_model_preLin.bugs")

validcases <- !is.na(preSurvey$Fav_AfD) & !is.na(preSurvey$leftright_self) & !is.na(preSurvey$q55d_recode)
preSurvey.valid <- preSurvey[validcases,]

jags_data.preLin <- list(y = preSurvey.valid$Fav_AfD, x1 = preSurvey.valid$leftright_self, x2 = preSurvey.valid$q55d_recode,
                       N = nrow(preSurvey.valid))

#--Run initial model--

jags_reg.preLin <- jags.model(file = "jags_model_preLin.bugs", data = jags_data.preLin, n.chains = 5)
jags_out.preLin <- coda.samples(model = jags_reg.preLin, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 200)

#--Inspect for convergence and autocorrelation--

plot(jags_out.preLin) #appears to converge after around 100 iterations
gelman.plot(jags_out.preLin) #shrink factor is bad until about 150, and stuck at around 1.1 even after that
autocorr.plot(jags_out.preLin) #autocorrelation seems substantial

jags_out.preLin <- coda.samples(model = jags_reg.preLin, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 10000, thin = 25)

plot(jags_out.preLin) #looks good
gelman.plot(jags_out.preLin) #looks good
autocorr.plot(jags_out.preLin) #Looks pretty good, one minor instance that might indicate autocorrelation in B0

summary(jags_out.preLin)
#Mean B1 = .35, B2 = .29, B0 = -.14
#95th quantiles  .29 to .36 for B1, .21 to .28 for B2, -.56 to +.27 for B0
#So we can say with a fairly high probabiliy that B1 has a larger effect than B2, depending on covariance

#Beta1 and Beta2 are negatively correlated at about .05
#This is a weak enough correlation that B1 is probably > B2, but would want to quantify the probability of this
cor(unlist(jags_out.preLin[,"Beta1"]), unlist(jags_out.preLin[,"Beta2"]))


