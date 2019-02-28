rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

preSurvey <- read.spss("Data/Pre-Election - Processed.sav", to.data.frame = TRUE)
postSurvey <- read.spss("Data/Post-Election - Processed.sav", to.data.frame = TRUE)


##===1. Pre-Election survey===

#We'll run a regression on the pre-election survey, setting an uninformative prior
#Then use this to set the prior for post-election

#--Define model and data--

#Defining means and variances of Betas in the data parameter, so I can recycle this model definition across multiple priors

jags_model.lin <- "model{
    for(i in 1:N){
y[i] ~ dnorm(mu[i], tau)
mu[i] <- Beta0 + Beta1 * x1[i] + Beta2 * x2[i]
}

Beta0 ~ dnorm(B0_mean, B0_var)
Beta1 ~ dnorm(B1_mean, B1_var)        
Beta2 ~ dnorm(B2_mean, B2_var)

tau ~ dgamma(.001, .001)
}"

write(jags_model.lin, "jags_model.lin.bugs")

validcases.pre <- !is.na(preSurvey$Fav_AfD) & !is.na(preSurvey$leftright_self) & !is.na(preSurvey$q55d_recode)
preSurvey.valid <- preSurvey[validcases.pre,]

jags_data.preLin <- list(y = preSurvey.valid$Fav_AfD, x1 = preSurvey.valid$leftright_self, x2 = preSurvey.valid$q55d_recode,
                       N = nrow(preSurvey.valid),
                       B0_mean = 2.5, B0_var = 0.2,
                       B1_mean = 0.0, B1_var = 0.2,
                       B2_mean = 0.0, B2_var = 0.2)

#--Run initial model--

jags_reg.preLin <- jags.model(file = "jags_model.lin.bugs", data = jags_data.preLin, n.chains = 5)
jags_out.preLin <- coda.samples(model = jags_reg.preLin, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 200)

#--Inspect for convergence and autocorrelation--

plot(jags_out.preLin) #appears to converge after around 100 iterations
gelman.plot(jags_out.preLin) #shrink factor is bad until about 150, and stuck at around 1.1 even after that
autocorr.plot(jags_out.preLin) #autocorrelation seems substantial

#--Run and assess adjusted model

jags_out.preLin <- coda.samples(model = jags_reg.preLin, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 10000, thin = 25)

plot(jags_out.preLin) #looks good
gelman.plot(jags_out.preLin) #looks good
autocorr.plot(jags_out.preLin) #Looks pretty good, one minor instance that might indicate autocorrelation in B0

jags_summary.preLin <- summary(jags_out.preLin)
jags_summary.preLin
#Mean B1 = .35, B2 = .29, B0 = -.14
#95th quantiles  .29 to .36 for B1, .21 to .28 for B2, -.56 to +.27 for B0
#So we can say with a fairly high probabiliy that B1 has a larger effect than B2, depending on covariance

#Beta1 and Beta2 are negatively correlated at about -.05
#This is a weak enough correlation that B1 is probably > B2, but would want to quantify the probability of this
cor(unlist(jags_out.preLin[,"Beta1"]), unlist(jags_out.preLin[,"Beta2"]))


##===2. Post-Election survey===

#--Define data--

#Note that q55 in the presurvey = q74 in the postsurvey
validcases.post <- !is.na(postSurvey$Fav_AfD) & !is.na(postSurvey$leftright_self) & !is.na(postSurvey$q74d_recode)
postSurvey.valid <- postSurvey[validcases.post,]

#Expand SD of priors, to account for the possibility of change over time
jags_data.postLin <- list(y = postSurvey.valid$Fav_AfD, x1 = postSurvey.valid$leftright_self, x2 = postSurvey.valid$q74d_recode,
                         N = nrow(postSurvey.valid), 
                         B0_mean = jags_summary.preLin$statistics["Beta0","Mean"], B0_var = 1 / (jags_summary.preLin$statistics["Beta0","SD"] * 2),
                         B1_mean = jags_summary.preLin$statistics["Beta1","Mean"], B1_var = 1 / (jags_summary.preLin$statistics["Beta1","SD"] * 2),
                         B2_mean = jags_summary.preLin$statistics["Beta2","Mean"], B2_var = 1 / (jags_summary.preLin$statistics["Beta0","SD"] * 2))

#--Initial run--

jags_reg.postLin <- jags.model(file = "jags_model.lin.bugs", data = jags_data.postLin, n.chains = 3)
jags_out.postLin <- coda.samples(model = jags_reg.postLin, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 5000)

plot(jags_out.postLin)
autocorr.plot(jags_out.postLin) #as expected, continuing autocorrelation; 25 lags look like it should solve things, but we can be conservative
gelman.plot(jags_out.postLin) #shrunken after about 1000 iterations

#--Subsequent run--
jags_out.postLin <- coda.samples(model = jags_reg.postLin, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 25000, thin = 50)

plot(jags_out.postLin) #iterations seem pretty well-mixed, but there's a weird lumpy bit in the B1 and B2 distributions
autocorr.plot(jags_out.postLin) #autocorrelation looks okay
gelman.plot(jags_out.postLin) #shrinkage factor is good

jags_summary.postLin <- summary(jags_out.postLin)
jags_summary.postLin
jags_summary.preLin

#Results are a bit unexpected - in the pre-election survey, Beta2 had a strong positive relation [.21, .35]
#in the post-survey, it has a negative estimate [-.28, .05]
#the size of Beta1 differs drastically too, from mean .28 in the pre to mean .54 in the post
#this seems to warrant further investigation

#--Visualizations--

#Consider adding some visualizations here - marginal effects plots, and graphs of prior vs posterior
