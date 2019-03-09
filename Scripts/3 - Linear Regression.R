rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

preSurvey <- read.spss("Data/Pre-Election - Processed.sav", to.data.frame = TRUE)
postSurvey <- read.spss("Data/Post-Election - Processed.sav", to.data.frame = TRUE)
source("Scripts/makeJagsData.R")


##===1. Pre-Election survey===

#We'll run a regression on the pre-election survey, setting an uninformative prior
#Then use this to set the prior for post-election

#--Define model and data--

#Recoding
preSurvey$voteAfD_2017 <- as.numeric(preSurvey$voteAfD_2017) - 1

#Define data
validcases.pre <- !is.na(preSurvey$leftright_self) & !is.na(preSurvey$q55d_recode)
preSurvey.valid <- preSurvey[validcases.pre,]
validcases.post <- !is.na(postSurvey$leftright_self) & !is.na(postSurvey$q74d_recode)
postSurvey.valid <- postSurvey[validcases.post,]

validcases.preBin <- !is.na(preSurvey$Fav_AfD) & !is.na(preSurvey$q55d_recode)
preSurvey.validBin <- preSurvey[validcases.preBin,]

validcases.postBin <- !is.na(postSurvey$Fav_AfD) & !is.na(postSurvey$q74d_recode)
postSurvey.validBin <- postSurvey[validcases.postBin,]

#Define models
jags_data.preLin <- makeJagsData(df = preSurvey.valid,  ivs = c("leftright_self", "q55d_recode"), dv = "Fav_AfD", addConstant = TRUE)
jags_data.preBin <- makeJagsData(df = preSurvey.validBin, ivs = c("Fav_AfD", "q55d_recode"), dv = "voteAfD_2017", addConstant = TRUE)
jags_data.preBin$J <- NULL
jags_data.postBin <- makeJagsData(df = postSurvey.validBin, ivs = c("Fav_AfD", "q74d_recode"), dv = "voteAfD_2017", addConstant = TRUE)
jags_data.postBin$J <- NULL


#Define priors for beta, via a matrix
jags_data.preLin$B_mean <- c(2.5, 0.0, 0.0)
jags_data.preLin$B_var <- diag(3) * c(0.2, 0.2, 0.2)
jags_data.preBin$B_mean <- c(0.0, 0.0, 0.0)
jags_data.preBin$B_var <- diag(3) * c(0.1, 0.1, 0.1)


# jags_data.preLin <- list(y = preSurvey.valid$Fav_AfD, x1 = preSurvey.valid$leftright_self, x2 = preSurvey.valid$q55d_recode,
#                        N = nrow(preSurvey.valid),
#                        B0_mean = 2.5, B0_var = 0.2,
#                        B1_mean = 0.0, B1_var = 0.2,
#                        B2_mean = 0.0, B2_var = 0.2)

#--Run initial model--
jags_reg.preLin <- jags.model(file = "JAGS Models/lin.bugs", data = jags_data.preLin, n.chains = 5)
jags_out.preLin <- coda.samples(model = jags_reg.preLin, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 200)

#--Inspect for convergence and autocorrelation--

plot(jags_out.preLin) #appears to converge after around 100 iterations
gelman.plot(jags_out.preLin) #shrink factor is bad until about 150, and stuck at around 1.1 even after that
autocorr.plot(jags_out.preLin, ask = FALSE) #autocorrelation seems substantial

#--Run and assess adjusted model

jags_out.preLin <- coda.samples(model = jags_reg.preLin, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 10000, thin = 25)

plot(jags_out.preLin) #looks good
gelman.plot(jags_out.preLin) #looks good
autocorr.plot(jags_out.preLin, ask = FALSE) #Looks pretty good, one minor instance that might indicate autocorrelation in B0

jags_summary.preLin <- summary(jags_out.preLin)
jags_summary.preLin
#Mean B1 = .35, B2 = .29, B0 = -.14
#95th quantiles  .29 to .36 for B1, .21 to .28 for B2, -.56 to +.27 for B0
#So we can say with a fairly high probabiliy that B1 has a larger effect than B2, depending on covariance

#Beta1 and Beta2 are negatively correlated at about -.05
#This is a weak enough correlation that B1 is probably > B2, but would want to quantify the probability of this
jags_dfout.preLin <-  do.call("rbind", jags_out.preLin)
cor(jags_dfout.preLin)


##===2. Post-Election survey (independent priors)===

#The assumption here is that there is no covariance between B0, B1, and B2

#--Define data--

#Note that q55 in the presurvey = q74 in the postsurvey
jags_data.postInd <- makeJagsData(df = postSurvey.valid,  ivs = c("leftright_self", "q74d_recode"), dv = "Fav_AfD", addConstant = TRUE)

#--Define priors for beta, via a matrix--
jags_data.postInd$B_mean <- jags_summary.preLin$statistics[,"Mean"]
jags_data.postInd$B_var <- diag(3) * jags_summary.preLin$statistics[,"SD"]

#--Initial run--
jags_reg.postInd <- jags.model(file = "JAGS Models/lin.bugs", data = jags_data.postInd, n.chains = 5)
jags_out.postInd <- coda.samples(model = jags_reg.postInd, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 5000)

plot(jags_out.postInd)
autocorr.plot(jags_out.postInd, ask = FALSE) #as expected, continuing autocorrelation; 25 lags look like it should solve things, but we can be conservative
gelman.plot(jags_out.postInd) #shrunken after about 1000 iterations

#--Subsequent run--
jags_out.postInd <- coda.samples(model = jags_reg.postInd, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 25000, thin = 50)

plot(jags_out.postInd) #iterations seem pretty well-mixed, but there's a weird lumpy bit in the B1 and B2 distributions
autocorr.plot(jags_out.postInd, ask = FALSE) #autocorrelation looks okay
gelman.plot(jags_out.postInd) #shrinkage factor is good

jags_summary.postInd <- summary(jags_out.postInd)
jags_summary.postInd
jags_summary.preLin

#Results are a bit unexpected - in the pre-election survey, Beta2 had a strong positive relation [.21, .35]
#in the post-survey, it has a negative estimate [-.28, -.05]
#the size of Beta1 differs drastically too, from mean .28 in the pre to mean .54 in the post
#this seems to warrant further investigation

#--Visualizations--

#Consider adding some visualizations here - marginal effects plots, and graphs of prior vs posterior


##===3. Post-Election survey (correlated priors)===

#--Define data and model--

jags_data.postCor <- jags_data.postInd
jags_data.postCor$B_var <- var(jags_dfout.preLin)
jags_data.postCor$J <- NULL #Since we define the distribution of variables via a multivariate normal rather than a for loop, we don't need J (the count of variables)

#--Inspect intiial run--

jags_reg.postCor <- jags.model(file = "JAGS Models/lin_cor.bugs", data = jags_data.postCor, n.chains = 5)
#jags_reg.postCor <- jags.model(file = "JAGS Models/lin.bugs", data = jags_data.postCor, n.chains = 5)
jags_out.postCor <- coda.samples(model = jags_reg.postCor, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 5000)

plot(jags_out.postCor) #Convergence looks good, but Beta3 has a slightly lumpy distribution
gelman.plot(jags_out.postInd) #Shrinkage looks good after about 2000 iterations
autocorr.plot(jags_out.postInd, ask = FALSE) #Autocorrelation looks good

#--Final run--
update(jags_out.postCor, 1000)

summary(jags_out.postCor) #B0: [-.15, +.73], B1: [.47, .60], B2: [-.29, -.06]
summary(jags_out.postInd) #B0: [-.13, +.70], B1: [.47, .60], B2: [-.29, -.06]
#Very little difference in posteriors between independent and correlated priors


##===4. Pre-Election survey - binomial regression===

#Try first calling bin.cc.bugs (from the "missing data" script) to generate posteriors from uncorrelated priors
#Then run model on the post-election survey, using priors drawn from the pre-election survey

jags_reg.preBin <- jags.model(file = "JAGS Models/bin_cor.bugs", data = jags_data.preBin, n.chains = 3)
jags_out.preBin <- coda.samples(model = jags_reg.preBin, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 50000)

jags_out.preBin <- coda.samples(model = jags_reg.preBin, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 50000, thin = 100)

plot(jags_out.preBin) #not good
autocorr.plot(jags_out.preBin) #autocorrelation is 1.0?????
gelman.plot(jags_out.preBin)

