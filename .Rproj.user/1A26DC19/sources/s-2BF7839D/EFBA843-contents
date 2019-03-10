rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)
library(Matrix) #for creating positive-definite matrices

setwd(dirname(dirname(getActiveDocumentContext()$path)))

preSurvey <- read.spss("Data/Pre-Election - Processed.sav", to.data.frame = TRUE)
postSurvey <- read.spss("Data/Post-Election - Processed.sav", to.data.frame = TRUE)
source("Scripts/makeJagsData.R")
load("3 - Linear Regression.RData")

##===1. Define data===

#Recoding
preSurvey$voteAfD_2017 <- as.numeric(preSurvey$voteAfD_2017) - 1
postSurvey$voteAfD_2017 <- as.numeric(postSurvey$voteAfD_2017) - 1

#Define data
validcases.pre <- !is.na(preSurvey$leftright_self) & !is.na(preSurvey$q55d_recode)
preSurvey.valid <- preSurvey[validcases.pre,]
validcases.post <- !is.na(postSurvey$leftright_self) & !is.na(postSurvey$q74d_recode)
postSurvey.valid <- postSurvey[validcases.post,]

jags_data.preLin <- makeJagsData(df = preSurvey.valid,  ivs = c("leftright_self", "q55d_recode"), dv = "Fav_AfD", addConstant = TRUE, J = FALSE)
jags_data.postInd <- makeJagsData(df = postSurvey.valid,  ivs = c("leftright_self", "q74d_recode"), dv = "Fav_AfD", addConstant = TRUE, J = FALSE) #Note that q55 in the presurvey = q74 in the postsurvey
jags_data.postCor <- makeJagsData(df = postSurvey.valid,  ivs = c("leftright_self", "q74d_recode"), dv = "Fav_AfD", addConstant = TRUE, J = FALSE)
jags_data.postBlank <- makeJagsData(df = postSurvey.valid,  ivs = c("leftright_self", "q74d_recode"), dv = "Fav_AfD", addConstant = TRUE, J = FALSE)

##===2. Pre-Election survey (linear)===

#We'll run a regression on the pre-election survey, setting an uninformative prior
#Then use this to set the prior for post-election

#Define priors for beta, via a matrix
jags_data.preLin$B_mean <- c(2.5, 0.0, 0.0)
jags_data.preLin$B_var <- diag(3) * c(0.2, 0.2, 0.2)

jags_reg.preLin <- jags.model(file = "JAGS Models/lin_cor.bugs", data = jags_data.preLin, n.chains = 5)
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


##===3. Post-Election survey (linear, independent priors)===

#The assumption here is that there is no covariance between B0, B1, and B2

#--Define priors for beta, via a matrix--
jags_data.postInd$B_mean <- jags_summary.preLin$statistics[,"Mean"]
jags_data.postInd$B_var <- diag(3) * (1 / (jags_summary.preLin$statistics[,"SD"] ^ 2) * 1.5)

#--Initial run--
jags_reg.postInd <- jags.model(file = "JAGS Models/lin_cor.bugs", data = jags_data.postInd, n.chains = 5)
jags_out.postInd <- coda.samples(model = jags_reg.postInd, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 5000)

plot(jags_out.postInd)
autocorr.plot(jags_out.postInd, ask = FALSE) #as expected, continuing autocorrelation; 25 lags look like it should solve things, but we can be conservative
gelman.plot(jags_out.postInd) #shrunken after about 1000 iterations

#--Subsequent run--
jags_out.postInd <- coda.samples(model = jags_reg.postInd, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 50000, thin = 20)

plot(jags_out.postInd) #iterations seem pretty well-mixed, but there's a weird lumpy bit in the B1 and B2 distributions
autocorr.plot(jags_out.postInd, ask = FALSE) #autocorrelation looks okay
gelman.plot(jags_out.postInd) #shrinkage factor is good

jags_summary.postInd <- summary(jags_out.postInd)
jags_summary.postInd
jags_summary.preLin

#the size of Beta1 differs drastically, from mean .28 in the pre to mean .42 in the post
#this seems to warrant further investigation


##===4. Post-Election survey (linear, non-informative priors)===

#--Define uninformative ("blank") priors
jags_data.postBlank$B_mean <- c(0.0, 0.0, 0.0)
jags_data.postBlank$B_var <- diag(3) * c(0.01, 0.01, 0.01)

#--Initial run--
jags_reg.postBlank <- jags.model(file = "JAGS Models/lin_cor.bugs", data = jags_data.postBlank, n.chains = 5)
jags_out.postBlank <- coda.samples(model = jags_reg.postBlank, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 5000)

plot(jags_out.postInd)
autocorr.plot(jags_out.postInd, ask = FALSE) #looks good
gelman.plot(jags_out.postInd) #looks good

jags_summary.postBlank <- summary(jags_out.postBlank)

##===5. Post-Election survey (linear, correlated priors)===

#--Define data and model--

jags_data.postCor$B_mean <- jags_summary.preLin$statistics[,"Mean"]
#jags_data.postCor$B_var <- 1 /  (var(jags_dfout.preLin) * 1.5) #The precision matrix isn't positive definite, so we need to use the nearPD function to generat an approximate one
PDMatrix <- Matrix::nearPD(1 /(var(jags_dfout.preLin) * 1.5), keepDiag = TRUE, maxit = 50000)$mat
jags_data.postCor$B_var <- as.matrix(PDMatrix)


# #--Inspect intiial run--

jags_reg.postCor <- jags.model(file = "JAGS Models/lin_cor.bugs", data = jags_data.postCor, n.chains = 5)
jags_out.postCor <- coda.samples(model = jags_reg.postCor, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 5000)

plot(jags_out.postCor) #Convergence looks good, but Beta3 has a slightly lumpy distribution
gelman.plot(jags_out.postInd) #Shrinkage looks good after about 2000 iterations
autocorr.plot(jags_out.postInd, ask = FALSE) #Autocorrelation looks good

# #--Final run--
summary(jags_out.postCor) #B0: [-.40, +.26], B1: [.38, .48], B2: [.14, .27]
summary(jags_out.postInd) #B0: [-.24, +.04], B1: [.38, .46], B2: [.16, -.6]
summary(jags_out.postBlank) #B0: [-.14, +.72], B1: [.47, .61], B2: [-.29, -.05]
summary(jags_out.preLin) 

#What's clear but slightly worrying here is that the pre-election survey and post-election survey show very different relationships
#This is surprising
#We see that the estimate of post-election survey using informative priors are "tilted" towards these priors - the result is a compromise between the prior and observed


save.image("3 - Linear Regression.RData")
