rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

preSurvey <- read.spss("Data/Pre-Election - Processed.sav", to.data.frame = TRUE)
postSurvey <- read.spss("Data/Post-Election - Processed.sav", to.data.frame = TRUE)
source("Scripts/makeJagsData.R")


##===1. Define data===

#Recoding
preSurvey$voteAfD_2017 <- as.numeric(preSurvey$voteAfD_2017) - 1
postSurvey$voteAfD_2017 <- as.numeric(postSurvey$voteAfD_2017) - 1

#Define data
validcases.preBin <- !is.na(preSurvey$Fav_AfD) & !is.na(preSurvey$q55d_recode)
preSurvey.validBin <- preSurvey[validcases.preBin,]
validcases.postBin <- !is.na(postSurvey$Fav_AfD) & !is.na(postSurvey$q74d_recode)
postSurvey.validBin <- postSurvey[validcases.postBin,]

#Define models
jags_data.preBin <- makeJagsData(df = preSurvey.validBin , ivs = c("Fav_AfD", "q55d_recode"), dv = "voteAfD_2017", addConstant = FALSE)
jags_data.postBin <- makeJagsData(df = postSurvey.validBin, ivs = c("Fav_AfD", "q74d_recode"), dv = "voteAfD_2017", addConstant = TRUE, J = FALSE)
jags_data.postBin_cor <- makeJagsData(df = postSurvey.validBin, ivs = c("Fav_AfD", "q74d_recode"), dv = "voteAfD_2017", addConstant = TRUE, J = FALSE)
jags_data.postBin_blank <- makeJagsData(df = postSurvey.validBin, ivs = c("Fav_AfD", "q74d_recode"), dv = "voteAfD_2017", addConstant = TRUE, J = FALSE)

load("5 - Binomial Regression.RData")

##===2. Pre-Election survey (binomial, independent priors)===

#Define priors for beta, via a matrix
jags_data.preBin$B_mean <- c(0.0, 0.0, 0.0)
jags_data.preBin$B_var <- diag(3) * c(0.1, 0.1, 0.1)

#--Initial run--

jags_reg.preBin <- jags.model(file = "JAGS Models/bin.cc.bugs", data = jags_data.preBin,
                              n.chains = 3)
jags_out.preBin <-  coda.samples(model = jags_reg.preBin, variable.names = c("Beta0", "Beta[1]", "Beta[2]"), n.iter = 5000)

plot(jags_out.preBin) #looks generally converged, but a bit lumpy
gelman.plot(jags_out.preBin) #seems good after about 3000 iterations
autocorr.plot(jags_out.preBin) #strong autocorrelation

#--Final run--

update(jags_reg.preBin, 3000)
jags_out.preBin <-  coda.samples(model = jags_reg.preBin, variable.names = c("Beta0", "Beta[1]", "Beta[2]"), n.iter = 75000, thin = 50)

plot(jags_out.preBin) #still slightly lumpy
gelman.plot(jags_out.preBin) #good
autocorr.plot(jags_out.preBin, ask = FALSE) #some first-lag autocorrelation, but not too bad

#--Review posterior--

jags_summary.preBin <- summary(jags_out.preBin)
jags_summary.preBin #B0: [-10.14, -7.35], B1: [0.83, 1.14], B2: [-0.08, 0.24]

jags_dfout.preBin <-  do.call("rbind", jags_out.preBin)
var(jags_dfout.preBin)


##===3. Post-Election survey (binomial, allow correlated priors but set initial covariance to zero)===

#--Define priors--

jags_data.postBin$B_mean <- jags_summary.preBin$statistics[,"Mean"]
jags_data.postBin$B_var <- diag(3) * (1 / ((jags_summary.preBin$statistics[,"SD"] ^ 2) * 1.5))

#--Initial run--

jags_reg.postBin <- jags.model(file = "JAGS Models/bin_cor.bugs", data = jags_data.postBin, n.chains = 3)
jags_out.postBin <- coda.samples(model = jags_reg.postBin, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 5000)

plot(jags_out.postBin) #B0 and B1 don't seem well-mixed
gelman.plot(jags_out.postBin) #issues with 1st 1500 iterations only
autocorr.plot(jags_out.postBin, ask = FALSE) #beta2 is stable, but series autocorrelation in b0 and b1

#--Final run--

jags_out.postBin <- coda.samples(model = jags_reg.postBin, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 100000, thin = 10)

plot(jags_out.postBin) #much better mixed
autocorr.plot(jags_out.postBin) #Still very substantial autocorrelation
gelman.plot(jags_out.postBin) #looks good

jags_summary.postBin <- summary(jags_out.postBin)
jags_summary.postBin$statistics[,"Naive SE"] / jags_summary.postBin$statistics[,"Time-series SE"] #on b0 and b1, time-series SE is 4x greater than naive SE

#--Review and compare--
jags_summary.postBin #B0: [-9.1, -7.4], B1: [0.9, 1.1], B2: [-0.1, 0.1]
jags_summary.preBin #B0: [-10.1, -7.3], B1: [0.8,1.1], B3: [-0.1, 0.25]


##===4. Post-Election survey (binomial, flat/uniform priors for comparison but allowing for correlation)===

#--Define priors--

jags_data.postBin_blank$B_mean <- c(0, 0, 0)
jags_data.postBin_blank$B_var <- diag(3) * c(0.01, 0.01, 0.01)

#--Initial run--

jags_reg.postBin_blank <- jags.model(file = "JAGS Models/bin_cor.bugs", data = jags_data.postBin_blank, n.chains = 3)
jags_out.postBin_blank <- coda.samples(model = jags_reg.postBin_blank, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 5000)

plot(jags_out.postBin_blank) #Not well-mixed at all

#--Next run--
jags_out.postBin_blank <- coda.samples(model = jags_reg.postBin_blank, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 75000)

plot(jags_out.postBin_blank) #This does not work

##===5. Post-Election survey (binomial, correlated priors)===

#--Define priors--

jags_data.postBin_cor$B_mean <- jags_summary.preBin$statistics[,"Mean"]
jags_data.postBin_cor$B_var <- 1 /  (var(jags_dfout.preBin) * 1.5) #The precision matrix isn't positive definite, so we need to use the nearPD function to generat an approximate one
PDMatrix <- Matrix::nearPD(1 /(var(jags_dfout.preBin) * 1.5), keepDiag = TRUE, maxit = 50000)$mat
jags_data.postBin_cor$B_var <- as.matrix(PDMatrix)

#--Initial run--

jags_reg.postBin_cor <- jags.model(file = "JAGS Models/bin_cor.bugs", data = jags_data.postBin_cor, n.chains = 3) #Error: invalid parent values
jags_out.postBin_cor <- coda.samples(model = jags_reg.postBin_cor, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 5000)

plot(jags_out.postBin_cor) #B0 and B1 don't seem well-mixed, plus time trends
gelman.plot(jags_out.postBin_cor) #problematic until at least 5000 iterations, maybe more
par(mfrow = c(3,2))
autocorr.plot(jags_out.postBin_cor, ask = FALSE) #beta2 is stable, but series autocorrelation in b0 and b1

#--Subsequent run--

jags_out.postBin_cor <- coda.samples(model = jags_reg.postBin_cor, variable.names = c("beta[1]", "beta[2]", "beta[3]"), n.iter = 100000, thin = 10)

plot(jags_out.postBin_cor) #Even after 100k iterations, we still appear to have non-converging markov chain
gelman.plot(jags_out.postBin_cor) #Problematic
autocorr.plot(jags_out.postBin_cor)

save.image("5 - Binomial Regression.RData")


