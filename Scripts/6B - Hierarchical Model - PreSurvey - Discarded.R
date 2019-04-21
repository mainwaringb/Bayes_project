rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

source("Scripts/6A - Hierararchical Model - Prep.R")
load("Data/JAGS Output - pre-full.RData")


####---2.1. Null model (individual-level predictors only); quasi-informed priors---####

#2.1.1 Define data and priors

#Define data 
jags_data.pre_null <- makeJagsData(df = preSurvey.valid,  ivs = c("education_1", "education_2", "gender", "age", "age_sq", "hh_unemp", "income"), dv = "Fav_AfD", addConstant = FALSE, J = TRUE)

#Add index of group (wahlkreis) to data, and use Q to count the number of wahlkreis
jags_data.pre_null$group_index <- as.numeric(factor(preSurvey.valid$wahlkreis_num))
jags_data.pre_null$Q <- max(jags_data.pre_null$group_index)

#Define priors for beta, via a matrix
#Priors: we expect low education and male gender to be positively associated with AfD support
#I am fairly certain about the effect of gender and education on AfD voting, so we can set a fairly narrow prior on this
jags_data.pre_null$B_mean <- c(1.0, 0.5, 1.0, 0.0, 0.0, 1.0, -0.2)
jags_data.pre_null$B_var <- c(0.2, 0.2, 0.2, 5.0, 5.0, 0.2, 1.0)

#2.1.2 Run and examine model

jags_reg.pre_null <- jags.model(file = "JAGS Models/h_lin.null.bugs", data = jags_data.pre_null, n.chains = 5)
update(jags_reg.pre_null, 45000) #The relationship between age and age_sq requires a long burn-in period
jags_out.pre_null <- coda.samples(model = jags_reg.pre_null, variable.names = "beta", n.iter = 500)

#For publication, will need to manipulate layout on these plots
#png(...)
#par(mfrow = c(5,2))
#plot(jags_out.pre_null, auto.layout = FALSE) #Two plots X five betas X five chains
#dev.off()
plot(jags_out.pre_null) #Age and Age-squared variables are not mixed at all
gelman.plot(jags_out.pre_null) #Gelamn plot confirms no mixture for beta[4] and beta[5]
autocorr.plot(jags_out.pre_null, ask = FALSE) #massive autocorrelation in beta[4] and beta[5]

jags_out.pre_null <- coda.samples(model = jags_reg.pre_null, variable.names = "beta", n.iter = 25000, thin = 25)
plot(jags_out.pre_null) #Looks reasonable
gelman.plot(jags_out.pre_null) #The Gelman plot here suggests reasonable shrinkage around something like 40k iterations
autocorr.plot(jags_out.pre_null, ask = FALSE) #Serious autocorrelation prolems in beta4 and beta5, but - given computational resources - there's not much more we can do
summary(jags_out.pre_null)

#Beta1 and Beta2 are slightly larger than the prior (1.3 and 0.8)
#While Beta3 is a little bit smaller than the prior
#Unemployed people are *less* supportive of AfD, while the wealthy are also less supportive
#Should plot marginal effects of age


####---2.2. Full model (individual + group-level predictors); quasi-informed priors, specified independently---####

#2.2.1 Define data and priors
#Create initial data object, for individual-level data
jags_data.pre_full <- makeJagsData(df = preSurvey.valid,  ivs = c("education_1", "education_2", "gender", "age", "age_sq", "hh_unemp", "income"), dv = "Fav_AfD", addConstant = FALSE, J = TRUE)

#Rename "x" vector to specify l0 (individual-level) predictor variables, and add l1 (group-level) predictor variables
jags_data.pre_full$x_l0 <- jags_data.pre_full$x
jags_data.pre_full$x <- NULL
jags_data.pre_full$x_l1 <- kreiseData[kreiseData$WahlkreisNr %in% preSurvey.valid$wahlkreis_num,
                                      c("MigrantBackground_Yes_Pct",
                                        "GDP_PerCap_1000", "Educ_Fachhoch_Pct", "Unemp_Tot", "Age_60plus")]
jags_data.pre_full$K <- length(jags_data.pre_full$x_l1) #Get number of L1 variables

#Create an index of groups, ranging from 1 to Q
jags_data.pre_full$group_index <- as.numeric(factor(preSurvey.valid$wahlkreis_num))
jags_data.pre_full$Q <- max(jags_data.pre_full$group_index)

#Define semi-informative priors for L0 variables
jags_data.pre_full$B_mean <- c(1.0, 0.5, 1.0, 0.0, 0.0, 1.0, -0.1)
jags_data.pre_full$B_var <-  c(0.2, 0.2, 0.2, 5.0, 5.0, 0.2, 1.0)
#For L1 variables - assume that the interquartile range has a 1-pt effect on AfD favourability, and set standard deviation at 2.5x mean
jags_data.pre_full$G_mean <- c(0, -0.1, -0.1, 0.25, 0.2)
jags_data.pre_full$G_var <-  1 / (abs(c(-.05, -0.1, -0.1, 0.25, 0.2) * 2.5) ^ 2)


jags_data.pre_miss$G_var <-  1 / (abs(c(.2, -.1, -0.1, 0.25, .2) * 2.5) ^ 2)

#2.2.2 Run and examine model

jags_reg.pre_full <- jags.model(file = "JAGS Models/h_lin.ind.bugs", data = jags_data.pre_full, n.chains = 5)
update(jags_reg.pre_full, 50000) #long burn-in period seems necessary for gammas to mix (in addition to problem with age_sq encountered above)

#Need to figure out how to put a trace on alphas, but not plot it
jags_out.pre_full <- coda.samples(model = jags_reg.pre_full, variable.names = c("beta", "gamma"), n.iter = 50000, thin = 10)
plot(jags_out.pre_full) #beta4 and beta5 look moderately well-mixed, but might still be trending
gelman.plot(jags_out.pre_full) #the gammas have a shrink factor issue; #shrink factor for beta4 and beta5 drops by 40000;
autocorr.plot(jags_out.pre_full) #autocorrelation is a severe issue for age

summary(jags_out.pre_full)
#Gamma2,  Gamma3, and Gamma5 have credibility intervals that lie entirely outside zero
#Migrant background, GDP per capita, and unemployment
#More diverse constituency have lower favourability towards AfD
#Wealthier constituencies, *and* those with higher unemployment rates, are more favourable to AfD

save(jags_out.pre_full, file = "Data/JAGS Output - pre-full.RData")


####---2.3. Full model (individual + group-level predictors); uninformed priors, specified independently---####

#2.3.1 Define data and priors
jags_data.pre_flat <- jags_data.pre_full

jags_data.pre_flat$B_mean <- c(1.0, 0.5, 1.0, 0.0, 0.0, 1.0, -0.1)
jags_data.pre_flat$B_var <- c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001)
jags_data.pre_flat$G_mean <- c(0, -0.1, -0.1, 0.25, 0.2)
jags_data.pre_flat$G_var <- c(0.001, 0.001, 0.001, 0.001, .001)

#2.3.2 Run and examine model

jags_reg.pre_flat <- jags.model(file = "JAGS Models/h_lin.ind.bugs", data = jags_data.pre_flat, n.chains = 5)
update(jags_reg.pre_flat, 50000) #long burn-in period seems necessary for gammas to mix (in addition to problem with age_sq encountered above)

#Need to figure out how to put a trace on alphas, but not plot it
jags_out.pre_flat <- coda.samples(model = jags_reg.pre_flat, variable.names = c("beta", "gamma"), n.iter = 50000, thin = 10)
plot(jags_out.pre_flat) 
gelman.plot(jags_out.pre_flat)
autocorr.plot(jags_out.pre_flat) 

summary(jags_out.pre_flat)

save(jags_out.pre_flat, file = "Data/JAGS Output - pre-flat.RData")
