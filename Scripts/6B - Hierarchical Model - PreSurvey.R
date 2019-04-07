rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

source("Scripts/6A - Hierararchical Model - Prep.R")
load("Data/JAGS Output - pre-full.RData")




####---2.4. Full model with missing data (individual + group-level predictors); quasi-informed priors, specified independently---####

#2.4.1 Define data and priors
#Create initial data object, for individual-level data
jags_data.pre_miss <- makeJagsData(df = preSurvey.merged,  ivs = c("education_1", "education_2", "gender", "age", "age_sq", "hh_unemp", "income"), dv = "Fav_AfD", addConstant = FALSE, J = TRUE)


#Rename "x" vector to specify l0 (individual-level) predictor variables, and add l1 (group-level) predictor variables
jags_data.pre_miss$x_l0 <- jags_data.pre_miss$x
jags_data.pre_miss$x <- NULL
jags_data.pre_miss$x_l1 <- kreiseData[kreiseData$WahlkreisNr %in% preSurvey.valid$wahlkreis_num,
                                      c("MigrantBackground_Yes_Pct",
                                        "GDP_PerCap_1000", "Educ_Fachhoch_Pct", "Unemp_Tot", "Age_60plus")]
jags_data.pre_miss$K <- length(jags_data.pre_miss$x_l1) #Get number of L1 variables

#Create an index of groups, ranging from 1 to Q
jags_data.pre_miss$group_index <- as.numeric(factor(preSurvey.merged$wahlkreis_num))
jags_data.pre_miss$Q <- max(jags_data.pre_miss$group_index)

#Define semi-informative priors for L0 variables
jags_data.pre_miss$B_mean <- c(1.0, 0.5, 1.0, 0.0, 0.0, 1.0, -0.1)
jags_data.pre_miss$B_var <-  c(0.2, 0.2, 0.2, 5.0, 5.0, 0.2, 1.0)

#For L1 variables - assume that the interquartile range has a 1-pt effect on AfD favourability, and set standard deviation at 2.5x mean
jags_data.pre_miss$G_mean <- c(0.0, -0.1, -0.1, 0.25, 0.2)
jags_data.pre_miss$G_var <-  1 / (abs(c(0.2, -0.1, -0.1, 0.25, 0.2) * 2.5) ^ 2)

#Define imputtation matrix
jags_data.pre_miss$imputematrix <- matrix(c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
                                        nrow = ncol(jags_data.pre_miss$x_l0), ncol = ncol(jags_data.pre_miss$x_l0), byrow = TRUE)
jags_data.pre_miss$imputematrix[,c(5,6)] <- FALSE
diag(jags_data.pre_miss$imputematrix) <- FALSE
jags_data.pre_miss$missing <- is.na(jags_data.pre_miss$x_l0)
jags_data.pre_miss$imputelist <- c(1,2,3,6)


#2.4.2 Run and examine model

jags_reg.pre_miss <- jags.model(file = "JAGS Models/h_lin_imp.ind.bugs", data = jags_data.pre_miss, n.chains = 3)
update(jags_reg.pre_miss, 10000)
jags_out.pre_miss <- coda.samples(model = jags_reg.pre_miss, variable.names = c("beta", "gamma"), n.iter = 50000, thin = 25)

plot(jags_out.pre_miss)
autocorr.plot(jags_out.pre_miss) #this behaves pretty well, except for severely autocrrelated age terms
gelman.plot(jags_out.pre_miss)

summary(jags_out.pre_miss)
#INSPECT NOW

save(jags_out.pre_miss, file = "Data/JAGS Output - pre-full imp.RData")



####---2.5. Full model with missing data (individual + group-level predictors); flat priors, specified independently---####

#2.4.1 Copy intiial object, and alter priors
jags_data.pre_flat_miss <- jags_data.pre_miss

#Define semi-informative priors for L0 variables
jags_data.pre_flat_miss$B_var <- c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001)
jags_data.pre_flat_miss$G_var <-  c(0.001, 0.001, 0.001, 0.001, .001)

#2.4.2 Run and examine model

jags_reg.pre_flat_miss <- jags.model(file = "JAGS Models/h_lin_imp.ind.bugs", data = jags_data.pre_flat_miss, n.chains = 3)
update(jags_reg.pre_flat_miss, 10000)
jags_out.pre_flat_miss <- coda.samples(model = jags_reg.pre_flat_miss, variable.names = c("beta", "gamma"), n.iter = 10000, thin = 5)

plot(jags_out.pre_flat_miss)
autocorr.plot(jags_out.pre_flat_miss) #beta4 and beta5 here are massively autocorrelated
gelman.plot(jags_out.pre_flat_miss)

summary(jags_out.pre_flat_miss)
#INSPECT NOW

save(jags_out.pre_flat_miss, file = "Data/JAGS Output - pre-flat imp.RData")


