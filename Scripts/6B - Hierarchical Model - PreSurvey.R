rm(list = ls())
library(foreign)
library(rjags)
library(MCMCvis)

source("Scripts/6A - Hierarchical Model - Prep.R")
source("Scripts/MakeJagsData.R")
load("Data/JAGS Output - pre-full imp.RData")
load("Data/JAGS Output - pre-flat imp.RData")


#Because these models are so time-consuming, set a flag to determine whether the model should actually be re-run
runmodel <- FALSE

burnin.pre <- 10000
iterations.pre <- 50000
thin.pre <- 125


####---2.4. Full model with missing data (individual + group-level predictors); quasi-informed priors, specified independently---####

#---2.4.1 Define data and priors---
#Create initial data object, for individual-level data
jags_data.pre_miss <- makeJagsData(df = preSurvey.merged,  ivs = c("education_1", "education_2", "gender", "age", "age_sq", "hh_unemp", "income"), dv = "Fav_AfD", addConstant = FALSE, J = FALSE)

#Rename "x" vector to specify l0 (individual-level) predictor variables, and add l1 (group-level) predictor variables
names(jags_data.pre_miss)[2] <- "x_l0"
jags_data.pre_miss$x_l1 <- kreiseData[kreiseData$WahlkreisNr %in% preSurvey.merged$wahlkreis_num,
                                      c("MigrantBackground_Yes_Pct",
                                        "GDP_PerCap_1000", "Educ_UnivQualif_Pct", "Unemp_Tot", "Age_60plus", "voteAfD")]
#jags_data.pre_miss$K <- length(jags_data.pre_miss$x_l1) #Get number of L1 variables

#Create an index of groups, ranging from 1 to Q
jags_data.pre_miss$group_index <- as.numeric(factor(preSurvey.merged$wahlkreis_num))
#jags_data.pre_miss$Q <- max(jags_data.pre_miss$group_index)

#Define semi-informative priors for L0 variables
jags_data.pre_miss$B_mean <- c(1.0, 0.5, 1.0, 0.0, 0.0, 1.0, -0.1)
jags_data.pre_miss$B_var <-  c(0.2, 0.2, 0.2, 5.0, 5.0, 0.2, 1.0)

#For L1 variables - assume that the interquartile range has a 1-pt effect on AfD favourability, and set standard deviation at 2.5x mean
jags_data.pre_miss$G_mean <- c(0.0, -0.1, -0.1, 0.25, 0.2, 0.33)
jags_data.pre_miss$G_var <-  1 / (abs(c(0.2, -0.1, -0.1, 0.25, 0.2, 0.33) * 2.5) ^ 2)

#Define imputtation matrix
jags_data.pre_miss$imputematrix <- matrix(c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
                                        nrow = ncol(jags_data.pre_miss$x_l0), ncol = ncol(jags_data.pre_miss$x_l0), byrow = TRUE)
jags_data.pre_miss$imputematrix[,c(5,6)] <- FALSE
diag(jags_data.pre_miss$imputematrix) <- FALSE
jags_data.pre_miss$missing <- is.na(jags_data.pre_miss$x_l0)


#---2.4.2 Run model---

if(runmodel == TRUE){
    jags_reg.pre_miss <- jags.model(file = "JAGS Models/h_lin_imp.ind.bugs", data = jags_data.pre_miss, n.chains = 3)
    update(object = jags_reg.pre_miss, n.iter = burnin.pre)
    jags_out.pre_miss <- coda.samples(model = jags_reg.pre_miss, variable.names = c("beta", "gamma"), n.iter = iterations.pre, thin = thin.pre)
    
    coefnames <- c("B[educ.low]", "B[educ.med]", "B[gend.male]", "B[age]", "B[age ^ 2]", "B[unemp]", "B[income]",
                   "G[migrant.yes]", "G[income.high]", "G[educ.high]", "G[unemp]", "G[age.60+]", "G[vote.AfD]")
    colnames(jags_out.pre_miss[[1]]) <- coefnames
    colnames(jags_out.pre_miss[[2]]) <- coefnames
    colnames(jags_out.pre_miss[[3]]) <- coefnames
    save(jags_out.pre_miss, burnin.pre, iterations.pre, thin.pre, file = "Data/JAGS Output - pre-full imp.RData")
}

plot(jags_out.pre_miss)
autocorr.plot(jags_out.pre_miss) #this behaves pretty well, except for severely autocrrelated age terms
gelman.plot(jags_out.pre_miss)

#Inspect problems variables (age and age^2) in more depth
par(mfrow = c(2,2))
plot(jags_out.pre_miss[,4], trace = TRUE, density = FALSE, main = "Trace of beta[age]", auto.layout = FALSE)
plot(jags_out.pre_miss[,5], trace = TRUE, density = FALSE, main = "Trace of beta[age ^ 2]", auto.layout = FALSE)
autocorr.plot(jags_out.pre_miss[[1]][,4], main = "Autocorrelation of beta[age]", sub = "chain 1, thin = 25", auto.layout = FALSE)
autocorr.plot(jags_out.pre_miss[[1]][,5], main = "Autocorrelation of beta[age ^ 2]", sub = "chain 1, thin = 25", auto.layout = FALSE)

#Look at naive vs time-series standard errors for beta4 and beta5, vs other variables
summary.pre_miss <- summary(jags_out.pre_miss)
summary.pre_miss$statistics[c(4:5), 4] / summary.pre_miss$statistics[c(4:5), 3] #time-series SE is 4x greater
summary.pre_miss$statistics[-c(4:5), 4] / summary.pre_miss$statistics[-c(4:5), 3] #except for gamma5, time-series SE is 1-1.5x greater
#(gamma5 is age 60+) 


#---2.4.3 Inspect model---

summ.pre_miss <- summary(jags_out.pre_miss)
summ.pre_miss$quantiles

#Gamma[1] has a median of -.02, with a 95% credibility interval from -.042 to +.012
#The level-1 variables with credibility intervals outside zero are gamma[2] and gamma[5]
#GDP per capita is *positively* assocaited with AfD, as is older population

#On an indivudal level - Low- and medium-education are associated with AfD support: Low (1.02, 1.40); Medium (0.57, 0.85)
#As is male gender (0.46, 0.69).
#household income is negativley associated (-0.21, -0.15)
#Surprisingly, unemployment is negatively associated with AfD support (-1.13, -.51)
#The age variables both have credibility intervals that lie outside zero: age has a positive associated, and the square of age has a negative association

par(mfrow = c(1,1))
MCMCplot(jags_out.pre_miss, ref_ovl = TRUE) #this works - but coefficients are on such different scales that the plot can be hard to read
#in particular, it's hard to tell that gamma2, beta4, and beta5 have credibility intervals outside 0



#---2.4.4 Check covariance matrix---

#coda.samples stores each markov chain as a separate list - use do.call("rbind"...) to convert them to a single data frame
jags_dfout.pre_miss <-  do.call("rbind", jags_out.pre_miss)

#Calculate the correlation and covariance matrix (correlation for easy display, covariance to feed in to priors of a future model)
#And format nicely
corr.pre_miss <- cor(jags_dfout.pre_miss)
cov.pre_miss <- cov(jags_dfout.pre_miss)

#View correlations
corr.pre_miss

#strong correlations in betas: beta1 and beta2 (.56), beta4 and beta5 (-.98)
#to a lesser extent - beta1 and beta7 (.30), beta1 and gamma5 (-.15), beta4 and beta7 (-.21)
    #beta5 and gamma5 (.27), beta6 and beta7 (.19)
#Generally speaking, beta7 tends to have high correlations with other variables, and gamma5 has quite high correlations with betas
#Correlations amongst gammas are all massively high, many in the +-.4 range

#Save covariance matrix and parameter means - for future important into post-election models
save(cov.pre_miss, summ.pre_miss, file = "Data/Covariance + Mean - pre-full imp.RData")


####---2.5. Full model with missing data (individual + group-level predictors); flat priors, specified independently---####

#2.5.1 Copy intitial object, and alter priors
jags_data.pre_flat_miss <- jags_data.pre_miss

#Define flat priors for L0 variables
jags_data.pre_flat_miss$B_var <- c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001)
jags_data.pre_flat_miss$G_var <-  c(0.001, 0.001, 0.001, 0.001, .001, .001)

#2.5.2 Run and examine model

if(runmodel == TRUE){
    jags_reg.pre_flat_miss <- jags.model(file = "JAGS Models/h_lin_imp.ind.bugs", data = jags_data.pre_flat_miss, n.chains = 3)
    update(object = jags_reg.pre_flat_miss, n.iter = burnin.pre)
    jags_out.pre_flat_miss <- coda.samples(model = jags_reg.pre_flat_miss, variable.names = c("beta", "gamma"), n.iter = iterations.pre, thin = thin.pre)
    
    coefnames <- c("B[educ.low]", "B[educ.med]", "B[gend.male]", "B[age]", "B[age ^ 2]", "B[unemp]", "B[income]",
                   "G[migrant.yes]", "G[income.high]", "G[educ.high]", "G[unemp]", "G[age.60+]", "G[vote.AfD]")
    colnames(jags_out.pre_flat_miss[[1]]) <- coefnames
    colnames(jags_out.pre_flat_miss[[2]]) <- coefnames
    colnames(jags_out.pre_flat_miss[[3]]) <- coefnames

    save(jags_out.pre_flat_miss, thin.pre, iterations.pre, burnin.pre, file = "Data/JAGS Output - pre-flat imp.RData")
}

plot(jags_out.pre_flat_miss)
autocorr.plot(jags_out.pre_flat_miss) #beta4 and beta5 here are massively autocorrelated
gelman.plot(jags_out.pre_flat_miss) #Some of these don't seem to really settle in until about 30,000 iterations (especially, but not only, beta4 and beta5)

summ.pre_flat_miss <- summary(jags_out.pre_flat_miss)
rownames(summ.pre_flat_miss$quantiles) <- coefnames
rownames(summ.pre_flat_miss$statistics) <- coefnames
summ.pre_flat_miss


#2.5.3 Export covariance matrix and coefficient means (I don't think I'll be using it, but just in case)

jags_dfout.pre_flat_miss <-  do.call("rbind", jags_out.pre_flat_miss)
cov.pre_flat_miss <- cov(jags_dfout.pre_flat_miss)
save(cov.pre_flat_miss, summ.pre_flat_miss, file = "Data/Covariance + Mean - pre-flat imp.RData")


####---2.6 Compare flat and semi-informed priors---####

summ.pre_miss$statistics
summ.pre_flat_miss$statistics
data.frame(semi_inf.mean = summ.pre_miss$statistics[,1], flat.mean = summ.pre_flat_miss$statistics[,1])
#Means are very similar
data.frame(semi_inf.sd = summ.pre_miss$statistics[,2], flat.sd = summ.pre_flat_miss$statistics[,2])
#As are variances


summary(jags_out.pre_flat_miss)[[2]]
summary(jags_out.pre_miss)[[2]]


