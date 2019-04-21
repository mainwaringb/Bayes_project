rm(list = ls())
library(foreign)
library(rjags)
library(MCMCvis)

source("Scripts/6A - Hierarchical Model - Prep.R")
source("Scripts/MakeJagsData.R")
load("Data/Covariance + Mean - pre-full imp.RData")
load("Data/JAGS Output - post-full imp.RData")

#Because these models are so time-consuming, set a flag to determine whether the model should actually be re-run
runmodel <- FALSE


burnin <- 35000
iterations <- 10000000
thin <- 125


####---3.1. Full model with missing data (individual + group-level predictors); specified via multivariate normal---####

#---3.1.1 Define data and model specs ---
#Create initial data object, for individual-level data
jags_data.post_miss <- makeJagsData(df = postSurvey.merged,  ivs = c("education_1", "education_2", "gender", "age", "age_sq", "hh_unemp", "income"), dv = "Fav_AfD", addConstant = FALSE, J = FALSE)

#Rename "x" vector to specify l0 (individual-level) predictor variables, and add l1 (group-level) predictor variables
names(jags_data.post_miss)[2] <- "x_l0"
jags_data.post_miss$x_l1 <- kreiseData[kreiseData$WahlkreisNr %in% postSurvey.merged$wahlkreis_num,
                                      c("MigrantBackground_Yes_Pct",
                                        "GDP_PerCap_1000", "Educ_UnivQualif_Pct", "Unemp_Tot", "Age_60plus", "voteAfD")]

#Create an index of groups, ranging from 1 to Q
jags_data.post_miss$group_index <- as.numeric(factor(postSurvey.merged$wahlkreis_num))

#Define imputtation matrix
jags_data.post_miss$imputematrix <- matrix(c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE),
                                          nrow = ncol(jags_data.post_miss$x_l0), ncol = ncol(jags_data.post_miss$x_l0), byrow = TRUE)
jags_data.post_miss$imputematrix[,c(5,6)] <- FALSE
diag(jags_data.post_miss$imputematrix) <- FALSE
jags_data.post_miss$missing <- is.na(jags_data.post_miss$x_l0)

#Define priors
jags_data.post_miss$BG_mean <-  summ.pre_miss$statistics[,1]
jags_data.post_miss$BG_var <- (cov.pre_miss * 1.5)
jags_data.post_miss$beta_index <- 1:7 #Specify which elements of the "betagamma" coefficient vector are betas (individual-level)
jags_data.post_miss$gamma_index <- 8:13 #Specify which elements of the "betagamma" coefficient vector are gammas (group-level)


#---2.4.2 Run model---

#This initial run was used for diangostics, but proved far too short
# if(runmodel == TRUE){
#     jags_reg.post_miss <- jags.model(file = "JAGS Models/h_lin_imp.cor.bugs", data = jags_data.post_miss, n.chains = 3)
#     update(jags_reg.post_miss, 20000)
#     jags_out.post_miss <- coda.samples(model = jags_reg.post_miss, variable.names = c("betagamma"), n.iter = 100000, thin = 25)
#     save(jags_out.post_miss, file = "Data/JAGS Output - post-full imp.RData")
# }
# 
# plot(jags_out.post_miss) #Severe autocorrelation, age and age_squared don't appear will-mixed, and gammas look consistently questionable
# autocorr.plot(jags_out.post_miss) #group-level variables have autocorrelations that drop after several hundred iterations, age remain high after several hundred
# gelman.plot(jags_out.post_miss) #gammas look mostly settled by around 30,000

if(runmodel == TRUE){
    jags_reg.post_miss <- jags.model(file = "JAGS Models/h_lin_imp.cor.bugs", data = jags_data.post_miss, n.chains = 3)
    update(jags_reg.post_miss, 35000)
    jags_out.post_miss <- coda.samples(model = jags_reg.post_miss, variable.names = c("betagamma"), n.iter = 1000000, thin = 125)
    
    coefnames <- c("B[educ.low]", "B[educ.med]", "B[gend.male]", "B[age]", "B[age ^ 2]", "B[unemp]", "B[income]",
                   "G[migrant.yes]", "G[income.high]", "G[educ.high]", "G[unemp]", "G[age.60+]", "G[vote.AfD]")
    colnames(jags_out.post_miss[[1]]) <- coefnames
    colnames(jags_out.post_miss[[2]]) <- coefnames
    colnames(jags_out.post_miss[[3]]) <- coefnames
    save(jags_out.post_miss, file = "Data/JAGS Output - post-full imp.RData")
}

#Rename columns


plot(jags_out.post_miss) #mixing on gamma4 and gamma5(age still isn't great)
autocorr.plot(jags_out.post_miss) #grautocorrelation is still a massive issue - endures at 3500+ lags for age, and a few hundred for gammas
gelman.plot(jags_out.post_miss) #shrink factor appears to settle down after around 100k-150k iterations on gammas


#---2.4.3 Inspect model in more depth---

summ.post_miss <- summary(jags_out.post_miss)
summ.post_miss$quantiles

#Inspect autocorrelation plots for potentially problematic variables
par(mfrow = c(1,2))
autocorr.plot(jags_out.post_miss[[1]][,c(4:5)], auto.layout = FALSE, main = "Autocorrelation of B[age] and B[age ^ 2]")
par(mfrow = c(3,2))
autocorr.plot(jags_out.post_miss[[1]][,c(8:13)], auto.layout = FALSE)

#Compare very well-behaved Markov chain with slightly-less-well-behaved one
plot(jags_out.post_miss[,c(1,5)], trace = TRUE, density = FALSE)

plot(jags_out.post_miss[,5], trace = TRUE, density = FALSE, main = "Trace of B[age ^ 2]")
plot(jags_out.post_miss[,1], trace = TRUE, density = FALSE, main = "Trace of B[educ.low]")



MCMCplot(jags_out.post_miss, ref_ovl = TRUE) #this works - but coefficients are on such different scales that the plot can be hard to read


