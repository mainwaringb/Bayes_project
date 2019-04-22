rm(list = ls())
library(foreign)
library(rjags)
library(MCMCvis)

source("Scripts/6A - Hierarchical Model - Prep.R")
source("Scripts/MakeJagsData.R")
load("Data/Covariance + Mean - pre-full imp.RData")
load("Data/JAGS Output - post-full imp.RData")
load("Data/JAGS Output - pre-full imp.RData")


#Because these models are so time-consuming, set a flag to determine whether the model should actually be re-run
runmodel <- FALSE

burnin <- 35000
iterations <- 10000000
thin <- 125

#Full model with missing data (individual + group-level predictors); specified via multivariate normal
####---3.1 Define data and model specs ---####

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


####---3.2 Run model---####

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


plot(jags_out.post_miss) #mixing on gamma4 and gamma5(age still isn't great)
autocorr.plot(jags_out.post_miss) #grautocorrelation is still a massive issue - endures at 3500+ lags for age, and a few hundred for gammas
gelman.plot(jags_out.post_miss) #shrink factor appears to settle down after around 100k-150k iterations on gammas


####---3.3 Inspect model in more depth---###

summ.post_miss <- summary(jags_out.post_miss)
summ.post_miss$quantiles
summ.post_miss$statistics

#Inspect autocorrelation plots for potentially problematic variables
par(mfrow = c(1,2))
autocorr.plot(jags_out.post_miss[[1]][,c(4:5)], auto.layout = FALSE, main = "Autocorrelation of B[age] and B[age ^ 2]")
par(mfrow = c(3,2))
autocorr.plot(jags_out.post_miss[[1]][,c(8:13)], auto.layout = FALSE)

#Compare very well-behaved Markov chain with slightly-less-well-behaved one
plot(jags_out.post_miss[,c(1,5)], trace = TRUE, density = FALSE)
plot(jags_out.post_miss[,5], trace = TRUE, density = FALSE, main = "Trace of B[age ^ 2]")
plot(jags_out.post_miss[,1], trace = TRUE, density = FALSE, main = "Trace of B[educ.low]")


####---3.4 Visualise and discuss results---####

#visualise posterior vs prior for Gamma[migrant.yes]
#use MCMCtrace function, since it enables the plotting of priors against posteriors - but we first need to draw from the distribution of priors
prior_draws <- rnorm(15000, mean =  summ.pre_miss$statistics[8,1], sd = sqrt(cov.pre_miss[8,8] * 1.5))
MCMCtrace(jags_out.post_miss, params = "G\\[migrant.yes\\]", type = "density", priors = prior_draws,
          main_den = "Prior and Posterior Density for G[migrant.yes]", lty_pr = "dashed", xlim = c(-0.06, 0.06), PPO_out = FALSE,
          ISB = FALSE, pdf = FALSE)

jags_out.pre_miss

#visualise all gammas
par(mfrow = c(1,2))
MCMCplot(jags_out.post_miss[,c(8:13)], ref_ovl = TRUE, xlim = c(-.20, .20), main = "Post-Election", sz_ax = 1.5) 
MCMCplot(jags_out.pre_miss[,c(8:13)], ref_ovl = TRUE, xlim = c(-.20, .20), main = "Pre-Election", sz_ax = 1.5) 

#visualize all betas
#This doesn't really work because parameters are on really different scales
par(mfrow = c(1,2))
MCMCplot(jags_out.post_miss[,c(1:7)], ref_ovl = TRUE, main = "Post-Election", sz_ax = 1.5) 
MCMCplot(jags_out.pre_miss[,c(1:7)], ref_ovl = TRUE, main = "Pre-Election", sz_ax = 1.5) 

beta.compare <- cbind(summ.post_miss$statistics[c(1:7),c(1:2)], summ.pre_miss$statistics[c(1:7),c(1:2)])
colnames(beta.compare) <- c("Post-Election Mean", "Post-Election SD", "Pre-Election Mean", "Pre-Election SD")


####---3.5 Explore age in both pre- and post-surves---####

#---visualise effect of age, at mean beta---
age_seq <- seq(from = 18, to = 90)
mfx.age.post <- data.frame(age = age_seq, pred = age_seq * summ.post_miss$statistics[4,1] + (age_seq ^ 2) * summ.post_miss$statistics[5,1])
mfx.age.pre <- data.frame(age = age_seq, pred = age_seq * summ.pre_miss$statistics[4,1] + (age_seq ^ 2) * summ.pre_miss$statistics[5,1])

par(mfrow = c(1,1))
plot(mfx.age.post, type = "l", main = "Effect of age on AfD Favourability (Post-Election)", xlab = "Age", ylab = "AfD Favourability")
plot(mfx.age.pre, type = "l", main = "Effect of age on AfD Favourability (Pre-Election)", xlab = "Age", ylab = "AfD Favourability")

#---visualise uncertainty about the effect of age---
jags_dfout.post_miss <-  do.call("rbind", jags_out.post_miss)
jags_dfout.pre_miss <-  do.call("rbind", jags_out.pre_miss)

#For each of 8000 iterations * 3 markov chains, generate the predicted value at each age
mfx_per_iter.age.post <- apply(jags_dfout.post_miss[,c(4,5)], 
      1, function(x){
          mfx_out <- (age_seq * x[1]) + (age_seq ^ 2 * x[2])
          return(mfx_out)
      })
mfx_per_iter.age.pre <- apply(jags_dfout.pre_miss[,c(4,5)], 
       1, function(x){
           mfx_out <- (age_seq * x[1]) + (age_seq ^ 2 * x[2])
           return(mfx_out)
       })

#For each iteration, find the age at which AfD support peaks
#Since our age sequence starts at 18 (the first element), add 17 to calculate age
peak_per_iter.age.post <- apply(mfx_per_iter.age.post, 2, which.max) + 17
peak_per_iter.age.pre <- apply(mfx_per_iter.age.pre, 2, which.max) + 17

#Calculate the age of peak AfD support
which.max(mfx.age.pre$pred) + 17
which.max(mfx.age.post$pred) + 17

#plot the distribution of peak ages
hist(peak_per_iter.age.post, freq = FALSE, breaks = 22,
     main = "Age of Maximum AfD Support (Post-Election)", xlim = c(18,40))
hist(peak_per_iter.age.pre, freq = FALSE, breaks = 22,
     main = "Age of Maximum AfD Support (Pre-Election)", xlim = c(18,40))
summary(peak_per_iter.age.post)
summary(peak_per_iter.age.pre)

#Compute kernel density estimates
peak_density.age.post <- density(peak_per_iter.age.post, bw = 1.5)
peak_density.age.pre <- density(peak_per_iter.age.pre, bw = 1.5)
plot(peak_density.age.post, xlim = c(18,40), main = "Distribution of Age of Peak AfD support")
lines(peak_density.age.pre, lty = "dashed", col = "red")
legend(x = "topleft", legend = c("Post-Election", "Pre-Election"),
       lty = c("solid", "dashed"), col = c("black", "red"))



