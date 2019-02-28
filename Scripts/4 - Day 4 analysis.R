rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

##===2. Data prep===

preSurvey <- read.spss("Data/Pre-Election - Processed.sav", to.data.frame = TRUE)
postSurvey <- read.spss("Data/Post-Election - Processed.sav", to.data.frame = TRUE)

#Recode dependent variable
preSurvey$voteAfD_2017 <- as.numeric(preSurvey$voteAfD_2017) - 1


##===2. binomial + Listwise missing data===

#--Define modek and data--

#Define model - for simplicity, we'll just define priors inside this loop
jags_model.bin1 <- "model{
    for(i in 1:N){
        ystar[i] = Beta0 + Beta1 * x1[i] + Beta2 * x2[i]
        logit(p[i]) = ystar[i]
        y[i] ~ dbern(p[i])
    }

    Beta0 ~ dnorm(0, 0.01)
    Beta1 ~ dnorm(0, 0.01)
    Beta2 ~ dnorm(0, 0.01)

}"
write(jags_model.bin1, "jags_model.bin1.bugs")

#Define data
isValid.1 <- with(preSurvey, !is.na(voteAfD_2017) & !is.na(Fav_AfD) & !is.na(q55d_recode))
preSurvey.valid1 <- preSurvey[isValid.1,]
#preSurvey.valid <- preSurvey.valid[1:45,]
jags_data.bin1 <- list(y = as.numeric(preSurvey.valid1$voteAfD_2017), x1 = preSurvey.valid1$Fav_AfD, x2 = preSurvey.valid1$q55d_recode,
                      N = nrow(preSurvey.valid1))

#Compile model
jags_reg.bin1 <- jags.model(file = "jags_model.bin1.bugs", data = jags_data.bin1,
                           n.chains = 3)

#--Initial run--

#Check initial run
jags_out.bin1 <- coda.samples(model = jags_reg.bin1, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 500)

plot(jags_out.bin1) 
#Beta0 doesn't appear converged, at least until late in the chain, around iter 1500
#Beta1 also takes some time, Beta2 looks a little better, but samples are lumpy

autocorr.plot(jags_out.bin1) #Autocorrelation looks managable in Beta2 by 25 iterations, but problematic in Beta0 and Beta1

#--Updated run--

jags_out.bin1 <- coda.samples(model = jags_reg.bin1, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 50000, thin = 50)
plot(jags_data.bin1) #The distribution is still a bit lumpy, although chains seem better-mixed

jags_out.bin1 <- coda.samples(model = jags_reg.bin1, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 50000, thin = 50)
plot(jags_out.bin1) #Lumpiness is still there, but at least we're better mixed
gelman.plot(jags_out.bin1) #Gelman plot also suggests chains are mixed
autocorr.plot(jags_out.bin1) #Still appear to have some autocorrelation at the first few lags

summary(jags_out.bin1) #time-series SE is about 20% larger on Beta1, but not a huge problem - we can probably settle for this, given that further thinning would take a while to run
#Beta1 has a strong positive effect; controllign for that, the probability of an effect for Beta2 isn't notably greater than 0

jags_out_df.bin1 <- data.frame(Beta0 = unlist(jags_out.bin1[,"Beta0"]), Beta1 = unlist(jags_out.bin1[,"Beta1"]), Beta2 = unlist(jags_out.bin1[,"Beta2"]))
cor(jags_out_df.bin1$Beta1, jags_out_df.bin1$Beta2) #Beta1 and Beta2 are negatively correlated
summary(jags_out_df.bin1$Beta1 - jags_out_df.bin1$Beta2) 
quantile(jags_out_df.bin1$Beta1 - jags_out_df.bin1$Beta2, probs = c(.025, .975)) #95% credibility interval for the difference between Beta1 and Beta2 shows Beta1 is clearly bigger
hist(jags_out_df.bin1$Beta1 - jags_out_df.bin1$Beta2)


##===2. Binomial with basic uniform imputation===

#Here we'll just do a basic uniform imputation (although this isn't a model we'd actually want to use)
#Could also try a normal (althouh this raises problems with bounds) or look at other distributons that are bounded
#Another possibiltiy is to take a draw from the multiplication of two PDFs: a uniform X a normal

#--Define model--

#Try specifying star based on matrix multiplication, rather than individually defining a sum of betas
jags_model.bin2 <- "model{
    for(n in 1:N){
        ystar[n] = inprod(x[n,],beta[])
        logit(p[n]) = ystar[n]
        y[n] ~ dbern(p[n])

        #Need to manually specify imputation, since the variables have different bounds
        #alternatively I could create a vector that lists upper and lower bounds, and loop through this
        x[n,2] ~ dunif(1,11)
        x[n,3] ~ dunif(1,7)
    }

    #If I'm using flat priors, I can just set them all here
    for(j in 1:J){
        beta[j] ~ dnorm(0, 0.1)
    }

}"
write(jags_model.bin2, "jags_model.bin2.bugs")

#--Define data, in a slightly more sophisticated way--

isValid.2 <- with(preSurvey, !is.na(voteAfD_2017))
preSurvey.valid2 <- preSurvey[isValid.2,]

x <- cbind(1, preSurvey.valid2$Fav_AfD, preSurvey.valid2$q55d_recode)
y <- preSurvey.valid2$voteAfD_2017

jags_data.bin2 <- list(y = y,
                      x = x,
                      N = nrow(x),
                      J = ncol(x))

#--Run and examine model--

jags_reg.bin2 <- jags.model(file = "jags_model.bin2.bugs", data = jags_data.bin2, n.chains = 3)
jags_out.bin2 <- coda.samples(model = jags_reg.bin2, variable.names = "beta", n.iter = 500)
plot(jags_out.bin2) #Unsurprisingly, this is definitely not converged
autocorr.plot(jags_out.bin2) #Autocorrelation also unsurprisingly an issue

#Let's run a big model and see what happens!
jags_out.bin2 <- coda.samples(model = jags_reg.bin2, variable.names = "beta", n.iter = 25000, thin = 25)
summary(jags_out.bin2) #time-series SE is substantially bigger (~50%) indicating continued problem with autocorrelation
par(mfrow = c(2,3))
autocorr.plot(jags_out.bin2, auto.layout = FALSE, ask = FALSE) #autocorrelation still pretty severe
plot(jags_out.bin2) #chains look mixed
gelman.plot(jags_out.bin2)
#Since I'm using a slow computer, I'll just accept the autocorrelation for now


##===3. Binomial with actual imputation on one variable ===

jags_model.bin3 <- "model{
    for(n in 1:N){
        ystar[n] = inprod(x[n,],beta[])
        logit(p[n]) = ystar[n]
        y[n] ~ dbern(p[n])
        
        #manually impute missing x's for only a single variable, x2 = fav_afd
        x[n, 2] ~ dnorm(mu[n], tau)
        mu[n] = gamma[1] + gamma[2] * x[n, 3]
    }
    
    for(j in 1:J){
        beta[j] ~ dnorm(0, 0.01)
    }

    gamma[1] ~ dnorm(0, 0.01)
    gamma[2] ~ dnorm(0, 0.01)
    tau ~ dgamma(0.001, 0.001)
}"
write(jags_model.bin3, "jags_model.bin3.bugs")

isValid.3 <- with(preSurvey, !is.na(voteAfD_2017) & !is.na(q55d_recode))
preSurvey.valid3 <- preSurvey[isValid.3,]

x <- cbind(1, preSurvey.valid3$Fav_AfD, preSurvey.valid3$q55d_recode)
y <- preSurvey.valid3$voteAfD_2017

jags_data.bin2 <- list(y = y,
                       x = x,
                       N = nrow(x),
                       J = ncol(x))

jags_reg.bin3 <- jags.model(file = "jags_model.bin3.bugs", data = jags_data.bin2, n.chains = 3)


##===4. Binomial with actual imputation on more than one variable ===

#I get a message about a "directed cycle"
#I think this means I need to somehow tell JAGS to use a previous estimate of x2 for imputing x1
#Not sure how to do this

jags_model.bin4 <- "model{
    for(n in 1:N){
        ystar[n] = inprod(x[n,],beta[])
        logit(p[n]) = ystar[n]
        y[n] ~ dbern(p[n])
        
        #manually impute missing x's for only a single variable, x2 = fav_afd
        x[n, 2] ~ dnorm(muA[n], tauA)
        muA[n] = gammaA[1] + gammaA[2] * x[n, 3]
        x[n, 3] ~ dnorm(muB[n], tauB)
        muB[n] = gammaB[1] + gammaB[2] * x[n, 2]
    }
    
    for(j in 1:J){
        beta[j] ~ dnorm(0, 0.01)
    }
    
    gammaA[1] ~ dnorm(0, 0.01)
    gammaA[2] ~ dnorm(0, 0.01)
    gammaB[1] ~ dnorm(0, 0.01)
    gammaB[2] ~ dnorm(0, 0.01)

    tauA ~ dgamma(0.001, 0.001)
    tauB ~ dgamma(0.001, 0.001)
}"
write(jags_model.bin4, "jags_model.bin4.bugs")

jags_reg.bin3 <- jags.model(file = "jags_model.bin4.bugs", data = jags_data.bin2, n.chains = 3)

