rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

preSurvey <- read.spss("Data/Pre-Election - Processed.sav", to.data.frame = TRUE)
postSurvey <- read.spss("Data/Post-Election - Processed.sav", to.data.frame = TRUE)
source("Scripts/makeJagsData.R")


#####====1. DATA CLEANING AND PREP====####

##---1.1 Import Kreise Data---

#Import kreise data
kreiseData <- read.table("Data/btw17_strukturdaten.txt", 
                   sep = ";", skip = 8, dec = ",", fileEncoding = "WINDOWS-1258",
                   stringsAsFactors = FALSE,  header = TRUE, na.strings = ".")
kreiseDataLabels <- read.csv("Data/Label translations.csv", stringsAsFactors = FALSE) #Read in short English-language variable labels and substitute for the long German labels
names(kreiseData)[-1] <- kreiseDataLabels$EnglishShort
kreiseData$GDP_PerCap_1000 <- kreiseData$GDP_PerCap / 1000 #Rescale for interpretability

#Inspect potentially useful variables
summary(kreiseData$Pop_Foreigners_Pct)
summary(kreiseData$MigrantBackground_Yes_Pct)
summary(kreiseData$GDP_PerCap_1000)
summary(kreiseData$Educ_Fachhoch_Pct) 
summary(kreiseData$Unemp_Tot)

#Check correlations between selected variables
with(kreiseData, cor(cbind(GDP_PerCap_1000, Educ_Fachhoch_Pct, Unemp_Tot, Pop_Foreigners_Pct, MigrantBackground_Yes_Pct), use = "pairwise.complete.obs"))


##---1.2. Merge Kreise Data---

#The structure of JAGS means that we don't actually use the merged data - however, we do need it for LMER, and a few miscellaneous purposes

#The GESIS data includes a factor variable with both name and numer in one variable
#Convert to a numeric
preSurvey$wahlkreis_num <- as.numeric(preSurvey$wahlkreis)
postSurvey$wahlkreis_num <- as.numeric(postSurvey$wahlkreis)

#Merge pre- and post-surveys
preSurvey.merged <- merge(x = preSurvey, y = kreiseData, by.x = "wahlkreis_num" , by.y = "WahlkreisNr",
      all.x = TRUE, all.y = FALSE)
postSurvey.merged <- merge(x = postSurvey, y = kreiseData, by.x = "wahlkreis_num" , by.y = "WahlkreisNr",
                          all.x = TRUE, all.y = FALSE)

##---1.3. Process GESIS data---

#Define data
#I have four cases with missing data for age, and much more substantial missing data for income
validcases.pre <- !is.na(preSurvey.merged$age) & !is.na(preSurvey.merged$income)
preSurvey.valid <- preSurvey.merged[validcases.pre,]
validcases.post <- !is.na(postSurvey.merged$age)  & !is.na(postSurvey.merged$income)
postSurvey.valid <- postSurvey.merged[validcases.post,]

#Convert factor variables to numeric
preSurvey.valid$education_1 <- as.numeric(preSurvey.valid$education_1) - min(as.numeric(preSurvey.valid$education_1))
preSurvey.valid$education_2 <- as.numeric(preSurvey.valid$education_2) - min(as.numeric(preSurvey.valid$education_2))
preSurvey.valid$education_3 <- as.numeric(preSurvey.valid$education_3) - min(as.numeric(preSurvey.valid$education_3))
preSurvey.valid$gender <- as.numeric(preSurvey.valid$gender) - min(as.numeric(preSurvey.valid$gender))

postSurvey.valid$education_1 <- as.numeric(postSurvey.valid$education_1) - min(as.numeric(postSurvey.valid$education_1))
postSurvey.valid$education_2 <- as.numeric(postSurvey.valid$education_2) - min(as.numeric(postSurvey.valid$education_2))
postSurvey.valid$education_3 <- as.numeric(postSurvey.valid$education_3) - min(as.numeric(postSurvey.valid$education_3))
postSurvey.valid$gender <- as.numeric(postSurvey.valid$gender) - min(as.numeric(postSurvey.valid$gender))

#Create squared age variable
preSurvey.valid$age_sq <- preSurvey.valid$age ^ 2
postSurvey.valid$age_sq <- postSurvey.valid$age ^ 2


#####====2. GENERATE PRE-ELECTION MODELS====####

##---2.1. Null model (individual-level predictors only); quasi-informed priors---

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
    jags_data.pre_null$B_var <- c(0.2, 0.2, 0.2, 0.05, 0.01, 0.2, 1)
    
#2.1.2 Run and examine model
        
    jags_reg.pre_null <- jags.model(file = "JAGS Models/h_lin.null.bugs", data = jags_data.pre_null, n.chains = 5)
    update(jags_reg.pre_null, 35000) #The relationship between age and age_sq requires a long burn-in period
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
    gelman.plot(jags_out.pre_null) #The Gelman plot here suggests reasonable shrinkage around something like 25-35k iterations
    autocorr.plot(jags_out.pre_null) #Serious autocorrelation prolems in beta4 and beta5, but - given computational resources - there's not much more we can do
    summary(jags_out.pre_null)
    
    #Beta1 and Beta2 are slightly larger than the prior (1.5 and 0.9)
    #While Beta3 is a little bit smaller than the prior
    #Should plot marginal effects of age
    

##---2.2. Full model (individual + group-level predictors); quasi-informed priors---
    
#2.2.1 Define data and priors
    #Create initial data object, for individual-level data
    jags_data.pre_full <- makeJagsData(df = preSurvey.valid,  ivs = c("education_1", "education_2", "gender", "age", "age_sq", "hh_unemp", "income"), dv = "Fav_AfD", addConstant = FALSE, J = TRUE)
    
    #Rename "x" vector to specify l0 (individual-level) predictor variables, and add l1 (group-level) predictor variables
    jags_data.pre_full$x_l0 <- jags_data.pre_full$x
    jags_data.pre_full$x <- NULL
    jags_data.pre_full$x_l1 <- kreiseData[kreiseData$WahlkreisNr %in% preSurvey.valid$wahlkreis_num,
                       c("Pop_Foreigners_Pct", "MigrantBackground_Yes_Pct",
                         "GDP_PerCap_1000", "Educ_Fachhoch_Pct", "Unemp_Tot")]
    jags_data.pre_full$K <- length(jags_data.pre_full$x_l1) #Get number of L1 variables

    #Create an index of groups, ranging from 1 to Q
    jags_data.pre_full$group_index <- as.numeric(factor(preSurvey.valid$wahlkreis_num))
    jags_data.pre_full$Q <- max(jags_data.pre_null$group_index)
    
    #Define semi-informative priors for L0 variables
    jags_data.pre_full$B_mean <- c(1.0, 0.5, 1.0, 0.0, 0.0, 1.0, -0.2)
    jags_data.pre_full$B_var <- c(0.2, 0.2, 0.2, 0.05, 0.01, 0.2, 1)
    #For L1 variables - assume that the interquartile range has a 1-pt effect on AfD favourability, and set standard deviation at 2.5x mean
    jags_data.pre_full$G_mean <- c(-.125, -.05, -.1, -0.1, 0.25)
    jags_data.pre_full$G_var <-  1 / (abs(c(-.125, -.05, -.1, -0.1, 0.25) * 2.5) ^ 2)
    
#2.2.2 Run and examine model
        
    jags_reg.pre_full <- jags.model(file = "JAGS Models/h_lin.full.bugs", data = jags_data.pre_full, n.chains = 5)
    update(jags_reg.pre_full, 35000) #long burn-in period seems necessary for gammas to mix (in addition to problem with age_sq encountered above)
    
    #Need to figure out how to put a trace on alphas, but not plot it
    jags_out.pre_full <- coda.samples(model = jags_reg.pre_full, variable.names = c("beta", "gamma"), n.iter = 25000, thin = 10)
    plot(jags_out.pre_full) #beta4 and beta5 look moderately well-mixed, but might still be trending
    gelman.plot(jags_out.pre_full) #the gammas have a shrink factor issue; #shrink factor for beta4 and beta5 drops by 40000;
    autocorr.plot(jags_out.pre_full) #autocorrelation is a severe issue for age
    
    summary(jags_out.pre_full)
    #Gamma2,  Gamma3, and Gamma5 have credibility intervals that lie entirely outside zero
    #Migrant background, GDP per capita, and unemployment
    #More diverse constituency have lower favourability towards AfD
    #Wealthier constituencies, *and* those with higher unemployment rates, are more favourable to AfD

##---2.3. Full model (individual + group-level predictors); uninformed priors---

#2.3.1 Define data and priors
    jags_data.pre_flat <- jags_data.pre_full
    
    jags_data.pre_flat$B_mean <- c(1.0, 0.5, 1.0, 0.0, 0.0)
    jags_data.pre_flat$B_var <- c(0.001, 0.001, 0.001, 0.001, 0.001)
    jags_data.pre_flat$G_mean <- c(-.125, -.05, -.1, -0.1, 0.25)
    jags_data.pre_flat$G_var <- c(0.001, 0.001, 0.001, 0.001, 0.001)

#2.3.2 Run and examine model

jags_reg.pre_flat <- jags.model(file = "JAGS Models/h_lin.full.bugs", data = jags_data.pre_flat, n.chains = 5)
update(jags_reg.pre_flat, 35000) #long burn-in period seems necessary for gammas to mix (in addition to problem with age_sq encountered above)

#Need to figure out how to put a trace on alphas, but not plot it
jags_out.pre_flat <- coda.samples(model = jags_reg.pre_flat, variable.names = c("beta", "gamma"), n.iter = 25000, thin = 10)
plot(jags_out.pre_flat) 
gelman.plot(jags_out.pre_flat) 
autocorr.plot(jags_out.pre_flat) 

summary(jags_out.pre_flat)
