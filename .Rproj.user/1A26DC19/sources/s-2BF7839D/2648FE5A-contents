rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))
source("Scripts/makeJagsData.R")

##===1. Data prep===

preSurvey <- read.spss("Data/Pre-Election - Processed.sav", to.data.frame = TRUE)
postSurvey <- read.spss("Data/Post-Election - Processed.sav", to.data.frame = TRUE)

#Recode dependent variable
preSurvey$voteAfD_2017 <- as.numeric(preSurvey$voteAfD_2017) - 1

#Define data
preSurvey.cc <- subset(preSurvey,  !is.na(Fav_AfD) & !is.na(q55d_recode))
PreSurvey.valid_onevar <- subset(preSurvey, !is.na(q55d_recode))

jags_data.cc <- makeJagsData(df = preSurvey.cc , ivs = c("Fav_AfD", "q55d_recode"), dv = "voteAfD_2017", addConstant = FALSE)
jags_data.imp_unif <- makeJagsData(df = preSurvey,  ivs = c("Fav_AfD", "q55d_recode"), dv = "voteAfD_2017", addConstant = FALSE)
jags_data.imp_onevar <- makeJagsData(df = PreSurvey.valid_onevar,  ivs = c("Fav_AfD", "q55d_recode"), dv = "voteAfD_2017", addConstant = TRUE)
jags_data.imp_multivar <- jags_data.imp_unif

##===2. Binomial + listwise missing data (complete cases on all variables - cc)===

#--Set up model--

#Compile model
jags_reg.cc <- jags.model(file = "JAGS Models/bin.cc.bugs", data = jags_data.cc,
                           n.chains = 3)

#Check initial run
jags_out.cc <- coda.samples(model = jags_reg.cc, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 500)
plot(jags_out.cc) 
autocorr.plot(jags_out.cc) 

#--Updated run--

jags_out.cc <- coda.samples(model = jags_reg.cc, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 50000, thin = 50)
plot(jags_out.cc) 
gelman.plot(jags_out.cc) 
autocorr.plot(jags_out.cc) 

summary(jags_out.cc)

jags_out_df.cc <- data.frame(Beta0 = unlist(jags_out.cc[,"Beta0"]), Beta1 = unlist(jags_out.cc[,"Beta1"]), Beta2 = unlist(jags_out.cc[,"Beta2"]))
cor(jags_out_df.cc$Beta1, jags_out_df.cc$Beta2) #Beta1 and Beta2 are negatively correlated
summary(jags_out_df.cc$Beta1 - jags_out_df.cc$Beta2) 


#Generate posterior correlation matrix
jags_dfout.preLin <-  do.call("rbind", jags_out.preLin)
cor(jags_dfout.preLin)


##===3. Binomial with basic uniform imputation===

#Here we'll just do a basic uniform imputation (although this isn't a model we'd actually want to use)
#Could also try a normal (althouh this raises problems with bounds) or look at other distributons that are bounded
#Another possibility might be to take a draw from the multiplication of two PDFs (a uniform X a normal) although I'm not sure how to implement that

#--Set up model--
jags_reg.imp_unif <- jags.model(file = "JAGS Models/bin.imp_unif.bugs", data = jags_data.imp_unif, n.chains = 3)

#--Run and examine model--

#Let's run a big model and see what happens!
jags_out.imp_unif <- coda.samples(model = jags_reg.imp_unif, variable.names = "beta", n.iter = 25000, thin = 25)
summary(jags_out.imp_unif) #time-series SE is substantially bigger (~50%) indicating continued problem with autocorrelation
par(mfrow = c(2,3))
autocorr.plot(jags_out.imp_unif, auto.layout = FALSE, ask = FALSE) #autocorrelation still pretty severe
plot(jags_out.imp_unif) #chains look mixed
gelman.plot(jags_out.imp_unif)
#Since I'm using a slow computer, I'll just accept the autocorrelation for now


##===4. Binomial with actual "serious" imputation on one independent variable ===

jags_reg.imp_onevar <- jags.model(file = "JAGS Models/bin.imp_onevar.bugs", data = jags_data.imp_onevar, n.chains = 3)
#Since I'm not that interested in single-variable, I'm going to push forward now that the model compiles - rather than look substantively at the model

##===5. Binomial with actual imputation on more than one IV ===

# jags_data.bin4 <- jags_data.bin2
# jags_data.bin4$x <- jags_data.bin2$x[,2:3]
# jags_data.bin4$J <- jags_data.bin2$J - 1
jags_data.imp_multivar$missing <- is.na(jags_data.imp_unif$x)
jags_data.imp_multivar$ZeroDiagonal <- 1 - diag(jags_data.imp_multivar$J)

jags_reg.bin4_alt <- jags.model(file = "JAGS Models/bin.imp_multivar.bugs", data = jags_data.imp_multivar, n.chains = 3)

