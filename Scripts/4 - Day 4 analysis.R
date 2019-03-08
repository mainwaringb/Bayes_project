rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

#JAGS models are now defined in external files

#Quick function to save space by repeating the code to make a JAGS object for given variables
#Could extend this by allowing variable specifications in the argument
makeJagsData <- function(df){
    x <- with(df, cbind(1, Fav_AfD, q55d_recode))
    y <- with(df, voteAfD_2017)
    jagsData <- list(y = y, x = x,
                           N = nrow(x),
                           J = ncol(x))
    return(jagsData)
}

##===1. Data prep===

preSurvey <- read.spss("Data/Pre-Election - Processed.sav", to.data.frame = TRUE)
postSurvey <- read.spss("Data/Post-Election - Processed.sav", to.data.frame = TRUE)

#Recode dependent variable
preSurvey$voteAfD_2017 <- as.numeric(preSurvey$voteAfD_2017) - 1


##===2. Binomial + listwise missing data===

#--Set up model--

#Define data
preSurvey.valid_allvars <- subset(preSurvey, !is.na(voteAfD_2017) & !is.na(Fav_AfD) & !is.na(q55d_recode))
jags_data.bin1 <- with(preSurvey.valid_allvars, list(y = as.numeric(voteAfD_2017), x1 = as.numeric(Fav_AfD), x2 = as.numeric(q55d_recode),
                      N = length(voteAfD_2017)))

#Compile model
jags_reg.bin1 <- jags.model(file = "JAGS Models/bin1.bugs", data = jags_data.bin1,
                           n.chains = 3)

#--Initial run--

#Check initial run
jags_out.bin1 <- coda.samples(model = jags_reg.bin1, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 500)
plot(jags_out.bin1) 
autocorr.plot(jags_out.bin1) 

#--Updated run--

jags_out.bin1 <- coda.samples(model = jags_reg.bin1, variable.names = c("Beta0", "Beta1", "Beta2"), n.iter = 50000, thin = 50)
plot(jags_out.bin1) 
gelman.plot(jags_out.bin1) 
autocorr.plot(jags_out.bin1) 

summary(jags_out.bin1)

jags_out_df.bin1 <- data.frame(Beta0 = unlist(jags_out.bin1[,"Beta0"]), Beta1 = unlist(jags_out.bin1[,"Beta1"]), Beta2 = unlist(jags_out.bin1[,"Beta2"]))
cor(jags_out_df.bin1$Beta1, jags_out_df.bin1$Beta2) #Beta1 and Beta2 are negatively correlated
summary(jags_out_df.bin1$Beta1 - jags_out_df.bin1$Beta2) 


##===2. Binomial with basic uniform imputation===

#Here we'll just do a basic uniform imputation (although this isn't a model we'd actually want to use)
#Could also try a normal (althouh this raises problems with bounds) or look at other distributons that are bounded
#Another possibiltiy is to take a draw from the multiplication of two PDFs: a uniform X a normal

#--Set up model--
PreSurvey.valid_dv <- subset(preSurvey, !is.na(voteAfD_2017))
jags_data.bin2 <- makeJagsData(df = PreSurvey.valid_dv)

jags_reg.bin2 <- jags.model(file = "JAGS Models/bin2.bugs", data = jags_data.bin2, n.chains = 3)

#--Run and examine model--

#Let's run a big model and see what happens!
jags_out.bin2 <- coda.samples(model = jags_reg.bin2, variable.names = "beta", n.iter = 25000, thin = 25)
summary(jags_out.bin2) #time-series SE is substantially bigger (~50%) indicating continued problem with autocorrelation
par(mfrow = c(2,3))
autocorr.plot(jags_out.bin2, auto.layout = FALSE, ask = FALSE) #autocorrelation still pretty severe
plot(jags_out.bin2) #chains look mixed
gelman.plot(jags_out.bin2)
#Since I'm using a slow computer, I'll just accept the autocorrelation for now


##===3. Binomial with actual imputation on one variable ===

PreSurvey.valid_twovar <- subset(preSurvey, !is.na(voteAfD_2017) & !is.na(q55d_recode))
jags_data.bin3 <- makeJagsData(df = PreSurvey.valid_twovar)

jags_reg.bin3 <- jags.model(file = "JAGS Models/bin3.bugs", data = jags_data.bin3, n.chains = 3)

#Since I'm not that interested in single-variable, I'm going to push forward now that the model compiles - rather than look substantively at the model

##===4. Binomial with actual imputation on more than one IV ===

#I get a message about a "directed cycle"
#I think this means I need to somehow tell JAGS to use a previous estimate of x2 for imputing x1
#Not sure how to do this

jags_data.bin4 <- jags_data.bin2
jags_data.bin4$missing <- is.na(jags_data.bin2$x)

jags_reg.bin4 <- jags.model(file = "JAGS Models/bin4.bugs", data = jags_data.bin4, n.chains = 3)

