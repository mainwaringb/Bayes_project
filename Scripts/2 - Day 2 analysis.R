rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

preSurvey <- read.spss("Data/Pre-Election - Processed.sav")
postSurvey <- read.spss("Data/Post-Election - Processed.sav")

##===Binomial distribution===


##===Binomial distribution===

#For the binomial analysis, we'll look at how different (non-data based) priors affect our estimate of the share of our population frame that voted AfD
#The substantive insight here is that the kind of people who answer our survey are not perectly representative, so the share who voted AfD isn't just the population vote total - it's an uknown quantity

table(postSurvey$voteAfD_2017)

#Binomial distribution will take beta as a prior
#Beta has parameters a and b (alpha and beta)
    # a = 1 and b = is a flat prior
    # a > b produces right-skewed
    #a, b <1 produces high dispersion

#---Uniform prior---

#Note that the data must specify a and b, as well as y and N
#Also note the funny format for specifying data

a <- 1
b <- 1

voteAfD_2017.list <- list(y = sum(postSurvey$voteAfD_2017 == "Yes", na.rm = TRUE), N = sum(!is.na(postSurvey$voteAfD_2017)),
                          a = a, b = b)


#Specify the function for our model
#JAGS uses R-like syntax, with parameter for the y (likelihood function) and p (prior)
#Note that we aren't actually running a model here, just defining its distributions
flat.modelSpec <- "
    model{
        y ~ dbin(p, N)
        p ~ dbeta(a,b)
    }"

flat.mcmc <- jags.model(file = textConnection(flat.modelSpec), data = )
