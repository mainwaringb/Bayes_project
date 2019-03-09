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

#---Uniform prior---

#Specify variables used in both the observed (y, distributed on P and N) and observed (p, distriubted on )
#Note that the data must specify a and b, as well as y and N
#Also note the funny format for specifying data

#From help file: "any numeric objects in DATA corresponding to node arrays used in FILE ar taken to represent the values of observed nodes in the model"
#I think this is why we can tag definiitons of a and b onto this datafile (say, defining an R variable "a" and "b" and then listing "a = a" and "b = b" to define the paramters of the beta distribution)
jagsdata  <- list(y = sum(postSurvey$voteAfD_2017 == "Yes", na.rm = TRUE), N = sum(!is.na(postSurvey$voteAfD_2017)))

#Specify the function for our model
#JAGS uses R-like syntax, with parameters for the y (likelihood function) and p (prior)
#Note that we aren't actually running a model here, just defining its distributions
modelSpec.flat <- "
model{
        y ~ dbin(p,N)
        p ~ dbeta(1,1)
}"

#Create a model object from the given model specification and data 
flat.mcmc <- jags.model(file = textConnection(modelSpec.flat), data = jagsdata ,
                        n.chain = 3)
#Note that the resulting object is a list of *functions*:
#An object of class jags is a list of functions that share a common environment. This environment encapsulates the state of the model, and the functions can be used to query or modify the model state.
flat.mcmc$data()
flat.mcmc$model()


#Update the model, replacing iterations in the burn-in phase
update(flat.mcmc, 500)

#Draw a sample distribution from the Markov Chain monte carlo draws
draw.flat <- coda.samples(flat.mcmc, variable.names = c("p"),
                          n.iter = 5000, thin = 1)
#coda.samples is a wrapper for jags.samples which produces a "mcmc.list" class with methods for plot, summary, etc
#but we can run without the wrapper as well
draw.flat2 <- jags.samples(flat.mcmc, variable.names = c("p"), 
                          n.iter = 5000, thin = 1)

#These two objects have different types
head(draw.flat)
str(draw.flat)  #list of three, one for each chain
draw.flat[[1]]
head(draw.flat2)
str(draw.flat2) #list of one (for each variable); the list element is a three-dimensional array with variables, chains, and cases; class "mcarray"
draw.flat2$p
draw.flat2$p[1,,]

#
#flat prior produces IQR of 9.1-10.1, and 95% credibility interval of 8.2 - 11.1
summary(draw.flat) #prints descriptives statistics, plus info on the chain
plot(draw.flat) #show trace and density plots


#---Informed prior---

#Binomial distribution will take beta as a prior
#Beta has parameters a and b (alpha and beta)
# a = 1 and b = is a flat prior
# a > b produces right-skewed
#a, b <1 produces high dispersion

#I don't really know how to pick the appropriate distribution - i could probably solve for a specific mean
#But for now I'll just make up distributiosn
pbeta(.15, 1.2, 7)
qbeta(.5, 1.2, 7)
#beta(a = 1.2, b = 7) has 60% of its mass below 15, and a mean of about .12 - this seems to work


modelSpec.informedBeta <- "
model{
y ~ dbin(p,N)
p ~ dbeta(1.2,7)
}"

informedBeta.mcmc <- jags.model(file = textConnection(modelSpec.informedBeta), data = jagsdata,
                                n.chains = 5)

#Try running descriptives before the jags/codasample command, just to experiment - it doens't work
plot(informedBeta.mcmc)
summary(informedBeta.mcmc)
#The first thing we notice is that this doesn't work, because the jags object ('.mcmc' here) doesn't store the posterior distribution - just the model specification

#Note that the "n.iter" parameter does additional draws here
#So really the "jags.model" command is what's doing the Markov Chain Monte Carlo
draw.informedBeta <- coda.samples(informedBeta.mcmc, variable.names = "p", n.iter = 20000)
plot(draw.informedBeta) #no pattern in the trace
summary(draw.informedBeta)

#Would be nice to get an easier-to-read version of the Markov chain plot
#Also interesting to plot the prior vs likelihood vs posterior in one plot - maybe there's a way?