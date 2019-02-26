rm(list = ls())
library(foreign)
library(rstudioapi)
library(rjags)

setwd(dirname(dirname(getActiveDocumentContext()$path)))

mydf <- read.spss("Data/Post-Election - Processed.sav")
