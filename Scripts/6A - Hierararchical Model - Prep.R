rm(list = ls())
library(foreign)
library(rjags)

preSurvey <- read.spss("Data/Pre-Election - Processed.sav", to.data.frame = TRUE)
postSurvey <- read.spss("Data/Post-Election - Processed.sav", to.data.frame = TRUE)
source("Scripts/makeJagsData.R")


####---1.1 Import Kreise Data---###

#Import kreise data
kreiseData <- read.table("Data/btw17_strukturdaten.txt", 
                         sep = ";", skip = 8, dec = ",", fileEncoding = "WINDOWS-1258",
                         stringsAsFactors = FALSE,  header = TRUE, na.strings = ".")
kreiseDataLabels <- read.csv("Data/Label translations.csv", stringsAsFactors = FALSE) #Read in short English-language variable labels and substitute for the long German labels
names(kreiseData)[-1] <- kreiseDataLabels$EnglishShort
kreiseData$GDP_PerCap_1000 <- kreiseData$GDP_PerCap / 1000 #Rescale for interpretability
kreiseData$Age_60plus <- kreiseData$Age_60_74_Pct + kreiseData$Age_75Plus_Pct

#Inspect potentially useful variables
summary(kreiseData$Pop_Foreigners_Pct)
summary(kreiseData$MigrantBackground_Yes_Pct)
summary(kreiseData$GDP_PerCap_1000)
summary(kreiseData$Educ_Fachhoch_Pct) 
summary(kreiseData$Unemp_Tot)
summary(kreiseData$Age_60plus)

#Check correlations between selected variables
with(kreiseData, cor(cbind(GDP_PerCap_1000, Educ_Fachhoch_Pct, Unemp_Tot, Pop_Foreigners_Pct, MigrantBackground_Yes_Pct), use = "pairwise.complete.obs"))


####---1.2. Merge Kreise Data---####

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


####---1.3. Process GESIS data---####

#Convert factor variables to numeric
preSurvey.merged$education_1 <- as.numeric(preSurvey.merged$education_1) - min(as.numeric(preSurvey.merged$education_1))
preSurvey.merged$education_2 <- as.numeric(preSurvey.merged$education_2) - min(as.numeric(preSurvey.merged$education_2))
preSurvey.merged$education_3 <- as.numeric(preSurvey.merged$education_3) - min(as.numeric(preSurvey.merged$education_3))
preSurvey.merged$gender <- as.numeric(preSurvey.merged$gender) - min(as.numeric(preSurvey.merged$gender))

postSurvey.merged$education_1 <- as.numeric(postSurvey.merged$education_1) - min(as.numeric(postSurvey.merged$education_1))
postSurvey.merged$education_2 <- as.numeric(postSurvey.merged$education_2) - min(as.numeric(postSurvey.merged$education_2))
postSurvey.merged$education_3 <- as.numeric(postSurvey.merged$education_3) - min(as.numeric(postSurvey.merged$education_3))
postSurvey.merged$gender <- as.numeric(postSurvey.merged$gender) - min(as.numeric(postSurvey.merged$gender))

#Create squared age variable
preSurvey.merged$age_sq <- preSurvey.merged$age ^ 2
postSurvey.merged$age_sq <- postSurvey.merged$age ^ 2

#Define data
#I have four cases with missing data for age, and much more substantial missing data for income
validcases.pre <- !is.na(preSurvey.merged$age) & !is.na(preSurvey.merged$income)
preSurvey.valid <- preSurvey.merged[validcases.pre,]
validcases.post <- !is.na(postSurvey.merged$age)  & !is.na(postSurvey.merged$income)
postSurvey.valid <- postSurvey.merged[validcases.post,]



