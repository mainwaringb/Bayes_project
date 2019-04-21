rm(list = ls())
library(foreign)

preSurvey <- read.spss("Data/Pre-Election - Processed.sav", to.data.frame = TRUE)
postSurvey <- read.spss("Data/Post-Election - Processed.sav", to.data.frame = TRUE)


####---1.1 Import Kreise Data---###

##--1.1.1---Import kreise demographic data
#This is a *relatively* easy import
kreiseData <- read.table("Data/btw17_strukturdaten.txt", 
                         sep = ";", skip = 8, dec = ",", fileEncoding = "WINDOWS-1258",
                         stringsAsFactors = FALSE,  header = TRUE, na.strings = ".")
kreiseDataLabels <- read.csv("Data/Label translations.csv", stringsAsFactors = FALSE) #Read in short English-language variable labels and substitute for the long German labels
names(kreiseData)[-1] <- kreiseDataLabels$EnglishShort
kreiseData$GDP_PerCap_1000 <- kreiseData$GDP_PerCap / 1000 #Rescale for interpretability
kreiseData$Age_60plus <- kreiseData$Age_60_74_Pct + kreiseData$Age_75Plus_Pct
kreiseData <- kreiseData[kreiseData$WahlkreisNr < 900,]

##--1.1.2---Import kreise voting data
#This is more difficult, since we have a CSV files that is essentially designed to look good in Excel but isn't very machine-readable
electionResults.names <- read.table("Data/btw17_kerg.csv", skip = 5, nrow = 3,
                                            sep = ";", dec = ",", fileEncoding = "UTF-8",
                                            stringsAsFactors = FALSE,  header = FALSE, na.strings = ".", fill = TRUE)
electionResults.data <- read.table("Data/btw17_kerg.csv", skip = 8, 
                                   sep = ";", dec = ",", fileEncoding = "UTF-8",
                                   stringsAsFactors = FALSE,  header = FALSE, na.strings = ".", fill = TRUE)
electionResults.data <- electionResults.data[,-192] #drop phantom column

#These are the names of the parties - each party takes four cells
#I am just hardcoding in my knowledge that the 1st two columns are "first vote" in the current and previous elections, and the second two columns as "second vote" in the current and previous elections
#A slightly more sophisticated approach would pull names the from the second and third rows of electionResults.names
electionResults.goodnames <- as.vector(sapply(electionResults.names[1, seq(from = 4, to = 188, by = 4)], function(x) paste0(x, c(".1.cur", ".1.prev", ".2.cur", ".2.prev"))))
names(electionResults.data)[4:191] <- electionResults.goodnames
names(electionResults.data)[1:3] <- c("Nr", "Gebiet", "gehört zu")

#Drop blank rows, and those that contain state totals
electionResults.data <- electionResults.data[electionResults.data$`gehört zu` != 99 & !is.na(electionResults.data$`gehört zu`),]
voteAfD <- data.frame(Nr = electionResults.data$Nr, voteAfD = electionResults.data$Wahlberechtigte.2.cur / electionResults.data$`Alternative für Deutschland.2.cur`)

####---1.2. Merge Kreise Data---####

##--1.2.---Merge kreise demographic and voting data
kreiseData <- merge(x = kreiseData, y = voteAfD, by.x = "WahlkreisNr", by.y = "Nr")

##--1.2.2---Merge kreise data with individual data

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


####---1.4. Clean up unneeded data---####
rm(electionResults.data, electionResults.names, electionResults.goodnames, voteAfD,
   kreiseDataLabels,
   validcases.pre, validcases.post,
   preSurvey, postSurvey)

