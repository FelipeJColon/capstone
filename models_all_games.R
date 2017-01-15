## ############################################################################
##
## DISCLAIMER: 
##
## This script has been developed for illustrative purposes only. 
## The script is provided without any warranty of any kind, either express or 
## implied. The entire risk arising out of the use or performance of the sample 
## script and documentation remains with you. In no event shall its
## author, or anyone else involved in the creation, production, or delivery of 
## the script be liable for any damages whatsoever (including, without 
## limitation, damages for loss of business profits, business interruption, 
## loss of business information, or other pecuniary loss) arising out of the use 
## of or inability to use the sample scripts or documentation, even if the 
## author has been advised of the possibility of such damages. 
##
## ############################################################################
##
## DESCRIPTION
## Analysis of all matches together
##
## Version 1: Initially created on 28 Nov 2016
##
## Dependencies: preprocess.R
##
## Written by: Felipe J Colón-González
## For any problems with this code, please contact f.colon@uea.ac.uk
## 
## ############################################################################

## -------------------------
## Set directory tree and 
## load data
## -------------------------

rm(list=ls())

require(MASS) 

mydir <- "H:/euro2016"


source(file.path(mydir, "preprocess.R")) # comment here

# ---------------------------
# Aggregate data by dateTime
# ---------------------------

eng2 <- dplyr::group_by(eng, dateTime)
eng2 <- eng2 %>% summarise(date=mean(date), hour=mean(hour),
		count=sum(count), dow=mean(dow), month=mean(month),
		bankHol=mean(bankHol), allKo=mean(allKo), 
		allOn=mean(allOn), pregame=mean(pregame), 
		postgame=mean(postgame))

fra2 <- dplyr::group_by(fra, dateTime)
fra2 <- fra2 %>% summarise(date=mean(date), hour=mean(hour),
		count=sum(count), dow=mean(dow), month=mean(month),
		bankHol=mean(bankHol), allKo=mean(allKo), 
		allOn=mean(allOn), pregame=mean(pregame), 
		postgame=mean(postgame))

nir2 <- dplyr::group_by(nir, dateTime)
nir2 <- nir2 %>% summarise(date=mean(date), hour=mean(hour),
		count=sum(count), dow=mean(dow), month=mean(month),
		bankHol=mean(bankHol), allKo=mean(allKo), 
		allOn=mean(allOn), pregame=mean(pregame), 
		postgame=mean(postgame))

roi2 <- dplyr::group_by(roi, dateTime)
roi2 <- roi2 %>% summarise(date=mean(date), hour=mean(hour),
                           count=sum(count), dow=mean(dow), month=mean(month),
                           bankHol=mean(bankHol), allKo=mean(allKo), 
                           allOn=mean(allOn), pregame=mean(pregame), 
                           postgame=mean(postgame))

wal2 <- dplyr::group_by(wal, dateTime)
wal2 <- wal2 %>% summarise(date=mean(date), hour=mean(hour),
		count=sum(count), dow=mean(dow), month=mean(month),
		bankHol=mean(bankHol), allKo=mean(allKo), 
		allOn=mean(allOn), pregame=mean(pregame), 
		postgame=mean(postgame))

# ---------------------------
# Fit models for all games
# ---------------------------

engAll <- glm.nb(count ~ factor(dow) + factor(hour) 
	+ factor(month) + date + factor(pregame) 
	+ factor(postgame) + factor(allOn), data=eng2)

fraAll <- glm.nb(count ~ factor(dow) + factor(hour) 
	+ factor(month) + date + factor(pregame) 
	+ factor(postgame) + factor(allOn), data=fra2)

nirAll <- glm.nb(count ~ factor(dow) + factor(hour) 
	+ factor(month) + date + factor(pregame) 
	+ factor(postgame) + factor(allOn), data=nir2)

roiAll <- glm.nb(count ~ factor(dow) + factor(hour) 
                 + factor(month) + date + factor(pregame) 
                 + factor(postgame) + factor(allOn), data=roi2)

walAll <- glm.nb(count ~ factor(dow) + factor(hour) 
	+ factor(month) + date + factor(pregame) 
	+ factor(postgame) + factor(allOn), data=wal2)

summary(engAll); summary(fraAll); summary(nirAll)
summary(roiAll); summary(walAll)

# Explained deviances
engR2L <- with(engAll, (null.deviance - deviance)/null.deviance)
fraR2L <- with(fraAll, (null.deviance - deviance)/null.deviance)
nirR2L <- with(nirAll, (null.deviance - deviance)/null.deviance)
roiR2L <- with(roiAll, (null.deviance - deviance)/null.deviance)
walR2L <- with(walAll, (null.deviance - deviance)/null.deviance)

# Filter 2016 data
eng3 <- dplyr::filter(eng, date > "2015-12-31")
fra3 <- dplyr::filter(fra, date > "2015-12-31")
nir3 <- dplyr::filter(nir, date > "2015-12-31")
roi3 <- dplyr::filter(roi, date > "2015-12-31")
wal3 <- dplyr::filter(wal, date > "2015-12-31")

# Fit models
eng16 <- glm.nb(count ~ factor(dow) + factor(hour) 
	+ date + factor(pregame) 
	+ factor(postgame) + factor(allOn), data=eng3)

fra16 <- glm.nb(count ~ factor(dow) + factor(hour) 
	+ factor(month) + date + factor(pregame) 
	+ factor(postgame) + factor(allOn), data=fra3)

nir16 <- glm.nb(count ~ factor(dow) + factor(hour) 
	+ date + factor(pregame) 
	+ factor(postgame) + factor(allOn), data=nir3)

roi16 <- glm.nb(count ~ factor(dow) + factor(hour) 
                + date + factor(pregame) 
                + factor(postgame) + factor(allOn), data=roi3)

wal16 <- glm.nb(count ~ factor(dow) + factor(hour) 
	+ factor(month) + date + factor(pregame) 
	+ factor(postgame) + factor(allOn), data=wal3)

summary(eng16); summary(fra16); summary(nir16)
summary(roi16); summary(wal16)



## -------------------------
## End of file
## -------------------------

