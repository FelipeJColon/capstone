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
## Pre-process data for word prediction
##
## Version 1
##
## Dependencies: capstoneFunctions.R and profanity.R
##
## Written by: Felipe J Colón-González
## For any problems with this code, please contact f.colon@uea.ac.uk
## 
## ############################################################################


# --------------------------------------------------
# Load packages for session
# --------------------------------------------------
require(ngram)
require(stringi)
require(ggplot2)
require(data.table)
require(RWeka)
require(xtable)
require(quanteda)
require(caTools)
require(dplyr)
require(tidyr)

# --------------------------------------------------
# Get data
# --------------------------------------------------
# Define directory tree
myDir <- file.path("~/Documents/COURSERA/CAPSTONE")

source(file.path(myDir, "capstoneFunctions.R"))
source(file.path(myDir, "profanity.R"))

# Define path to English dataset
newDir <- file.path(myDir, "final", "en_US")

# Read each of the sets
blogs <- readLines(file.path(newDir, "en_US.blogs.txt"), encoding="UTF-8",
                   skipNul=TRUE)
twits <- readLines(file.path(newDir, "en_US.twitter.txt"), encoding="UTF-8",
                   skipNul=TRUE)
news  <- readLines(file.path(newDir, "en_US.news.txt"), encoding="UTF-8",
                   skipNul=TRUE)

# ---------------------------------
# Clean dataset
# ---------------------------------

# Store data in a single object
# allData <- quanteda::corpus(c(blogs, twits, news))
allData <- c(blogs, twits, news)

# Create sample 
nDocs <- 1e5
set.seed(120478)
corpus <- base::sample(allData, size=nDocs, replace=FALSE)

# Remove input objects 
rm(blogs, twits, news)

# ------------------------------
# Generate n-grams
# ------------------------------
ngram1 <- tokenise(corpus, 1)
ngram2 <- tokenise(corpus, 2)
ngram3 <- tokenise(corpus, 3)
ngram4 <- tokenise(corpus, 4)

# Set ngrams as data_frames
ngram1 <- data_frame(newWord=ngram1)
ngram2 <- data_frame(newWord=ngram2)
ngram3 <- data_frame(newWord=ngram3)
ngram4 <- data_frame(newWord=ngram4)

# Get frequencies for each ngram and separate words
ngram1 <- frequence(ngram1)
ngram2 <- frequence(ngram2) %>%
       tidyr::separate(newWord, c("word1", "newWord"), " ") 
ngram3 <- frequence(ngram3) %>%
       separate(newWord, c("word1", "word2", "newWord"), " ")
ngram4 <- frequence(ngram4) %>%
       separate(newWord, c("word1", "word2", "word3", "newWord"), " ")

# Filter profanity
"%ni%" <- Negate("%in%")
ngram1 <- dplyr::filter(ngram1, newWord %ni% profanity)
ngram2 <- dplyr::filter(ngram2, word1 %ni% profanity & newWord %ni% profanity)
ngram3 <- dplyr::filter(ngram3, word1 %ni% profanity & word2 %ni% profanity & 
                               newWord %ni% profanity)
ngram4 <- dplyr::filter(ngram4, word1 %ni% profanity & word2 %ni% profanity & 
                               word3 %ni% profanity & newWord %ni% profanity)

# Paste initial words in 3-grams and 4-grams
ngram3 <- with(ngram3, data_frame(word1=paste(word1, word2),
                                  newWord=newWord, freq=freq))
ngram4 <- with(ngram4, data_frame(word1=paste(word1, word2, word3), 
                                  newWord=newWord, freq=freq))

# Save to file
dir.create(file.path(myDir, "output"))
saveRDS(ngram1, file=file.path(myDir, "output", "ngram1.rds"))
saveRDS(ngram2, file=file.path(myDir, "output", "ngram2.rds"))
saveRDS(ngram3, file=file.path(myDir, "output", "ngram3.rds"))
saveRDS(ngram4, file=file.path(myDir, "output", "ngram4.rds"))

# ----------------------------
# End of file
# ----------------------------

