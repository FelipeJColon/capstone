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
## Ad-hoc functions for word prediction
##
## Version 1
##
## Dependencies: None
##
## Written by: Felipe J Colón-González
## For any problems with this code, please contact f.colon@uea.ac.uk
## 
## ############################################################################

# Tokenize input and training data
tokenise <- function(x, ngrams=1) {
       
       # Fix contractions
       x <- gsub("won't", "will not", x)
       x <- gsub("n't", " not", x)
       x <- gsub("'ll", " will", x)
       x <- gsub("'re", " are", x)
       x <- gsub("'ve", " have", x)
       x <- gsub("'m", " am", x)
       x <- gsub("it's", "it is", x)
       x <- gsub("'s", "", x)
       x <- gsub("'d", " would", x)
       
       # Remove non-latin characters
       Encoding(x) <- "latin"
       x <- iconv(x, "latin1", "ASCII", sub="")
       
       # Remove URLs
       x <- gsub("(f|ht)(tp)(s?)(://)(\\S*)", "", x)
       
       # Remove hyphens and underscores
       x <- gsub("-", " ", x); x <- gsub("_", " ", x)
        
       # Remove punctuation
       x <- gsub("[[:punct:]]", "", x)
       
       # Remove numbers
       x <- gsub("[[:digit:]]+", "", x)
       
       # Set all letters to lower case
       x <- tolower(x)
       
       # Remove extra white space
       x <- gsub("\\s+", " ", x)
       
              quanteda::tokenize(x,
                                 what="word", removeNumbers=FALSE, 
                                 removePunct=FALSE, removeSymbols=TRUE, 
                                 removeSeparators=FALSE, removeTwitter=TRUE, 
                                 removeHyphens=TRUE, removeURL=FALSE, 
                                 simplify=TRUE, ngrams=ngrams, concatenator=" ")
}


# Calculate the frequencies for each ngram
frequence <- function(x, min=1) {
       x <- x %>% group_by(newWord) %>%
              summarize(count=n()) %>%
              filter(count>=min)
       
       x <- x %>% mutate(freq=count/sum(x$count)) %>% 
              select(-count) %>%
              arrange(desc(freq))
}


# Predict using stupid backoff 
predictWord <- function(x, nRows=5) {
       
       # Tokenize input
       x <- tokenise(x)
       
       # Transform to data_frame
       y <- data_frame(word=x)
       
       # Select words to be used for prediction
       if(length(x) == 1) {
              w1 <- y
              
       } else if (length(x) == 2) {
              w1 <- paste(y[1,1], y[2,1])
              
       } else if (length(x) >= 3) {
              wordsTail <- tail(y, n=3)
              w1 <- paste(wordsTail[1,1], wordsTail[2,1], wordsTail[3,1])
       }
       
       # Predicitive algorithm
       if(w1 %in% ngram4$word1) {
              prediction <- ngram4 %>% filter(word1 %in% w1) %>% 
                     select(newWord, freq)
              
       } else if(w1 %in% ngram3$word1) {
              prediction <- ngram3 %>% filter(word1 %in% w1) %>%
                     select(newWord, freq)
              
       } else if(w1 %in% ngram2$word1) {
              prediction <- ngram2 %>% filter(word1 %in% w1) %>%
                     select(newWord, freq)
              
       } else {
              prediction <- ngram1 %>% select(newWord, freq)
       }
       
       # Return predicted word as data frame
       return(data.frame(Suggestions=head(as.vector(unlist(prediction[,1])),
                                         n=nRows)))
}

# ----------------------------
# End of file
# ----------------------------

