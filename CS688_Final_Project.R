# Michael Barrera
# MET CS 688 - Spring 2, 2019
# Final Term Project - Option 3 Twitter Stock Market Sentiment Analysis

library(rtweet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(purrr)
library(wordcloud)

app_name <- "MikeB1869's TwitterR app"
consumer_key <- 'xxx'
consumer_secret <- 'xxx'
access_token <- 'xxx'
access_secret <- 'xxx'

create_token(app = app_name,
             consumer_key = consumer_key,
             consumer_secret = consumer_secret,
             access_token = access_token, 
             access_secret = access_secret)
# Part a
# Search for tweets associated with 3 gainers & 3 losers
KMX <- search_tweets(
  "$KMX", n = 100, include_rts = FALSE
)

CVNA <- search_tweets(
  "$CVNA", n = 100, include_rts = FALSE
)

Ford <- search_tweets(
  "$F Ford", n = 100, include_rts = FALSE # $F Ford gave more refined results
)

# Combine tweets into 1 set and add identifying column.
gainers <- tibble(text = c(KMX$text,Ford$text,CVNA$text))
gainers$symbol[1:75] <- c("KMX")
gainers$symbol[76:172] <- c("F")
gainers$symbol[173:270] <- c("CVNA")

# Losers 
TSLA <- search_tweets(
  "$TSLA", n = 100, include_rts = FALSE
)

GM <- search_tweets(
  "$GM", n = 100, include_rts = FALSE
)

SAH <- search_tweets(
  "$SAH", n = 150, include_rts = FALSE
)

# Combine tweets into 1 set and add identifying column.
losers <- tibble(text = c(TSLA$text,GM$text,SAH$text))
losers$symbol[1:100] <- c("TSLA")
losers$symbol[101:197] <- c("GM")
losers$symbol[198:304] <- c("SAH")

# Part b
library(tm)
# Write a function to convert tweet tibble to corpus
tweet_to_corpus <- function(i){
  VectorSource(i) %>% VCorpus()
}
#Pass gainers and losers to function
data.corpus1 <- tweet_to_corpus(gainers)
data.corpus2 <- tweet_to_corpus(losers)

# Part c
# Write a function to perform basic pre-processing on a raw corpus
process.corp <- function(corp) {
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeWords, stopwords("english"))
  return(corp)
}
# Pass gainers corpus to the processing function
data.corpus1 <- process.corp(data.corpus1)
inspect(data.corpus1[[1]])
# Remove ticker symbols to get cleaner word cloud and sentiment results
data.corpus1 <- tm_map(data.corpus1, removeWords, c("kmx", "cvna"))
# Pass losers corpus to the processing function
data.corpus2 <- process.corp(data.corpus2)
inspect(data.corpus2[[1]])
# Remove ticker symbols to get cleaner word cloud and sentiment results
data.corpus2 <- tm_map(data.corpus2, removeWords, c("sah", "tsla", "gm"))

# Part d
# Create DTMs for each set
dtm1 <- TermDocumentMatrix(data.corpus1)
inspect(dtm1)
# Find most frequent terms (Part e)
m <- as.matrix(dtm1)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
d[1:20,]

dtm2 <- TermDocumentMatrix(data.corpus2)
inspect(dtm2)
# Find most frequent terms (Part e)
m1 <- as.matrix(dtm2)
v1 <- sort(rowSums(m1), decreasing = TRUE)
d1 <- data.frame(word = names(v1), freq = v1)
d1[1:20,]

# Part e
# Create wordclouds for each set
set.seed(1234)
wordcloud(words = d$word, freq =  d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
set.seed(5678)
wordcloud(words = d1$word, freq =  d1$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Part f
# Compute sentiment score for each set
# Sentiment score function from Module 5
sentiment_bing = function(twt){
  #Step 1;  perform basic text cleaning (on the tweet), as seen earlier
  twt_tbl = tibble(text = twt) %>% 
    mutate(
      # Remove http elements manually
      stripped_text = gsub("http\\S+","",text)
    ) %>% 
    unnest_tokens(word,stripped_text) %>% 
    anti_join(stop_words, by="word") %>%  #remove stop words
    inner_join(get_sentiments("bing"), by="word") %>% # merge with bing sentiment
    count(word, sentiment, sort = TRUE) %>% 
    ungroup() %>% 
    ## Create a column "score", that assigns a -1 one to all negative words, and 1 to positive words. 
    mutate(
      score = case_when(
        sentiment == 'negative'~ n*(-1),
        sentiment == 'positive'~ n*1)
    )
  ## Calculate total score
  sent.score = case_when(
    nrow(twt_tbl)==0~0, # if there are no words, score is 0
    nrow(twt_tbl)>0~sum(twt_tbl$score) #otherwise, sum the positive and negatives
  )
  ## This is to keep track of which tweets contained no words at all from the bing list
  zero.type = case_when(
    nrow(twt_tbl)==0~"Type 1", # Type 1: no words at all, zero = no
    nrow(twt_tbl)>0~"Type 2" # Type 2: zero means sum of words = 0
  )
  list(score = sent.score, type = zero.type, twt_tbl = twt_tbl)
}

# Pass gainers to function to get sentiment scores
library(googleVis)
sentiment_gainers <- sentiment_bing(gainers$text)
table1 <- gvisTable(sentiment_gainers$twt_tbl)
plot(table1)

# Pass losers to function to get sentiment score
sentiment_losers <- sentiment_bing(losers$text)
table2 <- gvisTable(sentiment_losers$twt_tbl)
plot(table2)

