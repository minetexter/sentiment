# sentiment analysis
library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)

#
#key="hidden"
#secret="hidden"
#setwd("/text_mining_and_web_scraping")

#download.file(url="http://curl.haxx.se/ca/cacert.pem",
#             destfile="/text_mining_and_web_scraping/cacert.pem",
#             method="auto")
#authenticate <- OAuthFactory$new(consumerKey=key,
#                                consumerSecret=secret,
#                                requestURL="https://api.twitter.com/oauth/request_token",
#                                accessURL="https://api.twitter.com/oauth/access_token",
#                                authURL="https://api.twitter.com/oauth/authorize")
#setup_twitter_oauth(key, secret)
#ave(authenticate, file="twitter authentication.Rdata")


#=2000  # tweets to request from each query
#=200  # radius in miles
#ats=c(38.9,40.7,37.8,39,37.4,28,30,42.4,48,36,32.3,33.5,34.7,33.8,37.2,41.2,46.8,
#      46.6,37.2,43,42.7,40.8,36.2,38.6,35.8,40.3,43.6,40.8,44.9,44.9)

#ons=c(-77,-74,-122,-105.5,-122,-82.5,-98,-71,-122,-115,-86.3,-112,-92.3,-84.4,-93.3,
#      -104.8,-100.8,-112, -93.3,-89,-84.5,-111.8,-86.8,-92.2,-78.6,-76.8,-116.2,-98.7,-123,-93)
#
#cities=DC,New York,San Fransisco,Colorado,Mountainview,Tampa,Austin,Boston,
#       Seatle,Vegas,Montgomery,Phoenix,Little Rock,Atlanta,Springfield,
#       Cheyenne,Bisruk,Helena,Springfield,Madison,Lansing,Salt Lake City,Nashville
#       Jefferson City,Raleigh,Harrisburg,Boise,Lincoln,Salem,St. Paul
#
#donald=do.call(rbind,lapply(1:length(lats), function(i) searchTwitter('Donald+Trump',
#                                                                      lang="en",n=N,resultType="recent",
#                                                                      geocode=paste(lats[i],lons[i],paste0(S,"mi"),sep=","))))


#consumer_key <- 
#consumer_secret <- 
#access_token <- 
#access_secret <- 

#setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# use str to see if $statussource is iphone or computer
# NEED TO CREATE TWITTER ACCOUNT AND GET API

# WHY USE NLP, CHALLENGES, INTRO TO TEXT MINING, API WITH TWITTER ,SOCIAL MEDIA ANALYTICS
##Packages required:
#library(twitteR)

library(ggplot2)
library(httr)
library(rjson)
library(tm)
library(gridExtra)
library(lubridate)

require(plyr)
require(stringr)
library(rtweet)

## name assigned to created app
appname <- "statappsentiment"
## api key 
key <- ""
## api secret 
secret <- "" ""
twitter_token <- create_token(app = appname, consumer_key = key, consumer_secret = secret)

# start using rtweet package instead



get_trends()

get_trends("London")

acc <- "toyotagb" #account name

get_favorites(acc)


friends <- get_friends(acc)
followers <- get_followers(acc)

str(friends)
lookup_users(friends)

term_toyotachr <- search_tweets("#toyotachr", n = 1000)

term_toyotachr


users_data(term_toyotachr) %>%
head()

str(term_toyotachr)

term_toyotachr

term_toyotachr$text

search_users("toyota", n = 100) #automatically ordered by number followers

acc_lk <- lookup_users(acc)

tweets_data(acc_lk) #last tweet

acctw <- get_timeline(acc)

acctw$text <- sapply(acctw$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

acctw$text

#Remove punctuation, numbers, html-links and unecessary spaces:
textScrubber <- function(dataframe) {
  dataframe$text <-  gsub("RT", " ", dataframe$text)
  dataframe$text <-  gsub("ke$", "", dataframe$text)
  dataframe$text <-  gsub("@\\w+", " ", dataframe$text)
  dataframe$text <-  gsub("[[:punct:]]", "  ", dataframe$text)
  dataframe$text <-  gsub("[[:digit:]]", "", dataframe$text)
  dataframe$text <-  gsub("http\\w+", "", dataframe$text)
  dataframe$text <-  gsub("\n", " ", dataframe$text)
  dataframe$text <-  gsub("[ \t]{2,}", "", dataframe$text)
  dataframe$text <-  gsub("^\\s+|\\s+$", "", dataframe$text)
  dataframe$text <-  tolower(dataframe$text)

  return(dataframe)
}

head(textScrubber(acctw)$text) #this scrubber is not great...

# loading packages
require(wordcloud)
require(tm)
require(SnowballC)
require(ggplot2)
require(RColorBrewer)


#tdmCreator <- function(dataframe, stemDoc = T, rmStopwords = T){
#  
#  tdm <- Corpus(VectorSource(dataframe$text))
#  if (isTRUE(rmStopwords)) {
#    tdm <- tm_map(tdm, removeWords, stopwords())
#  }
#  if (isTRUE(stemDoc)) {
#    tdm <- tm_map(tdm, stemDocument)
#  }
#  tdm <- TermDocumentMatrix(tdm,
#                            control = list(wordLengths = c(1, Inf)))
#  tdm <- rowSums(as.matrix(tdm))
#  tdm <- sort(tdm, decreasing = T)
#  df <- data.frame(term = names(tdm), freq = tdm)
#  return(df)
#}
#
#
#
#acctw_tdm <- tdmCreator(acctw)

acctw_corpus <- Corpus(VectorSource(acctw$text))

inspect(acctw_corpus)

acctw_corpus <- tm_map(acctw_corpus, stripWhitespace)

acctw_corpus <- tm_map(acctw_corpus, removeWords, stopwords("english"))

#acctw_corpus <- tm_map(acctw_corpus, removeWords, c("pm", "mp", "st"))


dtm <- DocumentTermMatrix(acctw_corpus)
dtm
freq <- colSums(as.matrix(dtm))
length(freq)

ord <- order(freq,decreasing=TRUE)
#inspect most frequently occurring terms
freq[head(ord)]
#inspect least frequently occurring terms
freq[tail(ord)]   

#limit words by specifying min frequency
findFreqTerms(dtm,lowfreq=3)

# association analysis of words commonly found together
findAssocs(dtm,"brand",0.9)

#setting the same seed each time ensures consistent look across clouds
set.seed(39)

wc <- wordcloud(names(freq),freq, min.freq=3, scale=c(5,0.5), 
                max.words=100, random.order=FALSE, rot.per=0.3, 
                use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))


#sentiment

require(RSentiment)



#streaming data (uses eg if email campaign just released or twitter account status updated)

q <- paste0("toyota,yaris,auris,toyota chr")

streamtime <- 30

filename <- "toyota.json"

rt <- stream_tweets(q = q, timeout = streamtime, file_name = filename)

rt$text

ts_plot(rt, by = "1 secs")

rt %>%
  ts_filter(
    by = "secs",
    filter = c("toyota","yaris","auris","toyota chr"),
    key = c("toyota","yaris","auris","toyota chr"),
    trim = TRUE) %>%
  ## The pipe operator allows you to combine this with ts_plot
  ## without things getting too messy.
  ts_plot(
    theme = "spacegray",
    cols = c("#6699ee", "#dd7a7a", "#7acc7a", "#cc7ac9"),
    main = "Tweets",
    subtitle = "Tweets collected, parsed, and plotted using `rtweet`")


