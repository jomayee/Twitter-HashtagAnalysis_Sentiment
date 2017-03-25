# In this script, I am connecting to the twitter user timeline using twitter API. For this I have created twitter account
#and an application, generated consumer key and token, access key and token. 

install.packages("twitteR")
install.packages("ROAuth")
install.packages('scales')
install.packages('syuzhet')
install.packages('reshape2')

library(twitteR)
library(ROAuth)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr )

setup_twitter_oauth("consumer_key","consumer_secret", 
                    "access_key","access_secret")

#connect to @realDonaldTrump timeline and save the tweets in tweets.

tweets1<-userTimeline("@realDonaldTrump", n=3000)

#strip off retweets.
tweetsa<-(strip_retweets(tweets1, strip_manual = TRUE, strip_mt = TRUE))

Trump<-getUser('realDonaldTrump')

#get followercount
Trump$getFollowersCount()

#get friends list
Trump$getFriends(n=10)

#get favorites list
Trump$getFavorites(n=10)

#converting the tweets file into dataframe.
df<-twListToDF(tweetsa)

##############################

#text mining package is installed
install.packages('tm')
library(tm)
require(tm)

#creating a corpus vector with text attribute from the dataframe
case<-Corpus(VectorSource(df$text))
case

#creating a fucntion to remove url
removeURL<-function(x) gsub("http[^[:space:]]*", "", x)

#tranforming the content from the case vector to remove url. we use the function created above
case<-tm_map(case, content_transformer(removeURL))
case

#creating function to remove punctuation marks.
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

#pass the case vector to the function created above to remove punctutation marks
case<- tm_map(case, content_transformer(removeNumPunct))


# remove extra whitespace
case <- tm_map(case, stripWhitespace)

#converting the content of case to lower case
case<-tm_map(case, content_transformer(tolower))

# keep a copy for stem completion later
caseCopy <- case

#converting to matrix
a<-TermDocumentMatrix(case, control = list(wordLengths=c(1, Inf)))
a

#finding for frequent terms in a, frequency should be more than 10
freq.terms<-findFreqTerms(a, lowfreq = 50)

#calculating the frequency of every term in the matrix a.
term.freq<-rowSums(as.matrix(a))

#creating a subset from term.freq for items of freq>=10
term.freq<-subset(term.freq, term.freq>=50)

#convert to dataframe format, with fields term which means name of the frequent term and freq field with the
#frequency of the word.
df3<-data.frame(term=names(term.freq), freq=term.freq)

#using plot library
require(ggplot2)

#creating ggplot for dataframe df3 for fields term and freq.
ggplot(df3, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip() +
  
  theme(axis.text=element_text(size=7)) + ggtitle("Frequency of words")

#saving a as matrix in m
m<-as.matrix(a)

#sorting the words in the order of their frequency
word.freq <- sort(rowSums(m), decreasing = F)

library(wordcloud)
require(wordcloud)

# colors

pal <- brewer.pal(9, "BuGn")[-(1:4)]

# plot word cloud


wordcloud(words = names(word.freq), freq = word.freq, min.freq = 10, random.order = T, colors = pal)

##which words are associated with Microsoft

findAssocs(a, "hillary", 0.2)
findAssocs(a, "america", 0.2)
findAssocs(a, "trump", 0.2)
findAssocs(a, "obama", 0.2)

###############################################################################################################

#Search twitter for hash tag makeamericagreatgain and fetch the sentiment.

tweets2<-searchTwitter("#makeamericagreatagain", n=3000)
tweets2df<-twListToDF(tweets2)

case1<-tweets2df$text
removeURL<-function(x) gsub("http[^[:space:]]*", "", x) 

#tranforming the content from the case vector to remove url. we use the function created above
case1<-removeURL(case1)

#creating function to remove punctuation marks.
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

#pass the case vector to the function created above to remove punctutation marks
case1<- removeNumPunct(case1)


# remove extra whitespace
case1<- stripWhitespace(case1)

#converting the content of case to lower case
case1<-tolower(case1)
case

sentiment1<-get_nrc_sentiment(case1)
head(sentiment1)
tweets2<-cbind(tweets2df, sentiment1)



sentimentTotals1 <- data.frame(colSums(tweets2[,c(17:26)]))
names(sentimentTotals1) <- "count"
sentimentTotals1 <- cbind("sentiment" = rownames(sentimentTotals1), sentimentTotals1)
rownames(sentimentTotals1) <- NULL
ggplot(data = sentimentTotals1, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for #makeamericagreatagain Tweets")


#####################################################################################################

#connect to @BillGates timeline and save the tweets in tweets. We are going to analyze the tweets text sentiment
tweets3<-userTimeline("@BillGates", n=3000)

tweets3df<-twListToDF(tweets3)

case3<-tweets3df$text
removeURL<-function(x) gsub("http[^[:space:]]*", "", x) 

#tranforming the content from the case vector to remove url. we use the function created above
case3<-removeURL(case3)

#creating function to remove punctuation marks.
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)

#pass the case vector to the function created above to remove punctutation marks
case3<- removeNumPunct(case3)


# remove extra whitespace
case3<- stripWhitespace(case3)

#converting the content of case to lower case
case3<-tolower(case3)
case3

sentiment3<-get_nrc_sentiment(case3)
head(sentiment3)
tweets3<-cbind(tweets3df, sentiment3)



sentimentTotals2 <- data.frame(colSums(tweets3[,c(17:26)]))
names(sentimentTotals2) <- "count"
sentimentTotals2 <- cbind("sentiment" = rownames(sentimentTotals2), sentimentTotals2)
rownames(sentimentTotals2) <- NULL
ggplot(data = sentimentTotals2, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for Tweets by BillGates")
