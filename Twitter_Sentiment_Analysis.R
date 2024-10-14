### LOAD PACKAGES ###
require(rtweet)
require(tidyverse)
require(lubridate)
require(igraph)
require(tidytext)
library(tm)
require(wordcloud)
require(ggplot2)
require(dplyr)

### READ FILE ###
text_df1<-read.csv("SSR_tweet_dataset.csv",stringsAsFactors = FALSE)

### RESTORED DATA IN A CHARACTER FORMAT ###
tweets<-as.character(text_df1$text)
tweets
head(tweets,n=5)

### CREATE CORPUS ###
tw_document <- Corpus(VectorSource(tweets)) 

### CLEAN CORPUS ###
tw_document <- tm_map(tw_document,content_transformer(tolower))
tw_document <- tm_map(tw_document,removeNumbers)
tw_document <- tm_map(tw_document,removeWords, stopwords())
tw_document <- tm_map(tw_document,removePunctuation)
tw_document <- tm_map(tw_document,stripWhitespace)

### CLEAN CORPUS FOR FURTHER ANALYSIS ###
data <- TermDocumentMatrix(tw_document)
m <- as.matrix(data)
v <- sort(rowSums(m), decreasing = T)
d <- data.frame(frequency=v)
d <- data.frame(word=names(v), frequency=v)
head(d,10)

### CREATING BAR PLOT ###
barplot(d[1:10,]$freq, las=2, names.arg = d[1:10,]$word,
        col = "Purple", main = "Top ten most frequent words",
        ylab = "word frequencies")

### WORD CLOUD ###
set.seed(1234)
wordcloud(words = d$word, freq = d$frequency, min.freq = 3, max.freq = 40,
          random.order = FALSE, rot.per = 0.40, colors = brewer.pal(8,"Dark2"))

### GENERAL CONTEXT ###
syuzhet_vector <- get_sentiment(d, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

### US CONTEXT ###
bing_vector <- get_sentiment(tweets, method="bing")
# see the first row of the vector
head(bing_vector)
# see summary statistics of the vector
summary(bing_vector)

### UK CONTEXT ###
afinn_vector <- get_sentiment(tweets, method="afinn")
# see the first row of the vector
head(afinn_vector)
# see summary statistics of the vector
summary(afinn_vector)

### to see top 10 lines of the get_nrc_sentiment dataframe ###
t<-as.character(tweets)
result <- get_nrc_sentiment(t)
head(result,10)

### TRANSPOSE ###
td<-data.frame(t(result))
#The function rowSums computes column sums across rows for each level of a grouping variable
td_new <- data.frame(rowSums(td[2:50]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[8:10,]

#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, 
          geom="bar", fill=sentiment, ylab="count")+
  ggtitle("Survey sentiments")

# Plot two - count of words associated with each sentiment, 
# expressed as a percentage
barplot(
  sort(colSums(prop.table(result[, 1:3]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)
td_new 

