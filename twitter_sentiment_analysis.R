library(tm)
library(dplyr)
library(tidytext)
library(SnowballC)
tweets=SSR_tweets$text
mydata=VCorpus(VectorSource(tweets))
mydata
tospace=content_transformer(function(x,pattern)
  sub(pattern," ",x))

mydata_clean=tm_map(mydata,tospace,"@")
mydata_clean=tm_map(mydata_clean,tospace,"/")
mydata_clean=tm_map(mydata_clean,tospace,"\\|")
mydata_clean=tm_map(mydata_clean,tospace,"£")
mydata_clean=tm_map(mydata_clean,tospace,"€")
mydata_clean=tm_map(mydata_clean,tospace,"˜")
mydata_clean=tm_map(mydata_clean,tospace,"‘")
mydata_clean=tm_map(mydata_clean,tospace,"’")



mydata_clean=tm_map(mydata_clean,content_transformer(tolower))

mydata_clean=tm_map(mydata_clean,removeNumbers)
mydata_clean=tm_map(mydata_clean,removeWords,stopwords())
mydata_clean =tm_map(mydata_clean,removePunctuation)
mydata_clean=tm_map(mydata_clean,stripWhitespace)
mydata_clean=tm_map(mydata_clean,stemDocument)
lapply(mydata_clean[1:6],as.character)

mydata_dtm <- TermDocumentMatrix(mydata_clean)
dtm_m <- as.matrix(mydata_dtm)
dtm_m[1:10, 1:20]
terms=Terms(mydata_dtm)
terms[1:50]
w_terms=terms[1:50]
mydata_clean=tm_map(mydata_clean,removeWords,w_terms)

mydata_dtm
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
View(dtm_v)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 10 most frequent words
head(dtm_d, 10)

library(ggplot2)
# Plot the most frequent words
barplot(dtm_d[1:6,]$freq, las = 2, names.arg = dtm_d[1:6,]$word,
        col ="lavender", main ="Top 6 most frequent words",
        ylab = "Word frequencies")
library(wordcloud2)
set.seed(1234)
# wordcloud2( words = dtm_d$word, freq = dtm_d$freq, min.freq = 5,
#           max.words=100, random.order=FALSE, rot.per=0.40, 
#           colors=brewer.pal(8, "Dark2"))

library(wordcloud)
w <- sort(rowSums(dtm_m), decreasing = TRUE)
set.seed(701)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.9,
           shape = 'circle', 
           rotateRatio = 0.5,
           minSize = 20)
findAssocs(mydata_dtm,terms=c("ssr","drug","justic"),corlimit=0.23)

## Sentiment Analysis
library(syuzhet)
tweets=SSR_tweets$text
# regular sentiment score using get_sentiment() function and method of your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(tweets, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)

bing_vector <- get_sentiment(tweets, method="bing")
# see the first row of the vector
head(bing_vector)
# see summary statistics of the vector
summary(bing_vector)

afinn_vector <- get_sentiment(tweets, method="afinn")
# see the first row of the vector
head(afinn_vector)
# see summary statistics of the vector
summary(afinn_vector)


# found in each row
d<-get_nrc_sentiment(tweets[1:500])
# head(d,10) - to see top 10 lines of the get_nrc_sentiment dataframe
head (d,10)

#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level of a grouping variable.
td_new <- data.frame(rowSums(td[2:253]))
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
  sort(colSums(prop.table(d[, 1:8]))), 
  horiz = TRUE, 
  cex.names = 0.7, 
  las = 1, 
  main = "Emotions in Text", xlab="Percentage"
)
td_new 


