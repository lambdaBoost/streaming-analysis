library(dplyr)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(gridExtra)
library(graph)
library(Rgraphviz)
library(RTextTools)

df <- read.csv("fracking_dataset.csv")




#function to build text corpus for analysis

buildCorpus <- function(someText){
  
  # build a corpus, and specify the source to be character vectors
  myCorpus <- Corpus(VectorSource(someText))
  # I had to add this line to make the code work
  # For windows, it may not be an issue
  myCorpus <- tm_map(myCorpus,
                     content_transformer(function(x) iconv(x, to='UTF-8',sub='byte')))
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  # remove punctuation
  myCorpus <- tm_map(myCorpus, removePunctuation)
  # remove numbers
  myCorpus <- tm_map(myCorpus, removeNumbers)
  # remove URLs
  removeURL <- function(x) {
    gsub("http[[:alnum:]]*", "", x)
  }
  myCorpus <- tm_map(myCorpus, removeURL)
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL)) #??
  # add stopwords
  myStopwords <- c(stopwords("english"))
  #remove words CONTAINING certain terms
  myCorpus <- tm_map(myCorpus, content_transformer(gsub), pattern = "*frack*|*ing*", replacement = "")
  # remove stopwords from corpus
  myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
  #
  myCorpus <- tm_map(myCorpus, stripWhitespace)
  # Return the text corpus
  return(myCorpus)
}

#function to build term document matrix
make_tdm <- function(text_column,minfreq=1){
  text_column <- iconv(text_column, 'UTF-8', 'ASCII')
  corpus <- buildCorpus(text_column)
  # keep for later
  corpus_copy <- corpus
  # stem words
  corpus <- tm_map(corpus, stemDocument)
  tdm <- TermDocumentMatrix(corpus,control=list(bounds = list(global = c(minfreq,Inf)))) 
  dtm <- DocumentTermMatrix(corpus,control=list(bounds = list(global = c(minfreq,Inf)))) 
  m <- as.matrix(tdm)
  v <- sort(rowSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  return(list(corpus=corpus,freq_table=d,tdm=tdm , dtm=dtm))
}

#make corpuses and tdms from the tweets
all_tweets <- make_tdm(df$X.2)


#function to make wordclouds
make_wordcloud <- function(freq_tab){
  wordcloud(words = freq_tab$word, freq = freq_tab$freq, min.freq = 5,max.words=2000, random.order=FALSE, rot.per=0.2,colors=brewer.pal(8, "Dark2"))
}

#make wordclouds from the frequency tables
make_wordcloud(all_tweets$freq_tab)


plot_wordcount <- function(freq_tab,title_text){
  ggplot(head(freq_tab,20), aes(x=word,y=freq))+
    geom_bar(stat="identity")+
    labs(title=title_text)+
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

#list most frequent words in each list of tweets
plot_wordcount(all_tweets$freq_table,"most common words for all tweets")


#plot word associations
plot(all_tweets$tdm, term = findFreqTerms(all_tweets$tdm, lowfreq = 70), corThreshold = 0.1,
     weighting = T, main="word association for all tweets")

#text classifier
#######

#we already have functions to build the document term matrix above so lets use those
#transpose the dtm to get word count for each tweet
dtm<-make_tdm(df$X.2,25)
dtm<-dtm$dtm

#calculate distances
m  <- as.matrix(dtm)
m<-m[sample(nrow(m),size=5000,replace=FALSE),] #sample rows to prevent huge distance matrix
distMatrix <- dist(m, method="euclidean")

num_clusters <- 5

#perform hierachical clustering
hc <- hclust(distMatrix, "ward.D")
clustering <- cutree(hc, num_clusters)

test<-as.data.frame(cbind(m,clustering))

for (i in c(1:num_clusters)){
  tryCatch({
  cluster_dtm <- filter(test,clustering==as.character(i))%>%
    select(-clustering)
  
  m <- as.matrix(cluster_dtm)
  v <- sort(colSums(m), decreasing = TRUE)
  d <- data.frame(word = names(v), freq = v)
  
  make_wordcloud(d)
  },
  error = function(err){print("empty cluster")}
  )
}
