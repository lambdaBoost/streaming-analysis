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

df <- read.csv("leaveRemainTweets_CW.csv")



#seperate dataframes for leave and remain tweets
leave_tweets <- df%>%
  filter(label=="Leave")
remain_tweets <- df%>%
  filter(label=="Remain")

#check we have captured all the tweets
nrow(df)==(nrow(leave_tweets) + nrow(remain_tweets))

#close to balanced
nrow(leave_tweets)
nrow(remain_tweets)


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
  myStopwords <- c(stopwords("english"), "RT","rt","EU","eu","brexit")
  #remove words CONTAINING certain terms
  myCorpus <- tm_map(myCorpus, content_transformer(gsub), pattern = "*vote*|*leave*|*remain*|*stronger*", replacement = "")
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
all_tweets <- make_tdm(df$text)
remain <- make_tdm(remain_tweets$text)
leave <- make_tdm(leave_tweets$text)

#function to make wordclouds
make_wordcloud <- function(freq_tab){
wordcloud(words = freq_tab$word, freq = freq_tab$freq, min.freq = 5,max.words=2000, random.order=FALSE, rot.per=0.2,colors=brewer.pal(8, "Dark2"))
}

#make wordclouds from the frequency tables
make_wordcloud(remain$freq_tab)
make_wordcloud(leave$freq_tab)

plot_wordcount <- function(freq_tab,title_text){
  ggplot(head(freq_tab,20), aes(x=word,y=freq))+
    geom_bar(stat="identity")+
    labs(title=title_text)+
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}

#list most frequent words in each list of tweets
a<-plot_wordcount(all_tweets$freq_table,"most common words for all tweets")
b<-plot_wordcount(remain$freq_table,"most common words for remain tweets")
c<-plot_wordcount(leave$freq_table,"most common words for leave tweets")
grid.arrange(a,b,c, nrow = 1)

#plot word associations
plot(all_tweets$tdm, term = findFreqTerms(all_tweets$tdm, lowfreq = 20), corThreshold = 0.1,
     weighting = T, main="word association for all tweets")
plot(remain$tdm, term = findFreqTerms(remain$tdm, lowfreq = 10), corThreshold = 0.1,
     weighting = T, main="word association for remain tweets")
plot(leave$tdm, term = findFreqTerms(leave$tdm, lowfreq = 15), corThreshold = 0.1,
     weighting = T, main="word association for leave tweets")

#text classifier
#######

#we already have functions to build the document term matrix above so lets use those
#transpose the dtm to get word count for each tweet
dtm<-make_tdm(df$text,10)
dtm<-dtm$dtm

#checknumber of columns used for model
ncol(as.matrix(dtm))


#get most frequent terms to use for classification
#min_term_freq <- 10# minimum number of times a word must be used to be considered by model training
#top_terms <- findFreqTerms(all_tweets$tdm,min_term_freq)
#X_input <- dtm[,top_terms]

#create a container and split into train / test set (70/30 split for now)
container <- create_container(dtm, as.numeric(factor(df$label)),trainSize=1:round(0.7*nrow(df)), testSize=(round(0.7*nrow(df))+1):nrow(df),virgin=FALSE)

#train some models and se what we get
models <- train_models(container,algorithms=c("SVM","TREE","BAGGING","BOOSTING","RF"))
results <- classify_models(container, models) #can use for conf matrix
analytics <- create_analytics(container, results)