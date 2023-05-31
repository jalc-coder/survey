#needed libraries
library(openxlsx)
library(tm)
library(SnowballC)
library(readxl)
library(tidyverse)
library(wordcloud)
library(syuzhet)
library(lubridate)
library(scales)
library(reshape2)
library(wordcloud2)
library(kableExtra)
library(tidytext)
library(topicmodels)
library(pals)
library(lda)
library(ldatuning)
library(kableExtra)
library(DT)
library(flextable)
library(remotes)
library(klippy)
#data load. change file as needed
SP22 <- read_excel("~/2023 CESS.xlsx")
#Load each question to a separate questions
q1<-SP22$Q1
#change answers into corpus
corpus<-iconv(q1)
corpus<-Corpus(VectorSource(corpus))
#make everything lowercase
corpus<-tm_map(corpus, tolower)
#remove punctuation
corpus<-tm_map(corpus, removePunctuation)
#remove numbers
corpus<-tm_map(corpus, removeNumbers)
#remove common stopwords. If non english you can change stopwords
clean <- tm_map(corpus, removeWords, stopwords('english'))
#you can edit this to add/remove/change words that are going to be common in the questions. So, if you are talking about classes, you would remove class
clean <- tm_map(clean, removeWords, c('students', 'get', 'like', 'will', 'pcr', 'ccr', 'goal', 'pgr', 'class', 'complete', 'course', 'none', 'yes', 'however', 'college'))
#stem document to get the root word. This can be commented out if you don't care, but if you stem the words you would put words like become and becoming at the same word
clean <- tm_map(clean, stemDocument)
#remove excess whitespace
clean <- tm_map(clean, stripWhitespace)
#create matrix of term
tdm<-TermDocumentMatrix(clean)
tdm<-as.matrix(tdm)
#Find totals of the words
et<-rowSums(tdm)
#Find words that occur over 20 times
et<-subset(et, et>20)
#Reorder
et<-sort(et, decreasing = TRUE)
#Graph in barplot. You can rename main to change title
barplot(et, las=2, col=rainbow(50), main="Question 1 Individual Word Count")
#Create sentiment scores from question
et<-get_nrc_sentiment(q1)
#Add up sentiments for the words
et<-colSums(et)
#Sort words
et<-sort(et, decreasing = TRUE)
#Plot the sentiments
barplot(et, las = 2, col = rainbow(10), ylab = 'Count', main = 'Question 1 Sentiment Scores')
#Find the length of each comment
length<-colSums(tdm)
#Plot the histogram of length of survey response
ggplot() + aes(length) + geom_histogram(binwidth = 1, col="black", fill="blue") + labs(x="Response length minus stop words and common words", y="Frequency", title ="Question 1 Histogram of response length") + theme(plot.title = element_text(size=16, hjust=0.5, face="bold"))
#Create wordcloud of response
wordcloud(clean, color='random-dark', colors=brewer.pal(8, "Dark2"))
#Create two empty vectors
sent<-c()
values<-c()
#for loop to find the sentiment scores as either potitive or negative for each comment
for (i in 1:length(q1)){
  sent<-get_nrc_sentiment(q1[i])
  scores<-sent %>% summarise(overall.positive = anticipation+joy+surprise+trust+positive, overall.negative=anger+disgust+fear+sadness+negative)
  sent<-cbind(sent, scores)
  values<-rbind(values, sent)
}
#Sentiment scores for each comment
percent.q1 <- values
write.xlsx("percent.q1", file="Question 1 Sentiment Scores.xlsx")

#The following code is in progress
#change this based on the number of responses you get. If you have a lot of reports, increase this to filter out less common. If you don't get a lot, make it smaller
#minimumFrequency <- 5
#making a matrix of the data
#DTM <- DocumentTermMatrix(clean, control = list(bounds = list(global = c(minimumFrequency, Inf))))
# have a look at the number of documents and terms in the matrix
#dim(DTM)
#
#result <- FindTopicsNumber(
#  DTM,
#  topics = seq(from = 2, to = 20, by = 1),
#  metrics = c("CaoJuan2009",  "Deveaud2014"),
#  method = "Gibbs",
#  control = list(seed = 77),
#  verbose = TRUE
#)
#Finds topic number
#FindTopicsNumber_plot(result)
#Set the number of topics
#K <- 3
# set random number generator seed
#set.seed(42)
# compute the LDA model, inference via 1000 iterations of Gibbs sampling
#topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))
#tmResult <- posterior(topicModel)
# format of the resulting object
#attributes(tmResult)
#nTerms(DTM)
#beta <- tmResult$terms   # get beta from results
#dim(beta)
#rowSums(beta)
#nDocs(DTM)
#theta <- tmResult$topics 
#dim(theta)
#rowSums(theta)[1:10]
#termdata<-terms(topicModel, 10)
#top5termsPerTopic <- terms(topicModel, 5)
#topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
#topicToViz <- 1 # change for your own topic of interest
#topicToViz <- grep('mexico', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
#top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
#words <- names(top40terms)
# extract the probabilites of each of the 40 terms
#probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
#mycolors <- brewer.pal(8, "Dark2")
#wordcloud(words, probabilities, random.order = FALSE, color = mycolors)
#attr(topicModel, "alpha") 
#topicModel2 <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 0.2))
#tmResult <- posterior(topicModel2)
#theta <- tmResult$topics
#beta <- tmResult$terms
#topicNames <- apply(terms(topicModel2, 5), 2, paste, collapse = " ")  # reset topicnames
# get topic proportions form example documents
