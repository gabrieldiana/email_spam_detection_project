#install.packages
#not the full lists, depending on your pre-installed packages, add more
install.packages('caTools')

#Load the required packages

library(readxl)
library(tm)
library(xlsx)
library(caTools)
library(wordcloud)


library(readr)
library(psych)
library(pastecs)
library(caret)
library(httr)


#Read the data 
email_dataset=read.csv('spam.csv')

#check the data
str(email_dataset)
table(email_dataset$Category)


#pre-process the corpus using standard corpus mining steps

#optional manual transformation of text to delete specific words
# email_dataset$Message_edited<-gsub('http\\S+\\s*','',email_dataset$Message)
# email_dataset$Message_edited <- gsub("<.*?>", "", email_dataset$Message)
# email_dataset$Message_edited<-gsub("[\r\n]", "", email_dataset$Message)
# 

# Build a new corpus variable called corpus
corpus = VCorpus(VectorSource(email_dataset$Message))
corpus<-tm_map(corpus,stripWhitespace)
corpus<-tm_map(corpus, removeNumbers)  
corpus<-tm_map(corpus, removePunctuation)
corpus<-tm_map(corpus,removeWords, stopwords("english"))
corpus<-tm_map(corpus, tolower)
corpus<-tm_map(corpus,PlainTextDocument)
#stem the words
corpus<-tm_map(corpus,stemDocument)
#check the format
str(corpus)


#extract word frequencies - to check which words appear how many times. This will be used in the prediction model
dtm = DocumentTermMatrix(corpus)
#remove sparse words
dtm_sprs = removeSparseTerms(dtm, 0.99)
#check
dtm_sprs 

#convert dtm to data frame
emails_df=as.data.frame(as.matrix(dtm_sprs))
#change column and row names
colnames(emails_df) = make.names(colnames(emails_df))
rownames(emails_df)=rownames(email_dataset)
  
#check the count of each term
sort(colSums(emails_df))

#add the spam variable to the new df
emails_df$spam=as.factor(email_dataset$Category)


#wordcloud for spam and non spam emails

#wordcloud for spam emails
words_ham <- sort(colSums(emails_df[emails_df$spam=='ham',-ncol(emails_df)] ),decreasing=TRUE)
ham_df <- data.frame(word = names(words_ham),freq=words_ham)
head(d, 10)
wordcloud(words = ham_df$word, freq = ham_df$freq, 
          random.order = F,   max.words = 500, rot.per=0.35, colors = brewer.pal(8, "Dark2"))


#wordlcoud for spam words
words_spam <- sort(colSums(emails_df[emails_df$spam=='spam',-ncol(emails_df)] ),decreasing=TRUE)
spam_df <- data.frame(word = names(words_spam),freq=words_spam)
wordcloud(words = spam_df$word, freq = spam_df$freq, 
          random.order = F,   max.words = 500, rot.per=0.35, colors = brewer.pal(8, "Dark2"))


#MODELLING=============

#Split the emails dataset into training and test parts
set.seed(123)
split_df = sample.split(emails_df$spam, 0.8)
train_df = subset(emails_df, split_df == TRUE)
test_df = subset(emails_df, split_df == FALSE)


#Now let's apply different models to the email dataset and check the performance of each

#1---------------LOGISTIC REGRESSION
logistic_model_emails = glm(spam~., data=train_df, family="binomial")
summary(logistic_model_emails)



#CART MODEL
library(rpart)
library(rpart.plot)
card_model_emails = rpart(spam~., data=train, method="class")
prp(card_model_emails)

