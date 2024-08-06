#Load the required packages

library(readxl)
library(xlsx)
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


#pre-process the corpus using standard text mining steps