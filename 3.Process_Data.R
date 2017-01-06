#############################################
#Script to perform processing to produce data for prediction app
#Input: Sampled RDS files from 2.Subsample_Data.R
#Output: RDS file containing n-grams and probabilites of their next word
#


#Process Data 
library(quanteda)
library(stringr)
library(dplyr)
library(data.table)

# #Function to apply preprocessing functions to a corpus
create_corpus= function(data){
  #tokenize data using quanteda
  data_token<-tokenize(data,what=c("word"), removeNumbers=TRUE, removePunct = TRUE,
                    removeSymbols = TRUE, removeSeparators = TRUE, removeTwitter = TRUE,
                    removeHyphens = TRUE, removeURL = TRUE, ngrams = 1L, skip = 0L,
                    concatenator = "_", simplify = FALSE, verbose = TRUE)
  data_token = toLower(data_token)
  #data_token = removeFeatures(data_token, c(stopwords("english")))
  
  #create ngrams
  unigram<-ngrams(data_token, n = 1)
  bigram<-ngrams(data_token, n = 2)
  trigram<-ngrams(data_token, n = 3)
  fourgram<-ngrams(data_token, n = 4)
  
  unigram<-as.data.table(table(unlist(unigram)))
  bigram<-as.data.table(table(unlist(bigram)))
  trigram<-as.data.table(table(unlist(trigram)))
  fourgram<-as.data.table(table(unlist(fourgram)))
  
  #Only use most common bi, tri and fourgrams 
  #ie with more than minimum number of instances in the data
  #in this case 3 for unigram and bigrams, and 2 for trigram and bigram
  unigram<-unigram[unigram$N>3,]
  bigram<-bigram[bigram$N>3,]
  trigram<-trigram[trigram$N>2,]
  fourgram<-fourgram[fourgram$N>2,]
  
  setnames(unigram,c("stem","count"))
  
  #For each ngram, remove the last word which will be
  #defined as the "target" next word
  
  #bigram
  bigram[,stem:=as.character(lapply(bigram$V1,word, start = 1, sep="_"))]
  bigram[,target:=as.character(lapply(bigram$V1,word, start = -1, sep="_"))]
  bigram[,V1:=NULL]
  setnames(bigram,c("count","stem","target"))
  
  #trigram
  trigram[,stem:=lapply(trigram$V1,word, start = 1:2, sep="_")]
  trigram[,stem:=as.character(lapply(trigram$stem,paste, collapse="_"))]
  trigram[,target:=as.character(lapply(trigram$V1,word, start = -1, sep="_"))]
  trigram[,V1:=NULL]
  setnames(trigram,c("count","stem","target"))
  
  #fourgram
  fourgram[,stem:=lapply(fourgram$V1,word, start = 1:3, sep="_")]
  fourgram[,stem:=as.character(lapply(fourgram$stem,paste, collapse="_"))]
  fourgram[,target:=as.character(lapply(fourgram$V1,word, start = -1, sep="_"))]
  fourgram[,V1:=NULL]
  setnames(fourgram,c("count","stem","target"))
  
  ##For bigram trigram and fourgram convert raw counts into probabilities
  bigram[,total_sum:=sum(count), by=stem]
  bigram[,probability:=count/total_sum]
  bigram[,count:=NULL]
  bigram[,total_sum:=NULL]
  
  trigram[,total_sum:=sum(count), by=stem]
  trigram[,probability:=count/total_sum]
  trigram[,count:=NULL]
  trigram[,total_sum:=NULL]
  
  fourgram[,total_sum:=sum(count), by=stem]
  fourgram[,probability:=count/total_sum]
  fourgram[,count:=NULL]
  fourgram[,total_sum:=NULL]
  
  list(unigram,bigram,trigram,fourgram)
}

train = readRDS("Train_Data_60.RDS")


#Train
train_ngrams <- create_corpus(unlist(train))
train_unigram<-train_ngrams[[1]]
train_bigram<-train_ngrams[[2]]
train_trigram<-train_ngrams[[3]]
train_fourgram<-train_ngrams[[4]]


#For each n-gram stem only keep the top ten most probable target next words
#done to reduce size of data and to speed up model
train_bigram <- train_bigram[ave(-train_bigram$probability, train_bigram$stem, FUN = rank) <= 10, ]
train_trigram <- train_trigram[ave(-train_trigram$probability, train_trigram$stem, FUN = rank) <= 10, ]
train_fourgram <- train_fourgram[ave(-train_fourgram$probability, train_fourgram$stem, FUN = rank) <= 10, ]

save(train_unigram, train_bigram,train_trigram,train_fourgram, file = "train_processed_60Set_withstop.RData")

#Optional processing for CV and test which I never completed

#cv = readRDS("CV_Data.RDS")
#test = readRDS("Test_Data.RDS")

# #Cross Validation
# cv_ngrams <- create_corpus(unlist(cv))
# cv_unigram<-cv_ngrams[[1]]
# cv_bigram<-cv_ngrams[[2]]
# cv_trigram<-cv_ngrams[[3]]
# cv_fourgram<-cv_ngrams[[4]]
# 
# save(cv_unigram, cv_bigram,cv_trigram,cv_fourgram, file = "cv_processed_nostop.RData")

# #Test
# test_ngrams <- create_corpus(unlist(test))
# test_unigram<-test_ngrams[[1]]
# test_bigram<-test_ngrams[[2]]
# test_trigram<-test_ngrams[[3]]
# test_fourgram<-test_ngrams[[4]]
# 
# save(test_unigram, test_bigram,test_trigram,test_fourgram, file = "test_processed_nostop.RData")