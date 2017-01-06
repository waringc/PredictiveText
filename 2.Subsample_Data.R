#############################################
#The script loads in the RDS files of the input data
#Input: RDS files from 1.Load_Data.R
#Output: Three RDS files of data for training, cross validation and testing.
#
#The data is than split into three sets for training, cross validation
#and test (unfortunatly I never got far enough to cross validate).
#The script ensures there are no repeat observations in the three splits.
#


set.seed(2016)
#load data
twitter = readRDS("Twitter_Raw.RDS")
news = readRDS("News_Raw.RDS")
blogs = readRDS("Blogs_Raw.RDS")

#Randomize the order of each data set using sample
twitter<-sample(twitter)
news<-sample(news)
blogs<-sample(blogs)

all_data= list(twitter, news, blogs)

#set fractions if data to appear in each of the three splits
train_fraction=0.60
cv_fraction=0.001
test_fraction= 0.001

#create empty data sets
train=c()
cv=c()
test=c()

for (source in all_data){
  #Determine length of fraction
  train_length = floor(train_fraction*(length(source)))
  cv_length = train_length+floor(cv_fraction*(length(source)))
  test_length = cv_length+floor(test_fraction*(length(source)))
  
  #place data in proper list
  train=c(train,source[1:train_length])
  cv=c(cv,source[(train_length+1):cv_length])
  test=c(test,source[(cv_length+1):test_length])
}

#Save an RDS
saveRDS(train,file="Train_Data_60.RDS")
saveRDS(cv,file="CV_Data_001.RDS")
saveRDS(test,file="Test_Data_001.RDS")