#############################################
#The script loads in the RDS files of the input data
#Input: Raw text files of text from twitter, news sources and blogs
#Output: Three RDS files for twitter, news sources and blogs
#
#Load the provided text files into data frames and save as RDS to allow 
#for the files to be quickly loaded into R



TWITTER =  "~/Desktop/Data Science/JHU-Data Science/Coursework/Capstone/final/en_US/en_US.twitter.txt"
NEWS =  "~/Desktop/Data Science/JHU-Data Science/Coursework/Capstone/final/en_US/en_US.news.txt"
BLOGS =  "~/Desktop/Data Science/JHU-Data Science/Coursework/Capstone/final/en_US/en_US.blogs.txt"

#Read in data and combine
twitter_data <- readLines(TWITTER, skipNul=TRUE)
twitter_data <-strsplit(twitter_data,'\n')

news_data <- readLines(NEWS, skipNul=TRUE)
news_data <-strsplit(news_data,'\n')

blogs_data <- readLines(BLOGS,skipNul=TRUE)
blogs_data <-strsplit(blogs_data,'\n')

saveRDS(twitter_data,file="Twitter_Raw.RDS")
saveRDS(news_data,file="News_Raw.RDS")
saveRDS(blogs_data,file="Blogs_Raw.RDS")