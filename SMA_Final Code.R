#################### ACTION ANALYSIS #######################################

library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(rtweet)
library(SnowballC)
library(tm)
library(syuzhet)
consumer_key <- 'xDzqSdCgIxnrQb5erDVYuCET6'
consumer_secret <- 'YcJKbSXSL0poGqhe9ww60g45Ly4UnDl4lLRHnGXOTJJsKB2ftQ'
access_token <- '1091623712963457025-ayN0FajyXpD0SMXAOewzYktx2ssow8'
access_secret <- 'riXb90EZlCQvJyJcW2M5mIognQhz3bX9TRO8tAqSkM656'

create_token(app = "mytwitterapp", 'xDzqSdCgIxnrQb5erDVYuCET6', 
             'YcJKbSXSL0poGqhe9ww60g45Ly4UnDl4lLRHnGXOTJJsKB2ftQ', 
             '1091623712963457025-ayN0FajyXpD0SMXAOewzYktx2ssow8',
             'riXb90EZlCQvJyJcW2M5mIognQhz3bX9TRO8tAqSkM656')
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets <- userTimeline("narendramodi", n=200)

head(tweets)
library(rtweet)
#get freinds 
Friends = get_friends("narendramodi" , n = 200)
Friends
Followers = get_followers("narendramodi", n = 200)
Followers


#Retweets
Retweets = get_retweets("1092709705909071872", n = 20)
Retweets

############################### TEXT ANALYSIS#################################################

#Cleaning data
tweets.df <- twListToDF(tweets) 

head(tweets.df)
tweets.df2 <- gsub("http.*","",tweets.df$text)

tweets.df2 <- gsub("https.*","",tweets.df2)

tweets.df2 <- gsub("#.*","",tweets.df2)

tweets.df2 <- gsub("@.*","",tweets.df2)
#Getting sentiment score for each tweet
#Emotions
word.df <- as.vector(tweets.df2) #convert it into vector

emotion.df <- get_nrc_sentiment(word.df)

emotion.df2 <- cbind(tweets.df2, emotion.df) 

head(emotion.df2)
#Category of Emotions

#Sentiments
sent.value <- get_sentiment(word.df)

most.positive <- word.df[sent.value == max(sent.value)]
most.positive
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative 

#Alternative 
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))

head(category_senti)
table(category_senti)

#################### LOCATION ANALYSIS  ############################################

library("twitteR")
library(leaflet)
library(maps)
library(OpenStreetMap)
consumer_key <- 'xDzqSdCgIxnrQb5erDVYuCET6'
consumer_secret <- 'YcJKbSXSL0poGqhe9ww60g45Ly4UnDl4lLRHnGXOTJJsKB2ftQ'
access_token <- '1091623712963457025-ayN0FajyXpD0SMXAOewzYktx2ssow8'
access_secret <- 'riXb90EZlCQvJyJcW2M5mIognQhz3bX9TRO8tAqSkM656'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets <- userTimeline("narendramodi", n=200)
tweets.df <-twListToDF(tweets)
write.csv(tweets.df, "C:/Users/lab1/Desktop/tweets1.csv") 
#an example of a file extension of the folder in which you want to save the .csv file.
read.csv("C:/Users/lab1/Desktop/tweets1.csv", stringsAsFactors = FALSE)
mymap <- read.csv("C:/Users/lab1/Desktop/tweets.csv", stringsAsFactors = FALSE)
m <- leaflet(mymap) %>% addTiles()
m
m %>% addCircles(longitude ~longitude, latitude ~latitude, popup = mymap$type, weight = 8, radius = 40, color = "#fb3004", stroke = TRUE, fillOpacity = 0.8)

#################### NETWORK ANALYSIS  ############################################

library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(rtweet)

consumer_key <- 'xDzqSdCgIxnrQb5erDVYuCET6'
consumer_secret <- 'YcJKbSXSL0poGqhe9ww60g45Ly4UnDl4lLRHnGXOTJJsKB2ftQ'
access_token <- '1091623712963457025-ayN0FajyXpD0SMXAOewzYktx2ssow8'
access_secret <- 'riXb90EZlCQvJyJcW2M5mIognQhz3bX9TRO8tAqSkM656'
create_token(app = "mytwitterapp", 'Pi9ALzjZbuysh0qUVRF7in8Ay', 'F5DfSv0Skh9UoEAP2h6w6d4myAnhxNe0tLiD1RdkL8y3rfLnkF', '868879323872219137-rVBaR2tgvQ8rjrET12Mll6uN5W1hD2h','ym7VIFpApeYE7GirEI4IAKIGHjQe5zShNWBeT4RQ7xRCo')

tweets <- search_tweets("narendramodi", n = 100)
tweets

modi_profile <- search_users("narendramodi")
modi_profile$name
modi_profile$description
modi_profile$location

my_followers <- get_followers("narendramodi")
glimpse(my_followers)

Friends = get_friends("narendramodi" , n = 200)
Friends
m