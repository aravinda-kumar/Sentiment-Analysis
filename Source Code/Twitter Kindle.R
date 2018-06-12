rm(list=ls())

require("twitteR")||install.packages("twitteR");library(twitteR)
require(stringr)||install.packages("stringr"); library(stringr)
require(wordcloud )||install.packages("wordcloud "); library(stringr)
require(tdm)||install.packages("tdm"); library(tdm)
require(tm)||install.packages("tm"); library(tm)
require("RCurl")||install.packages("RCurl"); library(RCurl)

api_key <- ""   #Consumer key: *

api_secret <- ""   # Consumer secret: *

access_token <- ""  # Access token: 

access_token_secret <- "" # Access token secret: 

# After this line of command type 1 for selection as Yes 
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# KINDLE tweets
tweets_bose = userTimeline("@AmazonKindle", n=2000)



# get text
ver_txt = sapply(tweets_bose, function(x) x$getText())

clean.text = function(x)
{
  # tolower
  x = tolower(x)
  # remove rt
  x = gsub("rt", "", x)
  # remove at
  x = gsub("@\\w+", "", x)
  # remove punctuation
  x = gsub("[[:punct:]]", "", x)
  # remove numbers
  x = gsub("[[:digit:]]", "", x)
  # remove links http
  x = gsub("http\\w+", "", x)
  # remove tabs
  x = gsub("[ |\t]{2,}", "", x)
  # remove blank spaces at the beginning
  x = gsub("^ ", "", x)
  # remove blank spaces at the end
  x = gsub(" $", "", x)
  return(x)
}

ver_clean = clean.text(ver_txt)

#print(ver_clean)

#write.table(ver_clean, file='C:\\ISB/Class/DC/Home_Work/Tweets_Bose.txt', row.names=F, sep="\t")  # export to txt tab delemited file
print(ver_clean)
write.table(ver_clean, file.choose(), row.names=F, col.names=F)
