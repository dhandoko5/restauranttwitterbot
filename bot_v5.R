#!/usr/bin/Rscript

library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(RSQLite)
library(reshape2)
library(stringi)
library(stringr)
library(RCurl)
library(httpuv)
library(XML)
library(dplyr)

#Twitter login credentials
consumer_key = "1b6drS60WgpwW89art25qd2Uq"
consumer_secret = "dAix9LspW8Mmq6FEd1LmOiv6m0S5ojSQDzP4XOfxORPb6sK3U8"
access_token = "750634166136569856-YwDEaMMTEfPB9lF7hyHYNGg93ALiifL"
access_secret = "JCtKkOEDbDePhPOBdLAjcJsa880YotTjCA7nlRLC4qASZ"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Yelp login credentials
consumerKey.yelp = "kPeX84B33Hsj68E2MjNJsw"
consumerSecret.yelp = "pESo_IODJ8_tKNNPuKnSkkMnq4g"
token.yelp = "HcUaHBKYHBgyyvafzz0XkGKQrc4M2EcM"
token_secret.yelp = "Kb36oec8VmmrHCWfSL4PYVfADg8"

# Authorization for Yelp
require(httr)
require(httpuv)
require(jsonlite)
myapp = oauth_app("YELP", key=consumerKey.yelp, secret=consumerSecret.yelp)
sig=sign_oauth1.0(myapp, token=token.yelp,token_secret=token_secret.yelp)

#Create vector of searchable cuisines
foods = c('chinese', 'mexican', 'italian', 'japanese', 'greek', 'french', 'thai', 'spanish'
          , 'mediterranean', 'indian', 'seafood', 'pizza', 'burgers', 'bbq', 'barbeque', 'korean')

#reset all variables
zip_codes = 0
id = 0
food = 0
dollar_amounts = 0

#If first time running, run this code
#db <- datasetsDb()
#twitteR::register_db_backend(db)
#dbConnect(db)
#tweets = mentions()
#store_tweets_db(tweets, table_name = "tweets_database")
#tweets_db = load_tweets_db(as.data.frame = TRUE, table_name = "tweets_database")

#If not first time running, run this code
#Store into database
db <- datasetsDb()
twitteR::register_db_backend(db)
dbConnect(db)
tweets_db = load_tweets_db(as.data.frame = TRUE, table_name = "tweets_database")
placeholder = get_latest_tweet_id("tweets_database")
#gets tweets after time t
tweets = mentions(sinceID = placeholder)
store_tweets_db(tweets, table_name = "tweets_database")
tweets_db = load_tweets_db(as.data.frame = TRUE, table_name = "tweets_database")

#Get the tweet IDs
id = tweets_db$id[which(tweets_db$id > placeholder)]

#Extract individual usernames from db
user_name = tweets_db$screenName[which(tweets_db$id > placeholder)]
user_name = melt(user_name, value.name = "username")

#Extract dollar amounts from text in db
dollar_amounts = str_extract_all(tweets_db$text[which(tweets_db$id > placeholder)], "\\$[0-9]+", simplify = TRUE)
dollar_amounts = melt(dollar_amounts, value.name = "dollar amount")
#clean dollar_amounts
dollar_amounts$Var1 = NULL
dollar_amounts$Var2 = NULL
dollar_amounts[,1] = as.character(dollar_amounts[,1])

#parse out the food cuisine that the user specifies
food = data.frame(food = as.character(), stringsAsFactors = FALSE)
for(i in 1:length(tweets_db$text[which(tweets_db$id > placeholder)])){
  for(k in 1:length(foods)){
    if(grepl(foods[k],tolower(tweets_db$text[length(tweets_db$text) + 1 - i])) == TRUE){
      food[length(tweets_db$text[which(tweets_db$id > placeholder)]) + 1 - i , 1] = foods[k]
      break
    }
    else{
      food[length(tweets_db$text[which(tweets_db$id > placeholder)]) + 1 - i , 1] = "NA"
    }
  }
}

#Extract locations from db
zip_codes = str_extract_all(tweets_db$text[which(tweets_db$id > placeholder)], "[0-9]{4,5}", simplify = TRUE)
zip_codes = melt(zip_codes, value.name = "zip code")
#clean zip_codes
zip_codes$Var1 = NULL
zip_codes$Var2 = NULL
zip_codes[,1] = as.character(zip_codes[,1])

#empty zip_codes set to NA
for(i in 1:length(zip_codes[,1])){
  if(isTRUE(zip_codes[i,1] == "" | is.na(zip_codes[i,1]))){
    zip_codes[i,1] = "NA"
  }
}

#empty dollar_amounts set to NA
for(i in 1:length(dollar_amounts[,1])){
  if(isTRUE(dollar_amounts[i,1] == "" | is.na(dollar_amounts[i,1]))){
    dollar_amounts[i,1] = "NA"
  }
}

#compile everything into dataframe to pass to yelp
full_info = data.frame(user_name, zip_codes, dollar_amounts, id, food)

#full_info PASSES ON TO YELP API. BELOW IS EVERYTHING YELP RELATED. 

#Number of search terms
limit <- 2

#Convert price to Yelp $/$$/$$$ range
full_info_yelp <- full_info
for (i in 1:length(full_info$username)) {
  if (isTRUE(full_info_yelp[i,3] == "NA")) next
  full_info_yelp[i,3] <- str_replace_all(as.String(full_info_yelp[i,3]), "[\\$]", "")
  if (as.integer(full_info_yelp[i,3]) < 15) {
    full_info_yelp[i,3] = as.String(2.1)
  }  else if (as.integer(full_info_yelp[i,3]) < 45) {
    full_info_yelp[i,3] =  as.String(2.2)
  } else full_info_yelp[i,3] = as.String(2.3)
}

#Create Yelp html
mutate(full_info_yelp, yelpurl = 0)
for (i in 1:length(full_info$username)) {
  yelpurl <- paste0("http://api.yelp.com/v2/search/?limit=",limit)
  if (isTRUE((full_info_yelp[i,2]) != "NA")) {
    yelpurl <- paste0(yelpurl,"&location=", full_info_yelp[i,2])
  }
  if (isTRUE((full_info_yelp[i,5]) != "NA")) {
    yelpurl <- paste0(yelpurl,"&term=", full_info_yelp[i,5])
  }
  if (isTRUE((full_info_yelp[i,3]) != "NA")) {
    yelpurl <- paste0(yelpurl,"&attrs=RestaurantsPriceRange", full_info_yelp[i,3])
  }
  yelpurl <- encodeURI(as.String(yelpurl))
  full_info_yelp[i,6] <- yelpurl
}

#Return results and post to Twitter for each response received
for (i in 1:length(full_info_yelp$username)) {
  yelp_url <- full_info_yelp[i,6]
  locationdata=GET(yelp_url, sig)
  locationdataContent = content(locationdata)
  locationdataList=jsonlite::fromJSON(toJSON(locationdataContent))

  tweet(paste0("@", full_info$username, " ",locationdataList$businesses$name, " ", locationdataList$businesses$url), inReplyTo = full_info$id, bypassCharLimit = TRUE)
}