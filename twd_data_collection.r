#######################################################################################################################
   # Set up environment #
#######################################################################################################################

options(scipen = 999)
install.packages('academictwitteR')
library(tidyverse)
library(academictwitteR)
library(httr)
library(urltools)
library(stringr)
library(purrr)
library(data.table)
library(jsonlite)
library(scales)
library(urltools)

#######################################################################################################################
   # Get all english-language tweets discussing covid-19 in the period of interest #
#######################################################################################################################

covid_tweets <-
  get_all_tweets(
    query = "covid OR covid19 OR covid-19 OR coronavirus OR sars-cov-2 OR sarscov2 OR sarscov-2 OR sars-cov2",
    start_tweets = "2023-01-15T00:00:00Z",
    end_tweets = "2023-01-30T00:00:00Z",
    data_path = "twd_raw_data_covid/",
    bearer_token = "bearer_token",
    bind_tweets = FALSE,
    lang = "en",
    n= 10000000
  )

  climate_tweets <-
  get_all_tweets(
    query = "climate change OR global warming OR greenhouse gas OR climate crisis",
    start_tweets = "2023-01-15T00:00:00Z",
    end_tweets = "2023-01-30T00:00:00Z",
    data_path = "twd_raw_data/tw_raw_data_climate/",
    bearer_token = "bearer_token",
    bind_tweets = FALSE,
    lang = "en",
    n= 10000000
  )

# Extract data from the json files - academictwitteR has its own function to bind tweets, but this is much faster
extract_data_fromjson <- function(data_path) {
   file_list <- list.files(path = data_path, pattern = "^data_", full.names = TRUE)
   df_list <- lapply(file_list, function(file) {
   json_data <- fromJSON(file)
  })
   df_unnested <- map_dfr(df_list, as.data.frame)
   return(df_unnested)
  }

covid_tweets = extract_data_fromjson("twd_raw_data/tw_raw_data_covid/")
climate_tweets = extract_data_fromjson("twd_raw_data/tw_raw_data_climate/")

#######################################################################################################################
  # Create a function to flatten the twitter data. This is necessary as the twitter API returns a nested dataframe # 
#######################################################################################################################

flatten_data <- function(nonflat_data) {  

  # Extract URLs as dataframe
  url_list <- lapply(nonflat_data$entities$urls, "[[", "expanded_url")

  links <- sapply(url_list, function(x) {
    if (!is.null(x) && length(x) > 0) {
      paste(x, collapse = ";")
   } else {
     ""
    }
  })

  # Extract Hashtags as dataframe
  hash_list <- nonflat_data$entities$hashtags
  
  hashtags <- rep("",length(hash_list))
  for(i in 1:length(hash_list)){
    if(!is.null(hash_list[[i]]) && "tag" %in% colnames(hash_list[[i]])){
      temp_tags <- c()
      for(j in 1:nrow(hash_list[[i]])){
        temp_tags <- c(temp_tags, hash_list[[i]]$tag[j])
      }
      hashtags[i] <- paste(temp_tags, collapse = ";")
    }
  }

new_df <- data.frame(id = nonflat_data$id, author_id=nonflat_data$author_id, text=nonflat_data$text, urls=links, hashtags=hashtags, retweet_count=nonflat_data$public_metrics$retweet_count,reply_count=nonflat_data$public_metrics$reply_count, like_count=nonflat_data$public_metrics$like_count, quote_count=nonflat_data$public_metrics$quote_count, impressions_count=nonflat_data$public_metrics$impression_count, created_at=nonflat_data$created_at)

new_df <- as.data.frame(new_df)
return(new_df)
}

covid_tweets_flat <- flatten_data(covid_tweets)
climate_tweets_flat <- flatten_data(climate_tweets)

#Remove retweets from main dataset
covid_tweets_flat <- covid_tweets_flat[!grepl("^RT", covid_tweets_flat$text), ]
climate_tweets_flat <- climate_tweets_flat[!grepl("^RT", climate_tweets_flat$text), ]

#######################################################################################################################
  # Extract posts with low-trust domains # 
#######################################################################################################################
get_lowtrust_tweets <- function(tweets_flat, low_trust_domains_filepath) {
  
  # Load low-trust domains data
  low_trust_domains <- read.csv(low_trust_domains_filepath, header = TRUE, stringsAsFactors = FALSE)
  
  # Convert URLs to domains before matching with low-trust domains data
  tweets_flat$urls <- strsplit(as.character(tweets_flat$urls), ";")
  tweets_flat$domains <- lapply(tweets_flat$urls, function(x) domain(x))
  tweets_flat$domains <- lapply(tweets_flat$domains, function(x) gsub("^www.","",x))
  
  # Extract posts with at least one low-trust domain
  tweets_flat$matches <- tweets_flat$domains %in% low_trust_domains$Domain.name
  matched_tweets <- tweets_flat %>% filter(matches == TRUE)
  matched_tweets <- select(matched_tweets, everything())
  matched_tweets <- as.data.frame(matched_tweets)

  return(matched_tweets)
}
covid_lowtrust_tweets <- get_lowtrust_tweets(covid_tweets_flat, 'twd_processed_data/iffy_news_urls.csv')
climate_lowtrust_tweets <- get_lowtrust_tweets(climate_tweets_flat, 'twd_processed_data/iffy_news_urls.csv')
fwrite(covid_lowtrust_tweets, 'twd_processed_data/covid_lowtrust_tweets.csv')
fwrite(climate_lowtrust_tweets, 'twd_processed_data/climate_lowtrust_tweets.csv')

#######################################################################################################################
  # Collect timelines of users who have shared low-trust domains # 
#######################################################################################################################

collect_timelines <- function(lowtrust_tweets, directory) {
  tweets_list <- list()
  for (i in 1:nrow(lowtrust_tweets)) {
    user_name <- lowtrust_tweets[i, "author_id"]
    user_tweets <- get_user_timeline(user_name,
                                     start_tweets = "2023-01-01T00:00:00Z", 
                                     end_tweets = "2023-01-31T00:00:00Z",
                                     bearer_token = bearer_token,
                                     data_path = directory,
                                     bind_tweets = FALSE,
                                     lang = "en",
                                     n=100)
    tweets_list[[i]] <- user_tweets
  }
  return(tweets_list)
}

collect_timelines(covid_lowtrust_tweets,"twd_raw_data/tw_user_timelines_covid/")
collect_timelines(climate_lowtrust_tweets, "twd_raw_data/tw_user_timelines_climate/")

user_timelines_covid <-  extract_data_fromjson("twd_raw_data/tw_user_timelines_covid/")
user_timeline_climate <- extract_data_fromjson("twd_raw_data/tw_user_timelines_climate/")

# Create a different flattening function specific to timelines that ignores URLs. This saves a significant amount of time. 
flatten_timelines <- function(nonflat_data) {  
  # Extract Hashtags as dataframe
  hash_list <- nonflat_data$entities$hashtags
  
  hashtags <- rep("",length(hash_list))
  for(i in 1:length(hash_list)){
    if(!is.null(hash_list[[i]]) && "tag" %in% colnames(hash_list[[i]])){
      temp_tags <- c()
      for(j in 1:nrow(hash_list[[i]])){
        temp_tags <- c(temp_tags, hash_list[[i]]$tag[j])
      }
      hashtags[i] <- paste(temp_tags, collapse = ";")
    }
  }
  new_df <- data.frame(id = nonflat_data$id, author_id=nonflat_data$author_id, text=nonflat_data$text, hashtags=hashtags, retweet_count=nonflat_data$public_metrics$retweet_count, urls = links,
                       reply_count=nonflat_data$public_metrics$reply_count, like_count=nonflat_data$public_metrics$like_count, quote_count=nonflat_data$public_metrics$quote_count,
                       impressions_count=nonflat_data$public_metrics$impression_count, created_at=nonflat_data$created_at)
  return(new_df)
}

user_timelines_covid <- flatten_timelines(user_timelines_covid)
user_timeline_climate <- flatten_timelines(user_timeline_climate)
user_timelines_covid <- user_timelines_covid[!grepl("^RT", user_timelines_covid$text), ]
user_timelines_climate <- user_timelines_climate[!grepl("^RT", user_timelines_climate$text), ]

#######################################################################################################################
  # </end> - Continued in Python #
#######################################################################################################################