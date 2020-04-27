library(rtweet)
#devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
news_data <- read_twitter_csv("india_news.csv",unflatten = FALSE)
vec_index = 1
username <- c()
tweet    <-c()
twt_source  <- c()
activity <-c()
verified <-c()
country  <-c()
followers <-c()
isfake   <-c()

for (row in 1:20){
  if(is.na(news_data[row,"location"]) ==  FALSE ) {
    
    user_loc = nominatim::osm_geocode(unlist(news_data[row,"location"]),key=getOption("OSM_API_KEY","QKD8MG1qHwLyPAgmtZ9MTFAdHaHt1C5a"))
    if(nrow(user_loc) == 0){ next }
    locat = nominatim::reverse_geocode_coords(user_loc[5],user_loc[6],key=getOption("OSM_API_KEY","QKD8MG1qHwLyPAgmtZ9MTFAdHaHt1C5a"))
    
    username[vec_index]=unlist(news_data[row,"screen_name"])
    tweet[vec_index]=unlist(news_data[row,"text"])
    if(nrow(subset(news_data,news_data$screen_name==username[row]) ) > 17){
      activity[vec_index]="Suspecious"
    }
    else{
      activity[vec_index]="Normal"
    }
    vector_twt_src <- c("Twitter for Android", "Twitter Web App", "Twitter for iPhone", "Twitter Web Client", "Twitter for iPad")
    if(news_data[row,"source"] %in% vector_twt_src) {
      twt_source[vec_index] = "Normal"  
    }
    else {
      twt_source[vec_index] = "Suspecious"
    }
    
    if(unlist(news_data[row,"verified"] == TRUE)){
      verified[vec_index]= "YES"
    }else {
      verified[vec_index]= "NO"
    }
    country[vec_index]=unlist(locat["country"])
    
   if(unlist(news_data[row,"followers_count"]) < 50){
     followers[vec_index]="Low"
   }else if(unlist(news_data[row,"followers_count"]) < 1000) {
     followers[vec_index]="Medium"
   }else {
     followers[vec_index]="High"
   }
    isfake[vec_index]= "NO"
    vec_index=vec_index+1
  }
}
  
#View(tweets_data)
tweets_data = data.frame(username,tweet,twt_source,activity,verified,country,followers,isfake)# drop missing values ie values with NA
tweets_data <- na.omit(tweets_data)

#take size of 80% for dividing rows in 80 : 20
train_size <- floor(0.80 * nrow(tweets_data))

tweet_index <- sample(seq_len(nrow(tweets_data)), size = train_size)

#splitting into 80:20
training_data <- tweets_data[tweet_index,]
testing_data <- tweets_data[-tweet_index,]

#View(training_data)
#View(testing_data)

save_as_csv(training_data, "training_data1.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
save_as_csv(testing_data, "testing_data1.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

chunks = extractChunks("r u able to understand google how to use it")

library("NLP")
library("openNLP")
extractChunks <- function(x) {
  
  x <- as.String(x)
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  tokenizedAndTagged <- data.frame(Tokens = x[POSwords], Tags = tags)
  
  tokenizedAndTagged$Tags_mod = grepl("NN|JJ", tokenizedAndTagged$Tags)
  chunk = vector()
  
  chunk[1] = as.numeric(tokenizedAndTagged$Tags_mod[1])
  
  for (i in 2:nrow(tokenizedAndTagged)) {
    
    if(!tokenizedAndTagged$Tags_mod[i]) {
      chunk[i] = 0
    } else if (tokenizedAndTagged$Tags_mod[i] == tokenizedAndTagged$Tags_mod[i-1]) {
      chunk[i] = chunk[i-1]
    } else {
      chunk[i] = max(chunk) + 1
    }
    
  }
  
  text_chunk <- split(as.character(tokenizedAndTagged$Tokens), chunk)
  tag_pattern <- split(as.character(tokenizedAndTagged$Tags), chunk)
  names(text_chunk) <- sapply(tag_pattern, function(x) paste(x, collapse = "-"))
  
  # Extract chunks matching pattern
  res = text_chunk[grepl("JJ-NN|NN.-NN", names(text_chunk))]
  res = sapply(res, function(x) paste(x, collapse =  " "))
  print(res)
  return(res)
  
  gc()
  
}

GoogleHits <- function(input)
{
  require(XML)
  require(RCurl)
  url <- paste("https://www.google.com/search?q=",
               input, sep = "") # modified line      
  CAINFO = paste(system.file(package="RCurl"), "/CurlSSL/ca-bundle.crt", sep = "")
  script <- getURL(url, followlocation = TRUE, cainfo = CAINFO)
  doc <- htmlParse(script)
  res <- xpathSApply(doc, '//*/div[@id="resultStats"]', xmlValue)
  cat(paste("\nYour Search URL:\n", url, "\n", sep = ""))
  cat("\nNo. of Hits:\n") # get rid of cat text if not wanted
  return(as.integer(gsub("[^0-9]", "", res)))
}
library(httr)
dat<-GET("https://www.googleapis.com/customsearch/v1?key=AIzaSyAUKRDBSuaXvzTEpn13OLkio5NVwTYZhis&cx=017137198569103930420:hpuryupuqkn&q=covid")
