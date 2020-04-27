library(rtweet)
#devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
news_data <- read_twitter_csv("palghar_dataset.csv",unflatten = FALSE)
vec_index = 1
username <- c()
tweet    <-c()
twt_source  <- c()
activity <-c()
verified <-c()
country  <-c()
followers <-c()
isfake   <-c()

for (row in 1:300){
  if(is.na(news_data[row,"location"]) ==  FALSE ) {
    
    user_loc = nominatim::osm_geocode(unlist(news_data[row,"location"]),key=getOption("OSM_API_KEY","QKD8MG1qHwLyPAgmtZ9MTFAdHaHt1C5a"))
    if(nrow(user_loc) == 0){ next }
    locat = nominatim::reverse_geocode_coords(user_loc[5],user_loc[6],key=getOption("OSM_API_KEY","QKD8MG1qHwLyPAgmtZ9MTFAdHaHt1C5a"))
    
    username[vec_index]=unlist(news_data[row,"screen_name"])
    tweet[vec_index]=unlist(news_data[row,"text"])
    print(nrow(subset(news_data,news_data$screen_name==username[vec_index])))
    print(nrow(subset(news_data,news_data$screen_name==username[vec_index])) > 7)
    if(nrow(subset(news_data,news_data$screen_name==username[vec_index])) > 7){
     
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
#train_size <- floor(0.80 * nrow(tweets_data))

#tweet_index <- sample(seq_len(nrow(tweets_data)), size = train_size)

#splitting into 80:20
#training_data <- tweets_data[tweet_index,]
#testing_data <- tweets_data[-tweet_index,]

#View(training_data)
#View(testing_data)

save_as_csv(tweets_data, "palghar_dataset_training3.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
#save_as_csv(testing_data, "testing_data1.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")



