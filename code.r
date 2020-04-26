library(rtweet)
#devtools::install_github("hrbrmstr/nominatim")
library(nominatim)
news_data <- read_twitter_csv("india_news.csv",unflatten = FALSE)
vec_index = 1
username <- c()
tweet    <-c()
activity <-c()
verified <-c()
country  <-c()
followers <-c()
isfake   <-c()

for (row in 1:nrow(news_data)){
  if(is.na(news_data[row,"location"]) ==  FALSE ) {.
    
    user_loc = nominatim::osm_geocode(unlist(news_data[row,"location"]),key=getOption("OSM_API_KEY","QKD8MG1qHwLyPAgmtZ9MTFAdHaHt1C5a"))
    if(nrow(uer_loc) == 0){ 
      break }
    locat = nominatim::reverse_geocode_coords(user_loc[5],user_loc[6],key=getOption("OSM_API_KEY","QKD8MG1qHwLyPAgmtZ9MTFAdHaHt1C5a"))
    
    username[vec_index]=unlist(news_data[row,"screen_name"])
    tweet[vec_index]=unlist(news_data[row,"text"])
    if(nrow(subset(news_data,news_data$screen_name==username[row]) ) > 17){
      activity[vec_index]="Suspecious"
    }
    else{
      activity[vec_index]="Normal"
    }
    if(unlist(news_data[row,"verified"] == TRUE)){
      verified[vec_index]= "YES"
    }else {
      verified[vec_index]= "NO"
    }
    country[vec_index]=locat["country"]
    followers[vec_index]=unlist(news_data[row,"followers_count"])
    isfake[vec_index]= "NO"
    vec_index=vec_index+1
  }
  
}
training_data = data.frame(username,tweet,activity,verified,country,followers,isfake);
save_as_csv(training_data, "training_data.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

