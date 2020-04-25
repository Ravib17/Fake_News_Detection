library(rtweet)
news_data <- read_twitter_csv("india_news.csv",unflatten = FALSE)

username <- c()
tweet    <-c()
activity <-c()
verified <-c()
country  <-c()
followers <-c()
isfake   <-c()

for (row in 1:nrow(news_data)){
  
  username[row]=unlist(news_data[row,"screen_name"])
  tweet[row]=unlist(news_data[row,"text"])
  if(nrow(subset(news_data,news_data$screen_name==username[row]) ) > 17){
    activity[row]="Suspecious"
  }
  else{
    activity[row]="Normal"
  }
  if(unlist(news_data[row,"verified"] == TRUE)){
    verified[row]= "YES"
  }else {
    verified[row]= "NO"
  }
  country[row]=unlist(news_data[row,"location"])
  followers[row]=unlist(news_data[row,"followers_count"])
  isfake[row]= "NO"
  training_data = data.frame(username,tweet,activity,verified,country,followers,isfake);
  save_as_csv(training_data, "training_data.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
}

