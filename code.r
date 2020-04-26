library(rtweet)
news_data <- read_twitter_csv("india_news.csv",unflatten = FALSE)

username <- c()
tweet    <-c()
twt_source  <- c()
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
  vector_twt_src <- c("Twitter for Android", "Twitter Web App", "Twitter for iPhone", "Twitter Web Client", "Twitter for iPad")
  if(news_data[row,"source"] %in% vector_twt_src) {
    twt_source[row] = "Normal"  
  }
  else {
    twt_source[row] = "Suspecious"
  }
  
  if(unlist(news_data[row,"verified"] == TRUE)){
    verified[row]= "YES"
  }else {
    verified[row]= "NO"
  }
  country[row]=unlist(news_data[row,"location"])
  followers[row]=unlist(news_data[row,"followers_count"])
  isfake[row]= "NO"
  tweets_data = data.frame(username,tweet,activity,twt_source,verified,country,followers,isfake);
}
#View(tweets_data)

# drop missing values ie values with NA
tweets_data <- na.omit(tweets_data)

#take size of 80% for dividing rows in 80 : 20
train_size <- floor(0.80 * nrow(tweets_data))

tweet_index <- sample(seq_len(nrow(tweets_data)), size = train_size)

#splitting into 80:20
training_data <- tweets_data[tweet_index,]
testing_data <- tweets_data[-tweet_index,]

View(training_data)
View(testing_data)

save_as_csv(training_data, "training_data.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
