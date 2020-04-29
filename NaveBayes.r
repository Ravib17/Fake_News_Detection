#To read dataset from csv
twitter_dataset <- read.csv(file.choose(),header= T,stringsAsFactors = FALSE)

#converting numeric to nominal data

#twitter_dataset$followers <- cut(twitter_dataset$followers, breaks = c(0, 50, 200, 700000000), labels = c("Less", "Medium", "High"))

#factorize character value
twitter_dataset$activity <- as.factor(twitter_dataset$activity)
twitter_dataset$twt_source <- as.factor(twitter_dataset$twt_source)
twitter_dataset$verified <- as.factor(twitter_dataset$verified)
twitter_dataset$country <- as.factor(twitter_dataset$country)
twitter_dataset$followers <- as.factor(twitter_dataset$followers)
twitter_dataset$isfake <- as.factor(twitter_dataset$isfake)

# drop missing values ie values with NA
twitter_dataset <- na.omit(twitter_dataset)

#take size of 80% for dividing rows in 80 : 20
train_size <- floor(0.80 * nrow(twitter_dataset))

tweet_index <- sample(seq_len(nrow(twitter_dataset)), size = train_size)

#splitting into 80:20
training_data <- twitter_dataset[tweet_index,]
testing_data <- twitter_dataset[-tweet_index,]


set.seed(1235)
library(e1071)
sapply(training_data,class)
tweets_NB_classifier <- naiveBayes(isfake~., data = training_data)
tweets_NB_predict <- predict(tweets_NB_classifier,testing_data)
library(gmodels)
CrossTable(tweets_NB_predict,testing_data$isfake,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted','actual'))
confusion_mat <- table(testing_data$isfake, tweet_predict)
accuracy_Tree <- sum(diag(confusion_mat)) / sum(confusion_mat)
accuracy_Tree <- accuracy_Tree * 100.0;
print(paste('Accuracy ', accuracy_Tree, '%.'))