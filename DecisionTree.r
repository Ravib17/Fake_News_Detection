#To read dataset from csv
twitter_dataset <- read.csv(file.choose(),header= T,stringsAsFactors = FALSE)

#converting numeric to nominal data
twitter_dataset$followers <- cut(twitter_dataset$followers, breaks = c(0, 50, 200, 700000000), labels = c("Less", "Medium", "High"))

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

#for decision tree
library(C50)

#determining class of all columns in training_data dataframe
sapply(training_data,class)

#training_data where column 3 to 5 are deciding final class label
decisionTreeModel <- C50 :: C5.0(training_data[,c(3:5)], training_data$isfake)
decisionTreeModel
summary(decisionTreeModel) 

#to draw decision tree
plot(decisionTreeModel)
testing_data$activity <- as.factor(testing_data$activity)
testing_data$activity

#prediction on testing data
tweet_predict <- predict(decisionTreeModel, newdata = testing_data[,c(3:5)], type = "class")

#If want prediction on the basis of probability
#tweet_predict <- predict(decisionTreeModel, newdata = testing_data[,c(3:5)], type = "prob")

#for getting accuracy of model
confusion_mat <- table(testing_data$isfake, tweet_predict)
confusion_mat
accuracy_Tree <- sum(diag(confusion_mat)) / sum(confusion_mat)
accuracy_Tree <- accuracy_Tree * 100.0;
print(paste('Accuracy of decision tree is ', accuracy_Tree, '%.'))

#******************************************************************************
#Applying boosting with multiple trials
tree_with_trials <- C5.0(training_data[,c(3:5)], y = training_data$isfake, trials = 15)
summary(tree_with_trials)

tweet_predict <- predict(tree_with_trials, newdata = testing_data[,c(3:5)], type = "class")

#for getting accuracy of model ie(accuracy = (TP + TN) / (TP + TN + FP + FN))
confusion_mat <- table(testing_data$isfake, tweet_predict)
confusion_mat
accuracy_Tree <- sum(diag(confusion_mat)) / sum(confusion_mat)
accuracy_Tree <- accuracy_Tree * 100.0;
print(paste('Accuracy of decision tree is ', accuracy_Tree, '%.'))