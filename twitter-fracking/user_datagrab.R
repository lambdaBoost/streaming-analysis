#install.packages("twitteR")
library(twitteR)


#read in raw tweets and extract unique names
tweet_data <- read.csv("fracking_dataset.csv")
name_list<-unique(tweet_data$X)
name_list <- gsub("@","",name_list)

#set up api authentication
# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#populate DF of users (sleep when 15 have been read in to prevent api limit exceedance)
users_df <- data.frame("id","name","location","followers")
names(users_df)<- c("id","name","location","followers")

for(i in c(187:length(name_list))) {
  
  tryCatch({
  test <- getUser(name_list[i])

  user_id <- test$id
  user_name <- test$name
  user_location <- test$location
  user_followers <- paste(test$getFollowerIDs(),collapse=",")
  
  if(length(user_followers > 1)){
  
  user_vec <- c(user_id , user_name , user_location , user_followers)

  user_row <- data.frame(id=user_id , name=user_name , location=user_location , followers=user_followers)
  users_df <- bind_rows(users_df,user_row)
  }else{
    print("API calls exceeded")
  }
  },
  
  error=function(err){print('api error')}
  )#end trycatch
 
  if (i%%6 == 0){
    write.csv(users_df,'frac_users.csv')
    Sys.sleep(16*61)
    
  }
  print(i)
  
}