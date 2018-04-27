#THIS WILL BE APPENDED ONTO THE TASK 2 SCRIPT SO REQUIRES THE DATASET TO BE LOADED FIRST

#TODO - curently predicts on training set only - needto make a stream for the test set

library(ggplot2)
library(data.table)
library(RMOA)
library(stream)

#won't usea validation set here
df_test <- bind_rows(df_test,df_valid)


#ctrl <- MOAoptions(model = "OCBoost", randomSeed = 57, ensembleSize = 25,
#                   smoothingParameter = 0.5)

#ctrl <- MOAoptions(model = "HoeffdingTree", leafprediction = "MC",
#                   removePoorAtts = TRUE, binarySplits = TRUE, tieThreshold = 0.20)

#build online corrdinated boosting model
#mymodel <- HoeffdingTree(control = ctrl)

mymodel <- OzaBoost(baseLearner="trees.HoeffdingTree", ensemblesize=30)

# Create a stream 
dfStream <-datastream_dataframe(data=as.data.table(df_train))
# other variables for training
chunk <- 100
#use the chunk size and size of the training set to define the number of turns required for training
turns <- (nrow(dfStream$data)/chunk)-1
turns <- floor(turns)


#make a test stream
test_stream <- datastream_dataframe(data=as.data.table(df_test))
# other variables for testing
test_chunk <- floor(100 / (nrow(df_train)/nrow(df_test)))


test_sample <- test_stream$get_points(test_stream, n = test_chunk,
                                      outofpoints = c("stop", "warn", "ignore"))



## first sample (train) - retrieves first 100 records ##
sample <- dfStream$get_points(dfStream, n = chunk,
                              outofpoints = c("stop", "warn", "ignore"))



## Train the first chunk
myboostedclassifier <- trainMOA(model = mymodel,
                                formula = won ~.,
                                data = datastream_dataframe(sample))


## Do some prediction to test the model
predictions <- predict(myboostedclassifier, test_sample)
table(sprintf("Actuals: %s", test_sample$won),
      sprintf("Predicted: %s", predictions))
# calculate accuracy
cat("Accuracy is: ", sum(predictions==test_sample$won)/nrow(test_sample)*100,"%")


# hold results in a vector
accuracies <- c()
test_stream$reset()

#train over the stream
for (i in 1:turns){
  # next sample
  sample <- dfStream$get_points(dfStream, n = chunk,
                                outofpoints = c("stop", "warn", "ignore"))
  test_sample <- test_stream$get_points(test_stream, n = test_chunk,
                                        outofpoints = c("stop", "warn", "ignore"))
  ## Update the trained model with the new chunks
  myboostedclassifier <- trainMOA(model = myboostedclassifier$model,
                                  formula = won ~ .,
                                  data = datastream_dataframe(sample),
                                  reset = FALSE,trace = FALSE)
  cat("chunk: ",i,"\n")
  
  predictions <- predict(myboostedclassifier, test_sample)
  # calculate accuracy
  accuracies[i] <- sum(predictions==test_sample$won)/nrow(test_sample)*100
  if(i %% 100 = 0){
  cat("chunk ",i," accuracy = ",accuracies[i],"%","\n")
  }
}


#evaluating the model
#####



#for (i in 1:turns) {
  # next sample
#  test_sample <- test_stream$get_points(test_stream, n = test_chunk,
#                                outofpoints = c("stop", "warn", "ignore"))
#  predictions <- predict(myboostedclassifier, test_sample)
  # calculate accuracy
#  accuracies[i] <- sum(predictions==test_sample$won)/nrow(test_sample)*100
#  cat(accuracies[i],"%","\n")
#}

#plot accuracies for each chunk
plot(accuracies,type='l',col='red',
     xlab="Chunk Number",ylab="Accuracy",frame=FALSE)


accuracies <- as.data.frame(accuracies)
ggplot(accuracies,aes(y=accuracies,x=seq(1,length(accuracies))))+
  geom_line(col="blue",alpha=0.7)+
  geom_smooth(method="loess",col="red")+
  labs(title="accuracy of streaming classifier",subtitle="trendline shown in red")+
  xlab("chunk")+
  ylab("accuracy")
