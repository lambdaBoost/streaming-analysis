#THIS WILL BE APPENDED ONTO THE TASK 2 SCRIPT SO REQUIRES THE DATASET TO BE LOADED FIRST

#TODO - curently predicts on training set only - needto make a stream for the test set

library(ggplot2)
library(data.table)
library(RMOA)
library(stream)

#ctrl <- MOAoptions(model = "OCBoost", randomSeed = 57, ensembleSize = 25,
#                   smoothingParameter = 0.5)

ctrl <- MOAoptions(model = "HoeffdingTree", leafprediction = "MC",
                   removePoorAtts = TRUE, binarySplits = TRUE, tieThreshold = 0.20)

#build online corrdinated boosting model
mymodel <- HoeffdingTree(control = ctrl)

# Create a stream 
dfStream <-datastream_dataframe(data=as.data.table(df_train))
# define some variables to be used in the loop
chunk <- 100
turns <- (nrow(dfStream$data)/chunk)-1
turns <- floor(turns)
position <- chunk

## first sample (train) - retrieves first 100 records ##
sample <- dfStream$get_points(dfStream, n = chunk,
                              outofpoints = c("stop", "warn", "ignore"))



## Train the first chunk
myboostedclassifier <- trainMOA(model = mymodel,
                                formula = won ~.,
                                data = datastream_dataframe(sample))

#train over the stream
for (i in 1:turns){
  # next sample
  sample <- dfStream$get_points(dfStream, n = chunk,
                                outofpoints = c("stop", "warn", "ignore"))
  ## Update the trained model with the new chunks
  myboostedclassifier <- trainMOA(model = myboostedclassifier$model,
                                  formula = won ~ .,
                                  data = datastream_dataframe(sample),
                                  reset = FALSE,trace = FALSE)
  cat("chunk: ",i,"\n")
}


#evaluating the model
#####

#make a test stream
test_stream <- datastream_dataframe(data=as.data.table(df_test))
# define some variables to be used in the loop
chunk <- 100
turns <- (nrow(test_stream$data)/chunk)-1
turns <- floor(turns)
position <- chunk

sample <- test_stream$get_points(test_stream, n = chunk,
                                 outofpoints = c("stop", "warn", "ignore"))

## Do some prediction to test the model
predictions <- predict(myboostedclassifier, sample)
table(sprintf("Actuals: %s", sample$won),
      sprintf("Predicted: %s", predictions))
# calculate accuracy
cat("Accuracy is: ", sum(predictions==sample$won)/nrow(sample)*100,"%")

# hold results in a vector
accuracies <- c()
test_stream$reset()
for (i in 1:turns) {
  # next sample
  sample <- test_stream$get_points(test_stream, n = chunk,
                                outofpoints = c("stop", "warn", "ignore"))
  predictions <- predict(myboostedclassifier, sample)
  # calculate accuracy
  accuracies[i] <- sum(predictions==sample$won)/nrow(sample)*100
  cat(accuracies[i],"%","\n")
}

#plot accuracies for each chunk
plot(accuracies,type='l',col='red',
     xlab="Chunk Number",ylab="Accuracy",frame=FALSE)
