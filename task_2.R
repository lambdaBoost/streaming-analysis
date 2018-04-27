#source of data https://archive.ics.uci.edu/ml/datasets/Dota2+Games+Results

library(dplyr)
library(rjson)
library(caret)
library(randomForest)
library(ROCR)
library(dummies)

#read in the train and test datasets and combine 

df1 <- read.csv('dota_dataset\\dota2Train.csv')
df2 <- read.csv('dota_dataset\\dota2Test.csv')

names(df2) <- names(df1)

df <- rbind(df1,df2)
rm(df1,df2)

#dataset exploration
#######

#the dataset has no column names and is quite sparse
View(head(df))

#also, the types are all wrong - they should all be factors
str(df)

#record number of rows and columns
paste('number of columns =' , ncol(df) , '. number of rows =', nrow(df))

#most columns are not names, but we have a json file availible to correct this later
names(df)

#assess class label distribution - it is quite close to even
table(df[,1])

#check for missing values (there are none)
sum(complete.cases(df))==nrow(df)

#check there are equal number of heroes present in each team (hero values for all rows should sum to zero)
sum((rowSums(df[5:ncol(df)]))) == 0


#convert to factors
#####
df[sapply(df, is.numeric)] <- lapply(df[sapply(df, is.numeric)], as.factor)

#add column names to thte first few columns (https://archive.ics.uci.edu/ml/datasets/Dota2+Games+Results)
names(df)[1:4] <- c('won','cluster_id','game_mode','game_type')

#populate the other column names
#load in json file and use it to populate column names
hero_names <- fromJSON(file ="dota_dataset\\heroes.json")

for ( i in c(1:length(hero_names$heroes))){
  names(df)[i+4] <- hero_names$heroes[[i]]$name
}

#drop this column - it has the same value for every entry - must be a mandatory character
df <- df%>%
  select(-c(lina, cluster_id , game_mode , game_type))
 # select(-c(lina))

#make training and tst sets
#####
set.seed(57)

# Simple into train test and validation sets
index <- sample(c(1:3), size = nrow(df), replace = TRUE, prob = c(.6, .2, .2))
df_train <- df[index == 1,]
df_test <- df[index == 2,]
df_valid <- df[index == 3,]


#train up some classifiers and assess the result
#####
num_attributes <- ncol(df_train)
#build a parameter grid
# Random forest
mtry <- as.integer(c(num_attributes * 0.25, num_attributes / 3, num_attributes * 0.5, num_attributes * 0.8))
nodesize <- c( 3, 5, 20)
ntree <- c(200)

PG<- as.data.frame(expand.grid(mtry=mtry, nodesize=nodesize,
                                         ntree=ntree, stringsAsFactors=F))




#train models
#####
for (i in c(1:nrow(PG))){
  mtry <- PG[i, "mtry"]
  nodesize <- PG[i, "nodesize"]
  ntree <- PG[i, "ntree"]

  temp_model <- randomForest(formula(df_train) , data=df_train, mtry=mtry, ntree=ntree, nodesize=nodesize)
  predictions <- predict(temp_model , df_valid)
  cm<- confusionMatrix(predictions,df_valid$won)
  accuracy <- cm$overall['Accuracy']
  PG[i,'Accuracy'] <- accuracy
  print(i)
}

#assess best model on test set
PG <- PG%>%
  arrange(desc(Accuracy))

mtry <- PG[1, "mtry"]
nodesize <- PG[1, "nodesize"]
ntree <- PG[1, "ntree"]
best_model <- randomForest(formula(df_train) , data=df_train, mtry=mtry, ntree=ntree, nodesize=nodesize , importance=T)
predictions <- predict(temp_model , df_valid)
confusionMatrix(predictions,df_valid$won)

#plot ROC curve for best model
test_probs <- predict(best_model, df_test, type="prob")
plot(performance(prediction(test_probs[,2], df_test$won), "tpr", "fpr"), main=paste("ROC curve for best model"))
 
actual_probs <- dummy(df_test$won)

colnames(actual_probs) <- c("-1", "1")
AUC <- performance(prediction(test_probs, actual_probs), "auc")@y.values[[1]]
#

#save variable importances
varImps <- varImp(best_model)
colnames(varImps) <- c('importance'," ")
varImps <- varImps[order(-varImps$importance),] 
varImps$names <- rownames(varImps)

#lock in factor levels to maintain order
varImps$names <- factor(varImps$names, levels = varImps$names)

ggplot(varImps[1:20,],aes(x=names,y=importance))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 25, hjust = 1))+
  labs(title="variable importance for Random Forest Model")


#check variable balancing
numeric_df <- sapply(df,function(x){as.numeric(as.character(x))})%>%
  as.data.frame()%>%
  select(-seq(1,4))

#get number of instances of each hero for home team
home_team_heros <- ifelse(numeric_df==1,1,0)%>%
  colSums()%>%
  as.data.frame()

home_team_heros$name <- row.names(home_team_heros)

ggplot(home_team_heros,aes(x=name,y=.))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))+
  labs(title="number of times each hero was chosen by the home team")+
  xlab("hero name")+
  ylab("number of games the hero was picked")


#get number of occurances for each away team hero
away_team_heros <- ifelse(numeric_df==-1,1,0)%>%
  colSums()%>%
  as.data.frame()

away_team_heros$name <- row.names(away_team_heros)

ggplot(away_team_heros,aes(x=name,y=.))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size=7))+
  labs(title="number of times each hero was chosen by the away team")+
  xlab("hero name")+
  ylab("number of games the hero was picked")