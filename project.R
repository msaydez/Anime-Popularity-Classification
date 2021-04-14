#M'Saydez Campbell


library(tidyverse)
library(dplyr)
library(tidytext)
library(qdap)
library(tm)
library(textstem)
library(e1071)
library(SDMtune)
library(caret)
library(wordcloud)
library(RColorBrewer)
library(RTextTools)
library(tokenizers)


#Read data set
anime<-read.csv("animes.csv",header=TRUE,sep=",",na.strings=c("","NA"))

#Remove images and links from data set
anime=subset(anime,select=-c(img_url,link))

#Views the attributes in the data set
names(anime)

#Structure of the data set
str(anime)

#Summarizing each attribute in the data set
summary(anime)

#Identifying missing values
anime[!complete.cases(anime),]

#removes rows with na values
anime <- na.omit(anime)

#remove rows with no genre
empty<- c('[]')


#checking for rows without genre
anime %>%
  group_by(uid) %>%
  filter(all(genre %in% empty) & empty %in% genre)

#removing rows without genre
anime<-anime[!(anime$genre==empty),]


#checking if there are any remaining columns with na values
anime[!complete.cases(anime),]

#Length of dataframe
nrow(anime)

#Summarising each attribute in the dataset
summary(anime)


#Removing the duplicates by unique id number
anime<-anime[!duplicated(anime$uid),]

#Clean synopsis
anime$synopsis <- tolower(anime$synopsis) # Lowercase
anime$synopsis <- gsub("[[:punct:]]", " ", anime$synopsis) # Remove punctuation
anime$synopsis <- gsub("[[:digit:]]", " ", anime$synopsis)# Remove numbers
anime$synopsis <- gsub("[^a-zA-Z0-9]", " ", anime$synopsis)# Remove non alphanumeric characters
anime$synopsis <- removeWords(anime$synopsis,stopwords("english"))#Remove stopwords from synopsis
anime$synopsis <- gsub("\\s+", " ", str_trim(anime$synopsis))# Remove extra whitespaces
anime$synopsis <- gsub("\\W*\\b\\w\\b\\W*", " ", anime$synopsis)#Remove individual letters
anime$synopsis <- gsub("[[:punct:]]", " ", anime$synopsis) #Remove punctuation
anime$synopsis <- lemmatize_strings(anime$synopsis) #Lematizaion 
anime$synopsis <- lemmatize_words(anime$synopsis)#Lematization
anime$synopsis <- gsub("[[:digit:]]", " ", anime$synopsis)# Remove numbers

#Clean genre
anime$genre <- tolower(anime$genre) # Lowercase
anime$genre <- gsub("[[:punct:]]", "", anime$genre) # Remove punctuation
anime$genre <- gsub("[[:digit:]]", "", anime$genre) # Remove numbers
anime$genre <- gsub("\\s+", " ", str_trim(anime$genre))# Remove extra whitespaces
anime$genre[1]

#Vocab Metrics for Genre
vocab <- anime %>% 
  unnest_tokens(genre, genre)
head(vocab)

#Vocab Metrics for Genre
#vocab <- vocab %>% 
#  unnest_tokens(synopsis, synopsis)
#head(vocab)

#combining slice of life and super power genre
vocab$genre[vocab$genre=="slice"] <-"sliceoflife"
vocab$genre[vocab$genre=="super"] <-"superpower"
vocab$genre[vocab$genre=="sci"] <-"sci-fi"

#Removing "of", "life, "power" and "sci-fi
vocab<-vocab[!(vocab$genre=="of"),]
vocab<-vocab[!(vocab$genre=="life"),]
vocab<-vocab[!(vocab$genre=="power"),]
vocab<-vocab[!(vocab$genre=="fi"),]

#Creating bagofwords for genre
wordlist<-vocab %>% 
  select(genre)%>%
  count(genre)%>%
  filter(n>=10)%>%
  pull(genre)

bow_features<-vocab %>% 
  unnest_tokens(word, genre) %>%
  filter(word %in% wordlist) %>%
  count(uid, word) %>%
  spread(word, n)%>%
  map_df(replace_na,0)

df_anime<- anime %>%
  select(-genre)%>% 
  inner_join(bow_features, by = "uid") 

#Correlation Coefficients
cor(anime$popularity, anime$score)
cor(anime$popularity, anime$ranked)


#Plots
#Distribution of key words
ggplot(data=vocab1, aes(x=synopsis)) +
  geom_histogram(stat="count",width=0.5, colour="black", fill="white")

#Distribution of genre
ggplot(data=vocab, aes(x=genre)) +
  geom_histogram(stat="count",width=0.5, colour="red", fill="white")

#What is the most popular genre?
ggplot(data=vocab, aes(x=genre, y=popularity)) +
  geom_bar(stat="statistic", width=0.5)


#Are the anime that are popular also scored highly? 
plot(anime$popularity, anime$score, main="Are the anime that are popular also scored highly? ",
     xlab="Popularity of the Anime ", ylab="Score of Anime ")

#Are the anime that are popular also ranked highly?
#rank=1 and popular=1 is the highest
plot(anime$popularity, anime$ranked, main="Are the anime that are popular also ranked highly?",
     xlab="Popularity of the Anime ", ylab="Ranked of Anime ")

#Are the anime that are popular also scored highly? 
#rank=1 and popular=1 is the highest
plot(anime$popularity, anime$score, main="Are the anime that are popular also scored highly? ",
     xlab="Popularity of the Anime ", ylab="Score of Anime ")

#Does the number of episodes affect the score? Score based on episodes
plot( anime$members, anime$score, main="Are the anime that have alot of members also scored highly?",
      xlab="Number of Members ",ylab="Score of Anime ")

#Does the number of episodes affect the popularity? Popularity based on episodes
plot(anime$popularity,anime$episodes, main="Popularity based on episodes",
     xlab="Popularity of Anime" ,ylab="Number of episodes in the Anime " )

#Distribution of ratings
ggplot(data=anime, aes(x=score)) +
  geom_histogram(binwidth=0.5,colour="black", fill="white")


anime1<-df_anime

#Convert members to classses; popular, not popular, okay
anime1$popularity[anime$popularity>=8142] <-0# not popular
#anime1$popularity[anime$popularity>=5440 & anime$popularity<10880] <-1 #okay
anime1$popularity[anime$popularity<8142] <-1#popular

#List of key words that appear in the synopsis variable
vocab1 <- anime %>% 
  unnest_tokens(synopsis, synopsis)%>%
  count(synopsis)%>%
  filter(n>=10)

set.seed(1234)
wordcloud(words = vocab1$synopsis, freq = vocab1$n, min.freq = 600,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
#List of key words that appear in the synopsis variable
vocab1 <- anime1 %>% 
  filter(popularity==1)%>%
  unnest_tokens(synopsis, synopsis)%>%
  count(synopsis)%>%
  filter(n>=10)
set.seed(1234)
wordcloud(words = vocab1$synopsis, freq = vocab1$n, min.freq = 600,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#splitting the dataset
fractionTraining   <- 0.60
fractionValidation <- 0.20
fractionTest       <- 0.20

# Compute sample sizes.
sampleSizeTraining   <- floor(fractionTraining   * nrow(anime1))
sampleSizeValidation <- floor(fractionValidation * nrow(anime1))
sampleSizeTest       <- floor(fractionTest       * nrow(anime1))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining    <- sort(sample(seq_len(nrow(anime1)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(anime1)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

# Finally, output the three dataframes for training, validation and test.
dfTraining   <- anime1[indicesTraining, ]
dfValidation <- anime1[indicesValidation, ]
dfTest       <- anime1[indicesTest, ]



x_train<-dfTraining[10:49]
y_train<-dfTraining$popularity
model<-svm(x_train, y_train,kernel = "radial", type="nu-classification")
print(model)


x_val<-dfValidation[10:49]
y_val<-dfValidation$popularity
pred<-predict(model, x_val)
a<-table(pred,y_val, dnn=c("Prediction", "Actual"))
pred=as.factor(pred)
y_val=as.factor(y_val)
confusionMatrix(pred, y_val)
recall(a)




y_train<-as.factor(y_train)
               
svm_tune <- tune(svm, train.x = x_train, 
                 train.y = y_train, 
                 kernel  = "radial",
                 type    = "nu-classification",
                 tunecontrol = tune.control(sampling = "fix"),
                 ranges  = list(
                   gamma = 2^(-1:1),  
                   cost = 2^(2:4)
                 )
)

print(svm_tune)



best_mod <- svm_tune$best.model
best_mod_pred <- predict(best_mod, x_val)
best_mod_pred=as.factor(best_mod_pred)
y_val=as.factor(y_val)
confusionMatrix(best_mod_pred, y_val)

x_test<-dfTest[10:49]
y_test<-dfTest$popularity
pred_test<- predict(best_mod,x_test)
pred_test=as.factor(pred_test)
y_test=as.factor(y_test)
confusionMatrix(pred_test, y_test)







