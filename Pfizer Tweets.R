getwd()
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("syuzhet")
install.packages("tm")
install.packages("ggplot2")
install.packages("plotrix")
library(plotrix)
library(SnowballC)
library(tm)
library(syuzhet)
library(ggplot2)
library(wordcloud)

#Reading the data
Vaccination <- read.csv("vaccination_tweets.csv")

#Removing links,punctuatuions,blank spaces,tabs, hashtags,username  and symbols
Vaccination$text<- tolower(Vaccination$text)
Vaccination$text <- gsub("@\\w+", "", Vaccination$text)
Vaccination$text <- gsub("[[:punct:]]", "", Vaccination$text)
Vaccination$text <- gsub("http\\w+", "", Vaccination$text)
Vaccination$text <- gsub("^ ", "", Vaccination$text)
Vaccination$text <- gsub(" $", "", Vaccination$text)
Vaccination$text <- gsub("[ |\t]{2,}", "", Vaccination$text)
Vaccination$text <- gsub("#","",Vaccination$text)

#Checking Nas
summary(Vaccination)
sapply(Vaccination, function(x) sum(is.na(x)))
sum(Vaccination$text == "")
Vaccination <- Vaccination[Vaccination$text != "", ]

#Getting sentiment score for each tweet
word_df <- as.vector(Vaccination$text)
sentiment_df <- get_nrc_sentiment(word_df)
Sentiment_scores <- data.frame(colSums(sentiment_df[,]))

Combine_df <- cbind(Vaccination$text, sentiment_df) 
names(Sentiment_scores)<-"Score"
Sentiment_scores <-cbind("sentiment"=rownames(Sentiment_scores),Sentiment_scores)
rownames(Sentimentscores)<-NULL

#Visualisation
ggplot(data=Sentiment_scores,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("scores")+ggtitle("Sentiments of people behind the tweets Pfizer Covid Vaccine")

#generate wordcloud
wordcloud(Vaccination$text,min.freq = 10,colors=brewer.pal(8, "Dark2"),random.color = TRUE,max.words = 500)

Value <- get_sentiment(word_df)
most.positive <- word_df[Value == max(Value)]
most.negative <- word_df[Value <= min(Value)] 


View(word_df)
View(Value)
#Identifying the sentiment of each tweet
Vaccination_Final <- cbind(Vaccination,Value)
Vaccination_Final$Value[Vaccination_Final$Value > 0] = "Positive"
Vaccination_Final$Value[Vaccination_Final$Value < 0] = "Negative"
Vaccination_Final$Value[Vaccination_Final$Value == 0] = "Neutral"
View(Vaccination_Final)
#Preprocessing of other columns of data
sum(Vaccination_Final$user_location == "")
Vaccination_Final$user_location[Vaccination_Final$user_location == ""] = "Not Specified"
Vaccination_Final <- Vaccination_Final[c(,)]
Final <- Vaccination_Final[,c(-4,-5,-12,-13,-15)]
View(Final)
Final$date <- as.Date(Final$date) 
colnames(Final)[12] = "Tweet_Sentiment"
Final <- Vaccination_Final[,c(-11)]

#Tweet classification
ggplot(Final, aes(x=Tweet_Sentiment)) +
  geom_bar(aes(y=..count.., fill= Tweet_Sentiment)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="Number of Tweets")

#Tweets per day
Date <- table(Final$date)
View(Date)
barplot(Date, main="Number of tweets",
        xlab="Days")

#Tweets by Users verified account
Users_verified <- table(Final$user_verified)
View(Users_verified)
lbl <- c("False", "True")
pie3D(x,labels = piepercentage ,explode = 0.1, main = "Users Verified ")




