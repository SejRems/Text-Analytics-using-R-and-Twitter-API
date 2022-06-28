################################
#######TEXT ANALYTICS USING TWITTER API
#######SEJZI REMORQUE
#######HULT
######


library(quanteda)
library(twitteR)
library(rtweet)
library(dplyr)
library(tm)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)

setwd("C:/Users/sejzi/Desktop/Business insight Report")
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")


#connect to twitter API
consumer_key<-""
consumer_secret<-""
access_token<-""
access_secret<-""

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


###Get tweets### 
####Game of the year fro the past 4 years


gow4<-twitteR::searchTwitter("GodofWar",n=5000,since="2018-12-01",retryOnRateLimit = 1e3,lang = "en")
a = twitteR::twListToDF(gow4)

sekiro<-twitteR::searchTwitter("SekiroShadowsDieTwice ",n=5000,since="2019-12-01",retryOnRateLimit = 1e3,lang="en")
b = twitteR::twListToDF(sekiro)

tlou2<-twitteR::searchTwitter("TheLastofUs2",n=5000,since="2020-06-01",retryOnRateLimit = 1e3,lang = "en")
c = twitteR::twListToDF(tlou2)





######removing stop words

cust_stop <- data_frame(word=c("http", "https", "rt", "t.io","t.co"),
                        lexicon=rep("cust", each=5)
)


tidy_gow4 <- a %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop)

tidy_sekiro <- b %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop)

tidy_tlou2 <- c %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  anti_join(cust_stop)


#################
###COunting frequencies
########################

gow4_nostop<-tidy_gow4

gow4_nostop %>%
  count(word, sort=TRUE)

freq_hist <-gow4_nostop %>%
  count(word, sort=TRUE) %>%
  filter(n>100) %>% # we need this to eliminate all the low count words
  mutate(word = reorder(word,n )) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

#####################
####Sentiment Analysis
######################


########################################################
##### Comparing different sentiment ####
########################################################

godofwar <- tidy_gow4

afinn <- godofwar %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  godofwar%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  godofwar %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

##Sekiro

sekirod2 <- tidy_sekiro

afinn <- sekirod2 %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  sekirod2%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  sekirod2%>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

####Tlou2
lastofus <- tidy_tlou2

afinn <- lastofus %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  lastofus%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  lastofus%>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")


##############################################################
######## Most common positive and negative words #############
##############################################################

##God of War 
bing_counts_gow4 <- godofwar %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_gow4

bing_counts_gow4 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()



##Sekiro
bing_counts_sekiro <- sekirod2%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_sekiro 

bing_counts_sekiro %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


###Tlou2

bing_counts_tlou2 <- lastofus%>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts_tlou2

bing_counts_tlou2 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


#############################################
#########NGRAMS - Bigrams - QUADROGRAMS###########
################################################




######GOD OF WAR

gow4_bigrams <- a %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

gow4_bigrams #We want to see the bigrams (words that appear together, "pairs")

gow4_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated_gow4 <- gow4_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_gow4 <- bigrams_separated_gow4 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word1 %in% cust_stop$word)%>%
  filter(!word2 %in% cust_stop$word)
  

#creating the new bigram, "no-stop-words":
bigram_counts_gow4 <- bigrams_filtered_gow4  %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_gow4





#######SEKIRO

sekiro_bigrams <- b %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

sekiro_bigrams #We want to see the bigrams (words that appear together, "pairs")

sekiro_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated_sekiro <- sekiro_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_sekiro <- bigrams_separated_sekiro %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word1 %in% cust_stop$word)%>%
  filter(!word2 %in% cust_stop$word)


#creating the new bigram, "no-stop-words":
bigram_counts_sekiro <- bigrams_filtered_sekiro  %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_sekiro






#THE LAST OF US 2

tlou2_bigrams <- c %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

tlou2_bigrams #We want to see the bigrams (words that appear together, "pairs")

tlou2_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
bigrams_separated_tlou2 <- tlou2_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_tlou2 <- bigrams_separated_tlou2 %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>%
  filter(!word1 %in% cust_stop$word)%>%
  filter(!word2 %in% cust_stop$word)


#creating the new bigram, "no-stop-words":
bigram_counts_tlou2 <- bigrams_filtered_tlou2  %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_tlou2




#######################################
########Quadrograms###############
################################


###God of War
quadrogram_gow4 <- a %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word)%>%
  filter(!word2 %in% cust_stop$word)%>%
  filter(!word3 %in% cust_stop$word)%>%
  filter(!word4 %in% cust_stop$word)

quadrogram_gow4


####Sekiro
quadrogram_sekiro <- b %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  filter(!word1 %in% cust_stop$word)%>%
  filter(!word2 %in% cust_stop$word)%>%
  filter(!word3 %in% cust_stop$word)%>%
  filter(!word4 %in% cust_stop$word)

quadrogram_sekiro




###The Last of US

quadrogram_lastofus <- c %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
filter(!word1 %in% cust_stop$word)%>%
filter(!word2 %in% cust_stop$word)%>%
filter(!word3 %in% cust_stop$word)%>%
filter(!word4 %in% cust_stop$word)

quadrogram_lastofus















































###############
### AIRBNB AMERICA
airbnb_america <-  airbnb_all[str_detect(airbnb_all$host$host_location,"United States|Brazil|Canada|Mexico|Dominican Republic"),]
reviews_america <- data.frame(country=airbnb_america$host$host_location[[2]], text = airbnb_america$reviews[[2]][[6]])
for (i in 2:nrow(airbnb_america)) {
  try(airbnb_new <- data.frame(country=airbnb_america$host$host_location[[i]], text = airbnb_america$reviews[[i]][[6]]))
  try(reviews_america <- rbind(reviews_america,airbnb_new))
}
view(reviews_america)

for(i in 1:nrow(reviews_america)){
  try(airbnb_lang<-data.frame(language=textcat(reviews_america$text[[i]])))
}

view(airbnb_lang)



#########################################TD_IF
#####  netflix->reviews_america




airbnb_america_token <- reviews_america %>%
  unnest_tokens(word, text) %>%
  count(country, word, sort=TRUE) %>%
  ungroup()

airbnb_america_total_words <- airbnb_america_token %>%
  group_by(country) %>%
  summarize(total=sum(n))

airbnb_america_words <- left_join(airbnb_america_token, airbnb_america_total_words) %>%
  filter(!word %in% stop_words$word)%>%
  filter(country %in% c("United States","Mexico City", "Canada","Brazil","Dominican Republic"))





country_words <- airbnb_america_words %>%
  bind_tf_idf(word, country, n)

country_words # we get all the zeors because we are looking at stop words ... too common

country_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?


#############
# looking at the graphical apprach:
country_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(country) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=country))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~country, ncol=2, scales="free")+
  coord_flip()




###############filtering out

languages<-data.frame(text=reviews_america$text[[1]],language=textcat(reviews_america$text[[1]]))

