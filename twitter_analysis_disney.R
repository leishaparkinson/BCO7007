library(tidytext)
library(tidyverse)
library(lubridate)
library(textdata)

library(dplyr)

library(RColorBrewer)
library(wordcloud)
library(wordcloud2)

library(topicmodels)
library(tm)

#import twitter data
tweet_politics<-read_csv("final_politics_data.csv")

tweet_politics<-
  tweet_politics%>%
  mutate(
    timestamp=ymd_hms(created_at),
    day_of_week=wday(timestamp),
    id=as_factor(row_number())
  )

#wday(ymd(080101), label = TRUE, abbr = FALSE)
#wday(ymd(080101), label = TRUE, abbr = TRUE)

#explore wday() function

remove_reg <- "&amp;|&lt;|&gt;|\\d+\\w*\\d*|#\\w+|[^\x01-\x7F]|[[:punct:]]|https\\S*"
# &amp = @
# &lt;= <
# &gt; >


#removing retweets characters
tidy_tweets_politics <- tweet_politics %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) 


#unnesting tweets 
tidy_tweets_politics<-tidy_tweets_politics%>%
  unnest_tokens(word, text, token = "tweets") 

# Remove mentions, urls, emojis, numbers, punctuations, etc.
tidy_tweets_politics<-tidy_tweets_politics%>%
  filter(
    str_detect(word, "[a-z]")#keep only character-words
  )%>%
  anti_join((stop_words))

head(tidy_tweets_politics$word)
#!!!!
#if you want to use stem of the words (e.g. egg instead of eggs) you can use SnowballC::wordStem() option

#install.packages("SnowballC")
#library(SnowballC)
#tidy_tweets_politics_stemmed<-tidy_tweets_politics%>% mutate(word=SnowballC::wordStem(word))
#head(tidy_tweets_stemmed$word)

#sentiment
bing<-get_sentiments("bing")
nrc<-get_sentiments("nrc")

selected_tweets_tidy_politics<-tidy_tweets_politics%>% select(timestamp, day_of_week, location, word, user_id)

selected_tweets_tidy_politics<-selected_tweets_tidy_politics%>%
  inner_join(bing)%>%
  rename(sentiment_bing=sentiment)%>%
  inner_join(nrc)%>%
  rename(sentiment_nrc=sentiment)


selected_tweets_tidy_politics%>%
  count(sentiment_nrc, sort=TRUE)

selected_tweets_tidy_politics%>%
  count(sentiment_bing, sort=TRUE)

selected_tweets_tidy_politics%>%
  group_by(user_id)%>%
  count(sentiment_nrc, sort=TRUE)

#wordcloud

tidy_tweets_politics%>%count(word) %>%
  with(wordcloud(word, n, max.words = 100))

#more wordclouds with wordcloud2 package! - it is awesome!
# https://cran.r-project.org/web/packages/wordcloud2/vignettes/wordcloud.html
#install.packages("wordcloud2")
#library(wordcloud2)

tidy_tweets_wordcloud_politics<-tidy_tweets_politics%>%count(word) %>%
  select(word, n)%>%
  filter(!word=="politics") #too many occurencies!!!!


wordcloud2(tidy_tweets_wordcloud_politics,color = "random-light", backgroundColor = "black")

letterCloud(tidy_tweets_wordcloud_politics, word = "politics", size = 2)

#---------------------
#topic modeling

#create a document term frequency based on how often words are
tweets_dtm_politics<-tidy_tweets_politics%>%
  count(id, word)%>%
  cast_dtm( #converts our data to a special object for R = document term frequency matrix
    document=id,
    term=word,
    value=n,
    weighting=tm::weightTf
  )

tweets_dtm

#to speed up processing let's remove those words that are VERY rare
#install.packages("tm")
#sparse=rare
tweets_dtm_trim_politics<-tm::removeSparseTerms(tweets_dtm_politics, sparse=.99)
rowTotals <- apply(tweets_dtm_trim_politics, 1, sum) #Find the sum of words in each Document
tweets_dtm_trim_politics <- tweets_dtm_trim_politics[rowTotals> 0, ] 

tweets_dtm_trim

#LDA for 5 topics
tweets_lda_politics<-LDA(tweets_dtm_trim_politics, k=5, control=list(seed=1234))

tweet_topics_politics<-tidy(tweets_lda_politics, matrix="beta")

#let's look at them with 10 top words for each topic
tweet_top_terms_politics <- tweet_topics_politics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

tweet_top_terms_politics %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


#OPTIONAL
#if you want a "scientific" way to see how many topics you need to use
#you can use ldatuning package

install.packages("ldatuning") #select no for compilation if asked
library(ldatuning)

#it will take TIME!
topic_n <- FindTopicsNumber(dtm = tweets_dtm_trim_politics , topics = seq(from = 2, to = 30, by = 1), 
                            metrics = "Griffiths2004", 
                            method = "Gibbs",
                            control = list(seed = 123),
                            mc.cores = 4L,
                            verbose = TRUE)

FindTopicsNumber_plot(topic_n)
#it works the same way as "elbow method" for cluster analysis from bco6008
#if you have not done bco6008- just skip this part! 
#once are happy with the number of topics - 
#you can rerun the LDA with the number of topics you identified from ldatuning
