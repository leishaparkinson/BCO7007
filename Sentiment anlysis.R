library(tidytext)
library(tidyverse)
#install.packages("janeaustenr")
library(janeaustenr)
library(stringr)
library(dplyr)
install.packages("wordcloud")

#tidy_books<-austen_books()%>%
 # group_by(books)%>%
#  mutate(
#    linenumber=row_number(),
#    chapter=cumsum(
#      str_detect(text,
#                 regex("^chapter [\\divxlc]",
#                       ignore_case = TRUE))%>%
#      ungroup()%>%
#        unnest_token(word, token)

#install.packages("textdata")
library(textdata)

#nrc<-get_sentiments("nrc")
#nrs_joy<-get_sentiments("nrc")%>%
#  filter(sentiments=="joy")

#tidy_books%>%
#  filter(book="emma")%>%
#  inner_join(nrc)%>%
#  count(sentiment, sort = TRUE)

#bing<-get_sentiments("bing")


#disney data
disney<-read_csv("disney_data_25_10_2021.csv")

class(disney$created_at)

#install.packages("lubridate")
library(lubridate)

disney<-
  disney%>%
  mutate(timestamp=ymd_hms(created_at))

remove_reg <- "&amp;|&lt;|&gt;"

tidy_disney <- disney%>% 
  filter(!str_detect(text, "^RT"))%>%
  mutate(text = str_remove_all(text, remove_reg))
# removes certain characters

tidy_disney<-tidy_disney%>%
  unnest_tokens(word, text, token = "tweets")
#unnesting tweets

head(tidy_disney$word)
#show first six lines "word" column only

selected_disney<-tidy_disney%>%select(timestamp, word, user_id)


selected_disney<-selected_disney%>%
  filter(!word %in% stop_words$word, #remove stopwords
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]")) #keep only character-words


frequency<-tidy_disney%>%
  count(word, sort = TRUE)%>%
  left_join(tidy_disney%>%
              summarise(total = n()))%>%
  mutate(freq = n/total)


tidy_disney$word<-
  gsub("@\\w+", "", tidy_disney$word)
tidy_disney$word<-
  gsub("https?://.+", "", tidy_disney$word)
tidy_disney$word <- gsub("\\d+\\w*\\d*", "", tidy_disney$word)
tidy_disney$word <- gsub("#\\w+", "", tidy_disney$word)
tidy_disney$word <- gsub("[^\x01-\x7F]", "", tidy_disney$word)
tidy_disney$word <- gsub("[[:punct:]]", " ", tidy_disney$word)
# removes mentions, urls, emojis, numbers, punctuations, etc.


bing<-get_sentiments("bing")
nrc<-get_sentiments("nrc")

selected_disney_tidy<-tidy_disney%>%select(timestamp, word, user_id)

selected_disney_tidy<-selected_disney%>%
  inner_join(bing)

selected_disney_tidy%>%
  count(sentiment, sort = TRUE)

selected_disney_tidy%>%
  group_by(user_id)%>%
  count(sentiment, sort = TRUE)

library(wordcloud)

selected_disney_tidy%>%
  count(word)%>%
  with(wordcloud(word, n, max.words = 100))
