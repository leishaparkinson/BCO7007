library(tidytext)
library(tidyverse)
install.packages("janeaustenr")
library(janeaustenr)
library(stringr)
library(dplyr)

tidy_books<-austen_books()%>%
  group_by(books)%>%
  mutate(
    linenumber=row_number(),
    chapter=cumsum(
      str_detect(text,
                 regex("^chapter [\\divxlc]",
                       ignore_case = TRUE))%>%
      ungroup()%>%
        unnest_token(word, token)

install.packages("textdata")
library(textdata)

nrc<-get_sentiments("nrc")
nrs_joy<-get_sentiments("nrc")%>%
  filter(sentiments=="joy")

tidy_books%>%
  filter(book="emma")%>%
  inner_join(nrc)%>%
  count(sentiment, sort = TRUE)

bing<-get_sentiments("bing")