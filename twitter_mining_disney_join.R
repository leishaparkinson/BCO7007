library(tidyverse)
library(rtweet)

#every week do this
disney_data<-search_tweets(
  q="#disney",
  n=18000,
  include_rts = FALSE,
  lang="en",
  retryonratelimit = TRUE
)

disney_data<-disney_data%>%flatten()

#change the name of the file
#if you don't you are going to lose your data!!!!

disney_data%>%write_csv("Disney_04_11_2021.csv")

#-------------------
#do it when you start completing your assessment
#merging and opening all csv files
files<-list.files(pattern="\\.csv$",full.names = TRUE) #read files names with csv
all_data_disney_04112021<-map_df(files, ~read_csv(.x)) #open and merge

#you may have duplicate entries
final_disney_data_04112021<-all_data_disney_04112021%>%distinct()

final_disney_data_04112021%>%write_csv("final_disney_data_04112021.csv")
