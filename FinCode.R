library(dplyr)
library(tidytext)
library(tidyverse)
reviews<-read.csv("/Users/artemysafronov/Desktop/CommentsforSemantic/drive-download-20200523T092728Z-001/Trad Lands/MODERN FISHER ALPACA SMOKE.csv")
CSRlex<-read.csv("/Users/artemysafronov/Desktop/CommentsforSemantic/sustainability-10-04119-s001/CSRwordlist.txt")

#Check the format of data. Reviews should be in character (chr) format. As well as wordlists
reviews<-reviews %>% 
  mutate_if(is.factor, as.character)
reviews<-reviews %>% 
  mutate_if(is.logical, as.character)
reviews$FinRev <- paste(reviews$Short.review,reviews$Full.review)
CSRlex<-CSRlex %>% 
  mutate_if(is.factor, as.character)

#Text transormation into single word-token
reviews<- reviews %>%
  unnest_tokens(word, FinRev)
CSRlex<-CSRlex %>%
  unnest_tokens(word,list)

duplicated(CSRlex)
CSRlex<-CSRlex %>% 
  distinct(word, .keep_all = TRUE)

#Calculating every word mentions in reviews
totals <- reviews %>%
  anti_join(stop_words) %>%
  count(word,sort = TRUE)
totals<-totals%>%
  mutate(contr=n/sum(n)*100)
totals$contr<-round(totals$contr,digits = 3)

#Checking words from reviews with our wordlists and calculating them 
CSRjoin<-inner_join(reviews,CSRlex,by="word")
reviewsCSR<-CSRjoin %>%
  anti_join(stop_words)%>%
  count(word,sort = TRUE)
reviewsCSR<-reviewsCSR%>%
  mutate(contr=n/sum(totals$n)*100)
reviewsCSR$contr<-round(reviewsCSR$contr,digits = 3)
