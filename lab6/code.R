library(tidytext)
library(dplyr)
library(tidyr)
library(readr)
library(wordcloud)
library(RColorBrewer)
library(visNetwork)
library(ggplot2)
library(plotly)
library(textdata)
get_sentiments("afinn")



# 1
# reading the txt files
text5 <- readLines('/home/johed883/VisMAster/lab6/Five.txt')

text12 <- readLines('/home/johed883/VisMAster/lab6/OneTwo.txt')


# Five
text1Frame=tibble(text5=text5)%>%mutate(line = row_number())
pal <- brewer.pal(6,"Dark2")

text1Frame %>% unnest_tokens(word, text5) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors=pal, random.order=F))

# For text oneTwo
text2Frame=tibble(text12=text12)%>%mutate(line = row_number())





text2Frame %>% unnest_tokens(word, text12) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors=pal, random.order=F))


# The most common word in the text files is watch as its the biggest word in the cloud, casio, time and price is also more frequent than other words in the five text, in the onetwo text amazon and months are also more frequent. As these texts are costumer feedback for a casio watches its seems logical that these words occur often. 


# 2

tidy_frame5=text1Frame%>%unnest_tokens(word, text5)%>%
  mutate(line1=floor(line/10))%>%
  count(line1,word, sort=TRUE)

TFIDF5=tidy_frame5%>%bind_tf_idf(word, line1, n)

tidy_frame12=text2Frame%>%unnest_tokens(word, text12)%>%
  mutate(line1=floor(line/10))%>%
  count(line1,word, sort=TRUE)

TFIDF12=tidy_frame12%>%bind_tf_idf(word, line1, n)

TFIDF5  %>% 
  group_by(word) %>% 
  summarise(mean = mean(tf_idf)) %>% 
  with(wordcloud(word, mean, max.words = 100, colors=pal, random.order=F))



TFIDF12  %>% 
  group_by(word) %>% 
  summarise(mean = mean(tf_idf)) %>% 
  with(wordcloud(word, mean, max.words = 100, colors=pal, random.order=F))

# Watch is not emphasized in these clouds because its occurring often in most of the "documents" and are therefore not considered as an important word for each document. 

# There is a big difference compared to the plots in task 1, as now we have words that describe a "document" very good instead of the most occurring words.



# 3

tidy_frame55=text1Frame%>%unnest_tokens(word, text5)%>%
  left_join(get_sentiments("afinn"))%>%
  mutate(line1=floor(line/5))%>%
  group_by(line1, sort=TRUE)%>%
  summarize(Sentiment=sum(value, na.rm = T))

plot_ly(tidy_frame55, x=~line1, y=~Sentiment)%>%add_bars()

# these "documents" for the five feedback seem to have a positive sentiment as all bars show positive values, these is expected as when you rate something as 5 of 5 then you are satisfied with the product you bought. 


tidy_frame121=text2Frame%>%unnest_tokens(word, text12)%>%
  left_join(get_sentiments("afinn"))%>%
  mutate(line1=floor(line/5))%>%
  group_by(line1, sort=TRUE)%>%
  summarize(Sentiment=sum(value, na.rm = T))

plot_ly(tidy_frame121, x=~line1, y=~Sentiment)%>%add_bars()


# The onetwo customer feedback seem to be negative in the beginning as one bar have a really low value here, then its more neutral / positive as most bars are over 0. This is a bit unexpected as reviews of 1or 2 out of 5 should be customers that are dissatisfied with the product, a more negative tone should be expected in these comments and return more negative values. 



# 4


phraseNet =function(text, connectors){
  textFrame=tibble(text=paste(text, collapse=" "))
  tidy_frame3=textFrame%>%unnest_tokens(word, text, token="ngrams", n=3)
  tidy_frame3
  tidy_frame_sep=tidy_frame3%>%separate(word, c("word1", "word2", "word3"), sep=" ")
  
  #SELECT SEPARATION WORDS HERE: now "is"/"are"
  tidy_frame_filtered=tidy_frame_sep%>%
    filter(word2 %in% connectors)%>%
    filter(!word1 %in% stop_words$word)%>%
    filter(!word3 %in% stop_words$word)
  tidy_frame_filtered
  
  edges=tidy_frame_filtered%>%count(word1,word3, sort = T)%>%
    rename(from=word1, to=word3, width=n)%>%
    mutate(arrows="to")
  
  right_words=edges%>%count(word=to, wt=width)
  left_words=edges%>%count(word=from, wt=width)
  
  #Computing node sizes and in/out degrees, colors.
  nodes=left_words%>%full_join(right_words, by="word")%>%
    replace_na(list(n.x=0, n.y=0))%>%
    mutate(n.total=n.x+n.y)%>%
    mutate(n.out=n.x-n.y)%>%
    mutate(id=word, color=brewer.pal(9, "Blues")[cut_interval(n.out,9)],  font.size=40)%>%
    rename(label=word, value=n.total)
  
  #FILTERING edges with no further connections - can be commented
  edges=edges%>%left_join(nodes, c("from"= "id"))%>%
    left_join(nodes, c("to"="id"))%>%
    filter(value.x>1|value.y>1)%>%select(from,to,width,arrows)
  
  nodes=nodes%>%filter(id %in% edges$from |id %in% edges$to )
  
  visNetwork(nodes,edges)
  
}


phraseNet(text5, c('am', 'is', 'are', 'was', 'were'))
phraseNet(text5, c('at'))


# p1
# The size of the words is based on the frequency and a dark color indicates that a word is often located to the left. 

# time is 4, time at home? 
# analog time is 4 seconds slow); I use the digital display for home and the easy-to-set analog for my current location.

# of time at home and time in other time zones as I travel (in 3 weeks of use the digital time is exact while the analog time is 4 seconds slow); I use the digital display for home and the easy-to-set analog for my current location.


# watch at waalmart? 
# watch at waalmart for 89$ and saw it here for half the price. I am telling anyone this is one fo the best watches i have had in a long time.

phraseNet(text12, c('am', 'is', 'are', 'was', 'were'))
phraseNet(text12, c('at'))
# p2 


# Terrible at keeping 
# is terrible at keeping accurate time. Sometimes it just stops. The second hand just sticks a few times a day. It might go a few days without loosing time and then it will stop a few times a day.

# watch is hot
#  this watch is HOT. Looks perfect.At night, forget it. I mean it. When you wake up or go to bed, if it's your habit to look at your watch, you won't see it.

# perfect at night is also in the line above. 



# 5

# The properties that are mentioned the most are the alarm, the display and  the time keeping.

# Satisfied customers are commenting on the durability of the watch and its materials, the time keeping being good, that the price at amazon are good compared to other places and that its easy to use/understand. 


# Unsatisfied customers are commenting on the alarm being bad, the display not working properly and the watch not living up to their expectations. 

# You can get an understanding of some of the characteristics of the watch as watch have an arrow to huge on the satisfied customer plot so we guess the watch is kinda big. It should also maybe have some glowing parts on the display as the onetwo text have some complains about this feature.
# Other than those characteristics there isn't much to say from the two phrase nets. 




