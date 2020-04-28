View(Updated.df)
library(ggplot2)
library(data.table)
library(magrittr)
library(sentimentr)

country1<-Updated.df("#UK",include_rts = FALSE)
View(country1)
install.packages("tidytext")
library(tidytext)
install.packages("textdata")
library(textdata)
get_sentiments("bing")%>%filter(sentiment=="positive")
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
files <- list.files("../input")



###histogram##

ggplot(Updated.df, aes(x=created_at)) +
  geom_histogram(aes(y=..count..), #make histogram
                 binwidth=60, #each bar contains number of tweets during 60 s
                 colour="blue", #colour of frame of bars
                 fill="blue", #fill colour for bars
                 alpha=0.8) + # bars are semi transparant
  ggtitle(paste0("Activity ",16252," tweets")) + #title
  scale_y_continuous(name="Number of Tweets per minute") + 
  scale_x_datetime(name = "Time") +
  theme_minimal(base_family="Times New Roman")
library(quanteda)
install.packages("corpus")
library(corpus)


  #################ggplot frequencies###############
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
cols<-brewer.pal(n=4,name="Set1")

words.to.remove <- c(stopwords("english"),'domestic abuse','abuse','domestic')
dfmat_corp_twitter <- Updated.df$text %>% corpus() %>% 
  dfm(remove = words.to.remove,
      what = "word",
      stem = TRUE, 
      remove_punct = TRUE,
      remove_url=TRUE)


dfFreq <- textstat_frequency(dfmat_corp_twitter) %>% as.data.table
ggplot(dfFreq[1:20,], aes(x=feature, y=frequency)) + 
  geom_col() +
  coord_flip() +
  theme_minimal()



dfFreq <- textstat_frequency(dfmat_corp_twitter) %>% as.data.table
ggplot(dfFreq[1:20,], aes(x=feature, y=frequency)) + 
  geom_col() +
  coord_flip() +
  theme_minimal()

dfFreq[1:10,]

ggplot(dfFreq[1:20,], aes(x=reorder(feature, -rank), y=frequency)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Frequent Words", y = "Count") +
  theme_minimal(base_family="Times New Roman")



###########################wordcloud##############

stopwords(language = "en")[1:10]
Updated.df[grepl("bts_bighit",text), list(text) ]

textplot_wordcloud(dfmat_corp_twitter, min_count = 5, random_order = FALSE,
                   rotation = .25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))




##################docfrequency#####
dfFreq_long_top20 = dfFreq[rank <= 20] %>% 
  melt(id.vars = c("feature","group","rank"),
       measure.vars = c("frequency","docfreq")
  )

ggplot(dfFreq_long_top20, aes(x=reorder(feature,-rank), y=value, fill = variable)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete() + 
  labs(x = "", y = "Occurances", fill = "") +
  coord_flip() +
  theme_minimal()



##########topic modeling#####

require(topicmodels)
dtm <- convert(dfmat_corp_twitter, to = "topicmodels")
lda <- LDA(dtm, k = 6, control=list(seed=12))

terms(lda, 8) %>% utf8::utf8_print()

topics(lda)[1:4]

topicAssignment = 
  data.table(
    index = lda %>% 
      topics %>% 
      names %>% 
      gsub("text","", .) 
    %>% as.integer,
    topic = lda %>% topics
  )
topicAssignment %>% head(4)

Updated.df$Topic = NA # creates a new col ‘topic’, assign it to NA
Updated.df$Topic[topicAssignment$index] = topicAssignment$topic

Updated.df$Topic = Updated.df$Topic %>% as.factor

ggplot(Updated.df, aes(x=created_at, y=Topic, col=Topic)) +
  geom_jitter(aes(size = retweet_count)) +
  ggtitle(paste0("Each dot is a tweet matching '","kill","'")) +
  scale_y_discrete() +
  scale_x_datetime(name = "") + 
  scale_color_discrete(guide = FALSE) + 
  scale_size_continuous(name="Retweets")

Updated.df[,list(Total.Retweets = sum(retweet_count)),by=Topic] %>% 
  ggplot(aes(x = Topic, y = Total.Retweets)) + 
  geom_col()

##########clustering####

similar_wrds <- textstat_simil(dfmat_corp_twitter, 
                               dfmat_corp_twitter[,c("english")], 
                               margin="features")

head(as.matrix(similar_wrds), 10)

as.list(similar_wrds, n = 6)


fstat <- dfmat_corp_twitter %>% 
  dfm_trim(min_termfreq = 0.995, termfreq_type = "quantile") %>%
  textstat_dist(margin="features")

plot(hclust(as.dist(fstat)))


hc <- hclust(as.dist(fstat))
install.packages("dedextend")
library(dendextend)

hcd = as.dendrogram(hc)
d2 <- color_branches(hcd, k= 5, groupLabels = TRUE)




plot(d2, hang = -1, cex = 0.6)






                                    
#measure what countries tweet domestic abuse the most#


#setDT(tweets.df)
View(Updated.df[country == "United Kingdom"])

View(Updated.df[country == "United Kingdom",.(hashtags,retweet_count)])

Updated.df[,favorite_count+retweet_count]
Updated.df[,list(favorite_count+retweet_count)]
Updated.df[,list(Impact = favorite_count+retweet_count)]

Updated.df[,list(favorite_count+retweet_count), by = country]
Updated.df[,list(favorite_count+retweet_count, country)]
View(Updated.df[,list(sum(favorite_count+retweet_count)), by=country])

Updated.df[,list(.N), by=country]


Updated.df[,list(.N), by= hashtags]

Updated.df[,list(.N), by = hashtags]

Updated.df[,.(unlist(hashtags))]

Updated.df[,.(hashtag = unlist(hashtags))][,list(.N),by = hashtag]

Updated.df[,.(hashtag = unlist(hashtags))][,list(.N),by = hashtag][order(-N)]


tweets.df[,.(hashtag = unlist(hashtags))] %>% 
  .[,list(.N),by = hashtag] %>% 
  .[order(-N)]

tweets.df[,.(hashtag = unlist(hashtags)), by = country]

tweets.df[,.(hashtag = unlist(hashtags)), by = country] %>% 
  .[,.N, by = list(hashtag,country)] %>% 
  .[country == "United Kingdom"] %>% 
  .[order(-N)] %>% 
  head(10)


tweets.df[,.(hashtag = unlist(hashtags)), by = country] %>% 
  .[,.N, by = list(hashtag,country)] %>% 
  .[country == "United States"] %>% 
  .[order(-N)] %>% 
  head(10)



tweets.df[,.(hashtag = unlist(hashtags)), by = country] %>% 
  .[,.N, by = list(hashtag,country)] %>% 
  .[order(-N),.SD[1:6], by = country]

htbc = tweets.df[,.(hashtag = unlist(hashtags)), by = country] %>% 
  .[,.N, by = list(hashtag,country)] %>% 
  .[order(-N),.SD[1:6], by = country] %>% 
  .[order(country)]




tweets.df[,.(url = unlist(urls_expanded_url))] %>% 
  .[,.N,by = url] %>% 
  .[order(-N)] %>% 
  head(10)

url = tweets.df[,list(url = unlist(urls_expanded_url)), 
                by =list(retweet_count)]  %>% 
  .[,list(.N,total.retweets = sum(retweet_count)), by =url ] %>% 
  .[order(-total.retweets)] %>% 
  na.exclude()

View(url)

#####hastags by country###

#By country

library(dplyr)
library(tidyverse)
country.df<-Updated.df %>%
  group_by(country) 

country.df2<-country.df%>% group_by(country)%>%count()
country.df2<-country.df2[order(-country.df2$n),]
country.df2<-country.df2%>% drop_na(country)

rm(country.df)

ggplot(country.df2[1:20,], aes(x=country, y=n)) + 
  geom_col() +
  coord_flip() +
  labs(x = "Stemmed word", y = "Count") +
  theme_minimal(base_family="Times New Roman",col=("#69b3a2")

country.df2$country<- factor(country.df2$country, levels = country.df2$country[order(country.df2$n)])
p <- ggplot(country.df2[1:20], aes(y=n))
p + geom_bar(aes(x=country), stat="identity") + coord_flip()









                                       