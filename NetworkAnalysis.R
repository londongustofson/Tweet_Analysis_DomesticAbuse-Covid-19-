
####network analysis####

load(file="tweets_april22.RData")
View(Updated.df)
install.packages("devtools")
install.packages("vosonSML")
library(vosonSML)
?Create.activity.twitter
??Create.activity.twitter
Create.activity.twitter
class(Updated.df)
class(Updated.df) <- c(class(Updated.df),"datasource","twitter")
class(Updated.df)
library(magrittr)
actorGraph =  Updated.df[sample(1:16252,1000), ] %>%
  Create("actor") %>%
  Graph()
get_igraph_attrs <- function(igraph){
  library(igraph)
  if(!is_igraph(igraph)) stop("Not a graph object")
  list(graph_attrs = list.graph.attributes(igraph),
       vertex_attrs = list.vertex.attributes(igraph),
       edge_attrs = list.edge.attributes(igraph))
}
top.ranked.users <- function(actorGraph) {
  user.rank <- page.rank(actorGraph, directed=TRUE)
  user.top <-sort(user.rank$vector,decreasing=TRUE,index.return=TRUE)
  users.ranked <- V(actorGraph)$screen_name[user.top$ix]
  return(users.ranked)
}
simplify.actor.network <- function(igraph,
                                   remove.loops = TRUE,
                                   delete.zero.degree = FALSE) {
  library(igraph)
  igraph = simplify(igraph,
                    remove.multiple = FALSE,
                    remove.loops = remove.loops,
                    edge.attr.comb = "max")
  if (delete.zero.degree) {
    igraph=delete.vertices(simplify(igraph), degree(igraph)==0)
  }
  return(igraph)
}
get_igraph_attrs(actorGraph)
V(actorGraph)$name %>% head(4)
V(actorGraph)$screen_name %>% head(4)
V(actorGraph)$label %>% head(4)
E(actorGraph)$edge_type %>% as.factor() %>% levels
actorGraph.simplyfied = simplify.actor.network(actorGraph, remove.loops = TRUE, delete.zero.degree = TRUE)
grep("^layout_with_.*[^[sugiyama]]*", ls("package:igraph"), value = TRUE) %>%  print
plot.actor.Graph <- function(igraph, layout = layout_with_fr(igraph, niter = 1000),
                             ## aspect ratio =================================
                             asp = 0,
                             ## labels =======================================
                             ## colors =======================================
                             vertex.color = rgb(0.33,0.33,0.33,0.5),      ## grey with opacity 30%
                             vertex.frame.color = rgb(1.00,1.00,1.00,1), ## white border color no opacity
                             ## shapes =======================================
                             vertex.shape = "circle",      ## none, circle, square, csquare,
                             ## vrectangle, pie, raster, sphere
                             ## rectangle, crectangle
                             ## sizes =======================================
                             vertex.size = 2.1,             ## size, default = 15
                             vertex.size2 = NA,             ## second dimension size (for parallelograms)
                             ## edges =======================================
                             edge.color = rgb(0.5,0.5,0.5,0.5),      ## darkgrey with opacity 30%
                             edge.width = 0.5,             ## default = 1
                             edge.arrow.size = 0.2,        ## default = 1
                             edge.arrow.width = 0.5,       ## default = 1
                             edge.lty = "solid",           ## linetype: blank, solid, dashed, dotted,
                             ## dotdash, longdash, or twodash
                             edge.curved = 0.15,           ## 0 to 1 or TRUE (0.5)
                             ...) {
  y = list(...)
  if (length(y)==0) {plot.igraph(igraph, layout= layout, asp = asp, vertex.color = vertex.color, vertex.frame.color = vertex.frame.color,vertex.shape =vertex.shape,vertex.size = vertex.size, vertex.size2 = vertex.size2, edge.color = edge.color, edge.width = edge.width,  edge.arrow.size = edge.arrow.size, edge.arrow.width = edge.arrow.width, edge.lty = edge.lty, edge.curved = edge.curved) }
  else {plot.igraph(igraph, vertex.label = y$vertex.label, layout= layout, asp = asp, vertex.color = vertex.color, vertex.frame.color = vertex.frame.color,vertex.shape =vertex.shape,vertex.size = vertex.size, vertex.size2 = vertex.size2, edge.color = edge.color, edge.width = edge.width,  edge.arrow.size = edge.arrow.size, edge.arrow.width = edge.arrow.width, edge.lty = edge.lty, edge.curved = edge.curved) }
}
label.user.network <- function(actorGraph , named.users) {
  V(actorGraph)$label <- V(actorGraph)$screen_name
  V(actorGraph)$label[which(!V(actorGraph)$label %in% named.users)] <- NA
  return(actorGraph)
}
neighborhood.to.user <- function(actorGraph, screen_name, k.nearest.neighbours = 1) {
  index <- which(V(actorGraph)$screen_name==screen_name)
  neigborhood.of.index <- neighborhood(actorGraph,order = k.nearest.neighbours, nodes = index)
  v.index <- c(unlist(neigborhood.of.index),index)
  partialGraph <- induced_subgraph(actorGraph,v.index)
  return(partialGraph)
}
plot.actor.Graph(actorGraph.simplyfied,
                 vertex.label = NA,
                 layout = layout_with_kk)
top.ranked.users(actorGraph.simplyfied)[1:15]
named.users = top.ranked.users(actorGraph.simplyfied)[1:15]
actorGraph.named = label.user.network(actorGraph.simplyfied,
                                      named.users)
plot.actor.Graph(actorGraph.named,layout = layout_with_kk)
k = length(named.users)
color_vector = rainbow(k)
V(actorGraph.simplyfied)$color = rgb(0.33,0.33,0.33,0.5) # set color to grey and opacity to 50%
V(actorGraph.simplyfied)$frame.color = rgb(1.00,1.00,1.00,1) # set color to white and opacity to 100%
V(actorGraph.simplyfied)$color[which(V(actorGraph.simplyfied)$screen_name %in% named.users)] = color_vector
V(actorGraph.simplyfied)$label = NA # remove the label
plot.igraph(actorGraph.simplyfied,
            layout = layout_with_kk,
            ## shapes =======================================
            vertex.shape = "circle",
            ## sizes =======================================
            vertex.size = 2.1,             ## size, default = 15
            ## edges =======================================
            edge.color = rgb(0.5,0.5,0.5,0.5),      ## darkgrey with opacity 30%
            edge.width = 0.5,             ## default = 1
            edge.arrow.size = 0.2,        ## default = 1
            edge.arrow.width = 0.5,       ## default = 1
            edge.lty = "solid",           ## linetype: blank, solid, dashed, dotted,
            ## dotdash, longdash, or twodash
            edge.curved = 0.15           ## 0 to 1 or TRUE (0.5)
)
layout(matrix(1:2,1,2,byrow = TRUE), widths = c(5,2))
plot.igraph(actorGraph.simplyfied,
            layout = layout_with_kk,
            ## shapes =======================================
            vertex.shape = "circle",
            ## sizes =======================================
            vertex.size = 2.1,             ## size, default = 15
            ## edges =======================================
            edge.color = rgb(0.5,0.5,0.5,0.5),      ## darkgrey with opacity 30%
            edge.width = 0.5,             ## default = 1
            edge.arrow.size = 0.2,        ## default = 1
            edge.arrow.width = 0.5,       ## default = 1
            edge.lty = "solid",           ## linetype: blank, solid, dashed, dotted,
            ## dotdash, longdash, or twodash
            edge.curved = 0.15           ## 0 to 1 or TRUE (0.5)
)
plot.new()
legend(x = "left",      ## position, also takes x,y coordinates
       legend = named.users,
       pch = 19,              ## legend symbols see ?points
       col = color_vector,
       bty = "n",
       cex=0.5, #font size 50%
       title = "Users")
edge_colors = rainbow(6, alpha = 0.5) # 6 topics
E(actorGraph.simplyfied)$color <- rgb(0.33,0.33,0.33,0.5)
for (i in 1:6) {
  index <- which(
    V(actorGraph.simplyfied)$name %in% Updated.df[ Topic == i, user_id])
  E(actorGraph.simplyfied)$color[index] = edge_colors[i]
}
plot.igraph(actorGraph.simplyfied,
            layout = layout_with_kk,
            ## shapes =======================================
            vertex.shape = "circle",
            ## sizes =======================================
            vertex.size = 2.1,             ## size, default = 15
            ## edges =======================================
            edge.width = 0.5,             ## default = 1
            edge.arrow.size = 0.2,        ## default = 1
            edge.arrow.width = 0.5,       ## default = 1
            edge.lty = "solid",           ## linetype: blank, solid, dashed, dotted,
            ## dotdash, longdash, or twodash
            edge.curved = 0           ## 0 to 1 or TRUE (0.5)
)
neigbGraph = neighborhood.to.user(actorGraph,
                                  "CitizensAdvice",
                                  k.nearest.neighbours=1)
plot.actor.Graph(neigbGraph,
                 layout = layout_with_kk)
ranked.users = actorGraph %>%
  simplify.actor.network %>%
  neighborhood.to.user("loan",  k.nearest.neighbours=2) %>%
  top.ranked.users() %>% head(8)
actorGraph %>%
  simplify.actor.network %>%
  neighborhood.to.user("loan",  k.nearest.neighbours=2) %>%
  label.user.network(ranked.users) %>%
  plot.actor.Graph(layout = layout_with_fr)



###Sentiment Analysis###

#after topic modeling insert a new column with the topics
library(ggplot2)
library(data.table)
library(magrittr)
library(sentimentr)
class(Updated.df)

setDT(Updated.df)
df <- Updated.df[,list(created_at,text,Topic)] 
View(df)

df$roundTime <- as.POSIXct(cut(df$created_at, breaks = "5 mins"))

df$text[1]

df$text[1] %>% get_sentences 
df$text[1] %>% get_sentences %>% sentiment

df$text[1] %>% get_sentences %>% sentiment_by

sentiment_by_tweet = 
  df[,
     list(text %>% get_sentences %>% sentiment_by(),
          Topic)]
# In df:
#   select all rows
#          send text column to function get_sentences, then to
#          sentiment_by as above

sentiment_by_tweet

sentiment_by_Topic = 
  sentiment_by_tweet[, list(Tweets = .N,
                            ave_sentiment = mean(ave_sentiment),
                            sd_sentiment = sd(ave_sentiment),
                            Total_word_count = sum(word_count)),
                     by = Topic]
sentiment_by_Topic


t.test(sentiment_by_tweet[Topic ==1,ave_sentiment], sentiment_by_tweet[Topic ==2,ave_sentiment])

df$polarity_score = sentiment_by_tweet$ave_sentiment
ggplot(df,aes(x=roundTime, y=polarity_score, fill=roundTime)) + 
  geom_boxplot()
df$roundTime <- as.factor(df$roundTime)

ggplot(df,aes(x=roundTime, y=polarity_score, fill = roundTime)) + 
  geom_boxplot() +
  guides(fill=FALSE) + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))


ggplot(df,aes(x=created_at, y=polarity_score,col=roundTime)) + 
  geom_point(size=0.4, alpha=0.9) + 
  theme(legend.position="none")

Updated.df[Topic == 1,.(text)] %>% 
  head 

Updated.df[Topic == 1,.(text)] %>% 
  get_sentences() %>% #extract all sentences
  head 


Updated.df[Topic == 1,.(text)] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  head  


Updated.df[Topic == 1,.(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE)


Updated.df[,list(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table


Updated.df[,list(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,negative] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table




Updated.df[Topic == 2,.(text),] %>% 
  get_sentences() %>%              # get sentences
  extract_sentiment_terms() %>%    # extract negative terms
  .[,positive] %>%                 # select the negative colum
  unlist %>%                       # unlist
  table  %>%                       # create freq table
  sort(decreasing = TRUE) %>% 
  head(10) %>% 
  as.data.frame.table

max_terms = 10
for (i in topics) {
  neg <- tweets.df %>% subset(Topic == i) %>% 
    .[,text] %>% unlist() %>% 
    extract_sentiment_terms() %>% .[,negative] %>% unlist
  
  pos <- tweets.df %>% subset(Topic == i) %>% 
    .[,text] %>% unlist() %>% 
    extract_sentiment_terms() %>% .[,positive] %>% unlist
  
  pos <- sort(table(pos), decreasing = TRUE)
  # this is the same thing if you want to use pipes:
  #pos %>% table %>% sort(decreasing = TRUE)
  
  neg <- sort(table(neg), decreasing = TRUE)
  
  print(paste("Topic",i))
  print(pos[1:min(max_terms,length(pos))])
  
  print(neg[1:min(max_terms,length(neg))])
  print("------------------------------------------------")
}


head(lexicon::hash_sentiment_jockers_rinker)

my_key = lexicon::hash_sentiment_jockers_rinker


my_key <- update_key(my_key, drop = c("child", "please"))


pos <- Updated.df %>% subset(Topic == 1) %>% 
  .[,text] %>% unlist() %>% 
  extract_sentiment_terms(polarity_dt = my_key) %>% 
  .[,positive] %>% unlist

pos <- sort(table(pos), decreasing = TRUE)
print(pos)



#######Topic modeling

#1.  run LDA from library textmodels















