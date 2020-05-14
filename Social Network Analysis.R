options(stringsAsFactors = FALSE)

library(tidyverse)

#I pull all the user names and names of users mentioned
mentions <- Qanon %>% 
  select(screen_name, mentions_screen_name) 

#Next, since mentions_scree_names gives a list of names,
#I unnest all the mentioned user names to get two columns
#with only one name per row and column
mentions <- mentions %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name))

#Now I get the retweets
Qanon_rt <- Qanon %>% 
  filter(is_retweet == T)

#And I get the names of the users and the names of the people
#that got retweeted
Qanon_rt_net <- Qanon_rt %>% 
  select(screen_name, retweet_screen_name)

#I delete NAs
poster_retweet <- na.omit(Qanon_rt_net)

#change names of columns so both dfs have the same names
colnames(poster_retweet)[colnames(poster_retweet)== "retweet_screen_name"] <- "receiver"
colnames(mentions)[colnames(mentions)=="mentions_screen_name"] <- "receiver"

#create a df for both mentions and retweets
rt_ment <- tribble()

#join the dfs
rt_ment <- rt_ment %>% 
  bind_rows(poster_retweet, mentions)

#turn them into a matrix
rt_ment <- as.matrix(rt_ment)

library(igraph)

#and into an igraph object
g <- graph.edgelist(rt_ment)

#Now I calculate the eigenvector centrality
#And I turn the results into a df to later filter the users by 
#their centrality
eigen_cent <- eigen_centrality(g)
eigen_cent_users <- as.data.frame(eigen_cent[[1]])
eigen_cent_users$screen_name <- rownames(eigen_cent_users)
colnames(eigen_cent_users)[1] <- "eigen_cent"


#Since the network is very big I discard the 99.9% of users
#and keep only the top ones
top_5_eigen <- quantile(eigen_cent_users$eigen_cent, prob = .999)
eigen_cent_users$top_5_eigen_users <- ifelse(eigen_cent_users$eigen_cent >= top_5_eigen, eigen_cent_users$screen_name,NA)

#select those two columns
top_users <- eigen_cent_users %>% 
  select(eigen_cent, top_5_eigen_users)

#change the name of the column so they have the same one
colnames(top_users)[colnames(top_users)== "top_5_eigen_users"] <- "screen_name"

#and merge the dfs by sreen_name
poster_retweet <- left_join(as.data.frame(rt_ment), top_users, by = "screen_name")

#delete NAs
top_users2 <- na.omit(poster_retweet)

#and turn it into a matrix
top_users3 <- as.matrix(top_users2)

#and back into an igraph object again
g <- graph_from_data_frame(top_users3)

#I make it a weighted graph
wg <- g

E(wg)$weight <- runif(ecount(wg))

#I calculate the in-degree
degree_in <- sort(degree(wg, mode = "in"))

V(wg)$degree_in <- degree(wg, mode = "in")

#and the out-degree to maybe see who's retweeting and talking a lot
degree_out <- sort(degree(wg, mode = "out"))

V(wg)$degree_out <- degree(wg, mode = "out")

#to have a look at it
degree_in_df <- as.data.frame(degree_in)

degree_out_df <- as.data.frame(degree_out)

#calculate the strength
V(wg)$strength <- strength(wg, mode = "in")

#to have a look at it
strength <- sort(strength(wg))

strength_df <- as.data.frame(strength)

#I turn it into an undirected graph
und_net <-as.undirected(wg, mode= "collapse",
                        edge.attr.comb=list(weight="sum", "ignore"))


#the network has multiple edges and loops, I simplify it
und_net2 <- simplify(und_net, remove.multiple = T, remove.loops = T,
                     edge.attr.comb=c(weight="sum", type="ignore"))

#and run clustering algorithms
#12
mylouvain <- cluster_louvain(und_net2)

V(und_net2)$louvain <- mylouvain$membership

#21
mylabel <- cluster_label_prop(und_net2)

V(und_net2)$mylabel <- mylabel$membership

#25
myspinglass <- cluster_spinglass(und_net2)

V(und_net2)$spinglass <- myspinglass$membership

#12
myfastgreedy <- cluster_fast_greedy(und_net2)

V(und_net2)$myfastgreedy <- myfastgreedy$membership

#43
myinfo <- cluster_infomap(und_net2)

V(und_net2)$info <- myinfo$membership


#I calculate the betweenness
V(und_net2)$btw <- betweenness(und_net2, v = V(und_net2), directed = FALSE)

#turn it into a df to see
between <- betweenness(und_net2, v = V(und_net2), directed = FALSE)

between_df <- as.data.frame(between)

trans <- transitivity(und_net2)

trans_avg <- transitivity(und_net2, type = "average")

core <- coreness(und_net2)

core_df <- as.data.frame(core)

V(und_net2)$core <- coreness(und_net2)

core_df$screen_name <- rownames(core_df)

modularity(und_net2, V(und_net2)$louvain)

centralize(und_net2, theoretical.max = 0, normalized = F)

centr_degree(und_net2)$centralization
centr_clo(und_net2, mode="all")$centralization
centr_eigen(und_net2, directed=FALSE)$centralization

save.image("Qanon.Network.RData")

#and finally save it as a gephi file
write.graph(und_net2, "Qanon.graphml", format = "graphml")
