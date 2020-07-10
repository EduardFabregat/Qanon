options(stringsAsFactors = FALSE)

library(tidyverse)

colnames(Q)

pat <- sprintf('((?:\\b\\W*@\\w+)+)')

m <- gregexpr(pat, Q$Contents, perl = TRUE)

Q$RT <- regmatches(Q$Contents, m)

head(Q)

Q2 <- Q[,c(4,22)]

Q2 <- Q2[Q2$RT != "character0",]

Q3 <- unnest(Q2, RT)

Q3$newAuthor <- gsub("@", "", Q3$Author)

Q4 <- Q3 %>% 
    separate_rows(RT) %>%
    filter(!RT %in% c('c', ''))

Q4$newRT <- Q4$RT

Q5 <- Q4[, c(3,4)]

rt_ment <- as.matrix(Q5)

library(igraph)

## Create the network graph in R based on our edge matrix
g <- graph.edgelist(rt_ment)

#Now I calculate the eigenvector centrality
#a measure of the influence of a node in a network
#And I turn the results into a df to later filter the users by 
#their centrality
eigen_cent <- eigen_centrality(g)
eigen_cent_users <- as.data.frame(eigen_cent[[1]])
eigen_cent_users$newAuthor <- rownames(eigen_cent_users)
colnames(eigen_cent_users)[1] <- "eigen_cent"

#Since the network is very big I discard the 99% of users
#and keep only the top ones
top_1_eigen <- quantile(eigen_cent_users$eigen_cent, prob = .999)

eigen_cent_users$top_1_eigen_users <- ifelse(eigen_cent_users$eigen_cent >= top_1_eigen, eigen_cent_users$newAuthor,NA)

#select those two columns
top_users <- eigen_cent_users %>% 
  select(eigen_cent, top_1_eigen_users)

#change the name of the column so they have the same one
colnames(top_users)[colnames(top_users)== "top_1_eigen_users"] <- "newAuthor"

#and merge the dfs by screen_name
poster_retweet <- left_join(as.data.frame(rt_ment), top_users, by = "newAuthor")

#delete NAs
top_users2 <- na.omit(poster_retweet)

top_users3 <- as.matrix(top_users2)

#and back into an igraph object again
g <- graph_from_data_frame(top_users3)

#now we can find information about the network
is.connected(g)
is.directed(g)
vcount(g)
ecount(g)
is.weighted(g)

#Make it a weighted network
#shortest path functions use weighted network as the cost of the path; 
#community finding methods use weights as the strength of the relationship between two vertices
wg <- g

E(wg)$weight <- runif(ecount(wg))


#Degree is he number of adjacent edges to each node. 
#It is often considered a measure of direct influence.
#In directed networks the in-degree and the out-degree can be calculated
#The in-degree tells who is being retweeted and mentioned
degree_in <- sort(degree(wg, mode = "in"), decreasing = TRUE)

V(wg)$degree_in <- degree(wg, mode = "in")

#The out-degree who's retweeting and mentioning a lot
degree_out <- sort(degree(wg, mode = "out"), decreasing = TRUE)

V(wg)$degree_out <- degree(wg, mode = "out")

#to have a look at it
degree_in_df <- as.data.frame(degree_in)

degree_out_df <- as.data.frame(degree_out)

#The strength is a weighted degree distribution
#obtained simply by summing up the weights of edges incident to a given vertex
V(wg)$strength <- strength(wg, mode = "in")

#to have a look at it
strength <- sort(strength(wg),decreasing = TRUE)

strength_df <- as.data.frame(strength)


#Some community detection algorithms only work with undirected networks
#collapse doesn't create multiple edges

is.directed(wg)
is.simple(wg)
und_net <-as.undirected(wg, mode= "collapse",
                        edge.attr.comb=list(weight="sum", "ignore"))

is.directed(und_net)
is.simple(und_net)

#the network has multiple edges and loops
und_net2 <- igraph::simplify(und_net, remove.multiple = T, remove.loops = T,
                     edge.attr.comb=c(weight="sum", type="ignore"))

is.simple(und_net2)

vcount(und_net2)
ecount(und_net2)

library(skynet)

und_net3 <- disparity_filter(g=und_net2,alpha=0.12) #aquesta es l'alpha que redueix el numero de edges i mante la xarxa conectada

is.connected(und_net3)
vcount(und_net3)
ecount(und_net3)

#and run clustering algorithms
mylouvain <- cluster_louvain(und_net3)

V(und_net3)$louvain <- mylouvain$membership

mylabel <- cluster_label_prop(und_net3)

V(und_net3)$mylabel <- mylabel$membership

myspinglass <- cluster_spinglass(und_net3)

V(und_net3)$spinglass <- myspinglass$membership

myfastgreedy <- cluster_fast_greedy(und_net3)

V(und_net3)$myfastgreedy <- myfastgreedy$membership

myinfo <- cluster_infomap(und_net3)

V(und_net3)$info <- myinfo$membership


#Betweenness measures brokerage or gatekeeping potential, a centrality measure
#It measures the extent to which a vertex is located ‘between’ other pairs of vertices
V(und_net3)$btw <- betweenness(und_net3, v = V(und_net3), directed = FALSE)

#turn it into a df to see
between <- sort(betweenness(und_net3, v = V(und_net3), directed = FALSE), decreasing = TRUE)

between_df <- as.data.frame(between)

#K-core analysis identifies the core and the periphery of the network. A k-core is a maximal
#subnet of a network such that all nodes have at least degree K

V(und_net3)$core <- coreness(und_net3)

#and finally save we it as a gephi file
write.graph(und_net3, "Qanon.graphml", format = "graphml")
