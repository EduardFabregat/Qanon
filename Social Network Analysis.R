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


###############################################################
############ Weekly Qanon Networks vs Random Networks Same Characteristics

Q <- Q[order(Q$date),]

Q$week <- 1+ as.numeric(Q$date - as.Date("2017-11-05")) %/% 7

pat <- sprintf('((?:\\b\\W*@\\w+)+)')

m <- gregexpr(pat, Q$Contents, perl = TRUE)

Q$RT <- regmatches(Q$Contents, m)

Q$RT <- gsub("[[:punct:]]+","", Q$RT)


library(igraph)

nodes_list <- list()
edges_list <- list()
weights_list <- list()
comm_list <- list()
clusters_list <- list()
trans_list <- list()
#eff_list <- list()
dist_list <- list()
mod_list <- list()
degree_list <- list()
max_in <- list()
max_out <- list()

for (i in 1:134){
  print(i)
  Q1 <- Q[Q$week == i, ]
  Q2 <- Q1[,c(5,24)]
  Q2 <- Q2[Q2$RT != "character0",]
  print("Unnesting")
  Q3 <- unnest(Q2, RT)
  Q3$newAuthor <- gsub("@", "", Q3$Author)
  Q4 <- Q3 %>% 
    separate_rows(RT) %>%
    filter(!RT %in% c('c', ''))
  Q4$newRT <- Q4$RT
  Q5 <- Q4[, c(3,4)]
  rt_ment <- as.matrix(Q5)
  g <- graph.edgelist(rt_ment)
  wg <- g
  E(wg)$weight <- runif(ecount(wg))
  nodes_list[[i]] <- vcount(wg)
  print(nodes_list[[i]])
  edges_list[[i]] <- ecount(wg)
  weights_list[[i]] <- E(wg)$weight
  print(edges_list[[i]])
  degree_list[[i]] <- mean(degree(wg, mode = "total"))
  print(degree_list[[i]])
  max_in[[i]] <- max(degree(wg, mode = "in"))
  print(max_in[[i]])
  max_out[[i]] <- max(degree(wg, mode = "out"))
  print(max_out[[i]])
  und_net <-as.undirected(wg, mode= "collapse",
                        edge.attr.comb=list(weight="sum", "ignore"))

  und_net2 <- igraph::simplify(und_net, remove.multiple = T, remove.loops = T,
                             edge.attr.comb=c(weight="sum", type="ignore"))
  clusters_list[[i]] <- length(decompose(und_net2))
  print(clusters_list[[i]])
  mylouvain <- cluster_louvain(und_net2)
  print(length(mylouvain))
  comm_list[[i]] <- length(mylouvain)
  V(und_net2)$louvain <- mylouvain$membership
  if(is.connected(und_net2) == FALSE){
    trans_list[[i]] <- 0
  } else {
    trans <- transitivity(und_net2)
    trans_list[[i]] <- trans
  }
  print(trans_list[[i]])
  distance <- mean_distance(und_net2)
  print(distance)
  dist_list[[i]] <- distance
  modularity <- modularity(und_net2, V(und_net2)$louvain)
  print(modularity)
  mod_list[[i]] <- modularity
  print("Removing everything")
  rm(g,wg,und_net,und_net2,mylouvain,distance,modularity)
}


clusters_df <- data.frame(matrix(unlist(clusters_list), nrow=length(clusters_list), byrow=T))
colnames(clusters_df)[1] <- "clusters"
comm_df <- data.frame(matrix(unlist(comm_list), nrow=length(comm_list), byrow=T))
colnames(comm_df)[1] <- "communities"
edges_df <- data.frame(matrix(unlist(edges_list), nrow=length(edges_list), byrow=T))
colnames(edges_df)[1] <- "edges"
nodes_df <- data.frame(matrix(unlist(nodes_list), nrow=length(nodes_list), byrow=T))
colnames(nodes_df)[1] <- "nodes"
trans_df <- data.frame(matrix(unlist(trans_list), nrow=length(trans_list), byrow=T))
colnames(trans_df)[1] <- "transitivity"
mod_df <- data.frame(matrix(unlist(mod_list), nrow=length(mod_list), byrow=T))
colnames(mod_df)[1] <- "modularity"
dist_df <- data.frame(matrix(unlist(dist_list), nrow=length(dist_list), byrow=T))
colnames(dist_df)[1] <- "distance"
degree_df <- data.frame(matrix(unlist(degree_list), nrow=length(degree_list), byrow=T))
colnames(degree_df)[1] <- "degree"
in_df <- data.frame(matrix(unlist(max_in), nrow=length(max_in), byrow=T))
colnames(in_df)[1] <- "max_in"
out_df <- data.frame(matrix(unlist(max_out), nrow=length(max_out), byrow=T))
colnames(out_df)[1] <- "max_out"


net_info <- cbind(nodes_df,edges_df,clusters_df,dist_df,trans_df,mod_df,
                  comm_df,degree_df,in_df,out_df)

save(net_info, file="Network.Information.Weekly.Rda")
save(weights_list, file="Weights.Networks.Weekly.Rda")


set.seed(1983)
random_trans <- list()
random_mod <- list()
random_dist <- list()
random_clusters <- list()
random_comm <- list()
random_degree <- list()
random_in <- list()
random_out <- list()
sd_trans <- list()
sd_mod <- list()
sd_dist <- list()
sd_comm <- list()
sd_clusters <- list()
sd_degree <- list()
sd_in <- list()
sd_out <- list()

for (i in 1:nrow(net_info)) {
  print(i)
  louvain_temp <- list()
  trans_temp <- list()
  dist_temp <- list()
  mod_temp <- list()
  clusters_temp <- list()
  degree_temp <- list()
  in_temp <- list()
  out_temp <- list()
  for (x in seq_len(100L)) {
    print(x)
    g <- sample_gnm(net_info$nodes[i], net_info$edges[i], 
                    directed = TRUE, loops = TRUE)
    E(g)$weight <- sample(weights_list[[i]])
    degree_temp[[x]] <- mean(degree(g, mode = "total"))
    in_temp[[x]] <- max(degree(g, mode = "in"))
    out_temp[[x]] <- max(degree(g, mode = "out"))
    und_net <-as.undirected(wg, mode= "collapse",
                            edge.attr.comb=list(weight="sum", "ignore"))
    
    und_net2 <- igraph::simplify(und_net, remove.multiple = T, remove.loops = T,
                                 edge.attr.comb=c(weight="sum", type="ignore"))
    clusters_temp[[x]] <- length(decompose(und_net2))
    mylouvain <- cluster_louvain(und_net2)
    louvain_temp[[x]] <- length(mylouvain)
    V(und_net2)$louvain <- mylouvain$membership
    if(is.connected(und_net2) == FALSE){
      trans_temp[[x]] <- 0
    } else {
      trans_temp[[x]] <- transitivity(und_net2)
    }
    dist_temp[[x]] <- mean_distance(und_net2)
    mod_temp[[x]] <- modularity(und_net2, V(und_net2)$louvain)
  }
  random_trans[[i]] <- mean(unlist(trans_temp)) 
  random_dist[[i]] <- mean(unlist(dist_temp))
  random_mod[[i]] <- mean(unlist(mod_temp))
  random_comm[[i]] <- mean(unlist(louvain_temp))
  random_clusters[[i]] <- mean(unlist(clusters_temp))
  random_degree[[i]] <- mean(unlist(degree_temp))
  random_in[[i]] <- mean(unlist(in_temp))
  random_out[[i]] <- mean(unlist(out_temp))
  sd_trans[[i]] <- sd(unlist(trans_temp))
  sd_mod[[i]] <- sd(unlist(mod_temp))
  sd_dist[[i]] <- sd(unlist(dist_temp))
  sd_comm[[i]] <- sd(unlist(louvain_temp))
  sd_clusters[[i]] <- sd(unlist(clusters_temp))
  sd_degree[[i]] <- sd(unlist(degree_temp))
  sd_in[[i]] <- sd(unlist(in_temp))
  sd_out[[i]] <- sd(unlist(out_temp))
  rm(g,und_net,und_net2,mylouvain,trans_temp,dist_temp,mod_temp,
     louvain_temp,clusters_temp,degree_temp,in_temp,out_temp)
}

random_clusters_df <- data.frame(matrix(unlist(random_clusters), nrow=length(random_clusters), byrow=T))
colnames(random_clusters_df)[1] <- "random_clusters"
random_trans_df <- data.frame(matrix(unlist(random_trans), nrow=length(random_trans), byrow=T))
colnames(random_trans_df)[1] <- "random_transitivity"
random_mod_df <- data.frame(matrix(unlist(random_mod), nrow=length(random_mod), byrow=T))
colnames(random_mod_df)[1] <- "random_modularity"
random_dist_df <- data.frame(matrix(unlist(random_dist), nrow=length(random_dist), byrow=T))
colnames(random_dist_df)[1] <- "random_distance"
random_comm_df <- data.frame(matrix(unlist(random_comm), nrow=length(random_comm), byrow=T))
colnames(random_comm_df)[1] <- "random_communities"
random_degree_df <- data.frame(matrix(unlist(random_degree), nrow=length(random_degree), byrow=T))
colnames(random_degree_df)[1] <- "random_degree"
random_in_df <- data.frame(matrix(unlist(random_in), nrow=length(random_in), byrow=T))
colnames(random_in_df)[1] <- "random_in"
random_out_df <- data.frame(matrix(unlist(random_out), nrow=length(random_out), byrow=T))
colnames(random_out_df)[1] <- "random_out"

sd_clusters_df <- data.frame(matrix(unlist(sd_clusters), nrow=length(sd_clusters), byrow=T))
colnames(sd_clusters_df)[1] <- "sd_clusters"
sd_trans_df <- data.frame(matrix(unlist(sd_trans), nrow=length(sd_trans), byrow=T))
colnames(sd_trans_df)[1] <- "sd_transitivity"
sd_mod_df <- data.frame(matrix(unlist(sd_mod), nrow=length(sd_mod), byrow=T))
colnames(sd_mod_df)[1] <- "sd_modularity"
sd_dist_df <- data.frame(matrix(unlist(sd_dist), nrow=length(sd_dist), byrow=T))
colnames(sd_dist_df)[1] <- "sd_distance"
sd_comm_df <- data.frame(matrix(unlist(sd_comm), nrow=length(sd_comm), byrow=T))
colnames(sd_comm_df)[1] <- "sd_communities"
sd_degree_df <- data.frame(matrix(unlist(sd_degree), nrow=length(sd_degree), byrow=T))
colnames(sd_degree_df)[1] <- "sd_degree"
sd_in_df <- data.frame(matrix(unlist(sd_in), nrow=length(sd_in), byrow=T))
colnames(sd_in_df)[1] <- "sd_in"
sd_out_df <- data.frame(matrix(unlist(sd_out), nrow=length(sd_out), byrow=T))
colnames(sd_out_df)[1] <- "sd_out"

random_net_info <- cbind(random_clusters_df,random_dist_df,random_trans_df,random_mod_df,
                         random_comm_df,random_degree_df,random_in_df,random_out_df,
                         sd_clusters_df,sd_trans_df,sd_mod_df,sd_dist_df,sd_comm_df,
                         sd_degree_df,sd_in_df,sd_out_df)

random_net_info$random_clusters <- round(random_net_info$random_clusters)

net_info2 <- cbind(net_info, random_net_info)

save(net_info2, file = "Network.Information.With.Random.Weekly.Rda")

net_info2$norm_clust <- (net_info2$clusters-net_info2$random_clusters)/net_info2$sd_clusters
net_info2$norm_dist <- (net_info2$distance-net_info2$random_distance)/net_info2$sd_distance
net_info2$norm_trans <- (net_info2$transitivity-net_info2$random_transitivity)/net_info2$sd_transitivity
net_info2$norm_mod <- (net_info2$modularity-net_info2$random_modularity)/net_info2$sd_modularity
net_info2$norm_comm <- (net_info2$communities-net_info2$random_communities)/net_info2$sd_communities
net_info2$norm_in <- (net_info2$max_in-net_info2$random_in)/net_info2$sd_in
net_info2$norm_out <- (net_info2$max_out-net_info2$random_out)/net_info2$sd_out
net_info2$norm_degree <- (net_info2$degree-net_info2$random_degree)/net_info2$sd_degree


#net_info2$sd_clust <- (net_info2$clusters-net_info2$random_clusters)/sqrt(2)
#net_info2$sd_mod <- (net_info2$modularity-net_info2$random_modularity)/sqrt(2)
#net_info2$sd_dist <- (net_info2$distance-net_info2$random_distance)/sqrt(2)
#net_info2$sd_trans <- (net_info2$transitivity-net_info2$random_transitivity)/sqrt(2)

#Una altra manera de fer-ho
#net_info2$sd_mod <- apply(net_info2[,c(6,10)],1,sd)

#############################################
####### Plot network measurements

library(tidyverse)

net_info2$index <- 1:nrow(net_info2)

nodes_plot <- ggplot(net_info2, aes(x=index)) +
  #geom_line(aes(y=modularity),color="blue") +
  #geom_line(aes(y=random_modularity),color="red")+
  geom_line(aes(y=nodes),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Nodes")

edges_plot <- ggplot(net_info2, aes(x=index)) +
  #geom_line(aes(y=modularity),color="blue") +
  #geom_line(aes(y=random_modularity),color="red")+
  geom_line(aes(y=edges),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Edges")


mod_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=modularity),color="blue") +
  geom_line(aes(y=random_modularity),color="red")+
  geom_line(aes(y=norm_mod),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Modularity")

trans_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=transitivity),color="blue") +
  geom_line(aes(y=random_transitivity),color="red")+
  geom_line(aes(y=norm_trans),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Transitivity")


dist_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=distance),color="blue") +
  geom_line(aes(y=random_distance),color="red")+
  geom_line(aes(y=norm_dist),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Mean Distance")

clusters_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=clusters),color="blue") +
  geom_line(aes(y=random_clusters),color="red")+
  geom_line(aes(y=norm_clust),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Clusters")

comm_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=communities),color="blue") +
  geom_line(aes(y=random_communities),color="red")+
  geom_line(aes(y=norm_comm),color="black")+
  theme_bw()+
  xlab("Week") +
  ylab("Commuities")

in_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=max_in),color="blue") +
  geom_line(aes(y=random_in),color="red")+
  geom_line(aes(y=norm_in),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Degree In")

out_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=max_out),color="blue") +
  geom_line(aes(y=random_out),color="red")+
  geom_line(aes(y=norm_out),color="black")+
  theme_bw()+
  xlab("Week") +
  ylab("Degree Out")

degree_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=degree),color="blue") +
  geom_line(aes(y=random_degree),color="red")+
  #geom_line(aes(y=norm_degree),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Degree")

library(grid)
library(gridExtra)

grid.arrange(nodes_plot, edges_plot,mod_plot,trans_plot,dist_plot,clusters_plot, ncol=1)
grid.arrange(nodes_plot, edges_plot, ncol=1)
grid.arrange(degree_plot,in_plot,out_plot,ncol=1)


#############################
###### Qanon minus mean

net_info2$norm_clust <- (net_info2$clusters-net_info2$random_clusters)
net_info2$norm_dist <- (net_info2$distance-net_info2$random_distance)
net_info2$norm_trans <- (net_info2$transitivity-net_info2$random_transitivity)
net_info2$norm_mod <- (net_info2$modularity-net_info2$random_modularity)
net_info2$norm_comm <- (net_info2$communities-net_info2$random_communities)
net_info2$norm_in <- (net_info2$max_in-net_info2$random_in)
net_info2$norm_out <- (net_info2$max_out-net_info2$random_out)
net_info2$norm_degree <- (net_info2$degree-net_info2$random_degree)


mod_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=modularity),color="blue") +
  #geom_line(aes(y=random_modularity),color="red")+
  geom_line(aes(y=norm_mod),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Modularity")

trans_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=transitivity),color="blue") +
  #geom_line(aes(y=random_transitivity),color="red")+
  geom_line(aes(y=norm_trans),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Transitivity")


dist_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=distance),color="blue") +
  #geom_line(aes(y=random_distance),color="red")+
  geom_line(aes(y=norm_dist),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Mean Distance")

clusters_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=clusters),color="blue") +
  #geom_line(aes(y=random_clusters),color="red")+
  geom_line(aes(y=norm_clust),color="black")+
  theme_bw()+
  xlab("Week") +
  ylab("Clusters")

comm_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=communities),color="blue") +
  #geom_line(aes(y=random_communities),color="red")+
  geom_line(aes(y=norm_comm),color="black")+
  theme_bw()+
  xlab("Week") +
  ylab("Commuities")

library(grid)
library(gridExtra)

grid.arrange(mod_plot,rans_plot,dist_plot,clusters_plot,comm_plot,ncol=1)

sd(net_info2$clusters) #89.94383
sd(net_info2$random_clusters) #102.5114
sd(net_info2$distance) #0.2373969
sd(net_info2$random_distance) #0.7620563
sd(net_info2$modularity) #0.06117486
sd(net_info2$random_modularity) #0.07569048
sd(net_info2$transitivity) #0.00724264
sd(net_info2$random_transitivity) #0.0001887559

vardiff.est <- var(net_info2$modularity)+var(net_info2$random_modularity) # valid for independent normal distributions
(sd = sqrt(vardiff.est))

# non-parametric estimation
o <- outer(net_info2$modularity,net_info2$random_modularity,'-')
sd(o)  # vardiff.est seems to be close enough
mean(o)
hist(o, freq=F)
curve(dnorm(x, mean(net_info2$modularity)-mean(net_info2$random_modularity), sd), add=T)


vardiff.est = var(net_info2$distance)+var(net_info2$random_distance) # valid for independent normal distributions
(sd = sqrt(vardiff.est))

# non-parametric estimation
o <- outer(net_info2$distance,net_info2$random_distance,'-')
sd(o)  # vardiff.est seems to be close enough
mean(o)
hist(o, freq=F)
curve(dnorm(x, mean(net_info2$distance)-mean(net_info2$random_distance), sd), add=T)


vardiff.est = var(net_info2$transitivity)+var(net_info2$random_transitivity) # valid for independent normal distributions
(sd = sqrt(vardiff.est))

# non-parametric estimation
o <- outer(net_info2$transitivity,net_info2$random_transitivity,'-')
sd(o)  # vardiff.est seems to be close enough
mean(o)
hist(o, freq=F)
curve(dnorm(x, mean(net_info2$transitivity)-mean(net_info2$random_transitivity), sd), add=T)


vardiff.est = var(net_info2$clusters)+var(net_info2$random_clusters) # valid for independent normal distributions
(sd = sqrt(vardiff.est))

# non-parametric estimation
o <- outer(net_info2$clusters,net_info2$random_clusters,'-')
sd(o)  # vardiff.est seems to be close enough
mean(o)
hist(o, freq=F)
curve(dnorm(x, mean(net_info2$clusters)-mean(net_info2$clusters), sd), add=T)

#####################################################
####### With density

library(tidyverse)

Q <- Q[order(Q$date),]

Q$week <- 1+ as.numeric(Q$date - as.Date("2017-11-05")) %/% 7

pat <- sprintf('((?:\\b\\W*@\\w+)+)')

m <- gregexpr(pat, Q$Contents, perl = TRUE)

Q$RT <- regmatches(Q$Contents, m)

Q$RT <- gsub("[[:punct:]]+","", Q$RT)

rm(m)

library(igraph)

nodes_list <- list()
edges_list <- list()
density_list <- list()
weights_list <- list()
comm_list <- list()
clusters_list <- list()
trans_list <- list()
#eff_list <- list()
dist_list <- list()
mod_list <- list()
degree_list <- list()
max_in <- list()
max_out <- list()

for (i in 1:134){
  print(i)
  Q1 <- Q[Q$week == i, ]
  Q2 <- Q1[,c(5,24)]
  Q2 <- Q2[Q2$RT != "character0",]
  print("Unnesting")
  Q3 <- unnest(Q2, RT)
  Q3$newAuthor <- gsub("@", "", Q3$Author)
  Q4 <- Q3 %>% 
    separate_rows(RT) %>%
    filter(!RT %in% c('c', ''))
  Q4$newRT <- Q4$RT
  Q5 <- Q4[, c(3,4)]
  rt_ment <- as.matrix(Q5)
  g <- graph.edgelist(rt_ment)
  wg <- g
  E(wg)$weight <- runif(ecount(wg))
  nodes_list[[i]] <- vcount(wg)
  print(nodes_list[[i]])
  edges_list[[i]] <- ecount(wg)
  weights_list[[i]] <- E(wg)$weight
  print(edges_list[[i]])
  degree_list[[i]] <- mean(degree(wg, mode = "total"))
  print(degree_list[[i]])
  max_in[[i]] <- max(degree(wg, mode = "in"))
  print(max_in[[i]])
  max_out[[i]] <- max(degree(wg, mode = "out"))
  print(max_out[[i]])
  density_list[[i]] <- edge_density(wg)
  und_net <-as.undirected(wg, mode= "collapse",
                          edge.attr.comb=list(weight="sum", "ignore"))
  und_net2 <- igraph::simplify(und_net, remove.multiple = T, remove.loops = T,
                               edge.attr.comb=c(weight="sum", type="ignore"))
  clusters_list[[i]] <- length(decompose(und_net2))
  print(clusters_list[[i]])
  mylouvain <- cluster_louvain(und_net2)
  print(length(mylouvain))
  comm_list[[i]] <- length(mylouvain)
  V(und_net2)$louvain <- mylouvain$membership
  if(is.connected(und_net2) == FALSE){
    trans_list[[i]] <- 0
  } else {
    trans <- transitivity(und_net2)
    trans_list[[i]] <- trans
  }
  print(trans_list[[i]])
  distance <- mean_distance(und_net2)
  print(distance)
  dist_list[[i]] <- distance
  modularity <- modularity(und_net2, V(und_net2)$louvain)
  print(modularity)
  mod_list[[i]] <- modularity
  print("Removing everything")
  rm(g,wg,und_net,und_net2,mylouvain,distance,modularity)
}

density_df <- data.frame(matrix(unlist(density_list),nrow=length(density_list),byrow = T))
colnames(density_df)[1] <- "density"
clusters_df <- data.frame(matrix(unlist(clusters_list), nrow=length(clusters_list), byrow=T))
colnames(clusters_df)[1] <- "clusters"
comm_df <- data.frame(matrix(unlist(comm_list), nrow=length(comm_list), byrow=T))
colnames(comm_df)[1] <- "communities"
edges_df <- data.frame(matrix(unlist(edges_list), nrow=length(edges_list), byrow=T))
colnames(edges_df)[1] <- "edges"
nodes_df <- data.frame(matrix(unlist(nodes_list), nrow=length(nodes_list), byrow=T))
colnames(nodes_df)[1] <- "nodes"
trans_df <- data.frame(matrix(unlist(trans_list), nrow=length(trans_list), byrow=T))
colnames(trans_df)[1] <- "transitivity"
mod_df <- data.frame(matrix(unlist(mod_list), nrow=length(mod_list), byrow=T))
colnames(mod_df)[1] <- "modularity"
dist_df <- data.frame(matrix(unlist(dist_list), nrow=length(dist_list), byrow=T))
colnames(dist_df)[1] <- "distance"
degree_df <- data.frame(matrix(unlist(degree_list), nrow=length(degree_list), byrow=T))
colnames(degree_df)[1] <- "degree"
in_df <- data.frame(matrix(unlist(max_in), nrow=length(max_in), byrow=T))
colnames(in_df)[1] <- "max_in"
out_df <- data.frame(matrix(unlist(max_out), nrow=length(max_out), byrow=T))
colnames(out_df)[1] <- "max_out"

net_info_dens <- cbind(nodes_df,edges_df,density_df,clusters_df,dist_df,trans_df,mod_df,
                  comm_df,degree_df,in_df,out_df)

save(net_info_dens, file="Network.Information.Weekly.Density.Rda")

set.seed(1983)
random_trans <- list()
random_mod <- list()
random_dist <- list()
random_clusters <- list()
random_comm <- list()
random_degree <- list()
random_in <- list()
random_out <- list()
sd_trans <- list()
sd_mod <- list()
sd_dist <- list()
sd_comm <- list()
sd_clusters <- list()
sd_degree <- list()
sd_in <- list()
sd_out <- list()

for (i in 1:nrow(net_info_dens)) {
  print(i)
  louvain_temp <- list()
  trans_temp <- list()
  dist_temp <- list()
  mod_temp <- list()
  clusters_temp <- list()
  degree_temp <- list()
  in_temp <- list()
  out_temp <- list()
  for (x in seq_len(100L)) {
    #print(x)
    g <- sample_gnp(net_info_dens$nodes[i], net_info_dens$density[i], 
                    directed = TRUE, loops = TRUE)
    wg <- g
    E(wg)$weight <- runif(ecount(wg))
    degree_temp[[x]] <- mean(degree(wg, mode = "total"))
    in_temp[[x]] <- max(degree(wg, mode = "in"))
    out_temp[[x]] <- max(degree(wg, mode = "out"))
    und_net <-as.undirected(wg, mode= "collapse",
                            edge.attr.comb=list(weight="sum", "ignore"))
    und_net2 <- igraph::simplify(und_net, remove.multiple = T, remove.loops = T,
                                 edge.attr.comb=c(weight="sum", type="ignore"))
    clusters_temp[[x]] <- length(decompose(und_net2))
    mylouvain <- cluster_louvain(und_net2)
    louvain_temp[[x]] <- length(mylouvain)
    V(und_net2)$louvain <- mylouvain$membership
    if(is.connected(und_net2) == FALSE){
      trans_temp[[x]] <- 0
    } else {
      trans_temp[[x]] <- transitivity(und_net2)
    }
    dist_temp[[x]] <- mean_distance(und_net2)
    mod_temp[[x]] <- modularity(und_net2, V(und_net2)$louvain)
  }
  random_trans[[i]] <- mean(unlist(trans_temp)) 
  random_dist[[i]] <- mean(unlist(dist_temp))
  random_mod[[i]] <- mean(unlist(mod_temp))
  random_comm[[i]] <- mean(unlist(louvain_temp))
  random_clusters[[i]] <- mean(unlist(clusters_temp))
  random_degree[[i]] <- mean(unlist(degree_temp))
  random_in[[i]] <- mean(unlist(in_temp))
  random_out[[i]] <- mean(unlist(out_temp))
  sd_trans[[i]] <- sd(unlist(trans_temp))
  sd_mod[[i]] <- sd(unlist(mod_temp))
  sd_dist[[i]] <- sd(unlist(dist_temp))
  sd_comm[[i]] <- sd(unlist(louvain_temp))
  sd_clusters[[i]] <- sd(unlist(clusters_temp))
  sd_degree[[i]] <- sd(unlist(degree_temp))
  sd_in[[i]] <- sd(unlist(in_temp))
  sd_out[[i]] <- sd(unlist(out_temp))
  rm(g,und_net,und_net2,mylouvain,trans_temp,dist_temp,mod_temp,
     louvain_temp,clusters_temp,degree_temp,in_temp,out_temp)
}

random_clusters_df <- data.frame(matrix(unlist(random_clusters), nrow=length(random_clusters), byrow=T))
colnames(random_clusters_df)[1] <- "random_clusters"
random_trans_df <- data.frame(matrix(unlist(random_trans), nrow=length(random_trans), byrow=T))
colnames(random_trans_df)[1] <- "random_transitivity"
random_mod_df <- data.frame(matrix(unlist(random_mod), nrow=length(random_mod), byrow=T))
colnames(random_mod_df)[1] <- "random_modularity"
random_dist_df <- data.frame(matrix(unlist(random_dist), nrow=length(random_dist), byrow=T))
colnames(random_dist_df)[1] <- "random_distance"
random_comm_df <- data.frame(matrix(unlist(random_comm), nrow=length(random_comm), byrow=T))
colnames(random_comm_df)[1] <- "random_communities"
random_degree_df <- data.frame(matrix(unlist(random_degree), nrow=length(random_degree), byrow=T))
colnames(random_degree_df)[1] <- "random_degree"
random_in_df <- data.frame(matrix(unlist(random_in), nrow=length(random_in), byrow=T))
colnames(random_in_df)[1] <- "random_in"
random_out_df <- data.frame(matrix(unlist(random_out), nrow=length(random_out), byrow=T))
colnames(random_out_df)[1] <- "random_out"

sd_clusters_df <- data.frame(matrix(unlist(sd_clusters), nrow=length(sd_clusters), byrow=T))
colnames(sd_clusters_df)[1] <- "sd_clusters"
sd_trans_df <- data.frame(matrix(unlist(sd_trans), nrow=length(sd_trans), byrow=T))
colnames(sd_trans_df)[1] <- "sd_transitivity"
sd_mod_df <- data.frame(matrix(unlist(sd_mod), nrow=length(sd_mod), byrow=T))
colnames(sd_mod_df)[1] <- "sd_modularity"
sd_dist_df <- data.frame(matrix(unlist(sd_dist), nrow=length(sd_dist), byrow=T))
colnames(sd_dist_df)[1] <- "sd_distance"
sd_comm_df <- data.frame(matrix(unlist(sd_comm), nrow=length(sd_comm), byrow=T))
colnames(sd_comm_df)[1] <- "sd_communities"
sd_degree_df <- data.frame(matrix(unlist(sd_degree), nrow=length(sd_degree), byrow=T))
colnames(sd_degree_df)[1] <- "sd_degree"
sd_in_df <- data.frame(matrix(unlist(sd_in), nrow=length(sd_in), byrow=T))
colnames(sd_in_df)[1] <- "sd_in"
sd_out_df <- data.frame(matrix(unlist(sd_out), nrow=length(sd_out), byrow=T))
colnames(sd_out_df)[1] <- "sd_out"




random_net_info_dens <- cbind(random_clusters_df,random_dist_df,random_trans_df,random_mod_df,
                         random_comm_df,random_degree_df,random_in_df,random_out_df,
                         sd_clusters_df,sd_trans_df,sd_mod_df,sd_dist_df,sd_comm_df,
                         sd_degree_df,sd_in_df,sd_out_df)

net_info2 <- cbind(net_info_dens, random_net_info_dens)

save(net_info2, file = "Network.Information.With.Random.Weekly.Density.Rda")

net_info2$norm_clust <- (net_info2$clusters-net_info2$random_clusters)/net_info2$sd_clusters
net_info2$norm_dist <- (net_info2$distance-net_info2$random_distance)/net_info2$sd_distance
net_info2$norm_trans <- (net_info2$transitivity-net_info2$random_transitivity)/net_info2$sd_transitivity
net_info2$norm_mod <- (net_info2$modularity-net_info2$random_modularity)/net_info2$sd_modularity
net_info2$norm_comm <- (net_info2$communities-net_info2$random_communities)/net_info2$sd_communities
net_info2$norm_in <- (net_info2$max_in-net_info2$random_in)/net_info2$sd_in
net_info2$norm_out <- (net_info2$max_out-net_info2$random_out)/net_info2$sd_out
net_info2$norm_degree <- (net_info2$degree-net_info2$random_degree)/net_info2$sd_degree


#net_info2$sd_clust <- (net_info2$clusters-net_info2$random_clusters)/sqrt(2)
#net_info2$sd_mod <- (net_info2$modularity-net_info2$random_modularity)/sqrt(2)
#net_info2$sd_dist <- (net_info2$distance-net_info2$random_distance)/sqrt(2)
#net_info2$sd_trans <- (net_info2$transitivity-net_info2$random_transitivity)/sqrt(2)

#Una altra manera de fer-ho
#net_info2$sd_mod <- apply(net_info2[,c(6,10)],1,sd)

#############################################
####### Plot network measurements

library(tidyverse)

net_info2$index <- 1:nrow(net_info2)

nodes_plot <- ggplot(net_info2, aes(x=index)) +
  #geom_line(aes(y=modularity),color="blue") +
  #geom_line(aes(y=random_modularity),color="red")+
  geom_line(aes(y=nodes),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Nodes")

edges_plot <- ggplot(net_info2, aes(x=index)) +
  #geom_line(aes(y=modularity),color="blue") +
  #geom_line(aes(y=random_modularity),color="red")+
  geom_line(aes(y=edges),color="black")+
  theme_bw()+
  xlab("Week") +
  ylab("Edges")


mod_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=modularity),color="blue") +
  geom_line(aes(y=random_modularity),color="red")+
  #geom_line(aes(y=norm_mod),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Modularity")

trans_plot <- ggplot(net_info2, aes(x=index)) +
  #geom_line(aes(y=transitivity),color="blue") +
  geom_line(aes(y=random_transitivity),color="red")+
  #geom_line(aes(y=norm_trans),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Transitivity")


dist_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=distance),color="blue") +
  geom_line(aes(y=random_distance),color="red")+
  #geom_line(aes(y=norm_dist),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Mean Distance")

clusters_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=clusters),color="blue") +
  geom_line(aes(y=random_clusters),color="red")+
  #geom_line(aes(y=norm_clust),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Clusters")

comm_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=communities),color="blue") +
  geom_line(aes(y=random_communities),color="red")+
  #geom_line(aes(y=norm_comm),color="black")+
  theme_bw()+
  xlab("Week") +
  ylab("Commuities")

in_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=max_in),color="blue") +
  #geom_line(aes(y=random_in),color="red")+
  #geom_line(aes(y=norm_in),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Degree In")

out_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=max_out),color="blue") +
  #geom_line(aes(y=random_out),color="red")+
  #geom_line(aes(y=norm_out),color="black")+
  theme_bw()+
  xlab("Week") +
  ylab("Degree Out")

degree_plot <- ggplot(net_info2, aes(x=index)) +
  geom_line(aes(y=degree),color="blue") +
  #geom_line(aes(y=random_degree),color="red")+
  #geom_line(aes(y=norm_degree),color="black")+
  theme_bw()+
  xlab("") +
  ylab("Degree")

library(grid)
library(gridExtra)

grid.arrange(nodes_plot, edges_plot)

grid.arrange(mod_plot,dist_plot,clusters_plot,comm_plot, ncol=1)

grid.arrange(degree_plot,in_plot,out_plot,ncol=1)

