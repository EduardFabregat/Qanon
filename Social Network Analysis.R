library(readr)
library(tidyverse)

myfilelist<- list.files(path = "C:\\Users\\posit\\Dropbox\\Recerca\\Data\\Q Clearance\\Tweets\\Tweets good\\Second Round", full.names = TRUE)

content_list<-list()
for (i in 1:length(myfilelist)) {
  print (i)
  print (myfilelist[i])
  content<-readxl::read_xlsx(myfilelist[i])
  content_list[[i]]<-content
}


Q <- bind_rows(content_list)

library(lubridate)

Q$date <- date(Q$`Date (EST)`)
colnames(Q)[2] <- "Date_Hour"

save(Q, file = "Q.Tweets.Last.Weeks.Rda")

Q <- Q[order(Q$date),]

Q$week <- 1+ as.numeric(Q$date - as.Date("2020-06-01")) %/% 7


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

for (i in 1:9){
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

net_info_dens_last_weeks <- cbind(nodes_df,edges_df,density_df,clusters_df,dist_df,trans_df,mod_df,
                                  comm_df,degree_df,in_df,out_df)

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


for (i in 1:nrow(net_info_dens_last_weeks)) {
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
    g <- sample_gnp(net_info_dens_last_weeks$nodes[i], net_info_dens_last_weeks$density[i], 
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

random_net_info_dens_last_weeks <- cbind(random_clusters_df,random_dist_df,random_trans_df,random_mod_df,
                                         random_comm_df,random_degree_df,random_in_df,random_out_df,
                                         sd_clusters_df,sd_trans_df,sd_mod_df,sd_dist_df,sd_comm_df,
                                         sd_degree_df,sd_in_df,sd_out_df)

net_info3 <- cbind(net_info_dens_last_weeks, random_net_info_dens_last_weeks)

net_info4 <- rbind(net_info2, net_info3)


save(net_info4, file = "Network.Information.With.Random.Weekly.Density.With.Last.Weeks.Rda")

### Juntar net_info2 i net_info3


net_info4$norm_clust <- (net_info4$clusters-net_info4$random_clusters)
net_info4$norm_dist <- (net_info4$distance-net_info4$random_distance)
net_info4$norm_trans <- (net_info4$transitivity-net_info4$random_transitivity)
net_info4$norm_mod <- (net_info4$modularity-net_info4$random_modularity)
net_info4$norm_comm <- (net_info4$communities-net_info4$random_communities)
net_info4$norm_in <- (net_info4$max_in-net_info4$random_in)
net_info4$norm_out <- (net_info4$max_out-net_info4$random_out)
net_info4$norm_degree <- (net_info4$degree-net_info4$random_degree)

library(tidyverse)

net_info4$index <- 1:nrow(net_info4)

###############################################################
####################### Add date to weeks

Q2 <- Q

Q <- Q[order(Q$date),]

Q$week <- 1+ as.numeric(Q$date - as.Date("2017-11-05")) %/% 7

Q2 <- Q2[order(Q2$date),]

Q2$week <- 1+ as.numeric(Q2$date - as.Date("2017-11-05")) %/% 7

Q2$RT <- NULL

Q <- rbind(Q, Q2)

save(Q, file = "Q.All.Tweets.With.Last.Weeks.Rda")

load(file = "C:/Users/posit/Dropbox/Recerca/Data/Q Clearance/Q.All.Tweets.With.Last.Weeks.Rda")

date <- list()

for (i in 1:143){
  print(i)
  temp <- Q[Q$week == i, ]
  date[[i]] <- temp[1,22]
}

date <- do.call(rbind, date)

net_info4 <- cbind(net_info4, date)
net_info4$date <- lubridate::as_date(net_info4$date)
net_info4$month <- lubridate::month(net_info4$date,label=TRUE)

nodes_plot <- ggplot(net_info4, aes(x=date)) +
  geom_line(aes(y=nodes),color="black")+
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Nodes")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

edges_plot <- ggplot(net_info4, aes(x=date)) +
  geom_line(aes(y=edges),color="black")+
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Edges")+
  theme(axis.text.x = element_text(angle=90, vjust=0.2, hjust=0,size=10))

mod_plot <- ggplot(net_info4, aes(x=date)) +
  geom_line(aes(y=modularity),color="blue") +
  geom_line(aes(y=norm_mod),color="black")+
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

dist_plot <- ggplot(net_info4, aes(x=date)) +
  geom_line(aes(y=distance),color="blue") +
  geom_line(aes(y=norm_dist),color="black")+
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Mean Distance")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

comm_plot <- ggplot(net_info4, aes(x=date)) +
  geom_line(aes(y=communities),color="blue") +
  geom_line(aes(y=norm_comm),color="black")+
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Commuities")+
  theme(axis.text.x = element_text(angle=90, vjust=0.2, hjust=0,size=10))

in_plot <- ggplot(net_info4, aes(x=date)) +
  geom_line(aes(y=max_in),color="black") +
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Degree In")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

out_plot <- ggplot(net_info4, aes(x=date)) +
  geom_line(aes(y=max_out),color="black") +
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Degree Out")+
  theme(axis.text.x = element_text(angle=90, vjust=0.2, hjust=0,size=10))

degree_plot <- ggplot(net_info4, aes(x=date)) +
  geom_line(aes(y=degree),color="black") +
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Degree")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

library(grid)
library(gridExtra)
library(cowplot)

grid.arrange(nodes_plot, edges_plot)

cowplot::plot_grid(nodes_plot, edges_plot, 
          align = "v", nrow = 2, rel_heights = c(0.43, 0.57))

grid.arrange(mod_plot,dist_plot,comm_plot, ncol=1)

cowplot::plot_grid(mod_plot,dist_plot,comm_plot, 
          align = "v", nrow = 3, rel_heights = c(0.33, 0.33, 0.50))

grid.arrange(degree_plot,in_plot,out_plot,ncol=1)

cowplot::plot_grid(degree_plot,in_plot,out_plot, 
                   align = "v", nrow = 3, rel_heights = c(0.33, 0.33, 0.50))
