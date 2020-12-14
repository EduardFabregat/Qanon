options(stringsAsFactors = FALSE)
load("C:/Users/posit/Dropbox/Recerca/Data/Q Clearance/Q.All.Tweets.With.Last.Weeks.Rda")
library(tidyverse)
library(igraph)

pat <- sprintf('((?:\\b\\W*@\\w+)+)')

m <- gregexpr(pat, Q$Contents, perl = TRUE)

Q$RT <- regmatches(Q$Contents, m)

Q$RT <- gsub("[[:punct:]]+","", Q$RT)

rm(m)

Q2 <- Q[,c(5,24,23)]
Q2 <- Q2[Q2$RT != "character0",]
print("Unnesting")
Q3 <- unnest(Q2, RT)
Q3$newAuthor <- gsub("@", "", Q3$Author)
Q4 <- Q3 %>% 
  separate_rows(RT) %>%
  filter(!RT %in% c('c', ''))
Q4$newRT <- Q4$RT
Q5 <- Q4[, c(4,5,3)]
Q5 <- Q5[which(Q5$newRT %in% Q5$newAuthor),]


degree_list <- list()
degree_in <- list()
degree_out <- list()
btw_list <- list()
eigen_cent <- list()
close_list <- list()
all_weeks <- list()

for (i in 1:143){
  print(i)
  Q55 <- Q5[Q5$week == i, ]
  Q55 <- Q55[,c(1,2)]
  print("Unnesting")
  rt_ment <- as.matrix(Q55)
  g <- graph.edgelist(rt_ment)
  wg <- g
  E(wg)$weight <- runif(ecount(wg))
  degree <- centr_degree(wg, mode = "total")
  degree$users <- V(g)$name
  degree_list[[i]] <- degree
  eigen <- centr_eigen(wg)
  eigen$users <- V(g)$name
  eigen_cent[[i]] <- eigen
  und_net <-as.undirected(wg, mode= "collapse",
                          edge.attr.comb=list(weight="sum", "ignore"))
  und_net2 <- igraph::simplify(und_net, remove.multiple = T, remove.loops = T,
                               edge.attr.comb=c(weight="sum", type="ignore"))
  between <- centr_betw(und_net2)
  between$users <- V(g)$name
  btw_list[[i]] <- between
  temp_degree <- as.data.frame(degree_list[[i]])
  temp_btw <- as.data.frame(btw_list[[i]])
  temp_eigen <- as.data.frame(eigen_cent[[i]])
  measurements <- cbind(temp_degree,temp_btw,temp_eigen)
  all_weeks[[i]] <- measurements
  print("Removing everything")
  rm(g,wg,und_net,und_net2,temp_degree,temp_btw,temp_eigen,measurements)
}

save(all_weeks, file = "leadership.measurements.without.Outsiders.Centrality.Last.Weeks.Rda")

load("C:/Users/posit/Dropbox/Recerca/Data/Q Clearance/leadership.measurements.without.Outsiders.Centrality.Last.Weeks.Rda")


degree_50 <- list()

for (i in 1:143){
  print(i)
  temp <- all_weeks[[i]]
  temp_degree <- temp[,c(4,1)]
  temp_degree <- temp_degree[order(-temp_degree[,2]),]
  temp_degree <- temp_degree[1:50,]
  degree_50[[i]] <- temp_degree
}

btw_50 <- list()

for (i in 1:143){
  print(i)
  temp <- all_weeks[[i]]
  temp_degree <- temp[,c(8,5)]
  temp_degree <- temp_degree[order(-temp_degree[,2]),]
  temp_degree <- temp_degree[1:50,]
  btw_50[[i]] <- temp_degree
}


eigen_50 <- list()

for (i in 1:143){
  print(i)
  temp <- all_weeks[[i]]
  temp_degree <- temp[,c(33,9)]
  temp_degree <- temp_degree[order(-temp_degree[,2]),]
  temp_degree <- temp_degree[1:50,]
  eigen_50[[i]] <- temp_degree
}

date <- list()

for (i in 1:141){
  print(i)
  temp <- Q[Q$week == i, ]
  date[[i]] <- temp[1,22]
}

date <- do.call(rbind, date)

sim_eigen <- list()

for (i in 1:141){
  print(i)
  temp1 <- eigen_50[[i]]$users
  temp2 <- eigen_50[[i+1]]
  temp3 <- eigen_50[[i+2]]
  temp2 <- temp2[which(temp2$users %in% temp1),]
  temp2 <- temp2$users
  sim_eigen[[i]] <- nrow(temp3[which(temp3$users %in% temp2),])/50
}

sim_eigen_df <- as.data.frame(do.call(rbind, sim_eigen))
sim_eigen_df$week <- 1:nrow(sim_eigen_df)

sim_eigen_df <- cbind(sim_eigen_df, date)
sim_eigen_df$date <- lubridate::as_date(sim_eigen_df$date)
sim_eigen_df$month <- lubridate::month(sim_eigen_df$date,label=TRUE)

sim_btw <- list()

for (i in 1:141){
  print(i)
  temp1 <- btw_50[[i]]$users
  temp2 <- btw_50[[i+1]]
  temp3 <- btw_50[[i+2]]
  temp2 <- temp2[which(temp2$users %in% temp1),]
  temp2 <- temp2$users
  sim_btw[[i]] <- nrow(temp3[which(temp3$users %in% temp2),])/50
}

sim_btw_df <- as.data.frame(do.call(rbind, sim_btw))
sim_btw_df$week <- 1:nrow(sim_btw_df)

sim_btw_df <- cbind(sim_btw_df, date)
sim_btw_df$date <- lubridate::as_date(sim_btw_df$date)
sim_btw_df$month <- lubridate::month(sim_btw_df$date,label=TRUE)

sim_degree <- list()

for (i in 1:141){
  print(i)
  temp1 <- degree_50[[i]]$users
  temp2 <- degree_50[[i+1]]
  temp3 <- degree_50[[i+2]]
  temp2 <- temp2[which(temp2$users %in% temp1),]
  temp2 <- temp2$users
  sim_degree[[i]] <- nrow(temp3[which(temp3$users %in% temp2),])/50
}

sim_degree_df <- as.data.frame(do.call(rbind, sim_degree))
sim_degree_df$week <- 1:nrow(sim_degree_df)

sim_degree_df <- cbind(sim_degree_df, date)
sim_degree_df$date <- lubridate::as_date(sim_degree_df$date)
sim_degree_df$month <- lubridate::month(sim_degree_df$date,label=TRUE)

eigen_plot <- ggplot(sim_eigen_df, aes(x=date, y=V1, group=1))+
  geom_line()+
  #geom_smooth(se=T,span=.2,color="blue")+
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Eigenvector Centrality")+
  #theme(axis.text.x = element_text(angle=90, vjust=0.2, hjust=0,size=10))
  theme(axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank())


btw_plot <- ggplot(sim_btw_df, aes(x=date, y=V1, group=1))+
  geom_line()+
  #geom_smooth(span=.2,color="blue")+
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Betweenness") +
  #theme(axis.text.x = element_text(angle=90, vjust=0.2, hjust=0,size=10))
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

degree_plot <- ggplot(sim_degree_df, aes(x=date, y=V1, group=1))+
  geom_line()+
  #geom_smooth(span=.2,color="blue")+
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Degree") +
  theme(axis.text.x = element_text(angle=90, vjust=0.2, hjust=0,size=10))


library(gridExtra)
library(cowplot)

grid.arrange(eigen_plot,btw_plot,degree_plot,ncol=1)

plot_grid(eigen_plot,btw_plot,degree_plot, 
          align = "v", nrow = 3, rel_heights = c(0.33, 0.33, 0.50))


degree_df <- data.frame(screen_name = c("temp1"))

for (i in 1:143){
  print(i)
  temp1 <- degree_50[[i]]
  temp1 <- data.frame(screen_name=c(temp1$users))
  degree_df <- rbind(degree_df,temp1)
}

degree_count <- count(degree_df, screen_name)
degree_count <- degree_count[order(-degree_count$n),]

temp_df <- data.frame(screen_name = c("temp1"))

for (i in 1:143){
  print(i)
  temp1 <- eigen_50[[i]]
  temp1 <- data.frame(screen_name=c(temp1$users))
  temp_df <- rbind(temp_df,temp1)
}

eigen_count <- count(temp_df, screen_name)
eigen_count <- eigen_count[order(-eigen_count$n),]

temp_df <- data.frame(screen_name = c("temp1"))

for (i in 1:143){
  print(i)
  temp1 <- btw_50[[i]]
  temp1 <- data.frame(screen_name=c(temp1$users))
  temp_df <- rbind(temp_df,temp1)
}

btw_count <- count(temp_df, screen_name)
btw_count <- btw_count[order(-btw_count$n),]

options(stringsAsFactors = FALSE)
load("C:/Users/posit/Dropbox/Recerca/Data/Q Clearance/Q.All.Tweets.With.Last.Weeks.Rda")

pat <- sprintf("#\\S+")

m <- gregexpr(pat, Q$Contents, perl = TRUE)

Q$hashtags <- regmatches(Q$Contents, m)

hashtags <- Q$hashtags

hashtags[hashtags =="character(0)"] <- "NA"

Q$hashtags <- hashtags

rm(m)

library(tidyverse)

hash_week <- list()
for (i in 1:143){
  print(i)
  Q1 <- Q[Q$week == i, ]
  q <- Q1 %>%  
    select(Author, date, hashtags) %>% 
    unnest(hashtags) %>% 
    filter(hashtags != "NA")
  df_for_hash <- q[q$hashtags != "#Qanon" &
                     q$hashtags != "#qanon" &
                     q$hashtags != "#QANON" &
                     q$hashtags != "#QAnon", ]
  hash_n <- count(df_for_hash, hashtags)
  
  hash_n <- hash_n[order(hash_n$n,decreasing = TRUE),]
  
  hash_n15 <- hash_n[1:50,]
  hash_week[[i]] <- hash_n15
}

sim_hash <- list()

for (i in 1:142){
  print(i)
  temp1 <- hash_week[[i]]$hashtags
  temp2 <- hash_week[[i+1]]
  sim_hash[[i]] <- nrow(temp2[which(temp2$hashtags %in% temp1),])/50
}

sim_hash_df <- as.data.frame(do.call(rbind, sim_hash))
sim_hash_df$week <- 1:nrow(sim_hash_df)

date <- list()

for (i in 1:143){
  print(i)
  temp <- Q[Q$week == i, ]
  date[[i]] <- temp[1,22]
}

date <- do.call(rbind, date)


sim_hash_df <- cbind(sim_hash_df, date[-143,])
sim_hash_df$date <- lubridate::as_date(sim_hash_df$date)
sim_hash_df$month <- lubridate::month(sim_hash_df$date,label=TRUE)


ggplot(sim_hash_df, aes(x=date, y=V1, group=1))+
  geom_line()+
  geom_smooth(method = "lm",se=F,span=.3,color="red")+
  geom_smooth(se=T,span=.3,color="blue")+
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Hashtag Similarity")+
  theme(axis.text.x = element_text(angle=90, vjust=0.2, hjust=0,size=10))

library(ggpmisc)

#with r2 and p-value

ggplot(sim_hash_df, aes(x=date, y=V1, group=1))+
  geom_line()+
  geom_smooth(method = "lm", se=F,span=.3,color="red")+
  geom_smooth(se=T,span=.3,color="blue")+
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Hashtag Similarity")+
  theme(axis.text.x = element_text(angle=90, vjust=0.2, hjust=0,size=10))+
  stat_fit_glance(method = "lm",label.y = 0.05,label.x = 0.95,
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('r^2~"="~%.2f~~italic(p-value)~"="~%.2g',
                  stat(r.squared), stat(p.value))),parse = TRUE)


#with coefficient and p-value

ggplot(sim_hash_df, aes(x=date, y=V1, group=1))+
  geom_line()+
  geom_smooth(method = "lm", se=F,span=.3,color="red")+
  geom_smooth(se=T,span=.3,color="blue")+
  scale_x_date(date_breaks="month",expand = c(0, 0))+
  theme_bw()+
  xlab("") +
  ylab("Hashtag Similarity")+
  theme(axis.text.x = element_text(angle=90, vjust=0.2, hjust=0,size=10))+
  stat_fit_tidy(method = "lm",label.y = "bottom",label.x = "right",method.args = list(formula = y ~ x),
                mapping = aes(label = sprintf("Coefficient = %.3g\nP-value = %.3g",
                                              stat(x_estimate),stat(x_p.value))))
