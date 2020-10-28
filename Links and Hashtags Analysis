###########################################################
####### Links by community
library(tidyverse)

pat <- sprintf('[h][t][t][p]\\S+')

m <- gregexpr(pat, Q$Contents, perl = TRUE)

Q$links <- regmatches(Q$Contents, m)

links <- Q$links

links[links =="character(0)"] <- "NA"

Q$links <- links

q <- Q %>%  
  select(Author, date, links) %>% 
  filter(links != "NA")

df_for_link <- data.frame(screen_name=c("temp1"), date=c("2020-01-01"),links=c("temp"),comm=c(0))
df_for_link$date <- lubridate::date(df_for_link$date)

for (i in 1:max(user_comm_df$comm)){
  print(i)
  temp <- user_comm_df[user_comm_df$comm == i,]
  temp <- temp$label
  com <- q[which(q$Author %in% temp),]
  print("Date")
  com$date <- lubridate::date(com$date)
  print("Unnesting Links")
  com1 <- com %>% 
    unnest(links)
  print("Creating DF")
  communities <- data.frame(screen_name=c(com1$Author),
                            date=c(com1$date),
                            links=c(com1$links),
                            comm=c(i))
  print("Binding Rows")
  df_for_link <- rbind(communities,df_for_link)
}

df_for_link <- df_for_link[df_for_link$comm != 0, ]

save(df_for_link, file = "Links.by.date.user.community.Rda")

df_for_link$forsum <- 1

com1 <- df_for_link[df_for_link$comm == 1, ]

com1 <- aggregate(x = com1[,"forsum"],
                  by = list(com1$date), FUN = "sum")
colnames(com1)[2] <- "Community1"

com2 <- df_for_link[df_for_link$comm == 2, ]

com2 <- aggregate(x = com2[,"forsum"],
                  by = list(com2$date), FUN = "sum")
colnames(com2)[2] <- "Community2"

com3 <- df_for_link[df_for_link$comm == 3, ]

com3 <- aggregate(x = com3[,"forsum"],
                  by = list(com3$date), FUN = "sum")
colnames(com3)[2] <- "Community3"

com4 <- df_for_link[df_for_link$comm == 4, ]

com4 <- aggregate(x = com4[,"forsum"],
                  by = list(com4$date), FUN = "sum")
colnames(com4)[2] <- "Community4"

com5 <- df_for_link[df_for_link$comm == 5, ]

com5 <- aggregate(x = com5[,"forsum"],
                  by = list(com5$date), FUN = "sum")
colnames(com5)[2] <- "Community5"

com6 <- df_for_link[df_for_link$comm == 6, ]

com6 <- aggregate(x = com6[,"forsum"],
                  by = list(com6$date), FUN = "sum")
colnames(com6)[2] <- "Community6"

com7 <- df_for_link[df_for_link$comm == 7, ]

com7 <- aggregate(x = com7[,"forsum"],
                  by = list(com7$date), FUN = "sum")
colnames(com7)[2] <- "Community7"

q2 <- full_join(com1,com2,by="Group.1")

q2 <- q2 %>% 
  full_join(com3) %>% 
  full_join(com4) %>% 
  full_join(com5) %>% 
  full_join(com6) %>%
  full_join(com7)

q2[is.na(q2)] <- 0

com1$comm <- "Comm1"
com2$comm <- "Comm2"
com3$comm <- "Comm3"
com4$comm <- "Comm4"
com5$comm <- "Comm5"
com6$comm <- "Comm6"
com7$comm <- "Comm7"

pp <- rbind(com1,com2,com3,com4,com5,com6,com7)

data <- pp  %>%
  group_by(Group.1, comm) %>%
  summarise(n = sum(x))%>%
  mutate(percentage = n / sum(n))

library(viridis)
library(hrbrthemes)


ggplot(data, aes(x=Group.1, y=percentage, fill=comm)) + 
  geom_area(alpha=0.6 , size=1)+
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Qanon Thematic Communities Over Time") +
  theme_ipsum() +
  theme(legend.position="none")

ggplot(data, aes(x=Group.1, y=percentage, fill=comm)) + 
  geom_area(alpha=0.6 , size=1)+
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = FALSE) +
  xlab("Date") +
  ylab("Percentage") + 
  ggtitle("Frequency of Links by Community")


ggplot(q2, aes(x=Group.1)) +
  geom_line(aes(y=Community4),color="#01ba8f")+
  geom_line(aes(y=Community3),color="#b137ea")+
  geom_line(aes(y=Community2),color="#beff61")+
  geom_line(aes(y=Community1),color="#00299d")+
  geom_line(aes(y=Community5),color="#d9a700")+
  geom_line(aes(y=Community6),color="#be8eff")+
  geom_line(aes(y=Community7),color="#ffd477")+
  xlab("Date") +
  ylab("Community Frequency")+
  labs(title="Frequency of Links by Community")


list_page <- c()

get_long <- R.utils::withTimeout({function(url){
  crul::HttpClient$new(url)$get()$url
}},timeout=4,TimeoutException = function(ex) cat("Timed out\n"))

df_long_link <- data.frame(list_page=c("temp1"),comm=c(0))

set.seed(1983)

for (i in 1:max(df_for_link$comm)){
  print(i)
  temp_com <- df_for_link[df_for_link$comm == i, ]
  print(nrow(temp_com))
  sample_n <- temp_com[sample(nrow(temp_com), (nrow(temp_com)*0.10)),]
  print(nrow(sample_n))
  p <- sample_n[, 3]
  list_page <- c()
  for (l in p){
    #print("Adding Quotes")
    as.character(expression(l))
    #print("Reading Link")
    skip_to_next <- FALSE
    tryCatch(link <- get_long(l),error = function(e) {skip_to_next <<- TRUE})
    if(skip_to_next) {next}  
    #print("Adding to the list")
    list_page[l] <- link
  }
  communities <- data.frame(link=c(as.data.frame(list_page)),comm=c(i))
  print("Binding Rows")
  df_long_link <- rbind(communities,df_long_link)
  rm(sample_n,temp_com,list_page)
}

#####################################################
######### Extract URLs

URLs <- df_long_link$list_page
get_domain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
domains <- as.data.frame(sapply(URLs, get_domain))

domains_com <- cbind(df_long_link, domains)
colnames(domains_com)[3] <- "domains"

save(domains_com,file="10.percent.links.with.domain.Rda")


#############################################
####### Plot the topc 20 links by frequency total and the most representative
####### links for each community

library(tidyverse)

domains_com$domains <- as.character(domains_com$domains)

links_n <- count(domains_com, domains)

links_n <- links_n[order(links_n$n,decreasing = TRUE),]

links_n <- links_n[links_n$domains != "t.co",]

links_n20 <- links_n[1:20,]

ggplot(links_n20,aes(x=reorder(domains, n),y=n))+
  geom_col()+
  coord_flip()+
  xlab("Links") +
  ylab("")

counts_list <- list()

for (i in 1:max(domains_com$comm)){
  print(i)
  temp_comp <- count(domains_com[domains_com$comm == i,], domains)
  counts_list[[i]] <- temp_comp
}


for (i in 1:length(counts_list)) {
  mycom<-counts_list[[i]]
  counts_list[[i]]$domains<-as.character(counts_list[[i]]$domains)
}

for (i in 1:length(counts_list)) {
  mycom<-counts_list[[i]]
  counts_list[[i]]<-mycom[order(mycom$n,decreasing = TRUE),]
}

for (i in 1:length(counts_list)){
  temp_com <- counts_list[[i]]
  temp_n <- sum(temp_com$n)
  temp_com$percentage <- temp_com$n/temp_n
  counts_list[[i]] <- temp_com
}


comm1 <- counts_list[[1]]
colnames(comm1)[2:3] <- paste0(colnames(comm1)[2:3], "_1")
comm2 <- counts_list[[2]]
colnames(comm2)[2:3] <- paste0(colnames(comm2)[2:3], "_2")
comm3 <- counts_list[[3]]
colnames(comm3)[2:3]<- paste0(colnames(comm3)[2:3], "_3")
comm4 <- counts_list[[4]]
colnames(comm4)[2:3] <- paste0(colnames(comm4)[2:3], "_4")
comm5 <- counts_list[[5]]
colnames(comm5)[2:3] <- paste0(colnames(comm5)[2:3], "_5")
comm6 <- counts_list[[6]]
colnames(comm6)[2:3] <- paste0(colnames(comm6)[2:3], "_6")
comm7 <- counts_list[[7]]
colnames(comm7)[2:3] <- paste0(colnames(comm7)[2:3], "_7")

q <- full_join(comm1, comm2,by="domains")

q <- q %>% 
  full_join(comm3,by="domains") %>% 
  full_join(comm4,by="domains") %>% 
  full_join(comm5,by="domains") %>% 
  full_join(comm6,by="domains") %>% 
  full_join(comm7,by="domains")

q[is.na(q)] <- 0

q_perc <- q[,c(1,2,4,6,8,10,12,14)]

q_perc$sum <- rowSums(q_perc[2:8])

list_zs <- list()
for (i in 2:ncol(q_perc[2:9])){
  n <- i-1
  print(colnames(q_perc[i]))
  z_list <- list()
  z_list[[n]] <- (q_perc[i]/q_perc$sum)*log(q_perc$sum/q_perc[i])
  z_list_all <- do.call(rbind, z_list)
  list_zs[[n]] <- z_list_all
}


q_perc$z1 <- list_zs[[1]]
q_perc$z2 <- list_zs[[2]]
q_perc$z3 <- list_zs[[3]]
q_perc$z4 <- list_zs[[4]]
q_perc$z5 <- list_zs[[5]]
q_perc$z6 <- list_zs[[6]]
q_perc$z7 <- list_zs[[7]]


q_z <- q_perc[,c(1,10:16)]
q_z[is.na(q_z)] <- 0
list_for_graph <- list()
for (i in 2:ncol(q_z)){
  n <- i-1
  print(colnames(q_z[,i]))
  temp <- q_z[,c(1,i)]
  list_for_graph[[n]] <- temp
  list_for_graph[[n]] <- do.call(cbind, list_for_graph[[n]])
}


for (i in 1:length(list_for_graph)) {
  mycom<-list_for_graph[[i]]
  list_for_graph[[i]]<-mycom[order(mycom[,2],decreasing = TRUE),]
}

for (i in 1:length(list_for_graph)) {
  mycom<-list_for_graph[[i]]
  list_for_graph[[i]]<-mycom[1:10,]
}

graphlist<-list()
for (i in 1:length(list_for_graph)) {
  mycom<-list_for_graph[[i]]
  graphlist[[i]]<-ggplot(mycom,aes(x=reorder(domains, mycom[,2]),y=(mycom[,2])))+
    geom_col()+
    coord_flip()+
    xlab("") +
    ylab("")
}

library(gridExtra)
grid.arrange(graphlist[[1]],graphlist[[2]],graphlist[[3]],
             graphlist[[4]],graphlist[[5]],graphlist[[6]],
             graphlist[[7]])


#############################################
######## Hashtags

pat <- sprintf("#\\S+")

m <- gregexpr(pat, Q$Contents, perl = TRUE)

Q$hashtags <- regmatches(Q$Contents, m)

hashtags <- Q$hashtags

hashtags[hashtags =="character(0)"] <- "NA"

Q$hashtags <- hashtags

library(tidyverse)

q <- Q %>%  
  select(Author, date, hashtags) %>% 
  filter(hashtags != "NA")

df_for_hash <- data.frame(Author=c("temp1"), date=c("2020-01-01"),hashtags=c("temp"),comm=c(0))
df_for_hash$date <- lubridate::date(df_for_hash$date)

for (i in 1:max(user_comm_df$comm)){
  print(i)
  temp <- user_comm_df[user_comm_df$comm == i,]
  temp <- temp$label
  com <- q[which(q$Author %in% temp),]
  print("Date")
  com$date <- lubridate::date(com$date)
  print("Unnesting Hashtags")
  com1 <- com %>% 
    unnest(hashtags)
  print("Creating DF")
  communities <- data.frame(Author=c(com1$Author),
                            date=c(com1$date),
                            hashtags=c(com1$hashtags),
                            comm=c(i))
  print("Binding Rows")
  df_for_hash <- rbind(communities,df_for_hash)
}

df_for_hash <- df_for_hash[df_for_hash$comm != 0, ]

save(df_for_hash, file = "Hashtags.by.date.user.Rda")

df_for_hash$hashtags <- as.character(df_for_hash$hashtags)

df_for_hash <- df_for_hash[df_for_hash$hashtags != "#Qanon" &
                             df_for_hash$hashtags != "#qanon" &
                             df_for_hash$hashtags != "#QANON" &
                             df_for_hash$hashtags != "#QAnon", ]

hash_n <- count(df_for_hash, hashtags)

hash_n <- hash_n[order(hash_n$n,decreasing = TRUE),]

hash_n20 <- hash_n[1:20,]

ggplot(hash_n20,aes(x=reorder(hashtags, n),y=n))+
  geom_col()+
  coord_flip() +
  xlab("Hashtags") +
  ylab("")

counts_list <- list()

for (i in 1:max(df_for_hash$comm)){
  print(i)
  temp_comp <- count(df_for_hash[df_for_hash$comm == i,], hashtags)
  counts_list[[i]] <- temp_comp
}

for (i in 1:length(counts_list)) {
  mycom<-counts_list[[i]]
  counts_list[[i]]$hashtags<-as.character(counts_list[[i]]$hashtags)
}

for (i in 1:length(counts_list)) {
  mycom<-counts_list[[i]]
  counts_list[[i]]<-mycom[order(mycom$n,decreasing = TRUE),]
}

for (i in 1:length(counts_list)){
  temp_com <- counts_list[[i]]
  temp_n <- sum(temp_com$n)
  temp_com$percentage <- temp_com$n/temp_n
  counts_list[[i]] <- temp_com
}


comm1 <- counts_list[[1]]
colnames(comm1)[2:3] <- paste0(colnames(comm1)[2:3], "_1")
comm2 <- counts_list[[2]]
colnames(comm2)[2:3] <- paste0(colnames(comm2)[2:3], "_2")
comm3 <- counts_list[[3]]
colnames(comm3)[2:3]<- paste0(colnames(comm3)[2:3], "_3")
comm4 <- counts_list[[4]]
colnames(comm4)[2:3] <- paste0(colnames(comm4)[2:3], "_4")
comm5 <- counts_list[[5]]
colnames(comm5)[2:3] <- paste0(colnames(comm5)[2:3], "_5")
comm6 <- counts_list[[6]]
colnames(comm6)[2:3] <- paste0(colnames(comm6)[2:3], "_6")
comm7 <- counts_list[[7]]
colnames(comm7)[2:3] <- paste0(colnames(comm7)[2:3], "_7")


q <- full_join(comm1, comm2,by="hashtags")

q <- q %>% 
  full_join(comm3,by="hashtags") %>% 
  full_join(comm4,by="hashtags") %>% 
  full_join(comm5,by="hashtags") %>% 
  full_join(comm6,by="hashtags") %>% 
  full_join(comm7,by="hashtags") 

q[is.na(q)] <- 0

q_perc <- q[,c(1,2,4,6,8,10,12,14)]

q_perc$sum <- rowSums(q_perc[2:8])

list_zs <- list()
for (i in 2:ncol(q_perc[2:9])){
  n <- i-1
  print(colnames(q_perc[i]))
  z_list <- list()
  z_list[[n]] <- (q_perc[i]/q_perc$sum)*log(q_perc$sum/q_perc[i])
  z_list_all <- do.call(rbind, z_list)
  list_zs[[n]] <- z_list_all
}

q_perc$z1 <- list_zs[[1]]
q_perc$z2 <- list_zs[[2]]
q_perc$z3 <- list_zs[[3]]
q_perc$z4 <- list_zs[[4]]
q_perc$z5 <- list_zs[[5]]
q_perc$z6 <- list_zs[[6]]
q_perc$z7 <- list_zs[[7]]

colnames(q_perc)


q_z <- q_perc[,c(1,10:16)]
list_for_graph <- list()
for (i in 2:ncol(q_z)){
  n <- i-1
  print(colnames(q_z[,i]))
  temp <- q_z[,c(1,i)]
  list_for_graph[[n]] <- temp
  list_for_graph[[n]] <- do.call(cbind, list_for_graph[[n]])
}


for (i in 1:length(list_for_graph)) {
  mycom<-list_for_graph[[i]]
  list_for_graph[[i]]<-mycom[order(mycom[,2],decreasing = TRUE),]
}

for (i in 1:length(list_for_graph)) {
  mycom<-list_for_graph[[i]]
  list_for_graph[[i]]<-mycom[1:10,]
}

graphlist<-list()
for (i in 1:length(list_for_graph)) {
  mycom<-list_for_graph[[i]]
  graphlist[[i]]<-ggplot(mycom,aes(x=reorder(hashtags, mycom[,2]),y=(mycom[,2])))+
    geom_col()+
    coord_flip()+
    xlab("") +
    ylab("")
}

library(gridExtra)
grid.arrange(graphlist[[1]],graphlist[[2]],graphlist[[3]],
             graphlist[[4]],graphlist[[5]],graphlist[[6]],
             graphlist[[7]])
