##############################################################
######### Topic Modelling

install.packages("topicmodels")

library(doParallel)

mycores <- detectCores()

library(quanteda)
# deleting links - notice that we added a "text" column for quanteda
Qanon$text<-gsub("[h][t][t][p]\\S+","", Qanon$text)
Qanon$text<-gsub("pic.twitter\\S+","", Qanon$text)

#creem la variable index per a poder comparar l'lda i les dades originals
#i aixi poder juntar les dues bases de dades
#aixi despres podem trobar documents per a cada topic
Qanon$index <- 1:nrow(Qanon)

### removing duplicate documents
removed_df<-Qanon[duplicated(Qanon$text),]
Q3 <- Qanon[!duplicated(Qanon$text),]


# creating corpus object
mycorpus <- corpus(Q3)

# preprocessing
stopwords_and_single<-c(stopwords("english"), "amp", LETTERS,letters)
dfm_q <- dfm(mycorpus, tolower = TRUE, remove_punct = TRUE, 
                   remove_numbers=TRUE, remove = stopwords_and_single, 
                   stem = FALSE, remove_separators=TRUE)

docnames(dfm_q) <- dfm_q@docvars$index

dfm_q2 <- dfm_trim(dfm_q, max_docfreq = 0.95, min_docfreq = 0.01, 
                       docfreq_type = "prop")

dtm_lda <- convert(dfm_q2, to = "topicmodels")


full_data<-dtm_lda

n <- nrow(full_data)

print(Sys.time())
MainresultDF<-data.frame(k=c(1),perplexity=c(1),myalpha=c("x"))
MainresultDF<-MainresultDF[-1,]
#candidate_alpha<- c(0.01, 0.05, 0.1, 0.2, 0.5) # we choose variaty of alphas
candidate_alpha<- c(0.5) # we choose variaty of alphas
candidate_k <- c(seq(1,10)*10, 125, 150, 175, 200)

save.image(file="searchk_10-200.RData")
#m'he quedat aqui, fer el my cores i continuar amb el cross validation

library(doParallel)

for (eachalpha in candidate_alpha) { 
  print ("now running ALPHA:")
  print (eachalpha)
  print(Sys.time())
  #----------------5-fold cross-validation, different numbers of topics----------------
  cluster <- makeCluster(mycores) # leave one CPU spare...
  registerDoParallel(cluster)
  
  clusterEvalQ(cluster, {
    library(topicmodels)
  })
  
  folds <- 5
  splitfolds <- sample(1:folds, n, replace = TRUE)
  #candidate_k <- c(2, 3, 4, 5, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100) # candidates for how many topics
  
  #clusterExport(cluster, c("full_data", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))
  clusterExport(cluster, c("full_data", "splitfolds", "folds", "candidate_k"))
  
  # we parallelize by the different number of topics.  A processor is allocated a value
  # of k, and does the cross-validation serially.  This is because it is assumed there
  # are more candidate values of k than there are cross-validation folds, hence it
  # will be more efficient to parallelise
  system.time({
    results <- foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
      k <- candidate_k[j]
      print(k)
      results_1k <- matrix(0, nrow = folds, ncol = 2)
      colnames(results_1k) <- c("k", "perplexity")
      for(i in 1:folds){
        train_set <- full_data[splitfolds != i , ]
        valid_set <- full_data[splitfolds == i, ]
        
        fitted <- LDA(train_set, k = k, method = "Gibbs",
                      #control = list(alpha=eachalpha/k,burnin = burnin, iter = iter, keep = keep) )
                      control = list(verbose = 500,
                                     alpha=eachalpha))
        
        #fitted <- LDA(train_set, k = k, method = "Gibbs")
        
        results_1k[i,] <- c(k, perplexity(fitted, newdata = valid_set))
      }
      return(results_1k)
    }
  })
  stopCluster(cluster)
  
  results_df <- as.data.frame(results)
  results_df$myalpha<-as.character(eachalpha)
  MainresultDF<-rbind(MainresultDF,results_df)
  print ("DONE!!!")
  print(Sys.time())
}

save(MainresultDF, file = "MainresultDF.02.Rda")
save.image(file="Qanon.alpha.02.RData")

MainresultDF$kalpha=paste0(as.character(MainresultDF$k),MainresultDF$myalpha) 
ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  #geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.5)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.2)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.1)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.05)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.01)]),linetype = "dotted")

#geom_line(aes(x=k, y=mean(perplexity),color=myalpha))
#geom_smooth(se = TRUE, aes(x=k, y=perplexity,color=myalpha))

alpha005 <- MainresultDF %>% 
  filter(myalpha == 0.05)

alpha001 <- MainresultDF %>% 
  filter(myalpha == 0.01)


alpha005 <- alpha005[order(alpha005$k),]

MainresultDF<-MainresultDF[order(MainresultDF$k),]

cars.spl <- with(alpha005, smooth.spline(k, perplexity, df = 3))
with(cars, predict(cars.spl, x = alpha005$k, deriv = 2))

plot(with(cars, predict(cars.spl, x = alpha005$k, deriv = 2)), type = "l")
abline(v=70)

runsdf<-data.frame(myk=c(60,70))

mymodels<-list()

cluster <- makeCluster(detectCores(logical = TRUE)) # leave one CPU spare...
registerDoParallel(cluster)

clusterEvalQ(cluster, {
  library(topicmodels)
})

#clusterExport(cluster, c("full_data", "burnin", "iter", "keep", "splitfolds", "folds", "candidate_k"))
clusterExport(cluster, c("full_data","runsdf"))

system.time({
  mymodels <- foreach(j = 1:nrow(runsdf)) %dopar%{
    k_run <- runsdf[j,1]
    #alpha_run<-runsdf[j,2]
    fitted <- LDA(full_data, k = k_run, method = "Gibbs",
                  control = list(alpha=0.05, seed=267348))
                  #control = list(seed=3341) )
  }
})
stopCluster(cluster)

###################################################################
############ Thematic Communities

LDAfit<-mymodels[[2]]
metadf<-data33
meta_theta_df<-cbind(metadf,LDAfit@gamma)
#the lda got rid of some tweets, I delete them since they don't have any theta values
missing_docs<-setdiff(dfm_q2@Dimnames$docs,LDAfit@documents)
dfm_q3<-dfm_q2[-which(dfm_q2@Dimnames$docs %in% missing_docs),]
dfm_forsize<-data.frame(dfm_q3)
dfm_forsize<-dfm_forsize[,-1]
sizevect<-rowSums(dfm_forsize)
meta_theta_df<-data.frame(size=sizevect,meta_theta_df)

# now we prepare the removed duplicates dataset
duplicate_df<-removed_df
colnames(duplicate_df)<-paste0(colnames(duplicate_df),".1")

# we cycle through all removed documents to add the missing theta values
dflist<-list()
for (i in (1:nrow(duplicate_df))) {
  the_match<-match(duplicate_df$text.1[i],meta_theta_df$text)
  newvect<-c(duplicate_df[i,],meta_theta_df[the_match,])
  dflist[[i]]<-newvect
}

maintable<-data.frame(do.call(bind_rows,dflist))

# we now delete the metadata from orginal matched document 
#leaving only meta data for the actual document with the theta values and size 
maintable<-data.frame(size=maintable$size,maintable[,-c((ncol(duplicate_df)+1):(ncol(duplicate_df)+ncol(metadf)+1))])
colnames(maintable)<-gsub("\\.1","",colnames(maintable))

#there are some NAs
maintable2 <- maintable %>% 
  filter(!size == "NA")

meta_theta_df<-bind_rows(meta_theta_df,maintable2)

meta_theta_df2 <- meta_theta_df

tuisters <- meta_theta_df2 %>% 
  count(screen_name)

meta_theta_df3<-meta_theta_df2

meta_theta_by_user_volume <- meta_theta_df3 %>% 
  right_join(tuisters, by = "screen_name")


meta_nort <- meta_theta_by_user_volume[meta_theta_by_user_volume$is_retweet != TRUE, ]

tuisters <- count(meta_nort, screen_name)

meta_10 <- meta_nort[meta_nort$n >= 100, ]

rm(meta_theta_by_user_volume)

themesbyuser <- aggregate(x=meta_10[,10:79],
                          by=list(meta_10$screen_name),FUN="mean")

colnames(themesbyuser)[1] <- "screen_name"

tuisters <- meta_nort %>% 
  count(screen_name)

themesbyuser2 <- themesbyuser %>% 
  right_join(tuisters, by = "screen_name")

themesbyuser3 <- na.omit(themesbyuser2)

themesbyuser3 <- themesbyuser3[themesbyuser3$n >= 100, ]

themesbyuser4 <- themesbyuser3[,-72]

all_frames_by_user <- themesbyuser4

rownames(all_frames_by_user) <- all_frames_by_user$screen_name

all_frames_by_user <- all_frames_by_user[,-1]

all_frames_by_user2 <- t(all_frames_by_user)

library(lsa)

mycosine <- cosine(all_frames_by_user2)

library(igraph)
sem_net_weighted<-graph.adjacency(mycosine,mode="undirected",weighted=T,diag=F,add.colnames="label") # Assign colnames

V(sem_net_weighted)$name<-V(sem_net_weighted)$label

V(sem_net_weighted)$size<-themesbyuser3$n

vcount(sem_net_weighted)
ecount(sem_net_weighted)

library(skynet)

#getBackboneNetwork(newg)

g<-disparity_filter(g=sem_net_weighted,alpha=0.25) #aquesta es l'alpha que redueix el numero de edges i mante la xarxa conectada

vcount(g)
ecount(g)

table(clusters(g)$membership)

set.seed(433547)
mylouvain<-(cluster_louvain(g))
mywalktrap<-(cluster_walktrap(g)) 
myinfomap<-(cluster_infomap(g)) 
myfastgreed<-(cluster_fast_greedy(g))
mylabelprop<-(cluster_label_prop(g))

V(g)$louvain<-mylouvain$membership 
V(g)$walktrap<-mywalktrap$membership 
V(g)$infomap<-myinfomap$membership  
V(g)$fastgreed<-myfastgreed$membership 
V(g)$labelprop<-mylabelprop$membership

write.graph(g, "Qanon.user.net.bbone.PP2.graphml", format = "graphml")
print("done!")


nodelist<-list()
for (node in 1:length(V(g))) {
  #for (node in 1:100) {
  print(node)
  outside<-strength(g, vids = V(g)[node])
  tempg<-induced_subgraph(g,V(g)$louvain==V(g)$louvain[node])
  inside<-strength(tempg, vids = V(tempg)$label==V(g)[node]$label)
  nodelist[[node]]<-data.frame(
    node=node,label=V(g)[node]$label,inside,comm=V(g)$louvain[node],between=outside,within=inside,commstr=inside/outside)
}

user_comm_df<-do.call(rbind,nodelist)

##grab for each comm the top 20 users
top_user_com_df<-data.frame(matrix(NA, nrow = 1585, ncol = length(unique(user_comm_df$comm))))

for (i in 1:max(user_comm_df$comm)) {
  print (i)
  temp_df<-user_comm_df[user_comm_df$comm==i,]
  temp_df<-temp_df[order(temp_df$commstr,decreasing = TRUE),]
  towrite<-temp_df$label[1:1585]
  top_user_com_df[,i]<-towrite
}

### now we print top tweets
## go to data - filter by top 20 users
## grab 200 random tweets - print to dataframe and XL
comm_tweets_list<-list()
for (i in 1:max(user_comm_df$comm)) {
  print(i)
  temp_meta_theta_df<-meta_nort[meta_nort$screen_name %in% top_user_com_df[,i],]
  temp_meta_theta_df<- temp_meta_theta_df[sample(nrow(temp_meta_theta_df), 200), ]
  comm_tweets_list[[i]]<-c(temp_meta_theta_df)
  openxlsx::write.xlsx(temp_meta_theta_df,paste0(as.character(i),"_COMM_200_2tweets.xlsx"))
}

##########################################################

#ara calculem la frequecia dels topics per cada comunitat tematica per veure 
#quins topics utilitza cada comunitat
comm1 <- top_user_com_df %>% 
  select(X1)

comm1 <- na.omit(comm1)

colnames(comm1)[1] <- "screen_name"

comm1_mt <- comm1 %>% 
  left_join(themesbyuser4, by = "screen_name")

comm1_total <- sapply(Filter(is.numeric, comm1_mt), mean)
comm1_total <- as.data.frame(comm1_total)

comm2 <- top_user_com_df %>% 
  select(X2)

comm2 <- na.omit(comm2)

colnames(comm2)[1] <- "screen_name"

comm2_mt <- comm2 %>% 
  left_join(themesbyuser4, by = "screen_name")

comm2_total <- sapply(Filter(is.numeric, comm2_mt), mean)
comm2_total <- as.data.frame(comm2_total)

comm3 <- top_user_com_df %>% 
  select(X3)

comm3 <- na.omit(comm3)

colnames(comm3)[1] <- "screen_name"

comm3_mt <- comm3 %>% 
  left_join(themesbyuser4, by = "screen_name")

comm3_total <- sapply(Filter(is.numeric, comm3_mt), mean)
comm3_total <- as.data.frame(comm3_total)

comm4 <- top_user_com_df %>% 
  select(X4)

comm4 <- na.omit(comm4)

colnames(comm4)[1] <- "screen_name"

comm4_mt <- comm4 %>% 
  left_join(themesbyuser4, by = "screen_name")

comm4_total <- sapply(Filter(is.numeric, comm4_mt), mean)
comm4_total <- as.data.frame(comm4_total)

comm5 <- top_user_com_df %>% 
  select(X5)

comm5 <- na.omit(comm5)

colnames(comm5)[1] <- "screen_name"

comm5_mt <- comm5 %>% 
  left_join(themesbyuser4, by = "screen_name")

comm5_total <- sapply(Filter(is.numeric, comm5_mt), mean)
comm5_total <- as.data.frame(comm5_total)

comm6 <- top_user_com_df %>% 
  select(X6)

comm6 <- na.omit(comm6)

colnames(comm6)[colnames(comm6)=="X6"] <- "screen_name"

comm6_mt <- comm6 %>% 
  left_join(themesbyuser4, by = "screen_name")

comm6_total <- sapply(Filter(is.numeric, comm6_mt), mean)
comm6_total <- as.data.frame(comm6_total)

comm7 <- top_user_com_df %>% 
  select(X7)

comm7 <- na.omit(comm7)

colnames(comm7)[1] <- "screen_name"

comm7_mt <- comm7 %>% 
  left_join(themesbyuser4, by = "screen_name")

comm7_total <- sapply(Filter(is.numeric, comm7_mt), mean)
comm7_total <- as.data.frame(comm7_total)

comm8 <- top_user_com_df %>% 
  select(X8)

comm8 <- na.omit(comm8)

colnames(comm8)[1] <- "screen_name"

comm8_mt <- comm8 %>% 
  left_join(themesbyuser4, by = "screen_name")

comm8_total <- sapply(Filter(is.numeric, comm8_mt), mean)
comm8_total <- as.data.frame(comm8_total)


##########################################################
### Ara calculem la frequencia de tuits per comunitat, drops i tuits totals

q1 <- Gephi[Gephi$v_louvain == 1, ]
q2 <- Gephi[Gephi$v_louvain == 2, ]
q3 <- Gephi[Gephi$v_louvain == 3, ]
q4 <- Gephi[Gephi$v_louvain == 4, ]
q5 <- Gephi[Gephi$v_louvain == 5, ]
q6 <- Gephi[Gephi$v_louvain == 6, ]
q7 <- Gephi[Gephi$v_louvain == 7, ]
q8 <- Gephi[Gephi$v_louvain == 8, ]

q1 <- q1$Label
q2 <- q2$Label
q3 <- q3$Label
q4 <- q4$Label
q5 <- q5$Label
q6 <- q6$Label
q7 <- q7$Label
q8 <- q8$Label

com1 <- meta_nort[which(meta_nort$screen_name %in% q1),]
com2 <- meta_nort[which(meta_nort$screen_name %in% q2),]
com3 <- meta_nort[which(meta_nort$screen_name %in% q3),]
com4 <- meta_nort[which(meta_nort$screen_name %in% q4),]
com5 <- meta_nort[which(meta_nort$screen_name %in% q5),]
com6 <- meta_nort[which(meta_nort$screen_name %in% q6),]
com7 <- meta_nort[which(meta_nort$screen_name %in% q7),]
com8 <- meta_nort[which(meta_nort$screen_name %in% q8),]

com1$forsum <- 1
com2$forsum <- 1
com3$forsum <- 1
com4$forsum <- 1
com5$forsum <- 1
com6$forsum <- 1
com7$forsum <- 1
com8$forsum <- 1

library(lubridate)

com1$created_at <- as_date(com1$created_at)
com2$created_at <- as_date(com2$created_at)
com3$created_at <- as_date(com3$created_at)
com4$created_at <- as_date(com4$created_at)
com5$created_at <- as_date(com5$created_at)
com6$created_at <- as_date(com6$created_at)
com7$created_at <- as_date(com7$created_at)
com8$created_at <- as_date(com8$created_at)

com1 <- aggregate(x = com1[,"forsum"],
                  by = list(com1$created_at), FUN = "sum")
colnames(com1)[2] <- "Community1"
com2 <- aggregate(x = com2[,"forsum"],
                  by = list(com2$created_at), FUN = "sum")
colnames(com2)[2] <- "Community2"
com3 <- aggregate(x = com3[,"forsum"],
                  by = list(com3$created_at), FUN = "sum")
colnames(com3)[2] <- "Community3"
com4 <- aggregate(x = com4[,"forsum"],
                  by = list(com4$created_at), FUN = "sum")
colnames(com4)[2] <- "Community4"
com5 <- aggregate(x = com5[,"forsum"],
                  by = list(com5$created_at), FUN = "sum")
colnames(com5)[2] <- "Community5"
com6 <- aggregate(x = com6[,"forsum"],
                  by = list(com6$created_at), FUN = "sum")
colnames(com6)[2] <- "Community6"
com7 <- aggregate(x = com7[,"forsum"],
                  by = list(com7$created_at), FUN = "sum")
colnames(com7)[2] <- "Community7"
com8 <- aggregate(x = com8[,"forsum"],
                  by = list(com8$created_at), FUN = "sum")
colnames(com8)[2] <- "Community8"

q <- cbind(com1,com2,com3,com4,com5,com6,com7,com8)

q <- q[,c(1,2,4,6,8,10,12,14,16)]

save(q, file="Frequency.Tweets.by.Communities.Rda")

library(tidyverse)

ggplot(q, aes(x=Group.1)) +
  geom_line(aes(y=Community4),color="purple")+
  geom_line(aes(y=Community3),color="green")+
  geom_line(aes(y=Community2),color="blue")+
  geom_line(aes(y=Community1),color="orange")+
  geom_line(aes(y=Community5),color="yellow")+
  geom_line(aes(y=Community6),color="maroon")+
  geom_line(aes(y=Community7),color="pink")+
  geom_line(aes(y=Community8),color="brown")+
  xlab("Date") +
  ylab("Community Frequency")+
  labs(title="Frequency of Tweets by Community")

ggplot(q, aes(x=Group.1)) +
  geom_smooth(aes(y=Community4),se=F,color="purple",span=0.3)+
  geom_smooth(aes(y=Community3),se=F,color="green",span=0.3)+
  geom_smooth(aes(y=Community2),se=F,color="blue",span=0.3)+
  geom_smooth(aes(y=Community1),se=F,color="orange",span=0.3)+
  geom_smooth(aes(y=Community5),se=F,color="yellow",span=0.3)+
  geom_smooth(aes(y=Community6),se=F,color="maroon",span=0.3)+
  geom_smooth(aes(y=Community7),se=F,color="pink",span=0.3)+
  geom_smooth(aes(y=Community8),se=F,color="brown",span=0.3)+
  xlab("Date") +
  ylab("Community Frequency")+
  labs(title="Frequency of Tweets by Community")


nort <- Qanon[Qanon$is_retweet != TRUE, ]

library(lubridate)

nort$created_at <- date(nort$created_at)

colnames(nort)[1] <- "date"
colnames(Qdrops)[1] <- "date"

nort_date <- count(nort, date)

Q <- full_join(nort_date, Qdrops)

Q[is.na(Q)] <- 0

library(grid)
library(gridExtra)

tweets <- ggplot(Q, aes(x=date))+
  geom_line(aes(y=n))+
  xlab("")+
  ylab("Number of Tweets")+
  labs(title = "Frequency of Tweets with #Qanon")


drops <- ggplot(Q, aes(x=date))+
  geom_line(aes(y=x)) +
  xlab("")+
  ylab("Number of Drops")+
  labs(title = "Frequency of Q Drops")

communities <- ggplot(q, aes(x=date)) +
  geom_line(aes(y=Community4),color="darkgreen")+
  geom_line(aes(y=Community3),color="#CC0099")+
  geom_line(aes(y=Community2),color="brown")+
  geom_line(aes(y=Community1),color="yellow")+
  geom_line(aes(y=Community5),color="green")+
  geom_line(aes(y=Community6),color="blue")+
  geom_line(aes(y=Community7),color="purple")+
  geom_line(aes(y=Community8),color="orange")+
  xlab("Date") +
  ylab("Community Frequency")+
  labs(title="Frequency of Tweets by Community")

grid.arrange(drops, tweets, communities, nrow = 3)


##############################################################
######## Scrape Q drops
options(stringsAsFactors = FALSE)

install.packages("rvest")
library(rvest)

qdrops <- html("https://qmap.pub/?pg=19")

drops14 <- qdrops %>%
  html_nodes(".text-muted.mr-2")%>%
  html_text() %>% 
  as.data.frame()

drops <- rbind(drops1,drops2,drops3,drops4,drops5,drops6,drops7,drops8,drops9,drops10,drops11,
               drops12,drops13,drops14,drops15)


colnames(drops)[1] <- "drops"

save(drops,file="Qdrops.Scraped.Rda")

drops2 <- drops
library(stringr)

drop <- data.frame(temp=c(1))
drop <- drop[,-1]


for (date in 1:nrow(drops2)) {
  temp <- as.data.frame(regmatches(drops2[date,],gregexpr("*?...............",drops2[date,])))
  temp <- temp[1,]
  temp <- as.data.frame(temp)
  drop <- rbind(drop,temp)
}

Qdrops <- as.data.frame(mdy(drop$temp))

Qdrops$forsum <- 1

volume<-aggregate(x = Qdrops[,"forsum"],
                  by = list(Qdrops$`mdy(drop$temp)`), FUN = "sum")

openxlsx::write.xlsx(volume, "Qdrops.by.date.xlsx")

####################################################################
########## Scrape all Q drops with a function

url <-'https://qmap.pub/'

list_of_pages <- str_c(url, '?pg=', 1:67)

get_dates <- function(html){
  html %>%
    html_nodes(".text-muted.mr-2")%>%
    html_text() %>%
    str_trim() %>%                       
    unlist() 
}

get_drops <- function(html){
  html %>%
    html_nodes(".card-text")%>%
    html_text() %>%
    str_trim() %>%                       
    unlist() 
}

get_data_table <- function(html, company_name){
  
  # Extract the Basic information from the HTML
  dates_2 <- get_dates(html)
  drops_2 <- get_drops(html)
  
  # Combine into a tibble
  combined_data <- tibble(date = dates_2,
                          drops = drops_2) 
}

get_data_from_url <- function(url, company_name){
  html <- read_html(url)
  get_data_table(html)
}

scrape_write_table <- function(url, company_name){
  
  # Generate the target URLs
  list_of_pages <- str_c(url, '?pg=', 1:67)
  
  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a tsv file into the working directory
  list_of_pages %>% 
    # Apply to all URLs
    map(get_data_from_url, company_name) %>%  
    # Combine the tibbles into one tibble
    bind_rows() %>%                           
    # Write a tab-separated file
    openxlsx::write.xlsx(str_c(company_name,'.xlsx'))     
}

scrape_write_table(url,"QDropsAndDates")

########################################################
#######Time Series Analysis Tweets Frequency and Q Drops

library(forecast)
library(tseries)
library(vrtest)

tweets <- ts(Q$n, frequency=12)
drops <- ts(Q$x, frequency=12)

adf.test(tweets)
fd_tweets <- fracdiff::diffseries(Q$n, 1)
adf.test(drops)

vars <- data.frame(Tweets = fd_tweets,
                   Drops = drops)

tsDyn::lags.select(vars)

lags <- tsDyn::lags.select(vars)
lags$BICs

vars::VARselect(vars)

var_results <- vars::VAR(vars, p = 3, exogen = NULL) #2, 5
summary(var_results)

lmtest::grangertest(vars$Tweets ~ vars$Drops, order = 3)

vars::causality(var_results, cause = "Drops")

########################################################
########Time Series Analyis per community

library(forecast)
library(tseries)
library(vrtest)

com1 <- ts(q$Community1, frequency=12)
com2 <- ts(q$Community2, frequency=12)
com3 <- ts(q$Community3, frequency=12)
com4 <- ts(q$Community4, frequency=12)
com5 <- ts(q$Community5, frequency=12)
com6 <- ts(q$Community6, frequency=12)
com7 <- ts(q$Community7, frequency=12)
com8 <- ts(q$Community8, frequency=12)

Qdrops <- Qdrops_by_date[Qdrops_by_date$Group.1 >= "2019-02-26" & 
                           Qdrops_by_date$Group.1 < "2019-07-31", ]

colnames(q)[1] <- "date"
colnames(Qdrops)[1] <- "date"

Qdrops$date <- as_date(Qdrops$date)

q <- full_join(q, Qdrops)

q[is.na(q)] <- 0

drops <- ts(q$x, frequency=12)

adf.test(com1)
fd_com1 <- fracdiff::diffseries(q$Community1, 1)
adf.test(com2)
fd_com2 <- fracdiff::diffseries(q$Community2, 1)
adf.test(com3)
fd_com3 <- fracdiff::diffseries(q$Community3, 1)
adf.test(com4)
fd_com4 <- fracdiff::diffseries(q$Community4, 1)
adf.test(com5)
fd_com5 <- fracdiff::diffseries(q$Community5, 1)
adf.test(com6)
fd_com6 <- fracdiff::diffseries(q$Community6, 1)
adf.test(com7)
fd_com7 <- fracdiff::diffseries(q$Community7, 1)
adf.test(com8)
fd_com8 <- fracdiff::diffseries(q$Community8, 1)


adf.test(drops)

vars <- data.frame(Community1 = fd_com1,
                   Community2 = fd_com2,
                   Community3 = fd_com3,
                   Community4 = fd_com4,
                   Community5 = fd_com5,
                   Community6 = fd_com6,
                   Community7 = fd_com7,
                   Community8 = fd_com8,
                   Drops = drops)

tsDyn::lags.select(vars)

lags <- tsDyn::lags.select(vars)
lags$BICs

vars::VARselect(vars)

var_results <- vars::VAR(vars, p = 3, exogen = NULL)
summary(var_results)

lmtest::grangertest(vars$Community8 ~ vars$Drops, order = 3)

vars::causality(var_results, cause = "Drops")

##########################################################
############ Per veure quins hashtags utilitzen per communitat, usuari i data
options(stringsAsFactors = FALSE)
library(tidyverse)
library(tidytext)
library(widyr)


q <- Qanon %>% 
  filter(is_retweet != TRUE) %>% 
  select(screen_name, created_at, hashtags) %>% 
  filter(hashtags != "NA")

##### Creo una df buida on guardar-hi els hashtags
df_for_com <- data.frame(screen_name=c("temp1"), date=c("2020-01-01"),hashtags=c("temp"),comm=c(0))
df_for_com$date <- lubridate::date(df_for_com$date)

##### El loop agafa els usuaris de cada comunitat, separa tots els hashtags
##### I els fica en la df que hem fet abans
for (i in 1:max(Gephi$v_louvain)){
  print(i)
  temp <- Gephi[Gephi$v_louvain == i,]
  temp <- temp$Label
  com <- q[which(q$screen_name %in% temp),]
  print("Date")
  com$created_at <- lubridate::date(com$created_at)
  print("Unnesting Hashtags")
  com1 <- com %>% 
    unnest(hashtags)
  print("Creating DF")
  communities <- data.frame(screen_name=c(com1$screen_name),
                            date=c(com1$created_at),
                            hashtags=c(com1$hashtags),
                            comm=c(i))
  print("Binding Rows")
  df_for_com <- rbind(communities,df_for_com)
}

df_for_com <- df_for_com[-1,]

save(df_for_com, file = "Hashtags.by.user.date.community.Rda")

pp <- count(df_for_com, hashtags, comm)

hasthags_by_com <- pp %>%
  filter(comm != 0) %>%
  filter(!hashtags %in% c("QAnon", "qanon", "Qanon", "QANON")) %>% 
  arrange(desc(n)) %>%
  mutate(hashtags = factor(hashtags, levels = rev(unique(hashtags)))) %>% 
  group_by(comm) %>% 
  top_n(30) %>% 
  ungroup() %>%
  arrange(comm, n) %>%
  mutate(order = row_number())

ggplot(hasthags_by_com, aes(order, n)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ comm, scales = "free") +
  xlab("") +
  ylab("Freq") +
  theme_bw() +
  # Add categories to axis
  scale_x_continuous(
    breaks = hasthags_by_com$order,
    labels = hasthags_by_com$hashtags,
    expand = c(0,0)
  ) +
  coord_flip()

#####################################################
####### Per veure la frequency the tuits amb links

library(tidyverse)

pat <- sprintf('[h][t][t][p]\\S+')

m <- gregexpr(pat, Qanon$text, perl = TRUE)

Qanon$links <- regmatches(Qanon$text, m)

q <- Qanon %>% 
  filter(is_retweet != TRUE) %>% 
  select(screen_name, created_at, links) %>% 
  filter(links != "NA")

df_for_link <- data.frame(screen_name=c("temp1"), date=c("2020-01-01"),links=c("temp"),comm=c(0))
df_for_link$date <- lubridate::date(df_for_link$date)

for (i in 1:max(Gephi$v_louvain)){
  print(i)
  temp <- Gephi[Gephi$v_louvain == i,]
  temp <- temp$Label
  com <- q[which(q$screen_name %in% temp),]
  print("Date")
  com$created_at <- lubridate::date(com$created_at)
  print("Unnesting Links")
  com1 <- com %>% 
    unnest(links)
  print("Creating DF")
  communities <- data.frame(screen_name=c(com1$screen_name),
                            date=c(com1$created_at),
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
com8 <- df_for_link[df_for_link$comm == 8, ]

com8 <- aggregate(x = com8[,"forsum"],
                       by = list(com8$date), FUN = "sum")
colnames(com8)[2] <- "Community8"


q2 <- cbind(com1,com2,com3,com4,com5,com6,com7,com8)

q2 <- q2[,c(1,2,4,6,8,10,12,14,16)]

links_plot <- ggplot(q2, aes(x=Group.1)) +
  geom_line(aes(y=Community4),color="darkgreen")+
  geom_line(aes(y=Community3),color="#CC0099")+
  geom_line(aes(y=Community2),color="brown")+
  geom_line(aes(y=Community1),color="yellow")+
  geom_line(aes(y=Community5),color="green")+
  geom_line(aes(y=Community6),color="blue")+
  geom_line(aes(y=Community7),color="purple")+
  geom_line(aes(y=Community8),color="orange")+
  xlab("Date") +
  ylab("Community Frequency")+
  labs(title="Frequency of Links by Community")

grid.arrange(drops, tweets, communities, links_plot, nrow = 4)

################################################################
######### Time Series Analysis Between Tweets with Links and Q Drops

library(forecast)
library(tseries)
library(vrtest)

colnames(q2)[1] <- "date"
q2 <- full_join(q2, Qdrops)

q2[is.na(q2)] <- 0

drops <- ts(q2$x, frequency=12)

adf.test(q2$Community1)
fd_com1 <- fracdiff::diffseries(q2$Community1, 1)
adf.test(q2$Community2)
fd_com2 <- fracdiff::diffseries(q2$Community2, 1)
adf.test(q2$Community3)
fd_com3 <- fracdiff::diffseries(q2$Community3, 1)
adf.test(q2$Community4)
fd_com4 <- fracdiff::diffseries(q2$Community4, 1)
adf.test(q2$Community5)
fd_com5 <- fracdiff::diffseries(q2$Community5, 1)
adf.test(q2$Community6)
fd_com6 <- fracdiff::diffseries(q2$Community6, 1)
adf.test(q2$Community7)
fd_com7 <- fracdiff::diffseries(q2$Community7, 1)
adf.test(q2$Community8)
fd_com8 <- fracdiff::diffseries(q2$Community8, 1)


adf.test(drops)

vars <- data.frame(Community1 = fd_com1,
                   Community2 = fd_com2,
                   Community3 = fd_com3,
                   Community4 = fd_com4,
                   Community5 = fd_com5,
                   Community6 = fd_com6,
                   Community7 = fd_com7,
                   Community8 = fd_com8,
                   Drops = drops)

tsDyn::lags.select(vars)

lags <- tsDyn::lags.select(vars)
lags$BICs

vars::VARselect(vars)

var_results <- vars::VAR(vars, p = 3, exogen = NULL) #2, 5
summary(var_results)

lmtest::grangertest(vars$Community8 ~ vars$Drops, order = 3)

vars::causality(var_results, cause = "Drops")

#################################################
###### Aquest loop agafa el links reals dels links escurcats del twitter

list_page <- c()

#get_long <- function(url){
  #crul::HttpClient$new(url)$get()$url
#}

get_long <- R.utils::withTimeout({function(url){
 crul::HttpClient$new(url)$get()$url
}},timeout=4,TimeoutException = function(ex) cat("Timed out\n"))

df_long_link <- data.frame(list_page=c("temp1"),comm=c(0))

set.seed(1983)

for (i in 1:max(df_for_link$comm)){
  print(i)
  temp_com <- df_for_link[df_for_link$comm == i, ]
  print(nrow(temp_com))
  sample <- temp_com[sample(nrow(temp_com), (nrow(temp_com)*0.10)),]
  print(nrow(sample))
  p <- sample[, 3]
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
}
