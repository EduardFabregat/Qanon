##############################################################
######### Topic Modelling

library(tidyverse)
library(quanteda)

Q$text <- gsub("[h][t][t][p]\\S+","", Q$Contents)
Q$text <- gsub("pic.twitter\\S+","", Q$text)
Q$text <- gsub("RT", "", Q$text)
Q$text <- iconv(Q$text, "latin1", "ASCII", sub="")

colnames(Q)[22] <- "Date_day"
colnames(Q)[2] <- "date"

Q$index <- 1:nrow(Q)

Qwithdups<-Q
removed_df<-Q[duplicated(Q$text),]
Q <- Q[!duplicated(Q$text),]

mycorpus <- corpus(Q)
stopwords_and_single <- c(stopwords("english"), "amp", "&amp", "=", "+", ">", "&", 
                          "$", "<", LETTERS,letters)

dfm_Q <- tokens(mycorpus) %>%
  tokens_remove("[[:punct:]]+", valuetype = "regex", padding = TRUE,verbose = TRUE) %>%
  tokens_remove("[[:digit:]]+", valuetype = "regex", padding = TRUE,verbose = TRUE) %>% 
  tokens_remove(stopwords_and_single, padding  = TRUE, verbose = TRUE) %>%
  tokens_ngrams(n = c(1:3)) %>% #unigrams, bigrams & trigrams
  dfm()

docnames(dfm_Q) <- dfm_Q@docvars$index

library(RNewsflow)

#dfm_Q2 <- delete.duplicates(dfm_Q, similarity = .95, 
#                            keep = "first", tf.idf = FALSE, verbose = TRUE)

###Deleted 622076

#dfm_Q3 <- dfm_trim(dfm_Q2, max_docfreq = (0.95*nrow(dfm_Q)), min_docfreq = 10, 
#                   docfreq_type = "count")

dfm_Q3 <- dfm_trim(dfm_Q, max_docfreq = (0.95*nrow(dfm_Q)), min_docfreq = 10, 
                   docfreq_type = "count")

#dfm_Q
#dfm_Q2
#dfm_Q3

topfeatures(dfm_Q3, n = 30)
featnames(dfm_Q3)

dtm_lda <- convert(dfm_Q3, to = "topicmodels")

library(doParallel)

full_data<-dtm_lda

n <- nrow(full_data)

mycores <- detectCores()-1

print(Sys.time())
MainresultDF<-data.frame(k=c(1),perplexity=c(1),myalpha=c("x"))
MainresultDF<-MainresultDF[-1,]
candidate_alpha<- c(0.01, 0.05, 0.1, 0.2,0.5)
candidate_k <- c(seq(1,20)*5) #5 to 100 in jumps of 5


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
    results <- foreach::foreach(j = 1:length(candidate_k), .combine = rbind) %dopar%{
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

save(MainresultDF, file = "MainresultDF.Rda")

MainresultDF$kalpha <- paste0(as.character(MainresultDF$k),MainresultDF$myalpha) 
ggplot(MainresultDF) +geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))

ggplot(MainresultDF) +
  geom_boxplot(aes(x=k, y=perplexity, group=kalpha,color=myalpha))+
  #geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.5)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.2)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.1)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.05)]),linetype = "dotted")+
  geom_hline(yintercept=min(MainresultDF$perplexity[which(MainresultDF$myalpha==0.01)]),linetype = "dotted")

MainDF <- MainresultDF[MainresultDF$myalpha == 0.05, ] #correct alpha 

MainresultDF<-MainresultDF[order(MainresultDF$k),]

cars.spl <- with(MainresultDF, smooth.spline(k, perplexity, df = 3))
with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2))

plot(with(cars, predict(cars.spl, x = MainresultDF$k, deriv = 2)), type = "l")
abline(v=40)

runsdf<-data.frame(myk=c(40))

mymodels<-list()

cluster <- makeCluster(detectCores(logical = TRUE)-1) # leave one CPU spare...
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
                  control = list(alpha=0.05,seed=1983) )
    #control = list(seed=3341) )
  }
})
stopCluster(cluster)

save.image("Qanon.Final.Attempt.Rdata")

extract_excels<-function (mymodel) {
  #require(xlsx)
  LDAfit<-mymodel
  
  mybeta<-data.frame(LDAfit@beta)
  colnames(mybeta)<-LDAfit@terms
  mybeta<-t(mybeta)
  colnames(mybeta)<-seq(1:ncol(mybeta))
  mybeta=exp(mybeta)
  
  ##################################### Now we cycle and print top words for each topic
  nwords=50
  
  topwords <- mybeta[1:nwords,]
  for (i in 1:LDAfit@k) {
    tempframe <- mybeta[order(-mybeta[,i]),]
    tempframe <- tempframe[1:nwords,]
    tempvec<-as.vector(rownames(tempframe))
    topwords[,i]<-tempvec
  }
  
  rownames(topwords)<-c(1:nwords)
  
  kalpha<-paste0(as.character(LDAfit@k),"_",gsub("\\.","",as.character(LDAfit@alpha)))
  openxlsx::write.xlsx(topwords, paste0(kalpha,"_",project_name,"_Topwords.xlsx"))
  
  ################################ FREX TIME
  # get the beta
  mybeta<-data.frame(LDAfit@beta)
  colnames(mybeta)<-LDAfit@terms
  mybeta<-t(mybeta)
  colnames(mybeta)<-seq(1:ncol(mybeta))
  mybeta=exp(mybeta)
  
  # apply formula below
  # 1/(w/(bword/sumbrow)+(1-w)/(bword)) for each cell
  myw=0.3
  word_beta_sums<-rowSums(mybeta)
  my_beta_for_frex<-mybeta
  for (m in 1:ncol(my_beta_for_frex)) {
    for (n in 1:nrow(my_beta_for_frex)) {
      my_beta_for_frex[n,m]<-1/(myw/(my_beta_for_frex[n,m]/word_beta_sums[n])+((1-myw)/my_beta_for_frex[n,m]))
    }
    print (m)
  }
  ########  print 50 frex:
  nwords=50
  
  topwords <- my_beta_for_frex[1:nwords,]
  for (i in 1:LDAfit@k) {
    tempframe <- my_beta_for_frex[order(-my_beta_for_frex[,i]),]
    tempframe <- tempframe[1:nwords,]
    tempvec<-as.vector(rownames(tempframe))
    topwords[,i]<-tempvec
  }
  
  rownames(topwords)<-c(1:nwords)
  
  kalpha<-paste0(as.character(LDAfit@k),"_",gsub("\\.","",as.character(LDAfit@alpha)))
  openxlsx::write.xlsx(topwords, paste0(kalpha,"_",project_name,"_TopFREX.xlsx"))
  
  #######################
  
  ####################### TOP TEXTS --->
  #data3$index2<-data3$index+1
  data33<-data3
  #data33$index<-data33$index+1
  deleted_lda_texts<-(setdiff(data33$index, as.numeric(LDAfit@documents)))
  #deleted_lda_texts2<-(setdiff(as.character(LDAfit@documents),as.character(data3$doc_id)))
  
  #deleted_lda_texts<-unique(c(deleted_lda_texts1,deleted_lda_texts2))
  
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  #data33<-data3
  data33<-data33[data33$index %!in% deleted_lda_texts,]
  
  metadf<-data33
  meta_theta_df<-cbind(metadf["text"],LDAfit@gamma)
  
  ntext=50
  
  toptexts <- mybeta[1:ntext,]
  for (i in 1:LDAfit@k) {
    print(i)
    tempframe <- meta_theta_df[order(-meta_theta_df[,i+1]),]
    tempframe <- tempframe[1:ntext,]
    tempvec<-as.vector(tempframe[,1])
    toptexts[,i]<-tempvec
  }
  
  rownames(toptexts)<-c(1:ntext)
  
  kalpha<-paste0(as.character(LDAfit@k),"_",gsub("\\.","",as.character(LDAfit@alpha)))
  openxlsx::write.xlsx(toptexts, paste0(kalpha,"_",project_name,"_TopTexts.xlsx"))
  
}

#####################################
data3 <- Q
project_name <- "Qanon"

extract_excels(mymodels[[1]])

######################################

options(stringsAsFactors = FALSE)

data3 <- Q

LDAfit<-mymodels[[1]]
metadf<-data3

Q_short<-Q

missing_docs<-setdiff(dfm_Q@Dimnames$docs,LDAfit@documents)

Q_short<-Q_short[-which(Q_short$index %in% missing_docs),]
meta_theta_df<-cbind(Q_short,LDAfit@gamma)
colnames(meta_theta_df)[25:64]<-paste0("X",colnames(meta_theta_df)[25:64])


dfm_short <- dfm_Q3
missing_docs2<-setdiff(dfm_short@Dimnames$docs,LDAfit@documents)
dfm_short <- dfm_short[-which(dfm_short@Dimnames$docs %in% missing_docs2), ]

dfm_forsize<-data.frame(dfm_short)
dfm_forsize<-dfm_forsize[,-1]


sizevect<-rowSums(dfm_forsize)
meta_theta_df<-data.frame(size=sizevect,meta_theta_df)

duplicate_df<-removed_df
colnames(duplicate_df)<-paste0(colnames(duplicate_df),".1")

dflist<-list()
for (i in (1:nrow(duplicate_df))) {
  print(i)
  the_match<-match(duplicate_df$text.1[i],meta_theta_df$text)
  newvect<-c(duplicate_df[i,],meta_theta_df[the_match,])
  dflist[[i]]<-newvect
}

maintable<-data.frame(do.call(bind_rows,dflist))

maintable2<-data.frame(maintable[,-c((ncol(duplicate_df)):(ncol(duplicate_df)+ncol(metadf)-1))])

colnames(maintable2)<-gsub("\\.1","",colnames(maintable2))
meta_theta_df<-bind_rows(meta_theta_df,maintable2)

meta_theta_df <- meta_theta_df[,-c(65,66,67,68,69,70)]

save(meta_theta_df, file = "meta_theta_df_all.Rda")

save.image("Qanon.Final.Attempt.With.Duplicates.Rdata")

#there are some NAs
nas_ <- meta_theta_df[,c(24,25)]

nas_ <- na.omit(nas_)

meta_theta_df2 <- meta_theta_df[which(meta_theta_df$index %in% nas_$index),]

rm(meta_theta_df)

meta_theta_df2$forsum <- 1

users_n <- aggregate(x=meta_theta_df2[,"forsum"],
                     by=list(meta_theta_df2$Author),FUN="sum")

summary(users_n$x)

hist(log10(users_n$x), col='blue')

themesbyuser <- aggregate(x=meta_theta_df2[,25:64],
                          by=list(meta_theta_df2$Author),FUN="mean")


themesbyuser$num_tweets <- users_n$x

themes_10 <- themesbyuser[themesbyuser$num_tweets <= 10, ]
themes_50 <- themesbyuser[themesbyuser$num_tweets > 10 & themesbyuser$num_tweets <= 50, ]
themes_100 <- themesbyuser[themesbyuser$num_tweets > 50 & themesbyuser$num_tweets <= 100, ]
themes_1000 <- themesbyuser[themesbyuser$num_tweets > 100 & themesbyuser$num_tweets <=1000, ]
themes_2000 <- themesbyuser[themesbyuser$num_tweets > 1000,]

#Al final no he utilitzat el split i el cut, he utilitzat el que tinc a sobre
#stratified_df <- split(themesbyuser, cut(themesbyuser$num_tweets, 20))

sample_10 <- themes_10[sample(nrow(themes_10), (nrow(themes_10)*0.05)),]

hist(themes_10$num_tweets)
hist(sample_10$num_tweets)

sample_50 <- themes_50[sample(nrow(themes_50), (nrow(themes_50)*0.05)),]

hist(themes_50$num_tweets)
hist(sample_50$num_tweets)

sample_100 <- themes_100[sample(nrow(themes_100), (nrow(themes_100)*0.05)),]

hist(themes_100$num_tweets)
hist(sample_100$num_tweets)

sample_1000 <- themes_1000[sample(nrow(themes_1000), (nrow(themes_1000)*0.05)),]

hist(themes_1000$num_tweets)
hist(sample_1000$num_tweets)

sample_df <- rbind(sample_10, sample_50, sample_100, sample_1000, themes_2000)

hist(log10(users_n$x), col='blue')
hist(log10(sample_df$num_tweets), col='blue')

users_n2 <- users_n[which(users_n$Group.1 %in% sample_df$Group.1),]

colnames(sample_df)[1] <- "screen_name"

rownames(sample_df) <- sample_df$screen_name

sample_df2 <- sample_df[,-1]

sample_df3 <- t(sample_df2)

library(lsa)

mycosine <- cosine(sample_df3)

library(igraph)
sem_net_weighted <- graph.adjacency(mycosine,mode="undirected",weighted=T,diag=F,add.colnames="label") # Assign colnames

rm(mycosine)

V(sem_net_weighted)$name<-V(sem_net_weighted)$label

V(sem_net_weighted)$size<-users_n2$x

vcount(sem_net_weighted)
ecount(sem_net_weighted)

rm(sample_df3, users_n2)

library(skynet)
rm(g)
gc()
library(corpustools)
g<-backbone_filter(sem_net_weighted,alpha=0.3455)

is.connected(g)
vcount(g)
ecount(g)

save.image(file = "Sample.All.Tweets.Backbone.Rdata")

set.seed(1983)

mylouvain<-(cluster_louvain(g))

V(g)$louvain<-mylouvain$membership 

V(g)$degree <- degree(g, mode = "total")

V(g)$strength <- strength(g, mode = "total")

#V(g)$btw <- betweenness(g, v = V(g), directed = FALSE)

V(g)$core <- coreness(g)

write.graph(g, "Qanon.Them.Commn.Bbone.Sample.03455.graphml", format = "graphml")

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

save.image("Sample.All.Tweets.Backbone.with.Nodeslist.Rdata")

##grab for each comm the top 20 users
top_user_com_df<-data.frame(matrix(NA, nrow = 200, ncol = length(unique(user_comm_df$comm))))

for (i in 1:max(user_comm_df$comm)) {
  print (i)
  temp_df<-user_comm_df[user_comm_df$comm==i,]
  temp_df<-temp_df[order(temp_df$commstr,decreasing = TRUE),]
  towrite<-temp_df$label[1:200]
  top_user_com_df[,i]<-towrite
}

### now we print top tweets
## go to data - filter by top 20 users
## grab 200 random tweets - print to dataframe and XL
comm_tweets_list<-list()
for (i in 1:max(user_comm_df$comm)) {
  print(i)
  temp_meta_theta_df<-meta_theta_df2[meta_theta_df2$Author %in% top_user_com_df[,i],]
  temp_meta_theta_df<- temp_meta_theta_df[sample(nrow(temp_meta_theta_df), 200), ]
  comm_tweets_list[[i]]<-c(temp_meta_theta_df)
  openxlsx::write.xlsx(temp_meta_theta_df,paste0(as.character(i),"_COMM_200_2tweets.xlsx"))
}

save(user_comm_df, file="user.comm.df.Rda")

options(stringsAsFactors = FALSE)

nas_ <- meta_theta_df[,c(24,25)]

nas_ <- na.omit(nas_)

meta_theta_df2 <- meta_theta_df[which(meta_theta_df$index %in% nas_$index),]

meta_theta_df2$forsum <- 1

comm_list <- list()
for (i in 1:7){
  print(i)
  temp <- user_comm_df[user_comm_df$comm == i,]
  temp2 <- temp$label
  comm_list[[i]] <- meta_theta_df2[which(meta_theta_df2$Author %in% temp2),]
}

q1 <- comm_list[[1]]
q2 <- comm_list[[2]]
q3 <- comm_list[[3]]
q4 <- comm_list[[4]]
q5 <- comm_list[[5]]
q6 <- comm_list[[6]]
q7 <- comm_list[[7]]


q1$date <- lubridate::as_date(q1$date)
q1_bydate <- aggregate(x=q1[,"forsum"],by=list(q1$date),FUN="sum")
colnames(q1_bydate)[1] <- "date"
colnames(q1_bydate)[2] <- "n"

q2$date <- lubridate::as_date(q2$date)
q2_bydate <- aggregate(x=q2[,"forsum"],by=list(q2$date),FUN="sum")
colnames(q2_bydate)[1] <- "date"
colnames(q2_bydate)[2] <- "n"

q3$date <- lubridate::as_date(q3$date)
q3_bydate <- aggregate(x=q3[,"forsum"],by=list(q3$date),FUN="sum")
colnames(q3_bydate)[1] <- "date"
colnames(q3_bydate)[2] <- "n"

q4$date <- lubridate::as_date(q4$date)
q4_bydate <- aggregate(x=q4[,"forsum"],by=list(q4$date),FUN="sum")
colnames(q4_bydate)[1] <- "date"
colnames(q4_bydate)[2] <- "n"

q5$date <- lubridate::as_date(q5$date)
q5_bydate <- aggregate(x=q5[,"forsum"],by=list(q5$date),FUN="sum")
colnames(q5_bydate)[1] <- "date"
colnames(q5_bydate)[2] <- "n"

q6$date <- lubridate::as_date(q6$date)
q6_bydate <- aggregate(x=q6[,"forsum"],by=list(q6$date),FUN="sum")
colnames(q6_bydate)[1] <- "date"
colnames(q6_bydate)[2] <- "n"

q7$date <- lubridate::as_date(q7$date)
q7_bydate <- aggregate(x=q7[,"forsum"],by=list(q7$date),FUN="sum")
colnames(q7_bydate)[1] <- "date"
colnames(q7_bydate)[2] <- "n"


q1_bydate$comm <- "Comm1"
q2_bydate$comm <- "Comm2"
q3_bydate$comm <- "Comm3"
q4_bydate$comm <- "Comm4"
q5_bydate$comm <- "Comm5"
q6_bydate$comm <- "Comm6"
q7_bydate$comm <- "Comm7"

pp <- rbind(q1_bydate, q2_bydate,q3_bydate,q4_bydate,q5_bydate,q6_bydate,q7_bydate)

library(tidyverse)

data <- pp  %>%
  group_by(date, comm) %>%
  summarise(n = sum(n))%>%
  mutate(percentage = n / sum(n))

library(viridis)
library(hrbrthemes)


ggplot(data, aes(x=date, y=percentage, fill=comm)) + 
  geom_area(alpha=0.6 , size=1)+
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Qanon Thematic Communities Over Time") +
  theme_ipsum() +
  theme(legend.position="none")

ggplot(data, aes(x=date, y=percentage, fill=comm)) + 
  geom_area(alpha=0.6 , size=1)+
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = FALSE) +
  ggtitle("Qanon Thematic Communities Over Time")


tweets_com <- ggplot(data, aes(x=date, y=percentage, fill=comm)) + 
  geom_area(alpha=0.6 , size=1)+
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = FALSE) +
  theme(legend.position="none") +
  ggtitle("Qanon Thematic Communities Over Time")


q1_byuser <- aggregate(x=q1[,25:64],by=list(q1$Author),FUN="mean")

comm1_total <- sapply(Filter(is.numeric, q1_byuser), mean)
comm1_total <- as.data.frame(sort(comm1_total, decreasing=TRUE))

q2_byuser <- aggregate(x=q2[,25:64],by=list(q2$Author),FUN="mean")

comm2_total <- sapply(Filter(is.numeric, q2_byuser), mean)
comm2_total <- as.data.frame(sort(comm2_total, decreasing=TRUE))


q3_byuser <- aggregate(x=q3[,25:64],by=list(q3$Author),FUN="mean")

comm3_total <- sapply(Filter(is.numeric, q3_byuser), mean)
comm3_total <- as.data.frame(sort(comm3_total, decreasing=TRUE))


q4_byuser <- aggregate(x=q4[,25:64],by=list(q4$Author),FUN="mean")

comm4_total <- sapply(Filter(is.numeric, q4_byuser), mean)
comm4_total <- as.data.frame(sort(comm4_total, decreasing=TRUE))


q5_byuser <- aggregate(x=q5[,25:64],by=list(q5$Author),FUN="mean")

comm5_total <- sapply(Filter(is.numeric, q5_byuser), mean)
comm5_total <- as.data.frame(sort(comm5_total, decreasing=TRUE))


q6_byuser <- aggregate(x=q6[,25:64],by=list(q6$Author),FUN="mean")

comm6_total <- sapply(Filter(is.numeric, q6_byuser), mean)
comm6_total <- as.data.frame(sort(comm6_total, decreasing=TRUE))


q7_byuser <- aggregate(x=q7[,25:64],by=list(q7$Author),FUN="mean")

comm7_total <- sapply(Filter(is.numeric, q7_byuser), mean)
comm7_total <- as.data.frame(sort(comm7_total, decreasing=TRUE))
