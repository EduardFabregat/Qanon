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
