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
