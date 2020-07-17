options(stringsAsFactors = FALSE)

#########################################################
########### Load Datasets

library(readr)
library(tidyverse)

myfilelist<- list.files(path = "", full.names = TRUE)

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

save(Q, file = "Q.All.Tweets.Rda")

#############################################################################
############ Scrape Drops

library(tidyverse)
library(rvest)

url <-'https://qmap.pub/'

list_of_pages <- stringr::str_c(url, '?pg=', 1:74)

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
  list_of_pages <- str_c(url, '?pg=', 1:71)
  
  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a tsv file into the working directory
  list_of_pages %>% 
    # Apply to all URLs
    purrr::map(get_data_from_url, company_name) %>%  
    # Combine the tibbles into one tibble
    bind_rows() %>%                           
    # Write a tab-separated file
    openxlsx::write.xlsx(str_c(company_name,'.xlsx'))     
}

scrape_write_table(url,"QDropsAndDatesALL")

QDropsAndDates <- openxlsx::read.xlsx("QDropsAndDatesALL.xlsx")

drops <- QDropsAndDates[,-2]
drops <- as.data.frame(drops)

library(stringr)

drop <- data.frame(temp=c(1))
drop <- drop[,-1]


for (date in 1:nrow(drops)) {
  print(date)
  temp <- as.data.frame(regmatches(drops[date,],gregexpr("*?...............",drops[date,])))
  temp <- temp[1,]
  temp <- as.data.frame(temp)
  drop <- rbind(drop,temp)
}

Qdrops <- as.data.frame(mdy(drop$temp))

Qdrops$forsum <- 1

volume<-aggregate(x = Qdrops[,"forsum"],
                  by = list(Qdrops$`mdy(drop$temp)`), FUN = "sum")

openxlsx::write.xlsx(volume, "Qdrops.by.date.ALL.xlsx")

####################################################################
############# Time Series Analysis Q drops and tweets
library(tidyverse)
library(lubridate)
days <- as.data.frame(seq(as.Date("2017-10-28"), as.Date("2020-05-30"), by="days"))
colnames(days)[1] <- "date"

Q <- Qdrops_by_date_ALL

colnames(Q)[1] <- "date"

Q <- Q[Q$date < "2020-05-30", ]

Q$date <- date(Q$date)

Q2 <- full_join(days, Q, by = "date")

colnames(Q2)[2] <- "drops"

Q2[is.na(Q2)] <- 0

Q_freq <- Qanon_daily_tweets_count[Qanon_daily_tweets_count$Date >= "2017-10-27" &
                                     Qanon_daily_tweets_count$Date < "2020-05-30", ]

Q_freq$Date <- date(Q_freq$Date)

colnames(Q_freq)[1] <- "date"
colnames(Q_freq)[2] <- "tweets"

Q3 <- full_join(Q2, Q_freq, by = "date")

tweets <- ggplot(Q3, aes(x=date))+
  geom_line(aes(y=tweets))+
  xlab("Date")+
  ylab("Number of Tweets")+
  labs(title = "Frequency of Tweets with #Qanon Between Nov 2017 and May 20120")


drops <- ggplot(Q3, aes(x=date))+
  geom_line(aes(y=drops)) +
  xlab("")+
  ylab("Number of Drops")+
  labs(title = "Frequency of Q Drops Between Nov 2017 and May 2020")

library(grid)
library(gridExtra)

grid.arrange(drops, tweets, nrow = 2)

library(forecast)
library(tseries)
library(vrtest)

tweets <- ts(Q3$tweets, frequency=7)
drops <- ts(Q3$drops, frequency=7)

t = 0:945
par(mfcol=c(2,2))
#the tweets signal and ACF
plot(t,tweets,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Tweets signal")
acf(tweets,lag.max = length(tweets),
    xlab = "lag #", ylab = 'ACF',main=' ')
#the drops signal and ACF
plot(t,drops,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Drops signal")
acf(drops,lag.max = length(drops),
    xlab = "lag #", ylab = 'ACF', main=' ')

lag.length = 25
auto.arima(tweets) #non-stationary
Box.test(tweets, lag=lag.length, type="Ljung-Box") #non-stationary
adf.test(tweets) #stationary
kpss.test(tweets) #non-stationary
Auto.VR(tweets) #non-stationary
fd_tweets <- fracdiff::diffseries(Q3$tweets, 1)
auto.arima(drops) #non-stationary
Box.test(drops, lag=lag.length, type="Ljung-Box") #non-stationary
adf.test(drops) #stationary
kpss.test(drops) #non-stationary
Auto.VR(drops) #stationary
#fd_drops <- fracdiff::diffseries(Q3$drops, 1)


#miro si hi ha trencaments o si la linia es constant
#en aquest cas veiem que hi ha trencaments i per tant no podem utilitzar
#l'adf i el kpss, utilitzarem el philips perron
library(strucchange)
Ftweets <- Fstats(tweets~1)
plot(Ftweets)
sctest(Ftweets)
plot(tweets)
lines(breakpoints(Ftweets))

Fdrops <- Fstats(drops~1)
plot(Fdrops)
sctest(Fdrops)
plot(drops)
lines(breakpoints(Fdrops))

#philips-perron
pp.test(tweets) #stationary
pp.test(drops) #stationary

vars <- data.frame(Tweets = tweets,
                   Drops = drops)

tsDyn::lags.select(vars)

lags <- tsDyn::lags.select(vars)
lags$BICs
lags$AICs


vars::VARselect(vars)

var_results <- vars::VAR(vars, p = 8, exogen = NULL) #8, 2, 3
summary(var_results)

lmtest::grangertest(vars$Tweets ~ vars$Drops, order = 8)

vars::causality(var_results, cause = "Drops")

vars::irf(var_results, impulse = "Drops", 
          response = "Tweets", n.ahead = 10, 
          cumulative = F, ortho = T) %>% 
  plot()

vars::irf(var_results, impulse = "Tweets", 
          response = "Drops", n.ahead = 10, 
          cumulative = F, ortho = T) %>% 
  plot()


########################################################
############ Calculating pre and post 2019

Q31 <- Q3[Q3$date >= "2019-01-01", ]
Q32 <- Q3[Q3$date < "2019-01-01", ]

tweets <- ts(Q31$tweets, frequency=7)
drops <- ts(Q31$drops, frequency=7)

t = 0:515
par(mfcol=c(2,2))
#the tweets signal and ACF
plot(t,tweets,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Tweets signal")
acf(tweets,lag.max = length(tweets),
    xlab = "lag #", ylab = 'ACF',main=' ')
#the drops signal and ACF
plot(t,drops,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Drops signal")
acf(drops,lag.max = length(drops),
    xlab = "lag #", ylab = 'ACF', main=' ')

lag.length = 25
auto.arima(tweets) #non-stationary
Box.test(tweets, lag=lag.length, type="Ljung-Box") #non-stationary
adf.test(tweets) #stationary
kpss.test(tweets) #non-stationary
Auto.VR(tweets) #non-stationary
fd_tweets <- fracdiff::diffseries(Q31$tweets, 1)
auto.arima(drops) #non-stationary
Box.test(drops, lag=lag.length, type="Ljung-Box") #non-stationary
adf.test(drops) #stationary
kpss.test(drops) #non-stationary
Auto.VR(drops) #stationary
#fd_drops <- fracdiff::diffseries(Q3$drops, 1)


#miro si hi ha trencaments o si la linia es constant
#en aquest cas veiem que hi ha trencaments i per tant no podem utilitzar
#l'adf i el kpss, utilitzarem el philips perron
library(strucchange)
Ftweets <- Fstats(tweets~1)
plot(Ftweets)
sctest(Ftweets)
plot(tweets)
lines(breakpoints(Ftweets))

Fdrops <- Fstats(drops~1)
plot(Fdrops)
sctest(Fdrops)
plot(drops)
lines(breakpoints(Fdrops))

#philips-perron
pp.test(tweets) #stationary
pp.test(drops) #stationary

vars <- data.frame(Tweets = tweets,
                   Drops = drops)

tsDyn::lags.select(vars)

lags <- tsDyn::lags.select(vars)
lags$BICs
lags$AICs


vars::VARselect(vars)

var_results <- vars::VAR(vars, p = 7, exogen = NULL) #7, 3 significant at both 7 and 3
summary(var_results)

lmtest::grangertest(vars$Tweets ~ vars$Drops, order = 7) #***

vars::causality(var_results, cause = "Drops") #0.0005341

vars::irf(var_results, impulse = "Drops", 
          response = "Tweets", n.ahead = 7, 
          cumulative = F, ortho = T) %>% 
  plot()

vars::irf(var_results, impulse = "Tweets", 
          response = "Drops", n.ahead = 7, 
          cumulative = F, ortho = T) %>% 
  plot()

###############################################
###############

tweets <- ts(Q32$tweets, frequency=7)
drops <- ts(Q32$drops, frequency=7)

t = 0:429
par(mfcol=c(2,2))
#the tweets signal and ACF
plot(t,tweets,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Tweets signal")
acf(tweets,lag.max = length(tweets),
    xlab = "lag #", ylab = 'ACF',main=' ')
#the drops signal and ACF
plot(t,drops,
     type='l',col='red',
     xlab = "time (t)",
     ylab = "Y(t)",
     main = "Drops signal")
acf(drops,lag.max = length(drops),
    xlab = "lag #", ylab = 'ACF', main=' ')

lag.length = 25
auto.arima(tweets) #non-stationary
Box.test(tweets, lag=lag.length, type="Ljung-Box") #non-stationary
adf.test(tweets) #stationary
kpss.test(tweets) #non-stationary
Auto.VR(tweets) #non-stationary
fd_tweets <- fracdiff::diffseries(Q3$tweets, 1)
auto.arima(drops) #non-stationary
Box.test(drops, lag=lag.length, type="Ljung-Box") #non-stationary
adf.test(drops) #stationary
kpss.test(drops) #non-stationary
Auto.VR(drops) #stationary
#fd_drops <- fracdiff::diffseries(Q3$drops, 1)


#miro si hi ha trencaments o si la linia es constant
#en aquest cas veiem que hi ha trencaments i per tant no podem utilitzar
#l'adf i el kpss, utilitzarem el philips perron
library(strucchange)
Ftweets <- Fstats(tweets~1)
plot(Ftweets)
sctest(Ftweets)
plot(tweets)
lines(breakpoints(Ftweets))

Fdrops <- Fstats(drops~1)
plot(Fdrops)
sctest(Fdrops)
plot(drops)
lines(breakpoints(Fdrops))

#philips-perron
pp.test(tweets) #stationary
pp.test(drops) #stationary

vars <- data.frame(Tweets = tweets,
                   Drops = drops)

tsDyn::lags.select(vars)

lags <- tsDyn::lags.select(vars)
lags$BICs
lags$AICs


vars::VARselect(vars)

var_results <- vars::VAR(vars, p = 2, exogen = NULL) #4, 2 not significant
summary(var_results)

lmtest::grangertest(vars$Tweets ~ vars$Drops, order = 2)

vars::causality(var_results, cause = "Drops")


vars::irf(var_results, impulse = "Drops", 
          response = "Tweets", n.ahead = 10, 
          cumulative = F, ortho = T) %>% 
  plot()

vars::irf(var_results, impulse = "Tweets", 
          response = "Drops", n.ahead = 10, 
          cumulative = F, ortho = T) %>% 
  plot()


##########################################################
### Calculate Frequency of tweets by Community

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

library(tidyverse)
library(rvest)

url <-'https://qmap.pub/'

list_of_pages <- stringr::str_c(url, '?pg=', 1:71)

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
  list_of_pages <- str_c(url, '?pg=', 1:71)
  
  # Apply the extraction and bind the individual results back into one table, 
  # which is then written as a tsv file into the working directory
  list_of_pages %>% 
    # Apply to all URLs
    purrr::map(get_data_from_url, company_name) %>%  
    # Combine the tibbles into one tibble
    bind_rows() %>%                           
    # Write a tab-separated file
    openxlsx::write.xlsx(str_c(company_name,'.xlsx'))     
}

scrape_write_table(url,"QDropsAndDates")

QDropsAndDates <- read_excel("QDropsAndDates.xlsx")

drops <- QDropsAndDates[,-2]

drop <- data.frame(temp=c(1))

for (i in 1:nrow(drops)) {
  print(i)
  temp <- as.data.frame(regmatches(drops[i,],gregexpr("*?...............",drops[i,])))
  temp <- temp[1,]
  temp <- as.data.frame(temp)
  drop <- rbind(drop,temp)
}


Qdrops <- as.data.frame(lubridate::mdy(drop$temp))
colnames(Qdrops)[1] <- "dates"

Q <- cbind(QDropsAndDates, Qdrops[-1,])

colnames(Q)[3] <- "dates"

Q$forsum <- 1

volume<-aggregate(x = Q[,"forsum"],
                  by = list(Q$dates), FUN = "sum")

colnames(volume)[1] <- "date"

days <- as.data.frame(seq(as.Date("2017-10-28"), as.Date("2020-05-15"), by="days"))
colnames(days)[1] <- "date"

Q2 <- full_join(days, volume, by = "date")

Q2[is.na(Q2)] <- 0

ggplot(Q2, aes(x = date))+
  geom_line(aes(y = x))+
  ylab("") +
  xlab("Date") +
  labs(title = "Frequency of Drops")

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
####### Per veure la frequency de tuits amb links

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
  rm(sample,temp_com)
}
                                
#####################################################
######### Extract URLs
         
URLs <- df_long_link$list_page
get_domain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
domains <- as.data.frame(sapply(URLs, get_domain))

domains_com <- cbind(df_long_links, domains)
colnames(domains_com)[3] <- "domains"

save(domains_com,file="10.percent.links.with.domain.Rda")
