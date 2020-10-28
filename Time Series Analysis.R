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
