library(igraph)
library(tidyverse)

vcount(g)
ecount(g)


#small world property, which refers to the situation
#wherein (a) the shortest-path distance between pairs of vertices is generally
#quite small, but (b) the clustering is relatively high.

mod <- modularity(g, V(g)$louvain) #0.097

trans <- transitivity(g) #0.71

trans_local <- transitivity(g, type="local")

#The value of the longest distance in a graph is called the diameter of the graph

diameter(g) #1.95

mean_distance(g) #1.72

#Small World Network

V(g)$size <- sample_df$num_tweets

betw <- as.data.frame(sort(betweenness(g, v = V(g), directed = FALSE), decreasing = TRUE))

betw$name <- rownames(betw)

colnames(users_n2)[1] <- "name"

betw <- left_join(betw, users_n2)

colnames(users_n2)[1] <- "name"

colnames(betw)[1] <- "betweenness"

cor.test(betw$betweenness, betw$x) #-0.0465, p = 7.773e-06

cores <- as.data.frame((V(g)$core), (V(g)$name))

cores$name <- rownames(cores)

colnames(cores)[1] <- "cores"

cores_count <- count(cores, cores)

betw <- cbind(betw, cores)

cor.test(betw$betweenness, betw$cores) #-0.0594 p = 1.162e-08

cor.test(betw$x, betw$cores) #0.1056 p = 2.2e-16

louvain <- as.data.frame((V(g)$louvain), (V(g)$name))

colnames(louvain)[1] <- "louvain"

louvain$name <- rownames(louvain)

count_louvain <- count(louvain, louvain)

count_louvain$perc <- count_louvain$n/sum(count_louvain)

betw <- cbind(betw, louvain$louvain)

colnames(betw)[5] <- "louvain"

betw <- cbind(betw, as.data.frame(trans_local))

colnames(betw)[6] <- "trans_local"

cor.test(betw$betweenness, betw$trans_local) #-0.0757 p = 3.546e-13

summary(lm(betweenness ~ x + cores + louvain + trans_local, betw))

####################################
'
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  8.019e+03  7.317e+02  10.960  < 2e-16 ***
  x           -5.136e+00  1.673e+00  -3.069  0.00215 ** 
  cores       -3.113e-01  7.936e-02  -3.922 8.83e-05 ***
  louvain     -1.091e+00  4.721e+01  -0.023  0.98156    
trans_local -5.810e+03  1.024e+03  -5.672 1.46e-08 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 8003 on 9198 degrees of freedom
Multiple R-squared:  0.008676,	Adjusted R-squared:  0.008245 
F-statistic: 20.12 on 4 and 9198 DF,  p-value: < 2.2e-16

'

####################################

save(betw, file = "Betwenness.all.users.Rda")


########### Core
core_net <- induced.subgraph(g, V(g)$core > 687)

is.connected(core_net)
vcount(core_net)
ecount(core_net)

max(V(core_net)$core)
min(V(core_net)$core)

betw_core <- as.data.frame(sort(betweenness(core_net, v = V(core_net), directed = FALSE), decreasing = TRUE))

betw_core$name <- rownames(betw_core)

colnames(betw_core)[1] <- "betweenness"

colnames(users_n2)[1] <- "name"

betw_core <- left_join(betw_core, users_n2)

betw_core <- left_join(betw_core, louvain)

core_louvain <- count(betw_core, louvain)

core_louvain$perc <- core_louvain$n/sum(core_louvain)

cor.test(betw_core$betweenness, betw_core$x) #-0.0434, p = 0.002459

cor.test(betw_core$betweenness, betw_core$louvain) #-0.120 p = 2.2e-16

core_trans_local <- transitivity(core_net, type="local")

mean(core_trans_local) #0.88

betw_core <- cbind(betw_core, as.data.frame(core_trans_local))

colnames(betw_core)[5] <- "trans_local"

cor.test(betw_core$betweenness, betw_core$trans_local) #0.05474278 p = 0.0001346

betw_core <- left_join(betw_core, cores)

save(betw_core, file = "Betweenness.Core.Rda")

modularity(core_net, V(core_net)$louvain) #0.008

diameter(core_net) #1.97

mean_distance(core_net) #1.23

transitivity(core_net)#0.86

summary(lm(betweenness ~ x + cores + louvain + trans_local, betw_core))

######################

'
Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  8.502e+03  4.005e+02  21.230   <2e-16 ***
x            1.199e-03  3.820e-01   0.003    0.997    
cores       -3.075e+00  5.262e-02 -58.435   <2e-16 ***
louvain     -3.986e+00  1.766e+01  -0.226    0.821    
trans_local  4.907e+02  4.123e+02   1.190    0.234    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1827 on 4854 degrees of freedom
Multiple R-squared:  0.4242,	Adjusted R-squared:  0.4237 
F-statistic:   894 on 4 and 4854 DF,  p-value: < 2.2e-16

'
######################

#### Center

center <- induced.subgraph(g, V(g)$core <= 687)
center <- induced.subgraph(center, V(center)$core >= 647)

is.connected(center)
vcount(center)
ecount(center)

max(V(center)$core)
min(V(center)$core)

betw_center <- as.data.frame(sort(betweenness(center, v = V(center), directed = FALSE), decreasing = TRUE))

betw_center$name <- rownames(betw_center)

colnames(betw_center)[1] <- "betweenness"

betw_center <- left_join(betw_center, users_n2)

betw_center <- left_join(betw_center, louvain)

center_louvain <- count(betw_center, louvain)

center_louvain$perc <- center_louvain$n/sum(center_louvain)

cor.test(betw_center$betweenness, betw_center$x) #NA

cor.test(betw_center$betweenness, betw_center$louvain) #0.0734 p = 1.631e-05

center_trans_local <- transitivity(center, type="local")

mean(center_trans_local) #0.56

transitivity(center) #0.55

betw_center <- cbind(betw_center, as.data.frame(center_trans_local))

colnames(betw_center)[5] <- "trans_local"

cor.test(betw_center$betweenness, betw_center$trans_local) #0.026611 p = 0.1187

betw_center <- left_join(betw_center, cores)

save(betw_center, file = "Betweenness.Center.Rda")

modularity(center, V(center)$louvain) #0.43

diameter(center) #2.68

mean_distance(center) #1.98

summary(lm(betweenness ~ x + cores + louvain + trans_local, betw_center))

######################

'
Coefficients: (1 not defined because of singularities)
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -14560.28    3793.34  -3.838 0.000126 ***
x                  NA         NA      NA       NA    
cores           23.76       5.76   4.125  3.8e-05 ***
louvain         70.04      25.06   2.795 0.005216 ** 
trans_local    531.36     396.14   1.341 0.179898    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2976 on 3435 degrees of freedom
Multiple R-squared:  0.01099,	Adjusted R-squared:  0.01012 
F-statistic: 12.72 on 3 and 3435 DF,  p-value: 2.885e-08

'
######################


#### Periphery

per <- induced.subgraph(g, V(g)$core < 647)

is.connected(per)
vcount(per)
ecount(per)

max(V(per)$core)
min(V(per)$core)

betw_per <- as.data.frame(sort(betweenness(per, v = V(per), directed = FALSE), decreasing = TRUE))

betw_per$name <- rownames(betw_per)

colnames(betw_per)[1] <- "betweenness"

betw_per <- left_join(betw_per, users_n2)

betw_per <- left_join(betw_per, louvain)

per_louvain <- count(betw_per, louvain)

per_louvain$perc <- per_louvain$n/sum(per_louvain)

cor.test(betw_per$betweenness, betw_per$x) #NA

cor.test(betw_per$betweenness, betw_per$louvain) #0.0414 p = 0.2132

per_trans_local <- transitivity(per, type="local")

mean(per_trans_local) #NA

transitivity(per) #0.81

betw_per <- cbind(betw_per, as.data.frame(per_trans_local))

colnames(betw_per)[5] <- "trans_local"

cor.test(betw_per$betweenness, betw_per$trans_local) #-0.0191  p = 0.5662

betw_per <- left_join(betw_per, cores)

save(betw_per, file = "Betweenness.Periphery.Rda")

modularity(per, V(per)$louvain) #0.34

diameter(per) #5.15

mean_distance(per) #2.49

summary(lm(betweenness ~ x + cores + louvain + trans_local, betw_per))

######################

'
Coefficients: (1 not defined because of singularities)
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1528.4599   646.8971  -2.363  0.01835 *  
x                   NA         NA      NA       NA    
cores           3.7718     0.8304   4.542 6.32e-06 ***
louvain       121.1291    45.3122   2.673  0.00765 ** 
trans_local  -178.9031   447.2100  -0.400  0.68922    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1879 on 900 degrees of freedom
  (1 observation deleted due to missingness)
Multiple R-squared:  0.02442,	Adjusted R-squared:  0.02117 
F-statistic: 7.511 on 3 and 900 DF,  p-value: 5.74e-05

'
######################

######################

com1 <- induced.subgraph(g, V(g)$louvain == 1)

is.connected(com1)
vcount(com1) #3357
ecount(com1) #865464

max(V(com1)$core) #2893
min(V(com1)$core) #320

betw_com1 <- as.data.frame(sort(betweenness(com1, v = V(com1), directed = FALSE), decreasing = TRUE))

betw_com1$name <- rownames(betw_com1)

colnames(betw_com1)[1] <- "betweenness"

colnames(users_n2)[1] <- "name"

betw_com1 <- left_join(betw_com1, users_n2)

cor.test(betw_com1$betweenness, betw_com1$x) #0.51 p = 2.2e-16

com1_trans_local <- transitivity(com1, type="local")

mean(com1_trans_local) #0.64

transitivity(com1) #0.45

betw_com1 <- cbind(betw_com1, as.data.frame(com1_trans_local))

colnames(betw_com1)[4] <- "trans_local"

cor.test(betw_com1$betweenness, betw_com1$trans_local) #0.01 p = 0.5

betw_com1 <- left_join(betw_com1, cores)

save(betw_com1, file = "Betweenness.Com1.Rda")

diameter(com1) #1.93

mean_distance(com1) #1.84


######################

com2 <- induced.subgraph(g, V(g)$louvain == 2)

is.connected(com2)
vcount(com2) #591
ecount(com2) #129054

max(V(com2)$core) #2893
min(V(com2)$core) #543

betw_com2 <- as.data.frame(sort(betweenness(com2, v = V(com2), directed = FALSE), decreasing = TRUE))

betw_com2$name <- rownames(betw_com2)

colnames(betw_com2)[1] <- "betweenness"

betw_com2 <- left_join(betw_com2, users_n2)

cor.test(betw_com2$betweenness, betw_com2$x) #-0.001 p = 0.9

com2_trans_local <- transitivity(com2, type="local")

mean(com2_trans_local) #0.88

transitivity(com2) #0.87

betw_com2 <- cbind(betw_com2, as.data.frame(com2_trans_local))

colnames(betw_com2)[4] <- "trans_local"

cor.test(betw_com2$betweenness, betw_com2$trans_local) #0.05 p = 0.2

betw_com2 <- left_join(betw_com2, cores)

save(betw_com2, file = "Betweenness.Com2.Rda")

diameter(com2) #1.89

mean_distance(com2) #1.26


######################

com3 <- induced.subgraph(g, V(g)$louvain == 3)

is.connected(com3)
vcount(com3) #2759
ecount(com3) #3411194

max(V(com3)$core) #2893
min(V(com3)$core) #2388

betw_com3 <- as.data.frame(sort(betweenness(com3, v = V(com3), directed = FALSE), decreasing = TRUE))

betw_com3$name <- rownames(betw_com3)

colnames(betw_com3)[1] <- "betweenness"

betw_com3 <- left_join(betw_com3, users_n2)

cor.test(betw_com3$betweenness, betw_com3$x) #-0.04 p = 0.01

com3_trans_local <- transitivity(com3, type="local")

mean(com3_trans_local) #0.93

transitivity(com3) #0.91

betw_com3 <- cbind(betw_com3, as.data.frame(com3_trans_local))

colnames(betw_com3)[4] <- "trans_local"

betw_com3 <- left_join(betw_com3, cores)

cor.test(betw_com3$betweenness, betw_com3$trans_local) #0.10 p = 1.504e-08

save(betw_com3, file = "Betweenness.Com3.Rda")

diameter(com3) #1.97

mean_distance(com3) #1.1

##############################

com4 <- induced.subgraph(g, V(g)$louvain == 4)

is.connected(com4)
vcount(com4) #691
ecount(com4) #99580

max(V(com4)$core) #2893
min(V(com4)$core) #328

betw_com4 <- as.data.frame(sort(betweenness(com4, v = V(com4), directed = FALSE), decreasing = TRUE))

betw_com4$name <- rownames(betw_com4)

colnames(betw_com4)[1] <- "betweenness"

betw_com4 <- left_join(betw_com4, users_n2)

cor.test(betw_com4$betweenness, betw_com4$x) #0.13 p = 0.0002

com4_trans_local <- transitivity(com4, type="local")

mean(com4_trans_local) #0.81

transitivity(com4) #0.75

betw_com4 <- cbind(betw_com4, as.data.frame(com4_trans_local))

colnames(betw_com4)[4] <- "trans_local"

betw_com4 <- left_join(betw_com4, cores)

cor.test(betw_com4$betweenness, betw_com4$trans_local) #-0.02 p = 0.4

save(betw_com4, file = "Betweenness.Com4.Rda")

diameter(com4) #2.47

mean_distance(com4) #1.58


##############################

com5 <- induced.subgraph(g, V(g)$louvain == 5)

is.connected(com5)
vcount(com5) #907
ecount(com5) #173430

max(V(com5)$core) #2893
min(V(com5)$core) #490

betw_com5 <- as.data.frame(sort(betweenness(com5, v = V(com5), directed = FALSE), decreasing = TRUE))

betw_com5$name <- rownames(betw_com5)

colnames(betw_com5)[1] <- "betweenness"

betw_com5 <- left_join(betw_com5, users_n2)

cor.test(betw_com5$betweenness, betw_com5$x) #0.19 p = 4.152e-09

com5_trans_local <- transitivity(com5, type="local")

mean(com5_trans_local) #0.81

transitivity(com5) #0.79

betw_com5 <- cbind(betw_com5, as.data.frame(com5_trans_local))

colnames(betw_com5)[4] <- "trans_local"

betw_com5 <- left_join(betw_com5, cores)

cor.test(betw_com5$betweenness, betw_com5$trans_local) #-0.1 p = 0.003

save(betw_com5, file = "Betweenness.Com5.Rda")

diameter(com5) #1.92

mean_distance(com5) #1.6


##############################

com6 <- induced.subgraph(g, V(g)$louvain == 6)

is.connected(com6)
vcount(com6) #368
ecount(com6) #59733

max(V(com6)$core) #2893
min(V(com6)$core) #583

betw_com6 <- as.data.frame(sort(betweenness(com6, v = V(com6), directed = FALSE), decreasing = TRUE))

betw_com6$name <- rownames(betw_com6)

colnames(betw_com6)[1] <- "betweenness"

betw_com6 <- left_join(betw_com6, users_n2)

cor.test(betw_com6$betweenness, betw_com6$x) #-0.02 p = 0.63

com6_trans_local <- transitivity(com6, type="local")

mean(com6_trans_local) #0.93

transitivity(com6) #0.92

betw_com6 <- cbind(betw_com6, as.data.frame(com6_trans_local))

colnames(betw_com6)[4] <- "trans_local"

betw_com6 <- left_join(betw_com6, cores)

cor.test(betw_com6$betweenness, betw_com6$trans_local) #-0.08 p = 0.09

save(betw_com6, file = "Betweenness.Com6.Rda")

diameter(com6) #1.83

mean_distance(com6) #1.11

##############################

com7 <- induced.subgraph(g, V(g)$louvain == 7)

is.connected(com7)
vcount(com7) #530
ecount(com7) #120524

max(V(com7)$core) #2893
min(V(com7)$core) #537

betw_com7 <- as.data.frame(sort(betweenness(com7, v = V(com7), directed = FALSE), decreasing = TRUE))

betw_com7$name <- rownames(betw_com7)

colnames(betw_com7)[1] <- "betweenness"

betw_com7 <- left_join(betw_com7, users_n2)

cor.test(betw_com7$betweenness, betw_com7$x) #-0.02 p = 0.5

com7_trans_local <- transitivity(com7, type="local")

mean(com7_trans_local) #0.92

transitivity(com7) #0.91

betw_com7 <- cbind(betw_com7, as.data.frame(com7_trans_local))

colnames(betw_com7)[4] <- "trans_local"

betw_com7 <- left_join(betw_com7, cores)

cor.test(betw_com7$betweenness, betw_com7$trans_local) #0.07 p = 0.09

save(betw_com7, file = "Betweenness.Com7.Rda")

diameter(com7) #1.83

mean_distance(com7) #1.14
