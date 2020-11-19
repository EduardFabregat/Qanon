louvain <- as.data.frame((V(g)$louvain), (V(g)$name))

colnames(louvain)[1] <- "louvain"

louvain$name <- rownames(louvain)

comms <- meta_theta_df2[which(meta_theta_df2$Author %in% users_n2$Group.1), ]

colnames(comms)[5] <- "name"

comms <- left_join(comms, louvain)

comms_ <- comms[, c(22,4,5,24,66)]

dict <- data.frame(search=c("Epstein"))

result <- comms_ %>%
  filter(grepl(paste(dict$search, collapse = "|"), Contents))
