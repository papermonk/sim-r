df <- prediction_grid_kyani_1_model_8
df$is_fraud <- df$isfraud

library(lift)
#giving the data with the scores and the actual labels
lift <- df[,colnames(df) %in% c("p1","is_fraud")]
lift <- as.data.frame(lift)
num_decile <- 100
#########################################################
#sort decreasing by score
lift <- lift[order(-lift[, 1]), ]
#define deciles(equal to the number of rows)
decile <- ceiling(seq_along(lift[,2])/floor(length(lift[,2])/num_decile))
length(unique(decile))
#total scored
cap <- floor(length(lift[, 2])/num_decile) * num_decile
#total actuals caughts
out <- aggregate(lift[1:cap, 2], by = list(decile[1:cap]), sum)#total captured
colnames(out) <- c("ntile","actuals")
out$total <- floor(length(lift[, 2])/num_decile) # decile size
out$decile_response_rate <- out$actuals/out$total #precision
overall_response_rate <- sum(out$actuals)/cap
out$cumulative_response_rate <- cumsum(out$decile_response_rate)/seq_along(out$decile_response_rate) #cumsum
out$cumulative_lift <- out$cumulative_response_rate/overall_response_rate
out$min_score <- aggregate(lift[1:cap, 1], by = list(decile[1:cap]), min)[2]
out$max_score <- aggregate(lift[1:cap, 1], by = list(decile[1:cap]), max)[2]
out$average_score <- aggregate(lift[1:cap, 1], by = list(decile[1:cap]), mean)[2]
out$recall <- cumsum(out$actuals)/sum(lift$is_fraud)

View(out)


out1000 <- computeTilesData(1000,lift)
View(out1000)
out100 <- computeTilesData(100, lift)
View(out100)
out10 <- computeTilesData(10,lift)
View(out10)
out50 <- computeTilesData(50,lift)
View(out50)
