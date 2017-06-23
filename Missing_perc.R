#finds missing percentage of the coloumns in a df and returns a table miss
findMissingPerc <- function(df){
  miss <- as.data.frame(sapply(df, FUN = function(x){sum(ifelse(is.na(x),1,0))}))
  miss$var <- row.names(miss)
  row.names(miss) <- NULL
  colnames(miss)[1] <- "ct_miss"
  miss$pr_miss <- (miss$ct_miss*100)/nrow(df)
  return(miss)
}