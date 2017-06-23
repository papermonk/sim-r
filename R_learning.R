install.packages("lubridate")
install.packages("data.table")
install.packages("gmodels")
install.packages("plyr")
install.packages("caret")
install.packages("Information")
install.packages("smbinning")
install.packages("purrr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("DMwR")
install.packages("FFTrees")

library(FFTrees)
library(lubridate)
library(data.table)
library(gmodels)
library(plyr)
library(caret)
library(Information)
library(smbinning)
library(purrr)
library(tidyr)
library(ggplot2)
library(readr)
library(DMwR)

options(scipen = 99)

############################################################

#===============JOIN FILES SYNTAX ==========================
data <- merge(deDupAnalysis,eidncount,by="eid",all.x=T,sort = F)


#_____________________________________________________________________________________

#### FUNCTIONS  ######

#finds missing percentage of the coloumns in a df and returns a table miss
findMissingPerc <- function(df){
  miss <- as.data.frame(sapply(df, FUN = function(x){sum(ifelse(is.na(x),1,0))}))
  miss$var <- names(df)
  row.names(miss) <- NULL
  colnames(miss)[1] <- "ct_miss"
  miss$pr_miss <- (miss$ct_miss*100)/nrow(df)
  return(miss)
}


#not in version of %in% in dplyr
'%not in%' <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

#____________________________________________________________________________________


data <- fread("./MLData_Final.csv",data.table = FALSE, na.strings = "")
data$isfraud <- ifelse((data$kount_decision == "D" | data$actual_chargeback == "C"),1,0)
data$isfraud <- ifelse(is.na(data$isfraud),0,data$isfraud)
table(data$isfraud) 


# table(data$kount_decision, exclude = NULL)
#a <- grep(pattern = "kount",names(data),value = TRUE)
#  prop.table(table(data$kount_decision,exclude = NA))
# CrossTable(data$actual_charge,data$kount_decision,prop.t = FALSE, prop.chisq = FALSE)
#______________________________________________________________________________________


finalmiss <- findMissingPerc(rawData)
cols_to_keep <- subset(finalmiss, pr_miss < 10, select = "var")
prunedData <- subset(data, select = cols_to_keep[,1])

#  View(data_clean)
#  final_data <- findMissingPerc(data_clean)
#  table(prunedData$actual_chargeback,exclude = NULL)
#  unique(data_clean$actual_chargeback)

## Columns to be deleted ? "Reduce"

ews_col <- grep("ip_ews_",names(data), value = TRUE)
sim_col <- grep("sim_",names(data),value = TRUE)
ts_col <- grep("ts_",names(data),value = TRUE)
eid_col <- grep("eid", names(data), value = TRUE)
feedback <- grep("bad", names(data), value = TRUE)
prunedData <- prunedData[,names(prunedData) %not in% sim_col]
prunedData <- prunedData[,names(prunedData) %not in% ews_col]
prunedData <- prunedData[,names(prunedData) %not in% ts_col]
prunedData <- prunedData[,names(prunedData) %not in% eid_col]
prunedData <- prunedData[,names(prunedData) %not in% feedback]


#### NULLs Replacement
#### Mean, mode, median, zero or something else
#### H20 takes zero for null numerical values
#### Replace nulls via zero for num_distinct_
#### Replace nulls in base entity with String "Empty" and 
#### Get the behavioral signals on child entity eid = 'Empty' for text/categorical entity

table(is.na(data_clean$billing_country_variability_customer_name_to_billing_country_1day),is.na(data_clean$billing_country),exclude = NULL)
table(is.na(data_clean$billing_country_variability_sponsor_id_to_billing_country_7day))

table(data_clean$billing_country, exclude = NULL)

#_______________________________________________________________________________________

num_cols <- sapply(prunedData,is.numeric)
num_cols_data <- prunedData[,num_cols]


char_cols <- sapply(prunedData, is.character)
char_cols_data <- prunedData[,char_cols]


#numDistCols <- grep("num_distinct_", colnames(num_cols_data))
#NewDataset <- num_cols_data[,numDistCols]

NewDataset <- num_cols_data

#Pearson & Spearman,
#Cramers V
#____________________________________________________________________________

# Correlations

NewDataset[is.na(NewDataset)] <- 0   # clear all nulls to zero
plot(table(NewDataset[,1], exclude = NULL))

# Correlation Matrix


zv <- apply(NewDataset, 2, function(x) length(unique(x)) == 1)
sum(zv)
NewDataset <- NewDataset[, !zv]

core_mtx <- cor(NewDataset)


high_correlation_vars <- findCorrelation(core_mtx, cutoff = 0.8)
high_correlation_vars
high_correlation_vars_2 <- findCorrelation(core_mtx, cutoff = 0.9, names = TRUE)
high_correlation_vars_3 <- findCorrelation(core_mtx, cutoff = 0.95)
high_correlation_vars_4 <- findCorrelation(core_mtx, cutoff = 0.98)


LessCorr <- NewDataset[,colnames(NewDataset) %not in% high_correlation_vars_2]

# Plots ?

#___________________________________________________________________________________


summary(LessCorr)
LessCorr$isfraud <- data$isfraud
#___________________________________________________________________________________


total <- cbind(LessCorr,char_cols_data)


#___________________________________________________________________________________
### CREATE INFOTABLES



ivTables <- create_infotables(data=total, y="isfraud", bins=10,parallel = TRUE)
ivtDf <- as.data.frame(ivTables$Summary)

View(ivtDf)

ivTables$Tables

#____________________________________________________________________________________

#### SM BINNING OR NOT ip_num_distinct_email_domain_per_ip_7day

result <- smbinning(df=total,y="isfraud", x="ip_num_distinct_email_domain_per_ip_7day", p=0.01) 
result$ivtable

result <- smbinning.custom(total,"isfraud","payment_id_num_distinct_ip_per_payment_id_1day",cuts = c(0,1,2,3,4,5,6,7,8,9,10,11,12))

#__________________________________________________________________________________________

set.seed(1029290)

infogreat <- c('total_amount',
               'ip_num_distinct_shipping_country_per_ip_1day',
               'ip_num_distinct_email_domain_per_ip_1day',
               'ip_num_distinct_customer_name_per_ip_1day',
               'ip_amt_total_invoice_value_per_ip_7day',
               'ip_amt_avg_invoice_value_per_ip_lifetime_count',
               'email_num_distinct_shipping_country_per_email_30day',
               'email_num_distinct_payment_id_per_email_30day',
               'email_num_distinct_payment_id_per_email_1day',
               'email_num_distinct_ip_per_email_1day',
               'email_num_distinct_ip_per_email_7day',
               'email_num_distinct_invoice_per_email_7day',
               'email_num_distinct_invoice_per_email_30day',
               'email_num_distinct_invoice_per_email_1day',
               'email_num_distinct_device_fprint_per_email_7day',
               'email_num_distinct_device_fprint_per_email_30day',
               'email_num_distinct_device_fprint_per_email_1day',
               'email_num_distinct_customer_name_per_email_7day',
               'email_num_distinct_customer_name_per_email_1day',
               'email_amt_total_invoice_value_per_email_lifetime',
               'email_amt_total_invoice_value_per_email_1day',
               'isfraud')


infototal <- total[,infogreat]

oldnames <- colnames(infototal)
newnames <- paste("var",1:length(oldnames),sep="")

names(infototal) <- newnames


sample <- sample(1:10,size = 196841,replace = TRUE)
infototal_train <- infototal[sample>2,]
infototal_test <- infototal[sample<3,]

write.csv(cbind(oldnames,newnames),"./VarNames.csv", row.names = FALSE)

ft <- FFTrees(var22 ~ ., data = infototal_train,data.test = infototal_test, verbose = T )

plot(ft)
plot(ft, tree = 4)

cor(infototal$email_num_distinct_payment_id_per_email_1day,infototal$email_num_distinct_payment_id_per_email_30day)

print(ft, tree = 4)

write.csv(infototal,"./Data_for_ML.csv", row.names = FALSE)
