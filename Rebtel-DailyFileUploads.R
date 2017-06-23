#============================================#
################   INPUT #####################
#============================================#

        todaydate = "2017-06-22"

#============================================#
#============================================#


setwd("C:/Users/Hrishikesh/Documents/Work/Rebtel/Email Files/")
library(readr)

#================== BARRED TRANSACTION SPLIT =======================#

barred_users <- read_csv(file = paste("./",get("todaydate"),"/Simility_Barred yesterday with orders_Daily.csv",sep = ""))
barred_users <- barred_users[-1:-2,]
barred_transactions <- barred_users[,2]
names(barred_transactions) <- "eid"
barred_transactions$decision <- "FRAUD"
barred_users[,3] <- NULL
barred_users[,2] <- "Barred"
names(barred_users) <- c("eid","decision")

#=========================  UNBARRED =======================#

unbarred_users <- read_csv(file = paste("./",get("todaydate"),"/Simility_UnBarred yesterday_Daily.csv",sep = ""))
unbarred_users <- unbarred_users[-1:-2,]
unbarred_users[,1]<-NULL
names(unbarred_users) <- c("eid","decision")

#==================  DAILY TRANSACTION SPLIT ===============#

daily_decision <- read_csv(file = paste("./",get("todaydate"),"/Simility_Decisions_Daily.csv",sep = ""))
daily_decision$decision <- "NOT_FRAUD"
daily_decision$decision[daily_decision$Transaction_Status == "Chargeback" | daily_decision$User_Status == "Barred"] <- "FRAUD"
daily_user <- daily_decision[,c(1,5)]
daily_transaction <- daily_decision[,c(3,6)]
names(daily_transaction) <- c("eid","decision")
names(daily_user) <- c("eid","decision")

#======================== FINAL FILES ======================#

user_decision <- rbind(barred_users,daily_user,unbarred_users)
order_decision <- rbind(barred_transactions,daily_transaction)

write.csv(user_decision,paste("./",get("todaydate"),"/Decision_User_",get("todaydate"),".csv", sep = ""), row.names = FALSE)
write.csv(order_decision,paste("./",get("todaydate"),"/Decision_Order_",get("todaydate"),".csv", sep = ""), row.names = FALSE)

