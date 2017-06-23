numCols <- sapply(LessCorr, is.numeric) #finding numeric columns

numColsData <- LessCorr[, numCols] #select only numeric columns

numColsData.lng <- melt(numColsData, id="isfraud") #use your  bad indicator

#Commonly used ggplots

#density plot for 1/0 separately.
plotObj <- ggplot(aes(x=value, group=isfraud, color=factor(isfraud)), data=numColsData.lng) + geom_density() + facet_wrap(~ variable, scales="free")

print(plotObj)

names(LessCorr)



ggplot(aes(x=customer_name_num_distinct_billing_country_per_customer_name_1day, group=isfraud, color=factor(isfraud)), data=LessCorr) + geom_density()
table(LessCorr$customer_name_num_distinct_billing_country_per_customer_name_1day,LessCorr$isfraud)



library(gmodels)

CrossTable(LessCorr$customer_name_num_distinct_billing_country_per_customer_name_1day,LessCorr$isfraud, prop.t = FALSE, prop.chisq = FALSE)
plot(ecdf(LessCorr$customer_name_num_distinct_billing_country_per_customer_name_1day),verticals = TRUE, do.points = FALSE)

plot(ecdf(LessCorr[isfraud == 0, "customer_name_num_distinct_billing_country_per_customer_name_1day"])
     #  Distribution plots for density plot (gamma, beta, normal, etc.)







#Commonly used ggplots

#density plot for 1/0 separately.
ggplot(aes(x=LessCorr$domain_num_distinct_deviceid_per_email_domain_1day, 
           group=isfraud, 
           color=factor(isfraud)), 
       data=LessCorr) + 
  geom_density() + 
  facet_wrap(~ isfraud, scales="free")


isfraud <- as.factor(LessCorr$isfraud)


#density plot of all the variables
LessCorr %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(x = value, color = factor(isfraud))) +                 # Plot the values
  facet_wrap(~ key, scales = "free") +     # In separate panels
  geom_density()                           # as density


#histogram of all variables
LessCorr %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(x = value)) +  # Plot the values
  facet_wrap(~ key, scales = "free") +     # In separate panels
  geom_histogram(bins = 10, fill = "880011")   



#density plot of one variable w.r.t to isfraud
ggplot(data=LessCorr, aes(x=email_num_distinct_billing_country_per_email_30day)) + 
  geom_density() + ggtitle("Histogram of last_sign_in_ip_num_distinct_ssndigest_per_currentsigninip_7day\nby isfraud") +
  labs(x="email_num_distinct_billing_country_per_email_30day", y="Count\nof Records") +
  facet_wrap(~ isfraud) 


##__________________________NOT TRIED_______________________________________________________
#historgram of one variable wrt occupation
ggplot(data=data_final_h2o_70p, aes(x=full_address_num_distinct_user_per_address)) + 
  geom_histogram(fill="#880011", bins = 50) + ggtitle("Histogram of full_address_num_distinct_user_per_address\nby is_bad") +
  labs(x="full_address_num_distinct_user_per_address", y="Count\nof Records") +
  facet_wrap(~ is_bad) 


#scatter plot using ggplot
ggplot(data_final_h2o_70p, aes(num_total_declines_per_user, failed_attempts)) + geom_point() + scale_x_continuous("total declines", breaks = seq(0,2065,100))+ scale_y_continuous("failed attempts", breaks = seq(0,11,by = 1))+ theme_bw() 

#scatter plot using ggplot with third variable as color
ggplot(data_final_h2o_70p, aes(num_total_declines_per_user, failed_attempts)) + geom_point(aes(color = is_bad)) + scale_x_continuous("total declines", breaks = seq(0,2065,100))+ scale_y_continuous("failed attempts", breaks = seq(0,11,by = 1))+ theme_bw() 

#many scatter plot using ggplot with third variable as color - Occupation
ggplot(data_final_h2o_70p, aes(num_total_declines_per_user, failed_attempts)) + geom_point(aes(color = is_bad)) + scale_x_continuous("total declines", breaks = seq(0,2065,100))+ scale_y_continuous("failed attempts", breaks = seq(0,11,by = 1))+ theme_bw() + facet_wrap( ~ occupation)