customer <- read.csv('/Users/hardikbhojwani/Desktop/CASE STUDIES/R case studies (1)/Customer.csv')
prod_cat_info <- read.csv('/Users/hardikbhojwani/Desktop/CASE STUDIES/R case studies (1)//prod_cat_info.csv')
transaction <- read.csv('/Users/hardikbhojwani/Desktop/CASE STUDIES/R case studies (1)/Transactions.csv')
View(customer)
View(prod_cat_info)
View(transaction)
customer$DOB=as.Date(customer$DOB,format="%d-%m-%Y")
transaction$tran_date=as.Date(transaction$tran_date,format="%d-%m-%Y")

#---------------------------------------------Q1a------------------------------------
left_join <- merge(x=customer,y=transaction,by.x = "customer_Id",by.y = "cust_id",all.x = TRUE)
Customer_Final <- merge(x=left_join,y=prod_cat_info,by.x = "prod_cat_code",by.y = "prod_cat_code",all.x = TRUE)
View(Customer_Final)
#----------------------------------------------Q1b-----------------------------------
install.packages('dplyr')
library(dplyr)
lj <- dplyr::left_join(customer,transaction,by=c("customer_Id"="cust_id"))
customer_final <- dplyr::left_join(lj,prod_cat_info,by=c("prod_cat_code"="prod_cat_code"))
#-------------------------------------------------------------------------------------
Customer_Final <- na.omit(Customer_Final)
#--------------------------------------------Q2--------------------------------------


str(Customer_Final)
head(Customer_Final,10)
tail(Customer_Final,10)
summary(Customer_Final)
Customer_Final %>% count(city_code,Gender,prod_cat,prod_subcat)
freq_table=Customer_Final %>% count(Gender)
freq_table

#mytable <- table(Customer_Final$prod_cat_code,Customer_Final$Gender, Customer_Final$prod_subcat_code)
#ftable(mytable)
#----------------------------------------------Q3-------------------------------------
hist(Customer_Final$Qty)
hist(Customer_Final$Rate)
hist(Customer_Final$Tax)
hist(Customer_Final$total_amt)
#Customer_Final[!(is.na(Customer_Final$Gender) | Customer_Final$Gender==""), ]
library(ggplot2)
plot_prod_cat <- ggplot(data=Customer_Final) +aes(x=prod_cat,fill=prod_cat)+ geom_bar()
plot_prod_cat
plot_prod_subcat <- ggplot(data=Customer_Final) +aes(x=prod_subcat,fill=prod_subcat)+ geom_bar()
plot_prod_subcat
plot_gender <- ggplot(data=Customer_Final) +aes(x=Gender,fill=Gender)+ geom_bar()
plot_gender

#----------------------------------------------Q4---------------------------------
Customer_Final
#install.packages('lubridate')
#library('lubridate')
Customer_Final$tran_date <- lubridate::dmy(Customer_Final$tran_date)
Customer_Final$tran_date
min_date=min(Customer_Final$tran_date,na.rm = T)
max_date=max(Customer_Final$tran_date,na.rm = T)
difftime(max_date, min_date, units = "days")
#----------------------------------------------Q4b---------------------------------
negative_trans <- Customer_Final$total_amt[Customer_Final$total_amt<0]
length(negative_trans)

#----------------------------------------------Q5---------------------------------
more_popular=Customer_Final %>% count(Gender,prod_cat)
more_popular
#-------------------------------------------Q6-----------------------
tail(names(sort(table(customer$city_code))), 1)
t <- sort(table(customer$city_code))
which.max(round(100*prop.table(t),digits=0))
tail(names(sort(table(Customer_Final$Store_type))), 1)
#--------------------------------------------Q7----------------------
newdata <- Customer_Final[order(Store_type,Qty)]
library('dplyr')
Datalong <- transaction %>% dplyr::group_by(Store_type) %>% dplyr::summarise(SumofQty=sum(Qty),Sumofamount=sum(total_amt))
str(Datalong)
#---------------------------------------------Q8--------------------------
Datalong2 <- Customer_Final %>% dplyr::group_by(prod_cat,Store_type) %>% dplyr::summarise(Sumofamount=sum(total_amt))
res <- Datalong2[Datalong2$Store_type=='Flagship store'& Datalong2$prod_cat=='Electronics',] 
#---------------------------------------------Q9--------------------------
Datalong3 <- Customer_Final %>% dplyr::group_by(prod_cat,Gender) %>% dplyr::summarise(Sumofamount=sum(total_amt))
res2 <- Datalong3[Datalong3$Gender=='M' & Datalong3$prod_cat=='Electronics',]
re <- res2$Sumofamount
#---------------------------------------------Q10---------------------------------
install.packages('sqldf')
library('sqldf')
res3 <- sqldf("SELECT cust_id FROM transaction WHERE total_amt>0 AND COUNT(transaction_id)>10")
res4 <- subset(transaction,subset = total_amt>0 & length(transaction_id>10), select = c('cust_id'))
install.packages('eeptools')
install.packages('eeptools')
library('eeptools')
today_date <- format(Sys.Date(), format="%d-%m-%Y")
today_date <- as.Date(today_date, format="%d-%m-%Y")
to<- lubridate::dmy(today_date)
year(to)
str(todays)
Customer_Final$age <- age_calc(customer$DOB, enddate = today_date, units = "months", precise = TRUE)
install.packages("installr"); library(installr) # install+load installr
library(lubridate)
Customer_Final$age <- year(Customer_Final$DOB)


updateR() # updating R.




