#install.packages("arules")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("readxl")

library(arules)
library(plyr)
library(dplyr)
library(readxl)

getwd()

data<-read_excel("Online Retail Corrected.xlsx")

View(data)

str(data)
data$InvoiceDate<-as.Date(data$InvoiceDate)
Transaction_Data = ddply(data,c("InvoiceNo","InvoiceDate"),
                         function(df)paste(df$Description,collapse = ","))

View(head(Transaction_Data))
Transaction_Data$InvoiceNo<-NULL
Transaction_Data$InvoiceDate<-NULL
colnames(Transaction_Data)=c("items")
write.csv(Transaction_Data,"Basket Informations.csv",quote = FALSE, row.names = FALSE)
Transaction = read.transactions("Basket Informations.csv", format = 'basket', sep = ',')

#Transaction = read.transactions("Transactions_New.csv", format = 'basket', sep = ',')
#Transaction_1 = read.transactions("Market_Basket_Optimisation.csv", format = 'basket', sep = ',')
summary(Transaction)

rules<-apriori(Transaction,parameter = list(support = 0.004, conf = 0.8), appearance = list(lhs = 'COFFEE'))
?apriori
summary(rules)
inspect(rules)

#1. Execute market basket with supp=0.01, conf=0.8 and answer below question
#    a) What is the support for {BACK DOOR}  => {KEY FOB}  
rules<-apriori(Transaction,parameter = list(support = 0.01, conf = 0.8), appearance = list(lhs = 'BACK DOOR', rhs = 'KEY FOB'))
inspect(rules)

#    b) What is the confidence for  {COFFEE} => {SUGAR}
rules<-apriori(Transaction,parameter = list(support = 0.01, conf = 0.8), appearance = list(lhs = 'COFFEE', rhs = 'SUGAR'))
inspect(rules)

#2. supp=0.001, conf=0.8 what are the items with which metal are purchased
rules<-apriori(Transaction,parameter = list(support = 0.001, conf = 0.8), appearance = list(rhs = 'METAL'))
inspect(rules)

#3. supp=0.004, conf=0.8 what are the items that are purchased with coffee
rules<-apriori(Transaction,parameter = list(support = 0.004, conf = 0.8), appearance = list(lhs = 'COFFEE'))
inspect(rules)