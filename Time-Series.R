####################################################################################################################################
####################################################################################################################################
############################### BIG DATA TIME SERIES FORECASTING ###################################################################
####################################################################################################################################
####################################################################################################################################


setwd("C:/Rworkspace/Project")

# BIG DATA 
# > 6 crore of data in each training and test data

##########################################################################################################
########### ff and ffbase packages help storing BIG DATA in chunks to external memory instead of RAM #####
##########################################################################################################

library("ff")

trainingData<- read.csv.ffdf(file="train.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA,nrows=60000000)

tail(trainingData)
str(trainingData)
class(trainingData)  # ffdf  or VIRTUAL DATA FRAME


# step 1 : Impute the missing data in 'onpromotion' column in training set
#--------------------------------------------------------------------------------------------------------

onPromo_ffdf <- ff(trainingData[,6])
class(onPromo_ffdf)
library(ffbase)
onPromo_ffdf_new <- is.na(onPromo_ffdf) ## ffbase pckg, returns TRUE for values which are NA, FALSE for values other than NA

onPromo_ffdf_new <- ffwhich(onPromo_ffdf_new, onPromo_ffdf_new == TRUE) #ffbase pckg, returns indexes of the vector which have TRUE ( meaning NA as the value)

onPromo_ffdf[onPromo_ffdf_new][]    # gives all values which are NA in that column
onPromo_ffdf[onPromo_ffdf_new][]<-0  # replacing all NA with FALSE ( 0- FALSE, 1 - TRUE)

onPromo_ffdf_new <- ffdf(onPromo_ffdf)
class(onPromo_ffdf_new)

trainingData[,6]<-onPromo_ffdf_new  # Merged / replaced the trainingData's onPromotion column with the new column now
                                    # 'onPromo_ffdf_new' which had all 'NAs' replaced with 'FALSE'
head(trainingData)

nrow(trainingData)  # 6,00,00,000



# step 2: Merge/LEFT JOIN train.csv and stores.csv
#--------------------------------------------------------------------------------------------------------

storesData<- read.csv.ffdf(file="stores.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)
max(storesData$cluster) # 17 

# NOTE  LEFT JOIN Training data and the Stores Data

set1 <- merge(trainingData[,], storesData[,c(1,5)],by="store_nbr")
head(set1)
tail(set1)
unique(set1$cluster)


library(data.table)
class(set1$date)
set1$date<-sort(set1$date)   # sorting the date field
head(set1)
tail(set1)

# step 3 : Merge/LEFT JOIN 'set1' and 'items.csv'
#--------------------------------------------------------------------------------------------------------------

itemsData<- read.csv.ffdf(file="items.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)

set2 <- merge(set1[,], itemsData[,],by="item_nbr")

head(set2)
tail(set2)
set2$date<-sort(set2$date) 
head(set2)
tail(set2)

# step 4 : Subsetting the data from set2 based on the clusters or store type
#-----------------------------------------------------------------------------------------------
  
mycluster1 <- subset(set2, cluster==1)  # STORE TYPE 1
class(mycluster1)  # data frame
head(mycluster1)
tail(mycluster1)
nrow(mycluster1)  # 3723150

countOfClusters <- length(unique(set2$cluster))  # 17

mycluster2 <- subset(set2, cluster==2)  # STORE TYPE 2
mycluster3 <- subset(set2, cluster==3)  # STORE TYPE 3
mycluster4 <- subset(set2, cluster==4)  # STORE TYPE 4
mycluster5 <- subset(set2, cluster==5)  # STORE TYPE 5
mycluster6 <- subset(set2, cluster==6)  # STORE TYPE 6
mycluster7 <- subset(set2, cluster==7)  # STORE TYPE 7
mycluster8 <- subset(set2, cluster==8)  # STORE TYPE 8
mycluster9 <- subset(set2, cluster==9)  # STORE TYPE 9
mycluster10 <- subset(set2, cluster==10)  # STORE TYPE 10
mycluster11<- subset(set2, cluster==11)  # STORE TYPE 11
mycluster12<- subset(set2, cluster==12)  # STORE TYPE 12
mycluster13<- subset(set2, cluster==13)  # STORE TYPE 13
mycluster14<- subset(set2, cluster==14)  # STORE TYPE 14
mycluster15<- subset(set2, cluster==15)  # STORE TYPE 15
mycluster16<- subset(set2, cluster==16)  # STORE TYPE 16
mycluster17<- subset(set2, cluster==17)  # STORE TYPE 17



# step 5: Forecasting sales / performance of different Stores or Clusters
# -----------------------------------------------------------------------------------------------------------------------------

require(forcast)

################### STORE 1 ######################

head(mycluster1)
#demand.ts <- ts(mycluster1[,c(4,5)],frequency=365,start=c(2013,1))
#plot(demand.ts,main="Daily sales from 2013-2015 for Store 1")

# forecasting
require(forecast)
require(ggplot2)

uniqueItems <- unique(mycluster1[,8])  # Unique classes/ categories of items

demand.cluster1  <-aggregate(mycluster1[,5], by=list(mycluster1[,4]), FUN = sum)
demand.cluster1.ts1 <- ts(demand.cluster1,frequency=365, start=c(2013,1))
plot(demand.cluster1.ts1,main="Daily sales from 2013-2015 for Store 1",ylab="Demand")


demand.cluster1.forecast1 <- forecast(demand.cluster1.ts1)
plot(demand.cluster1.forecast1,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 1")
demand.cluster1.forecast1
demand.cluster1.forecast1$method


######### STORE 2

demand.cluster2 <- aggregate(mycluster2[,5], by=list(mycluster2[,4]), FUN = sum)
demand.cluster2.ts2 <- ts(demand.cluster2,frequency=365,start=c(2013,1))

demand.cluster2.forecast2 <- forecast(demand.cluster2.ts2)
plot(demand.cluster2.forecast2,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 2")


######### STORE 3

demand.cluster3 <- aggregate(mycluster3[,5], by=list(mycluster3[,4]), FUN = sum)
demand.cluster3.ts3 <- ts(demand.cluster3,frequency=365,start=c(2013,1))

demand.cluster3.forecast3 <- forecast(demand.cluster3.ts3)
plot(demand.cluster3.forecast3,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 3")

########### STORE 4

demand.cluster4  <-aggregate(mycluster4[,5], by=list(mycluster4[,4]), FUN = sum)
demand.cluster4.ts4 <- ts(demand.cluster4,frequency=365, start=c(2013,1))

demand.cluster4.forecast4 <- forecast(demand.cluster4.ts4)
plot(demand.cluster4.forecast4,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 4")
demand.cluster4.forecast4
demand.cluster4.forecast4$method


########### STORE 5

demand.cluster5  <-aggregate(mycluster5[,5], by=list(mycluster5[,4]), FUN = sum)
demand.cluster5.ts5 <- ts(demand.cluster5,frequency=365, start=c(2013,1))

demand.cluster5.forecast5 <- forecast(demand.cluster5.ts5)
plot(demand.cluster5.forecast5,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 5")
demand.cluster5.forecast5
demand.cluster5.forecast5$method

########### STORE 6

demand.cluster6  <-aggregate(mycluster6[,5], by=list(mycluster6[,4]), FUN = sum)
demand.cluster6.ts6 <- ts(demand.cluster6,frequency=365, start=c(2013,1))

demand.cluster6.forecast6 <- forecast(demand.cluster6.ts6)
plot(demand.cluster6.forecast6,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 6")
demand.cluster6.forecast6
demand.cluster6.forecast6$method


########### STORE 7

demand.cluster7 <-aggregate(mycluster7[,5], by=list(mycluster7[,4]), FUN = sum)
demand.cluster7.ts7 <- ts(demand.cluster7,frequency=365, start=c(2013,1))


demand.cluster7.forecast7 <- forecast(demand.cluster7.ts7)
plot(demand.cluster7.forecast7,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 7")
demand.cluster7.forecast7
demand.cluster7.forecast7$method

########### STORE 8

demand.cluster8  <-aggregate(mycluster8[,5], by=list(mycluster8[,4]), FUN = sum)
demand.cluster8.ts8 <- ts(demand.cluster8,frequency=365, start=c(2013,1))

demand.cluster8.forecast8 <- forecast(demand.cluster8.ts8)
plot(demand.cluster8.forecast8,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 8")
demand.cluster8.forecast8
demand.cluster8.forecast8$method

########### STORE 9

demand.cluster9  <-aggregate(mycluster9[,5], by=list(mycluster9[,4]), FUN = sum)
demand.cluster9.ts9 <- ts(demand.cluster9,frequency=365, start=c(2013,1))

demand.cluster9.forecast9 <- forecast(demand.cluster9.ts9)
plot(demand.cluster9.forecast9,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 9")
demand.cluster9.forecast9
demand.cluster9.forecast9$method

########### STORE 10

demand.cluster10  <-aggregate(mycluster10[,5], by=list(mycluster10[,4]), FUN = sum)
demand.cluster10.ts10 <- ts(demand.cluster10,frequency=365, start=c(2013,1))

demand.cluster10.forecast10 <- forecast(demand.cluster10.ts10)
plot(demand.cluster10.forecast10,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 10")
demand.cluster10.forecast10
demand.cluster10.forecast10$method

########### STORE 11

demand.cluster11  <-aggregate(mycluster11[,5], by=list(mycluster11[,4]), FUN = sum)
demand.cluster11.ts11 <- ts(demand.cluster11,frequency=365, start=c(2013,1))

demand.cluster11.forecast11 <- forecast(demand.cluster11.ts11)
plot(demand.cluster11.forecast11,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 11")
demand.cluster11.forecast11
demand.cluster11.forecast11$method

########### STORE 12

demand.cluster12  <-aggregate(mycluster12[,5], by=list(mycluster12[,4]), FUN = sum)
demand.cluster12.ts12 <- ts(demand.cluster12,frequency=365, start=c(2013,1))

demand.cluster12.forecast12 <- forecast(demand.cluster12.ts12)
plot(demand.cluster12.forecast12,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 12")


########### STORE 13

demand.cluster13  <-aggregate(mycluster13[,5], by=list(mycluster13[,4]), FUN = sum)
demand.cluster13.ts13 <- ts(demand.cluster13,frequency=365, start=c(2013,1))

demand.cluster13.forecast13 <- forecast(demand.cluster13.ts13)
plot(demand.cluster13.forecast13,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 13")


########### STORE 14

demand.cluster14  <-aggregate(mycluster14[,5], by=list(mycluster14[,4]), FUN = sum)
demand.cluster14.ts14 <- ts(demand.cluster14,frequency=365, start=c(2013,1))

demand.cluster14.forecast14 <- forecast(demand.cluster14.ts14)
plot(demand.cluster14.forecast14,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 14")


########### STORE 15

demand.cluster15  <-aggregate(mycluster15[,5], by=list(mycluster15[,4]), FUN = sum)
demand.cluster15.ts15 <- ts(demand.cluster15,frequency=365, start=c(2013,1))

demand.cluster15.forecast15 <- forecast(demand.cluster15.ts15)
plot(demand.cluster15.forecast15,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 15")


########### STORE 16

demand.cluster16  <-aggregate(mycluster16[,5], by=list(mycluster16[,4]), FUN = sum)
demand.cluster16.ts16 <- ts(demand.cluster16,frequency=365, start=c(2013,1))

demand.cluster16.forecast16 <- forecast(demand.cluster16.ts16)
plot(demand.cluster16.forecast16,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 16")


########### STORE 17

demand.cluster17 <-aggregate(mycluster17[,5], by=list(mycluster17[,4]), FUN = sum)
demand.cluster17.ts17 <- ts(demand.cluster17,frequency=365, start=c(2013,1))

demand.cluster17.forecast17 <- forecast(demand.cluster17.ts17)
plot(demand.cluster17.forecast17,shaded = TRUE,fcol="red",main="Forecast from 2015-2018 for Store 17")


# step 6: Forecasting sales of different items ( family) within the Clusters / Store Types
# -----------------------------------------------------------------------------------------------------------------------------


#### Store 1  #######
#####################

require(dplyr)

install.packages("tidyquant", dependencies = TRUE)
install.packages("timetk", dependencies = TRUE)
install.packages("sweep", dependencies = TRUE)

require(tidyquant)
require(timetk)
require(sweep)

monthly_by_family_1 <- mycluster1 %>% mutate(order.month = as_date(as.yearmon(date))) %>% group_by(family,order.month) %>% summarise(Qty = sum(unit_sales))

# use nest () function from tidyr package to consolidate each time series by group
monthly_by_family_1f <- monthly_by_family_1 %>% group_by(family) %>% nest(.key="data.tbl")


# COERCE TO A TS OBJECT
monthly_by_family_1f_ts <- monthly_by_family_1f %>% mutate(data.ts = map(.x=data.tbl,.f=tk_ts,select=-order.month,start=2013,freq=12))


# MODELLING THE TIME SERIES
require(forecast)
monthly_by_family_1f_model <- monthly_by_family_1f_ts %>% mutate(fit.ets = map(data.ts,ets))


# FORECASTING THE MODEL

monthly_by_family_1f_fcast <- monthly_by_family_1f_model %>% mutate(fcast.ets = map(fit.ets,forecast))

# A tibble: 31 x 5
#family          data.tbl  data.ts   fit.ets      fcast.ets
#<fctr>            <list>   <list>    <list>         <list>
#  1   AUTOMOTIVE  <tibble [8 x 2]> <S3: ts> <S3: ets> <S3: forecast>
#  2       BEAUTY  <tibble [9 x 2]> <S3: ts> <S3: ets> <S3: forecast>
#  3    BEVERAGES <tibble [31 x 2]> <S3: ts> <S3: ets> <S3: forecast>
#  4 BREAD/BAKERY <tibble [30 x 2]> <S3: ts> <S3: ets> <S3: forecast>
#  5  CELEBRATION  <tibble [1 x 2]> <S3: ts> <S3: ets> <S3: forecast>
#  6     CLEANING <tibble [34 x 2]> <S3: ts> <S3: ets> <S3: forecast>
#  7        DAIRY <tibble [33 x 2]> <S3: ts> <S3: ets> <S3: forecast>
#  8         DELI <tibble [24 x 2]> <S3: ts> <S3: ets> <S3: forecast>
#  9         EGGS <tibble [16 x 2]> <S3: ts> <S3: ets> <S3: forecast>
#  10 FROZEN FOODS <tibble [20 x 2]> <S3: ts> <S3: ets> <S3: forecast>
  # ... with 21 more rows


# TIDY THE FORECAST

monthly_by_family_1f_tidy <- monthly_by_family_1f_fcast %>% mutate(sweep=map(fcast.ets,sw_sweep)) %>% unnest(sweep)
monthly_by_family_1f_tidy %>% ggplot(aes(x=index,y=Qty,color=key,group=family)) + geom_ribbon(aes(ymin=lo.95,ymax=hi.95),fill="#D5DBFF",color=NA,size=0) + geom_ribbon(aes(ymin=lo.80,ymax=hi.80,fill=key),fill="#596DD5",color=NA,size=0,alpha=0.8)+geom_line()+labs(title="Grocery Sales",x="",y="Units")+facet_wrap(~family,scales="free_y",ncol=4)




#### Store 2  #######
#####################

monthly_by_family_2 <- mycluster2 %>% mutate(order.month = as_date(as.yearmon(date))) %>% group_by(family,order.month) %>% summarise(Qty = sum(unit_sales))

# use nest () function from tidyr package to consolidate each time series by group
monthly_by_family_2f <- monthly_by_family_2 %>% group_by(family) %>% nest(.key="data.tbl")


# COERCE TO A TS OBJECT
monthly_by_family_2f_ts <- monthly_by_family_2f %>% mutate(data.ts = map(.x=data.tbl,.f=tk_ts,select=-order.month,start=2013,freq=12))


# MODELLING THE TIME SERIES
monthly_by_family_2f_model <- monthly_by_family_2f_ts %>% mutate(fit.ets = map(data.ts,ets))


# FORECASTING THE MODEL

monthly_by_family_2f_fcast <- monthly_by_family_2f_model %>% mutate(fcast.ets = map(fit.ets,forecast))

# TIDY THE FORECAST

monthly_by_family_2f_tidy <- monthly_by_family_2f_fcast %>% mutate(sweep=map(fcast.ets,sw_sweep)) %>% unnest(sweep)
monthly_by_family_2f_tidy %>% ggplot(aes(x=index,y=Qty,color=key,group=family)) + geom_ribbon(aes(ymin=lo.95,ymax=hi.95),fill="#D5DBFF",color=NA,size=0) + geom_ribbon(aes(ymin=lo.80,ymax=hi.80,fill=key),fill="#596DD5",color=NA,size=0,alpha=0.8)+geom_line()+labs(title="Grocery Sales",x="",y="Units")+facet_wrap(~family,scales="free_y",ncol=4)














