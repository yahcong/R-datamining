setwd("F:/R/CCF/超市")

library(data.table)
library(dplyr)
#library(xgboost)
library(sampling)
library(ggplot2)
library(zoo)
library(forecast)

#标准误差
rmse = function(pred,test){ 
  res<- sqrt(mean((pred-test)^2) )
  res
}

ID_pred = function(id,dt){
  dt = dt[dt$id==id,]
  dt = merge(date,dt,by = "sale_date",all.x = T)
  dt$id[is.na(dt$id)] = id
  dt$count[is.na(dt$count)] = ifelse(is.na(mean(dt$count,na.rm = T)),0,mean(dt$count,na.rm = T))
  dt_series = zoo(x = dt[,3],order.by = dt$sale_date)
  
  train = ts(dt_series,frequency=7,start=c(1,1))
  fit<-auto.arima(train)
  
  x = forecast(fit,h=30)$mean
  predict = round(as.data.frame(x))
  predict$sale_date = pred_time
  predict$id = id
  predict$x = as.numeric(predict$x)
  return(predict)
}

"#timeformat = function(x){
  return(as.integer(format(as.POSIXct(x),format=%d)))
}"

data = fread("data/训练集.csv",stringsAsFactors=FALSE,encoding = "unknown")
submission = fread("data/用户提交.csv",stringsAsFactors=FALSE,encoding = "unknown")
names(data) = c("custid","big_id","big_name","m_id","m_name","small_id","small_name","sale_date",
                "sale_month","SPBM","GGXH","SPLX","DW","XSSL","XSJE","SPDJ","CX")
names(submission) = c("id","sale_date","count")
data = data[,sale_date := as.Date(as.character(sale_date),format = "%Y%m%d")]
data = data[data$sale_date>="2015-03-01",]

count1 = data %>%
  select(big_id,sale_date) %>%
   group_by(big_id,sale_date) %>%
     summarize(count = n())

count2 = data%>%select(m_id,sale_date)%>%group_by(m_id,sale_date)%>%summarize(count = n())

names(count1) = c("id","sale_date","count");names(count2) = c("id","sale_date","count")
count_12 = rbind(as.data.frame(count1),as.data.frame(count2))

sale_date = as.Date(seq.Date(from = as.Date("2015-03-01",format = "%Y-%m-%d"),by = "day",length.out = 61))
date = as.data.frame(sale_date)
pred_time = c(20150501:20150530)

result = NULL
for(id in unique(submission$id)){
  #print(id)
  sub = ID_pred(id,count_12)
  result = rbind(result,sub)
}

result = result[,c("id","sale_date","x")]
names(result) = c("编码","日期","销量")
#result$销量[result$销量<0]=0
write.csv(result,"output/submission.csv",row.names = F)
