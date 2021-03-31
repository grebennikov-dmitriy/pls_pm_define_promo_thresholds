library('RODBC')

library(readxl)
library(dplyr)
library("stringi")
library("writexl")
library(fpp2)
library(lubridate)
library(dplyr)
library(plyr)
library(ggplot2)
library(stringi)
library(tidyr)
library(plspm)
library(reshape)


myConn<- odbcDriverConnect(connection = "")
get_hist_data <- function(myConn, client,prod_id,percentile_w,percentile_p){
  # СЂР°Р·РґРµР»СЏРµС‚ РёСЃС‚СЂРёС‡РµСЃРєРёРµ РґР°РЅРЅС‹Рµ РґРѕ РјСЏСЃСЏС†Р° РїРѕСЃР»РµРґРЅРµРіРѕ РїСЂРѕРјРѕ Рё СЃР°Рј РјРµСЃСЏС† РїРѕСЃР»РµРґРЅРµРіРѕ РїСЂРѕРјРѕ РЅР° РєРѕС‚РѕСЂРѕРј Р±СѓРґСѓС‚ РїРѕРґР±РёСЂР°С‚СЊСЃСЏ РїР°СЂР°РјРµС‚СЂС‹
  sql_command <-sqlQuery(myConn, paste(";with cte1 as (
	  SELECT  [date]                      
	  
	  ,[prod_id]
	  ,sum([weight_sum]) as weight_sum
	  ,avg(price_mean) as price_mean

	    FROM [DWH_SPSS].[dbo].[Days] 
	  where  client = '",client,"'  
  	and date < '2020-11-01'
  
  	 and prod_id = ",prod_id,"
	  group by [date],[prod_id])
  
  ,cte as (
	  SELECT  [date]                      
	 
	  ,[prod_id]
	  ,[weight_sum]
	  ,price_mean

	   ,percentile_disc(0.5) within group(order by [weight_sum])
	  over (partition by  prod_id ) as median_weight 
	  ,percentile_disc(0.5) within group(order by [price_mean])
	  over (partition by  prod_id ) as median_price


	  FROM cte1 
	
	  )	
	  , cte2 as (select [date]                      
	
	,[prod_id]
	,[weight_sum]
	,price_mean

	 ,median_weight
	 ,median_price
	 ,([weight_sum] -median_weight)/median_weight*100 as change_percent_weight
	 ,(price_mean -median_price)/median_price*100 as change_percent_price

	from cte )
	  
	  select [date]                      

	,[prod_id]
	,[weight_sum]
	,price_mean
	 ,median_weight
	 ,median_price
	 ,change_percent_weight
	 ,change_percent_price
	,case when [change_percent_weight] >=",percentile_w," then 1 else 0 end as is_promo
	,case when [change_percent_price] <=",percentile_p," then 1 else 0 end as is_promo_price
	from cte2	", sep = "") )   
  all_data<-sql_command
  head(all_data)
  #all_data$date <- as.Date(all_data$date)
  
  #all_data$client<- stri_trans_general(all_data$client, "russian-latin/bgn")
  #all_data <- all_data[(all_data$client == 'Lenta'),]
  
  return (all_data)
  
}

########PLSPM#####################################
A<- seq(10, 1350, by=10)
B<- seq(-5, -32, by=-2)
quantiles = expand.grid(x = A, y = B)
list_of_quantiles_w <- c(unique(as.character(quantiles$x)))
list_of_quantiles_p <- c(unique(as.character(quantiles$y)))

all_data <- NULL
for ( quantile_w in list_of_quantiles_w){   
  for (quantile_p in list_of_quantiles_p){ 
    cat("   ", "!!!!!", quantile_w,'!!!!!!!',quantile_p,"           |||")
    df <- get_hist_data (myConn,  'Лента',434766,quantile_w,quantile_p)
    df %>% select(weight_sum,is_promo, price_mean, is_promo_price ) ->df
    weight_sum =c(0,0,0,0,0)
    is_promo =c(0,0,0,0,0)
    price_mean=c(0,0,0,0,0)
    is_promo_price =c(0,0,0,0,0)
    promo   =c(1,1,1,1,0)
    # path matrix (inner model)
    promo_path = rbind(weight_sum,is_promo, price_mean, is_promo_price , promo)
    # add column names
    colnames(promo_path) = rownames(promo_path)
     # blocks of indicators (outer model)
    df$N_price_mean <- -1*df$price_mean
    names(df)
    promo_blocks = list(1,2,3,4,1:4)
    new_promo_blocks = list(1,2,4,5,c(1,2,4,5))
    # vector of modes (reflective)
    promo_modes = c("A","A","A","A","A")
    promo_pls = try(plspm(df, promo_path, new_promo_blocks, modes = promo_modes))
    if(!class(promo_pls) =="try-error" ){
      
    
      value <- abs((promo_pls$path_coefs[5]+promo_pls$path_coefs[10]) -(promo_pls$path_coefs[15]+promo_pls$path_coefs[20]))
      
      list_errors <- list('quantile_w'=quantile_w, 'quantile_p' = quantile_p,
                          'prod_id'= 434766, "value" = value)
      df_errors <-as.data.frame(list_errors)
      all_data <-rbind(all_data,df_errors)
    }
  }
}

best_params <- all_data%>%
  group_by(  quantile_w, quantile_p,prod_id)%>%
  summarise(value = min(value))
best_params
best_params <-merge(best_params, all_data, by= c( 'value'))
best_params
all_data1 <-all_data[(all_data$value <0.02537 ),]
all_data1
summary(all_data)
