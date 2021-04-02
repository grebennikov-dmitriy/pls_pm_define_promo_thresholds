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
library(hrbrthemes)

myConn <-
  odbcDriverConnect(connection = "")
get_hist_data <-
  function(myConn,
           client,
           prod_id,
           percentile_w,
           percentile_p) {
    
    sql_command <- sqlQuery(
      myConn,
      paste(
        ";with cte1 as (
	  SELECT  [date]

	  ,[prod_id]
	  ,sum([weight_sum]) as weight_sum
	  ,avg(price_mean) as price_mean

	    FROM [DWH_SPSS].[dbo].[Days]
	  where  client = '",
        client,
        "'
  	and date < '2020-11-01'

  	 and prod_id = ",
        prod_id,
        "
  	 and month(date) !=12
	  group by [date],[prod_id]
	  )

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
	,case when [change_percent_weight] >=",
        percentile_w,
        " then 1 else 0 end as is_promo
	,case when [change_percent_price] <= -10 then 1 else 0 end as is_promo_price
	--,case when [change_percent_price] <=",
        percentile_p,
        " then 1 else 0 end as is_promo_price
	from cte2	",
        sep = ""
      )
    )
    all_data <- sql_command
    head(all_data)
    #all_data$date <- as.Date(all_data$date)
    
    #all_data$client<- stri_trans_general(all_data$client, "russian-latin/bgn")
    #all_data <- all_data[(all_data$client == 'Lenta'),]
    
    return (all_data)
    
  }

get_list_of_products <- function(myConn, client, category) {
  sql_command <- sqlQuery(
    myConn,
    paste(
      ";with cte as (
		SELECT
		date,
	  [prod_id], prod_name
	  ,[weight_sum]
	  , ROW_NUMBER ( )  over (partition by  [prod_id], prod_name  ORDER BY [weight_sum]) as r


	  FROM [DWH_SPSS].[dbo].[Days]
	  where  [category_id] =",
      category,
      "
	   and   client = '",
      client,
      "'
	   ),
	 
cte1 as(
SELECT
	  [prod_id], prod_name
	  ,sum([weight_sum]) as weight_sum
	 ,count(r) as r
	  from cte
  	 group by prod_id, prod_name
	 )
	 select top 10 [prod_id]
	  from cte1
	  where r >10
	   order by weight_sum desc",
      sep = ""
    )
  )
  products <- sql_command
  list_of_products <- c(unique(products$prod_id))
  return (list_of_products)
}


########PLSPM#####################################


all_data <- NULL
list_of_categories<- c(11,18,7,4)
for(category in   list_of_categories){
  
  # list_of_products<- get_list_of_products (myConn,'Лента', category)
  list_of_networks <- c('Ашан','Дикси','Лента','Магнит','Метро','Окей')
  for(client in list_of_networks){
    list_of_products<- get_list_of_products (myConn,client, category)
    for(prod_id in list_of_products){
      df <- get_hist_data (myConn,  client,prod_id,5,5)
      print(head(df))
      max_percent_change <- max(df$change_percent_weight)
      A<- seq(10, max_percent_change, by=10)
      B<- seq(-5, -32, by=-2)
      quantiles = expand.grid(x = A, y = B)
      quantiles$x <- as.integer(quantiles$x)
      list_of_quantiles_w <- c(unique(as.character(quantiles$x)))
      list_of_quantiles_p <- c(unique(as.character(quantiles$y)))
      temp_df <- NULL
      for ( quantile_w in list_of_quantiles_w){   
        for (quantile_p in c(-10)){#list_of_quantiles_p){ 
          
          cat("   ", "!!!!!", quantile_w,'!!!!!!!',prod_id, "__   ",quantile_p,"||", client,"           |||")
          df <- get_hist_data (myConn,  client,prod_id,quantile_w,quantile_p)
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
         
          promo_blocks = list(1,2,3,4,1:4)
          new_promo_blocks = list(1,2,4,5,c(1,2,4,5))
          # vector of modes (reflective)
          promo_modes = c("A","A","A","A","A")
          promo_pls = try(plspm(df, promo_path, new_promo_blocks, modes = promo_modes))
          if(!class(promo_pls) =="try-error" ){
            
          
            value <- abs((promo_pls$path_coefs[5]+promo_pls$path_coefs[10]) -(promo_pls$path_coefs[15]+promo_pls$path_coefs[20]))
            
            list_errors <- list('quantile_w'=quantile_w, 'quantile_p' = quantile_p,
                                'prod_id'= 434766, "value" = value
                                ,"client" = client)
            df_errors <-as.data.frame(list_errors)
            temp_df <-rbind(temp_df,df_errors)
          }
        }
      }
      try({best_params <- temp_df%>%
        group_by(  quantile_w, quantile_p,prod_id,client)%>%
        summarise(value = min(value))
      best_params <-merge(best_params, temp_df, by= c( 'value'))
      best_quantile_w <- min(best_params$quantile_w)
      best_quantile_w<-as.integer(best_quantile_w)
      cat("+++++++++++++++++++++++++++++++++++++++++++___",best_quantile_w)
      temp <- get_hist_data (myConn,  client,prod_id,best_quantile_w,quantile_p)
      temp$best_quantile_w<- best_quantile_w
      temp$client<- client
      cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! TEMP!!!!!!!!!!!")
      print((head(temp)))
      csvfile<-paste0("C:/Users/dgrebennikov/JupyterFiles/ARIMA_R/data/plspm_promo/",prod_id,"__",client,"_.xlsx")
      write_xlsx(as.data.frame(temp),csvfile)
      all_data <-rbind(all_data,temp)})
    }
    
  }
  
}

csvfile<-paste0("C:/Users/dgrebennikov/JupyterFiles/ARIMA_R/data/Plspm1_.xlsx")
write_xlsx(as.data.frame(all_data),csvfile)


