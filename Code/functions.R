#-- define function to plot production data by month
plot_by_month = function(df, x, y){
  #-- x and y are two characters
  #-- df is a data frame with a column Timestamp and Turbine_ID, and for one year
  #-- df must contain columns with names x and y
  #-- It will then do a scatterplot for each month of the year of y in function of x
  
  sel = df %>% select(Turbine_ID,Datetime,all_of(x), all_of(y))
  m = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  month_ID=month(sel$Datetime)
  month_ID=as.factor(month_ID)
  levels(month_ID) = m
  sel$Month_ID=month_ID
  year=year(sel$Datetime[1])
  
  print(ggplot(sel)+aes_string(x=x,y=y,color="Turbine_ID")+
          geom_point(alpha = 0.4, size = 0.5)+ggtitle(paste("Production power with respect to Windpseed in year",year))+
          facet_wrap(.~Month_ID,ncol=4,nrow = 3)+theme_bw() +
          xlab("Windspeed [m/s]")+
          ylab("Production power [Wh]")+
          theme(axis.text.x = element_text(hjust = 0.6),
                plot.title = element_text(size = 10, face = "bold", hjust = 0.6), 
                legend.title = element_text(size = 8, face = "bold"),
                legend.text = element_text(size = 6),
                axis.text=element_text(size=6),
                axis.title=element_text(size=8,face="bold"))) 
}

#-- define function that fills missing timestamps
fill_my_na = function(df){
  if(!require(dplyr)) require(dplyr)
  min_ts = min(df$Datetime, na.rm = T)
  max_ts = max(df$Datetime, na.rm = T)
  Datetime_full = seq.POSIXt(min_ts, max_ts, by = "10 min")
  
  df_2 = as.data.frame(Datetime_full) %>%
    left_join(df, by = c("Datetime_full" = "Datetime")) %>% 
    remove_rownames() %>%
    fill(Turbine_ID) %>% as_tibble()
  
  #cat("A Total of", sum(is.na(df_2)), "missing values have been introduced for all", dim(df_2)[2], "variables\n\n" )
  return(df_2)
}

#-- define function that detect values outside the bounds defined by the quantiles of order given
outlier.uni=function(min,max,vec){
  
  quant=quantile(vec,probs=c(min,max),na.rm = TRUE)
  ret=rep(FALSE,length(vec))
  return(vec<quant[1] | vec>quant[2])
}

#-- function to find anomalies in each turbine. We do two loops to avoid selection of the same anomaly datetime
#   object more than once (i.e. date of an anomaly for T06 is different from date of anomaly for T07). 
find_my_anomalies_ts = function(df){
  
  #-- define variables for the loops
  t_id = levels(df$Turbine_ID)
  names = names(select(df, contains("Temp")))
  anom_list = list()
  df2 = list()
  
  #-- loop over each turbine to find anomalies
  for (j in seq_along(t_id)) {
    
    #-- loop over each variable in each turbine to identify anomalies in each Time-Series
    for (i in seq_along(names)){
      
      #-- WE SHOULD NOT CHECK ANOMALIES FOR THE AMBIANT TEMPERATURE
      if(paste0(names[i] != "Amb_Temp_Avg")){
        
        #-- do time decomposition to find anomalies in each temperature variable
        anom = df %>% filter(Turbine_ID == paste0(t_id[j])) %>%
          time_decompose(paste0(names[i]), method = "stl", frequency = "auto", trend = "auto", message = FALSE) %>%
          anomalize(remainder) %>%
          time_recompose() %>%
          filter(anomaly == 'Yes')
        anom_list[[i]] = anom$Datetime
      }
    }
    #-- each element in the list is a data frame contains anomalies of each turbine
    df2[[j]] = df %>% filter(Turbine_ID == paste0(t_id[j]), Datetime %in% do.call(c, anom_list))
    cat(dim(df2[[j]])[1], "observations have been identified as anomalies in turbine", paste0(t_id[j]), "\n")
  }
  
  #-- bind all data frames in the list in a big data frame that only contains anomalies
  df3 = df2 %>% 
    bind_rows() %>% 
    droplevels()
  
  cat("A total of", dim(df3)[1], "anomalies have been detected\n\n")
  return(df3)
}

#-- function to scale the data
scale_my_df = function(df){
  
  #-- select variables to scale and return scaled data frame
  n = df %>% select(-Turbine_ID, -Datetime) %>% names()
  df_scaled = df %>% 
    mutate_at(n, scale) %>%
    as_tibble()
  return(df_scaled)
}
