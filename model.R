t = train_data %>%
       mutate(week =  week(date) , year = year(date) ,day =  weekdays(date) )


train_data %>%
  mutate(week =  week(date) , year = year(date) ,day =  weekdays(date) ) %>%
  group_by(week,year) %>%
  mutate( weekly_normalized_sales = sales - mean(sales) ) %>%
  head(14)

train_data_enriched  = train_data %>%
  mutate(week =  week(date) , year = year(date) ,day =  weekdays(date) )

total_sales_by_month = train_data %>%
  mutate(month =  month(date) , year = year(date)) %>%
  group_by(month,year) %>%
  summarize( total_sales =sum(sales))

total_sales_by_week = train_data %>%
  mutate(month =  week(date) , year = year(date)) %>%
  group_by(month,year) %>%
  summarize( total_sales =sum(sales))

#### Model 
total_sales_by_day = train_data %>%
  group_by(date) %>%
  summarize( total_sales =sum(sales)) %>%
  mutate(month = format.Date(date , format = "%B") , year = as.numeric(format.Date(date , format = "%Y")),
         day = weekdays(date))

glm_model = glm(formula = log(total_sales) ~ year + month + day, data = total_sales_by_day)
predicted = predict.glm(glm_model  ,total_sales_by_day[-c(1,2)])
total_sales_by_day$predicted = predicted
ggplot(total_sales_by_day , aes(x = date)) + geom_line(aes(y = exp(predicted)) , color = "red") + geom_line(aes(y = (total_sales)))






