#loading in the data and libraries I will use
library(readr)
library(tidyverse)
causal <- read_csv("dh_causal_lookup.csv")
product <- read_csv("dh_product_lookup.csv")
store <- read_csv("dh_store_lookup.csv")
transactions <- read_csv("dh_transactions.csv")

#looking at the data
str(transactions)
str(product)

#changing variable types
product$commodity <- as.factor(product$commodity)
transactions$upc <- as.numeric(transactions$upc)

#Question A
prod_trans <- transactions %>% left_join(product, by="upc")

levels(product$commodity)

#pancake mixes
top5_pancake <- prod_trans %>% 
  filter(commodity == "pancake mixes") %>%
  group_by(upc) %>%
  summarise(sum=sum(dollar_sales)) %>%
  arrange(desc(sum)) %>%
  head(5)

top5_pancake

upc1 <- prod_trans %>% filter(upc==top5_pancake$upc[[1]]) %>% select(product_description, brand, product_size)
upc2 <- prod_trans %>% filter(upc==top5_pancake$upc[[2]]) %>% select(product_description, brand, product_size)
upc3 <- prod_trans %>% filter(upc==top5_pancake$upc[[3]]) %>% select(product_description, brand, product_size)
upc4 <- prod_trans %>% filter(upc==top5_pancake$upc[[4]]) %>% select(product_description, brand, product_size)
upc5 <- prod_trans %>% filter(upc==top5_pancake$upc[[5]]) %>% select(product_description, brand, product_size)
df_pancake <- rbind(upc1[1,],upc2[1,],upc3[1,],upc4[1,],upc5[1,])
df_pancake

#pasta
top5_pasta <- prod_trans %>% 
  filter(commodity == "pasta") %>%
  group_by(upc) %>%
  summarise(sum=sum(dollar_sales)) %>%
  arrange(desc(sum)) %>%
  head(5)

upc_p1 <- prod_trans %>% filter(upc==top5_pasta$upc[[1]]) %>% select(product_description, brand, product_size)
upc_p2 <-prod_trans %>% filter(upc==top5_pasta$upc[[2]]) %>% select(product_description, brand, product_size)
upc_p3 <-prod_trans %>% filter(upc==top5_pasta$upc[[3]]) %>% select(product_description, brand, product_size)
upc_p4 <-prod_trans %>% filter(upc==top5_pasta$upc[[4]]) %>% select(product_description, brand, product_size)
upc_p5 <-prod_trans %>% filter(upc==top5_pasta$upc[[5]]) %>% select(product_description, brand, product_size)
df_pasta <- rbind(upc_p1[1,],upc_p2[1,],upc_p3[1,],upc_p4[1,],upc_p5[1,])
df_pasta

#pasta sauce
top5_pastas <- prod_trans %>% 
  filter(commodity == "pasta sauce") %>%
  group_by(upc) %>%
  summarise(sum=sum(dollar_sales)) %>%
  arrange(desc(sum)) %>%
  head(5)
top5_pastas
upcps1 <- prod_trans %>% filter(upc==top5_pastas$upc[[1]]) %>% select(product_description, brand, product_size)
upcps2 <- prod_trans %>% filter(upc==top5_pastas$upc[[2]]) %>% select(product_description, brand, product_size)
upcps3 <- prod_trans %>% filter(upc==top5_pastas$upc[[3]]) %>% select(product_description, brand, product_size)
upcps4 <- prod_trans %>% filter(upc==top5_pastas$upc[[4]]) %>% select(product_description, brand, product_size)
upcps5 <- prod_trans %>% filter(upc==top5_pastas$upc[[5]]) %>% select(product_description, brand, product_size)

df_pastas <- rbind(upcps1[1,],upcps2[1,],upcps3[1,],upcps4[1,],upcps5[1,])
df_pastas

#syrups
top5_syrup <- prod_trans %>% 
  filter(commodity == "syrups") %>%
  group_by(upc) %>%
  summarise(sum=sum(dollar_sales)) %>%
  arrange(desc(sum)) %>%
  head(5)

top5_syrup

upcs1 <- prod_trans %>% filter(upc==top5_syrup$upc[[1]]) %>% select(product_description, brand, product_size)
upcs2 <- prod_trans %>% filter(upc==top5_syrup$upc[[2]]) %>% select(product_description, brand, product_size)
upcs3 <- prod_trans %>% filter(upc==top5_syrup$upc[[3]]) %>% select(product_description, brand, product_size)
upcs4 <- prod_trans %>% filter(upc==top5_syrup$upc[[4]]) %>% select(product_description, brand, product_size)
upcs5 <- prod_trans %>% filter(upc==top5_syrup$upc[[5]]) %>% select(product_description, brand, product_size)

df_syrup <- rbind(upcs1[1,],upcs2[1,],upcs3[1,],upcs4[1,],upcs5[1,])
df_syrup

#Question B

#pancake mixes
top5_pancakeb <- prod_trans %>% 
  filter(commodity == "pancake mixes") %>%
  group_by(brand) %>%
  summarise(sum=sum(dollar_sales)) %>%
  arrange(desc(sum)) %>%
  head(5)
top5_pancakeb

#pasta
top5_pastab <- prod_trans %>% 
  filter(commodity == "pasta") %>%
  group_by(brand) %>%
  summarise(sum=sum(dollar_sales)) %>%
  arrange(desc(sum)) %>%
  head(5)
top5_pastab

#pasta sauce
top5_pastasb <- prod_trans %>% 
  filter(commodity == "pasta sauce") %>%
  group_by(brand) %>%
  summarise(sum=sum(dollar_sales)) %>%
  arrange(desc(sum)) %>%
  head(5)
top5_pastasb

#syrups
top5_syrupb <- prod_trans %>% 
  filter(commodity == "syrups") %>%
  group_by(brand) %>%
  summarise(sum=sum(dollar_sales)) %>%
  arrange(desc(sum)) %>%
  head(5)
top5_syrupb

#Additional things to explore: does this grocery store have a generic/store brand? 
#Why isn't that brand doing as well?
#I would think that it would be in the grocery store's best interests to sell a lot of their own brand

#Question C

#join on zip code
library(zipcode)
data("zipcode")
prod_trans <- prod_trans %>% left_join(store, by ="store")
#these brands drive sales: 
prod_trans %>% 
  filter(commodity == "pasta") %>%
  group_by(brand) %>%
  summarise(sum=sum(dollar_sales)) %>%
  arrange(desc(sum)) %>%
  head(5)

#geography 1 has more sales than geography 2
prod_trans %>% 
  filter(commodity == "pasta") %>%
  group_by(geography) %>%
  summarise(sum=sum(dollar_sales), sd=sd(dollar_sales), mean=mean(dollar_sales))

#looking at sales by store
prod_trans %>% 
  filter(commodity == "pasta") %>%
  group_by(store) %>%
  summarise(sum=sum(dollar_sales), sd=sd(dollar_sales), mean=mean(dollar_sales)) %>%
  arrange(desc(sum))

#looking at sales by zip code
prod_trans %>% 
  filter(commodity == "pasta") %>%
  group_by(store_zip_code) %>%
  summarise(sum=sum(dollar_sales), sd=sd(dollar_sales), mean=mean(dollar_sales)) %>%
  arrange(desc(sum))

zipcode$zip <- as.numeric(zipcode$zip)
prod_trans <- prod_trans %>% inner_join(zipcode, by = c("store_zip_code"="zip"))

#looking at sales by city
prod_trans %>% 
  filter(commodity == "pasta") %>%
  group_by(city) %>%
  summarise(sum=sum(dollar_sales), sd=sd(dollar_sales), mean=mean(dollar_sales)) %>%
  arrange(desc(sum))

#looking at sales by state
prod_trans %>% 
  filter(commodity == "pasta") %>%
  group_by(state) %>%
  summarise(sum=sum(dollar_sales), sd=sd(dollar_sales), mean=mean(dollar_sales)) %>%
  arrange(desc(sum))

#also looking into whether customers that use coupons drive sales (looking at amount spent per transaction)
prod_trans %>%
  group_by(coupon) %>%
  summarise(sum(dollar_sales)/n())
#It looks like non-coupon users spend more on average per transaction than coupon users

# Question D
#repeat rate for pasta
pasta_all <- prod_trans %>% 
  filter(commodity=="pasta")
pasta <- prod_trans %>% 
  filter(commodity=="pasta") %>%
  group_by(household) %>%
  count(upc) %>% filter(n>1)
rrpasta <- nrow(pasta)/length(table(pasta_all$household))
rrpasta

#repeat rate for syrups
syrup_all <- prod_trans %>% 
  filter(commodity=="syrups")
syrup <- prod_trans %>% 
  filter(commodity=="syrups") %>%
  group_by(household) %>%
  count(upc) %>% filter(n>1)
rrs <- nrow(syrup)/length(table(syrup_all$household))
rrs

#repeat rate for pancake mixes
pancake_all <- prod_trans %>% 
  filter(commodity=="pancake mixes")
pancake <- prod_trans %>% 
  filter(commodity=="pancake mixes") %>%
  group_by(household) %>%
  count(upc) %>% filter(n>1)
rrpan <- nrow(pancake)/length(table(pancake_all$household))
rrpan

#repeat rate for pasta sauce
pastas_all <- prod_trans %>% 
  filter(commodity=="pasta sauce")
pastas <- prod_trans %>% 
  filter(commodity=="pasta sauce") %>%
  group_by(household) %>%
  count(upc) %>% filter(n>1)
rrps <- nrow(pastas)/length(table(pastas_all$household))
rrps

#Question E
#Overall health of the category
#High repeat rate for pasta and pasta sauce so that's good. 
#But this could also just mean that pasta and pasta sauce are bought more frequently than syrup and pancake mix
#Would want to compare the repeat rate for other "staple" type items that people eat frequently like bread, eggs, etc.
#This would be a good time to get advice from industry experts in the field to see which items would be good comparison

#Let's look at the trend of sales over time

prod_trans %>%
  filter(commodity=="pasta") %>%
  group_by(week) %>%
  summarise(salesbyweek = sum(dollar_sales)) %>%
  ggplot(aes(x=week,y=salesbyweek))+geom_point()

#from the plot it looks like there are trends in sales.
#low sales around week 0 and week 45
#high sales around week 25 and week 75
#sales don't appear to be decreasing over the given period of time so that shows good health of the category

#Question F
#want to join causal with transactions and run a linear regression on dollar_sales (only for pasta)
#with feature_desc, display_desc, store #, and brand to see if any of these things are good predictors of sales
#but data was too large for my computer so I looked at some graphs and aggregate summary statistics to see
#if there was anything else interesting that I could find in the data

causal$upc <- as.numeric(causal$upc)

transactions %>% 
  group_by(store) %>%
  summarise(sales=sum(dollar_sales)) %>%
  ggplot(aes(x=store,y=sales))+geom_point()

causal$display_desc <- as.factor(causal$display_desc)

causal %>%
  group_by(store) %>%
  summarise(not_display=sum(display_desc=="Not on Display")) %>%
  arrange(desc(not_display))

causal %>%
  group_by(store) %>%
  summarise(reardisplay=sum(display_desc=="Rear End Cap")) %>%
  arrange(desc(reardisplay))

causal %>%
  group_by(store,display_desc) %>%
  filter(display_desc!="Not on Display") %>%
  summarise(display=n()) %>%
  arrange(desc(display))
# store 270 is the only store that is top 5 in # of displays and also in sales. 
# would be interesting to look at sales when no display and then look at sales when there is a display
# and see if there is a statistical difference in sales
# because maybe we are spending money on displays when we don't need to be.

#Then decided to look at whether mean sales were statistically significantly larger in the non-coupon users category
pasta_data <- prod_trans %>% filter(commodity=="pasta")
t.test(dollar_sales ~ coupon, data = prod_trans)
t.test(dollar_sales ~ coupon, data = pasta_data)
