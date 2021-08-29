library(readr)
library(dplyr)



orders               <- read_csv('./data/orders.csv') %>%  select(-eval_set)
products             <- read_csv('./data/products.csv')
aisles               <- read_csv('./data/aisles.csv')
departments          <- read_csv('./data/departments.csv')


order_products <-
  rbind(
    read_csv('./data/order_products__train.csv'),
    read_csv('./data/order_products__prior.csv')
  )



product_catalog <-  products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(departments, by = "department_id") %>%
  select(-aisle_id, -department_id)



save(order_products,
     product_catalog,
     orders,
     file =  "./data/instcart_data.RData")


rm(list = ls()); gc()
