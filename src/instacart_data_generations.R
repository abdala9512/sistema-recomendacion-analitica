library(readr)
library(dplyr)


set.seed(1008)

orders               <- read_csv('./data/orders.csv') %>%  select(-eval_set)
products             <- read_csv('./data/products.csv')
aisles               <- read_csv('./data/aisles.csv')
departments          <- read_csv('./data/departments.csv')


users_sample <- sample.int(length(unique(orders$user_id)), 10000)

orders %<>%
  filter(user_id %in% users_sample)

orders_id <- unique(orders$order_id)

order_products <-
  rbind(
    read_csv('./data/order_products__train.csv'),
    read_csv('./data/order_products__prior.csv')
  ) %>% filter(order_id %in% orders_id)


product_catalog <-  products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(departments, by = "department_id") %>%
  select(-aisle_id, -department_id)



save(order_products,
     product_catalog,
     orders,
     file =  "./data/instcart_data_sample.RData")


rm(list = ls()); gc()
