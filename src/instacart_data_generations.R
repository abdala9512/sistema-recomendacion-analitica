library(readr)
library(dplyr)
library(magrittr)



orders               <- read_csv('./data/orders.csv') %>%  select(-eval_set)
products             <- read_csv('./data/products.csv')
aisles               <- read_csv('./data/aisles.csv')
departments          <- read_csv('./data/departments.csv')

# Semilla para reproduciblidad
set.seed(1008)

# Muestra de 10.00 clientes aleatorios
users_sample <- sample.int(length(unique(orders$user_id)), 10000)

orders %<>%
  filter(user_id %in% users_sample)

orders_id <- unique(orders$order_id)

# Aca estoy uniendo los datos de ambas tablas, son los mismos pero separados con proposito de la competencia de kaggle
order_products <-
  rbind(
    read_csv('./data/order_products__train.csv'),
    read_csv('./data/order_products__prior.csv')
  ) %>% filter(order_id %in% orders_id)



# Unifico los datos de los productos en un solo catalogo
product_catalog <-  products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(departments, by = "department_id") %>%
  select(-aisle_id, -department_id)


# Dejo 3 tablas finales con la muestra de 10.00 clientes
save(order_products,
     product_catalog,
     orders,
     file =  "./data/instcart_data_sample.RData")


# csv para libreria arules
write_csv(order_products %>%
            left_join(product_catalog, by = "product_id") %>%
           select(order_id, product_name, aisle, department),
          file = "./data/order_products_samples.csv")

rm(list = ls()); gc()
