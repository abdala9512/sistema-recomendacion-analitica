library(readr)
library(dplyr)
library(magrittr)


# Calculo de muestra representativa de la poblacion
sample_size_calc <- function(population, error, conf, p = 0.5){

  zscore <- qnorm((1-conf) / 2) ^ 2
  num <- ( zscore * p * (1 - p) ) / (error ^ 2)
  den <- 1 + ( zscore * p * (1 - p) ) / (error^2 * population)

  return(num / den)

}


orders               <- read_csv('./data/orders.csv') %>%  select(-eval_set)
products             <- read_csv('./data/products.csv')
aisles               <- read_csv('./data/aisles.csv')
departments          <- read_csv('./data/departments.csv')


# Tamano de muestra con un margen de error del 1% y confianza del 99%
sample_size <- sample_size_calc(
  population = length(unique(orders$user_id)),
  error      = 0.01,
  conf       = 0.99
)


# Semilla para reproduciblidad
set.seed(1008)

# Muestra de 10.00 clientes aleatorios
users_sample <- sample.int(length(unique(orders$user_id)), sample_size)

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


# .csv  sin PAsillos populares
write_csv(order_products %>%
            left_join(product_catalog, by = "product_id") %>%
            select(order_id, product_name, aisle, department) %>%
            filter(!aisle %in% c("fresh fruits","fresh vegetables", "packaged vegetables fruits" )),
          file = "./data/order_products_samples_no_popular.csv")


rm(list = ls()); gc()




load("../data/instcart_data_sample.RData")


