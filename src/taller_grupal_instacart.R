

# TALLER INSTACART - REGLAS DE ASOCIACION ---------------------------------
#
# EFRAIN DIAZ OSORIO
# ANDRES FELIPE CARDONA RODRIGUEZ
# MIGUEL ARQUEZ ABDALA
# ANALITICA 1 - MAESTRIA EN ANALITICA PARA LA INTELIGENCIA DE NEGOCIOS

# -------------------------------------------------------------------------


library(readr)
library(dplyr)
library(magrittr)
library(readr)
library(dplyr)
library(arules)
library(arulesViz)
library(ggplot2)
library(lattice)


options(scipen = 999, digits = 4)
windowsFonts("Roboto" = windowsFont("Roboto"))



# -------------------------------------------------------------------------
# PREPARACION DE DATOS ----------------------------------------------------
# -------------------------------------------------------------------------



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

order_products <-
  rbind(
    read_csv('./data/order_products__train.csv'),
    read_csv('./data/order_products__prior.csv')
  ) %>% filter(order_id %in% orders_id)



# Unificamos los datos de los productos en un solo catalogo
product_catalog <-  products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(departments, by = "department_id") %>%
  select(-aisle_id, -department_id)


# Dejamos 3 tablas finales con la muestra de 10.00 clientes
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



# -------------------------------------------------------------------------
# MODELAMIENTO ------------------------------------------------------------
# -------------------------------------------------------------------------



# Iteracion 1 -------------------------------------------------------------

transactionsDF <- read.transactions(file = "../data/order_products_samples.csv",
                                    format = "single",
                                    sep = ",",
                                    header = TRUE,
                                    cols = c("order_id", "aisle"),
                                    rm.duplicates = TRUE)


rule_strategy <- apriori(data = transactionsDF,
                         parameter = list(support = 0.01,
                                          confidence=0.5,
                                          maxlen=3)
)


# Iteracion 2 -------------------------------------------------------------


rule_strategy <- apriori(data = transactionsDF,
                         parameter = list(support = 0.01,
                                          confidence=0.7,
                                          maxlen=2)
)


# Iteracion 3 -------------------------------------------------------------



rules_aisles <- order_products %>%
  select(aisle) %>%
  distinct() %>%
  pull()

rules_def <- vector(mode = "list",length = length(rules_aisles))

for (i in seq_along(rules_aisles)) {

  message("Iteracion ", i)

  rule_strategy1 <- apriori(data = products_,
                            parameter = list(support = 0.0001,
                                             confidence=0.5,
                                             maxlen=2),
                            appearance = list(rhs = rules_aisles[i]))

  if (length(rule_strategy1) == 0) {
    next
  } else {

    rules_df <- as(rule_strategy1, "data.frame") %>%
      arrange(desc(lift)) %>%
      separate(rules, c("lhs", "rhs"), "=>") %>%
      top_n(3)

    rules_def[[i]] <- rules_df
  }


}

reglas_finales <- rules_def %>%
  bind_rows()

rules_df %>%
  rename(consecuente = rhs,
         transacciones = count ) %>%
  ggplot(aes(x = support, y = lift, size = transacciones, color = consecuente)) +
  geom_point() +
  custom_style() +
  labs(
    title = "Reglas de asociación por pasillos",
    subtitle = "Soporte vs. Lift",
    caption = "Soporte: 1%, Confianza: 70%, MaxLen: 3"

  ) +
  xlab("Soporte") +
  ylab("Lift") -> assoc_rules_ggplot

ggsave(filename = "../img/supp_lift.png",
       plot = assoc_rules_ggplot)


# -------------------------------------------------------------------------
# VISUALIZACIONES ---------------------------------------------------------
# -------------------------------------------------------------------------


orders$order_hour_of_day <- as.numeric(orders$order_hour_of_day)

# Productos comparados por departamento

custom_style <- function(){

  font <- "Roboto"

  ggplot2::theme(


    plot.title = ggplot2::element_text(
      family = font,
      size = 20,
      face = 'bold'
    ),

    plot.subtitle = ggplot2::element_text(family=font,
                                          size=12,
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_text(
      family = font,
      size = 10
    ),



    legend.position = "right",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_text(
      family = font,
      size = 12
    ),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=font,
                                        size=12,
                                        color="#222222"),


    axis.title = ggplot2::element_text(
      family = font,
      size = 12,
      face = 'bold'
    ),
    axis.text = ggplot2::element_text(family=font,
                                      size=14,
                                      color="#222222"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks = ggplot2::element_blank(),
    axis.line = ggplot2::element_blank(),
    #axis.line.x  = element_line(color =  "#000000"),



    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.x = ggplot2::element_blank(),



    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(size  = 22,  hjust = 0)
  )
}



order_products %>%
  left_join(product_catalog, by = "product_id") %>%
  group_by(department) %>%
  count() %>%
  arrange(n) %>%
  ggplot(aes(y = n, x = reorder(department, -n))) +
  geom_bar(stat = 'identity', fill = "#28FFBF") +
  labs(
    title =  "Productos comprados por departamento",
  ) +
  ylab("") +
  xlab("Número de productos") +
  custom_style() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) -> producto_port_depto

ggsave(filename = "./img/producto_por_departamento1.png",
       plot = producto_port_depto )



# Top 10 pasillos
order_products %>%
  left_join(product_catalog, by = "product_id") %>%
  group_by(aisle) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  top_n(n = 20) %>%
  ggplot(aes(y = n, x = reorder(aisle, n))) +
  geom_bar(stat = 'identity', fill = "#6F69AC") +
  labs(
    title =  "Productos comprados por pasillo",
  ) +
  ylab("Número de productos") +
  xlab("") +
  custom_style() +
  coord_flip() +
  theme(
    panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.y = ggplot2::element_blank(),
  ) -> producto_por_pasillo


ggsave(filename = "./img/top_20_producto_por_pasillo.png",
       plot = producto_por_pasillo )


# Top 20 productos
order_products %>%
  left_join(product_catalog, by = "product_id") %>%
  group_by(product_name) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  top_n(n = 20) %>%
  ggplot(aes(y = n, x = reorder(product_name, n))) +
  geom_bar(stat = 'identity', fill = "#911F27") +
  labs(
    title =  "Top Productos comprados",
  ) +
  ylab("Número de productos") +
  xlab("") +
  custom_style() +
  coord_flip() +
  theme(
    panel.grid.major.x = ggplot2::element_line(color="#cbcbcb"),
    panel.grid.major.y = ggplot2::element_blank(),
  ) -> top_productos


ggsave(filename = "./img/top20_productos.png",
       plot = top_productos )



# Distribucion dias desde  la ultima compra

orders %>%
  mutate(
    order_dow = factor(order_dow,levels = c( "6", "5", "4", "3", "2", "1", "0")),
    order_hour_of_day = as.factor(order_hour_of_day)
  ) %>%
  group_by(order_dow, order_hour_of_day) %>%
  count() %>%
  rename(Ordenes = n) %>%
  ggplot( aes(order_hour_of_day,order_dow, fill= Ordenes)) +
  geom_tile() +
  custom_style() +
  labs(
    title = "Frecuencia Ordenes",
    subtitle = "Por día y Hora"

  ) +
  xlab("Hora del día") +
  ylab("Día de la semana") +
  scale_fill_gradient(low = "#F8F8F8", high = "#1E3163") -> ordenes_dia_hora

ggsave(filename = "./img/ordenes_dia_hora.png",
       plot = ordenes_dia_hora )

# Distribucion compras por hora del dia

orders %>%
  ggplot(aes(x = order_hour_of_day)) +
  geom_histogram(bins = 24, fill = "#3DB2FF", color = "#ffffff") +
  custom_style() +
  labs(
    title =  "Distribución ordenes en el día"
  ) +
  ylab("") +
  xlab("Hora del día") -> dist_hora_dia

ggsave(filename = "./img/hist_hora_dia.png",
       plot = dist_hora_dia )


# Frecuencia ultimo dia de compra

orders %>%
  filter(!is.na(days_since_prior_order)) %>%
  group_by(user_id) %>%
  summarise(frencuencia_compra = mean(days_since_prior_order)) %>%
  ggplot(aes(x = frencuencia_compra)) +
  geom_histogram(bins = 24, fill = "#AE00FB", color = "#ffffff") +
  custom_style() +
  labs(
    title =  "Distribución frecuencia compra",
    subtitle = "En promedio, cada cuantos días una persona realiza una orden"
  ) +
  ylab("") +
  xlab("días") -> freq_compra

ggsave(filename = "./img/hist_freq_compra.png",
       plot = freq_compra )


# Ordenes promedio usuario
orders %>%
  group_by(user_id) %>%
  count() %>%
  ggplot(aes(x = n)) +
  geom_histogram(bins = 24, fill = "#E63E6D", color = "#ffffff") +
  custom_style() +
  labs(
    title =  "Distribución Ordenes promedio por cliente",
  ) +
  ylab("") +
  xlab("Ordenes promedio") -> ordenes_promedio_usuario

ggsave(filename = "./img/hist_orden_promedio_usuario.png",
       plot = ordenes_promedio_usuario )


# Productos promedio por orden
order_products %>%
  group_by(order_id) %>%
  count() %>%
  ggplot(aes(x = n)) +
  geom_histogram(bins = 24, fill = "#6B7AA1", color = "#ffffff") +
  custom_style() +
  labs(
    title =  "Distribución productos promedio por orden",
  ) +
  ylab("") +
  xlab("Productos promedio") -> productos_promedio_orden

ggsave(filename = "./img/hist_producto_promedio_orden.png",
       plot = productos_promedio_orden )

## Compras de productos pro departamento

order_products %>%
  left_join(product_catalog, by = "product_id") %>%
  group_by(order_id, department) %>%
  count() %>%
  # mutate(n = log1p(n)) %>%
  ggplot(aes(x = department, y = n)) +
  geom_boxplot(color="#297F87", fill="#297F87", alpha=0.2) +
  custom_style() +
  coord_flip() +
  labs(
    title =  "Distribución compras por departamento"
  ) +
  xlab("Departamento") +
  ylab("Número de productos") -> dist_por_depto


ggsave(filename = "./img/box_compras_departamento.png",
       plot = dist_por_depto )

# Ordenes promedio vs. productos promedio


order_clientes <-
  order_products %>%
  left_join(orders, by = "order_id") %>%
  group_by(user_id, order_id) %>%
  count() %>%
  group_by(user_id ) %>%
  summarise(productos_promedio = mean(n)) %>%
  left_join(
    orders %>%
      group_by(user_id) %>%
      count(),
    by = "user_id"
  )


ggplot(order_clientes, aes(x = n, y = productos_promedio)) +
  geom_point()

