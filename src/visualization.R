library(ggplot2)
library(dplyr)
library(tidyr)
library(lattice)
library(ggridges)



options(scipen = 999, digits = 4)
windowsFonts("Roboto" = windowsFont("Roboto"))
source("./src/ggplot_custom_theme.R")
load("./data/instcart_data_sample.RData")


orders$order_hour_of_day <- as.numeric(orders$order_hour_of_day)

# Productos comparados por departamento



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
