library(ggplot2)
library(dplyr)
library(tidyr)


options(scipen = 999, digits = 4)
windowsFonts("Roboto" = windowsFont("Roboto"))
source("./src/ggplot_custom_theme.R")
load("./data/instcart_data_sample.RData")



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

ggsave(filename = "./img/producto_por_departamento.png",
       plot = producto_port_depto )



# Top 10 pasillos
order_products %>%
  left_join(product_catalog, by = "product_id") %>%
  group_by(aisle) %>%
  count() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  top_n(n = 10) %>%
  ggplot(aes(y = n, x = reorder(aisle, n))) +
  geom_bar(stat = 'identity', fill = "#6F69AC") +
  labs(
    title =  "Productos comprados por pasillo",
  ) +
  ylab("Número de productos") +
  xlab("") +
  custom_style() +
  coord_flip()
