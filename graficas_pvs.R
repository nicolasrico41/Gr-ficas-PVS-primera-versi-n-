rm(list = ls()); gc()

library(ggplot2)
library(dplyr)
library(rADRES)
library(lubridate)
library(stringr)
library(readr)
library(ggversa)
library(openxlsx)
library(sf)
library(scales)

# Cargar datos ----

# Codigo para pc que no esté en proyecto

setwd("C:/Users/Usuario/OneDrive - ADRES/Bases de datos/PVS") # Acomodar

## Gráfica de relación afiliados malla validación y total afiliados contributivo----

Afiliados_EPS_mallas_suficiencia <- readr::read_csv("data/Afiliados_EPS_mallas_suficiencia.csv")
Afiliados_total_contributivo <- readr::read_csv("data/Afiliados_total_contributivo.csv")

Afiliados_total_contributivo_sufi <- Afiliados_total_contributivo %>%
  rename(total_afiliados_contri = TOTAL_CANTIDAD) %>% 
  filter(ANNO <= 2022)

Afiliados_EPS_mallas_suficiencia <- Afiliados_EPS_mallas_suficiencia %>%
  left_join(Afiliados_total_contributivo_sufi, by = c("ANNO", "MES"))

Afiliados_EPS_mallas_suficiencia <- Afiliados_EPS_mallas_suficiencia %>%
  mutate(fecha = as.Date(paste(ANNO, MES, 1, sep = "-"), format = "%Y-%m-%d"))

Afiliados_EPS_mallas_suficiencia_vline <- Afiliados_EPS_mallas_suficiencia %>%
  filter(MES == 1)

Afiliados_EPS_mallas_suficiencia <- Afiliados_EPS_mallas_suficiencia %>%
  mutate(Series = "No. de afiliados total\ndel régimen contributivo")

porcentaje_afilaidos_mallas <- Afiliados_EPS_mallas_suficiencia %>% 
  mutate(Porcentaje = TOTAL_CANTIDAD / total_afiliados_contri) %>% 
  group_by(ANNO) %>%
  summarize(Porcentaje = mean(Porcentaje, na.rm = TRUE))

labels_contri <- data.frame(
  fecha = as.Date(paste(2018:2022, 1, 1, sep = "-"), format = "%Y-%m-%d"),
  can_eps = c("9 EPS", "9 EPS", "9 EPS", "7 EPS", "4 EPS")) %>% 
  mutate("ANNO" = lubridate::year(fecha)) %>% 
  left_join(porcentaje_afilaidos_mallas, by = c("ANNO" = "ANNO")) %>% 
  select(-ANNO)

graf_pvs_mensual <- ggplot() +
  geom_area(data = Afiliados_EPS_mallas_suficiencia,
            aes(x = fecha, y = total_afiliados_contri, fill = Series),
            alpha = 0.5) +
  geom_line(
    data = Afiliados_EPS_mallas_suficiencia,
    aes(x = fecha, y = TOTAL_CANTIDAD, color = "No. de afiliados de EPS\nque pasaron la malla de\nvalidación de Suficiencia"),
    size = 1.1
  ) +
  geom_vline(
    data = Afiliados_EPS_mallas_suficiencia_vline,
    aes(xintercept = as.numeric(fecha)),
    color = "black",
    linetype = "dashed"
  ) +
  geom_text(
    data = labels_contri,
    aes(x = fecha, y = 0, label = paste(can_eps, " (", scales::percent(Porcentaje, accuracy = 0.1), ")")),
    angle = 90,
    vjust = 1.2,
    hjust = -0.02,
    color = "gray23"
  ) +
  labs(x = "",
       y = "",
       fill = "",
       color = "") +
  scale_fill_manual(values = c("No. de afiliados total\ndel régimen contributivo" = "#79E9E8")) +
  scale_color_manual(values = c("No. de afiliados de EPS\nque pasaron la malla de\nvalidación de Suficiencia" = "#273656")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 months") +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",",),
    breaks = seq(0, max(Afiliados_total_contributivo_sufi$total_afiliados_contri,
                        na.rm = TRUE),
                 by = 5000000)
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ),
  legend.position = "bottom")

graf_pvs_mensual <- rADRES::formatoADRES(graf_pvs_mensual)


ggsave(
  "plot/pvs_mensual.png",
  graf_pvs_mensual,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave("plot/pvs_mensual.svg",
       graf_pvs_mensual,
       width = 10,
       height = 6)

#### Total subsidiado y pvs #######################
#### (reconozco que no es PVS, pero al principio si, lo dejo por facilidad
### de adaptación al codigo anterior)

pvs_mesual_subsi <- read.csv("data/mensual_subsi.csv", sep = ",")

subsi_mensual <- read.csv("data/tot_subsi.csv", sep = ",")

subsi_mensual <- subsi_mensual %>%
  rename(total_subsi = TOTAL_CANTIDAD)

pvs_mesual_subsi <- pvs_mesual_subsi %>%
  left_join(subsi_mensual, by = c("ANNO", "MES"))

pvs_mesual_subsi <- pvs_mesual_subsi %>%
  mutate(fecha = as.Date(paste(ANNO, MES, 1, sep = "-"), format = "%Y-%m-%d"))

Afiliados_ESS_mallas_suficiencia_vline <- pvs_mesual_subsi %>%
  filter(MES == 1)

pvs_mesual_subsi <- pvs_mesual_subsi %>%
  mutate(Series = "Afiiación subsidiado")

porcentaje_afilaidos_malla_subsi <- pvs_mesual_subsi %>% 
  mutate(Porcentaje = TOTAL_CANTIDAD / total_subsi) %>% 
  group_by(ANNO) %>%
  summarize(Porcentaje = mean(Porcentaje, na.rm = TRUE))

labels_subsi <- data.frame(
  fecha = as.Date(paste(2018:2022, 1, 1, sep = "-"), format = "%Y-%m-%d"),
  can_eps = c("9 EPS", "4 EPS", "12 EPS", "9 EPS", "11 EPS")) %>% 
  mutate("ANNO" = lubridate::year(fecha)) %>% 
  left_join(porcentaje_afilaidos_malla_subsi, by = c("ANNO" = "ANNO")) %>% 
  select(-ANNO)

graf_pvs_mensual_subsi <- ggplot() +
  geom_area(data = pvs_mesual_subsi,
            aes(x = fecha, y = total_subsi, fill = Series),
            alpha = 0.5) +
  geom_line(
    data = pvs_mesual_subsi,
    aes(x = fecha, y = TOTAL_CANTIDAD, color = "Afiliados PVS"),
    size = 1.1
  ) +
  geom_vline(
    data = Afiliados_ESS_mallas_suficiencia_vline,
    aes(xintercept = as.numeric(fecha)),
    color = "black",
    linetype = "dashed"
  ) +
  geom_text(
    data = labels_subsi,
    aes(x = fecha, y = 0, label = paste(can_eps, " (", scales::percent(Porcentaje, accuracy = 0.1), ")")),
    angle = 90,
    vjust = 1.2,
    hjust = -0.02,
    color = "gray23"
  ) +
  labs(x = "",
       y = "",
       fill = "",
       color = "") +
  scale_fill_manual(values = c("Afiiación subsidiado" = "#79E9E8"),
                    labels = "Número de afiliados total\ndel subsidiado") +
  scale_color_manual(values = c("Afiliados PVS" = "#273656"),
                     labels = "Número de afiliados de EPS\nque pasaron la malla de\nvalidación de Suficiencia") +
  scale_x_date(date_labels = "%b", date_breaks = "1 months") +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ","),
    breaks = seq(0, max(pvs_mesual_subsi$total_subsi, na.rm = TRUE), by = 5000000),
    limits = c(0, NA)
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ),
  legend.position = "bottom")

graf_pvs_mensual_subsi <- rADRES::formatoADRES(graf_pvs_mensual_subsi)

ggsave(
  "plot/pvs_mensual_subsi.svg",
  graf_pvs_mensual_subsi,
  width = 10,
  height = 6
)

ggsave(
  "plot/pvs_mensual_subsi.png",
  graf_pvs_mensual_subsi,
  width = 10,
  height = 6,
  dpi = 540
)


### Cargar datos, graficas excel Diego

tabla_pvs <- readxl::read_excel("xlsx/tablas_boletin_pvs_suficiencia_ok.xlsx",
                                sheet = "hoja_pvs_r")

colores <- c(
  "Mismo Grupo Económico PMP - EPS" = "#273656",
  "Diferente Grupo Económico PMP - EPS" = "#79E9E8",
  "Sin PVS - Reg. Con." = "#4E67B4",
  "PAC" = "#838B8B",
  "Reg. Sub." = "#0BDAB2",
  "Reg. Con." = "#9BCD9B",
  "Total" = "#008B8B"
)

colores_totales <- c(
  "Reg. Sub." = "#273656",
  "Reg. Con." = "#79E9E8"
)


# paleta_adres =c("#273656","#79E9E8","#4E67B4","#0BDAB2"
## Gráfica per capita ----

graf_per_capita <- ggplot(data = filter(tabla_pvs, `Tipo de PVS` != "Total")
                                        , aes(x = factor(Año), y = `Valor per-capita`, fill = `Tipo de PVS`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "") +
  scale_fill_manual(values = colores, name = "") +
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "$",
    big.mark = ".",
    decimal.mark = ","
  )) +
  theme_classic() +
  theme(legend.position = "bottom") +
  geom_text(
    aes(
      label = scales::dollar(
        `Valor per-capita`,
        prefix = "$",
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    position = position_dodge(width = 0.9),
    vjust = 0.7,
    size = 3,
    angle = 90,
    color = "#eeeeee",
    hjust = 1.1
  )


ggsave(
  "plot/pvs_per_capita.png",
  graf_per_capita,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave(
  "plot/pvs_per_capita.svg",
  graf_per_capita,
  width = 10,
  height = 6
)

## Gráfica variación per capita ----

graf_var_per_capita <- ggplot(filter(tabla_pvs, `Tipo de PVS` != "Sin PVS - Reg. Con."),
                              aes(x = factor(Año), y = `Variación porcentual percapita`, fill = `Tipo de PVS`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "") +
  scale_fill_manual(values = colores, name = "") +
  scale_y_continuous(
    labels = scales::percent_format(# scale = 1,
      big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal() +
  guides(color = guide_legend(nrow = 2)) +
  theme(legend.position = "bottom") +
  geom_text(
    aes(
      label = scales::percent(
        `Variación porcentual percapita`,
        suffix = "",
        big.mark = ".",
        decimal.mark = ",",
        accuracy = 0.1
      )
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.7,
    size = 3,
    # angle = 90,
    color = "#000000",
    # hjust = 1.1
  )

graf_var_per_capita <- rADRES::formatoADRES(graf_var_per_capita)

ggsave(
  "plot/pvs_var_per_capita.png",
  graf_var_per_capita,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave(
  "plot/pvs_var_per_capita.svg",
  graf_var_per_capita,
  width = 10,
  height = 6
)

# ## Razón entre la participación del gasto y el porcentaje de usuarios
# 
# divisores <- tabla_pvs %>%
#   filter(Tipo.de.PVS == "Total Contributivo") %>%
#   group_by(Año) %>%
#   summarize(denom_sufi = Valor.total.suminsitrado * 1, demon_usua = No..de.usuarios * 1)
# 
# tabla_razon <- tabla_pvs %>%
#   left_join(divisores, by = "Año") %>%
#   mutate(
#     raz_sufi = Valor.total.suminsitrado / denom_sufi,
#     raz_usua = No..de.usuarios / demon_usua,
#     razon = raz_sufi / raz_usua
#   )
# 
# tabla_razon_transformed <- tabla_razon %>%
#   mutate(razon_adjusted = razon - 1)


## Grafico del índice de equidad del gasto ----
graf_razon_grupos <- ggplot(
  filter(tabla_pvs, `Tipo de PVS` != "Reg. Con." &
           `Tipo de PVS` != "Reg. Sub."&
           `Tipo de PVS` != "Total"),
  aes(x = factor(Año), y = `Índice de Equidad de Gasto` - 1, fill = `Tipo de PVS`)
) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_line(size = 1) +
  labs(x = "", y = "") +
  scale_fill_manual(values = colores, name = "") +
  scale_y_continuous(
    # breaks = y_breaks,
    labels = function(x)
      x + 1#,
    # limits = c(y_min_gr, ceiling(y_max_gr / 0.12) * 0.12),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2)) +
  geom_text(
    aes(
      label = scales::number(
        `Índice de Equidad de Gasto`,
        suffix = "",
        big.mark = ".",
        decimal.mark = ",",
        accuracy = 0.01
      )
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.7,
    size = 3,
    # angle = 90,
    color = "#000000",
    # hjust = 1.1
  )

graf_razon_grupos <- rADRES::formatoADRES(graf_razon_grupos)

ggsave(
  "plot/pvs_razon_grupos.png",
  graf_razon_grupos,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  "plot/pvs_razon_grupos.svg",
  graf_razon_grupos,
  width = 10,
  height = 6
)

## Ahora se realiza con los totales (contributivo y subsidiado) ----

# Crear el gráfico
graf_razon_tot <- ggplot(
  filter(tabla_pvs, `Tipo de PVS` == "Reg. Con." |
           `Tipo de PVS` == "Reg. Sub."),
  aes(x = factor(Año), y = `Índice de Equidad de Gasto` - 1, fill = `Tipo de PVS`)
) +
  geom_bar(stat = "identity", position = "dodge") +
  #geom_line(size = 1) +
  labs(x = "", y = "") +
  scale_fill_manual(values = colores_totales, name = "") +
  scale_y_continuous(
    # breaks = y_breaks,
    labels = function(x)
      x + 1#,
    # limits = c(y_min_gr, ceiling(y_max_gr / 0.12) * 0.12),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  geom_text(
    aes(
      label = scales::number(
        `Índice de Equidad de Gasto`,
        suffix = "",
        big.mark = ".",
        decimal.mark = ",",
        accuracy = 0.01
      )
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.7,
    size = 3,
    # angle = 90,
    color = "#000000",
    # hjust = 1.1
  )

graf_razon_tot <- rADRES::formatoADRES(graf_razon_tot)

ggsave(
  "plot/pvs_razon_totales.png",
  graf_razon_tot,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave(
  "plot/pvs_razon_totales.svg",
  graf_razon_tot,
  width = 10,
  height = 6
)


## Gráfico de lineas de índice de equidad (por tipo PVS) ----
graf_razon_grupos_line <- ggplot(
  filter(tabla_pvs, `Tipo de PVS` != "Reg. Con." &
           `Tipo de PVS` != "Reg. Sub."&
           `Tipo de PVS` != "Total"),
  aes(x = Año, y = `Índice de Equidad de Gasto` - 1, colour = `Tipo de PVS`)
) +
  geom_line(size = 1) +
  labs(x = "", y = "") +
  scale_colour_manual(values = colores, name = "") +
  scale_y_continuous(
    # breaks = y_breaks,
    labels = function(x)
      x + 1#,
    # limits = c(y_min_gr, ceiling(y_max_gr / 0.12) * 0.12),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggrepel::geom_text_repel(
    aes(
      label = scales::number(
        `Índice de Equidad de Gasto`,
        suffix = "",
        big.mark = ".",
        decimal.mark = ",",
        accuracy = 0.01
      )
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.7,
    size = 3,
    # angle = 90,
    color = "#000000",
    # hjust = 1.1
  )

graf_razon_grupos_line <- rADRES::formatoADRES(graf_razon_grupos_line)

ggsave(
  "plot/pvs_razon_grupos_line.png",
  graf_razon_grupos_line,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  "plot/pvs_razon_grupos_line.svg",
  graf_razon_grupos_line,
  width = 10,
  height = 6
)

## Gráfico de lineas de índice de equidad (por tipo régimen) ----

graf_razon_grupos_line_tot <- ggplot(
  filter(tabla_pvs, `Tipo de PVS` == "Reg. Con." |
           `Tipo de PVS` == "Reg. Sub."),
  aes(x = Año, y = `Índice de Equidad de Gasto` - 1, colour = `Tipo de PVS`)
) +
  geom_line(size = 1) +
  labs(x = "", y = "") +
  scale_colour_manual(values = colores_totales, name = "") +
  scale_y_continuous(
    # breaks = y_breaks,
    labels = function(x)
      x + 1#,
    # limits = c(y_min_gr, ceiling(y_max_gr / 0.12) * 0.12),
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ggrepel::geom_text_repel(
    aes(
      label = scales::number(
        `Índice de Equidad de Gasto`,
        suffix = "",
        big.mark = ".",
        decimal.mark = ",",
        accuracy = 0.01
      )
    ),
    position = position_dodge(width = 0.9),
    vjust = -0.7,
    size = 3,
    # angle = 90,
    color = "#000000",
    # hjust = 1.1
  )

graf_razon_grupos_line_tot <- rADRES::formatoADRES(graf_razon_grupos_line_tot)

ggsave("plot/pvs_razon_total_line.png",
       graf_razon_grupos_line_tot,
       width = 10,
       height = 6, dpi = 300)

ggsave("plot/pvs_razon_total_line.svg",
       graf_razon_grupos_line_tot,
       width = 10,
       height = 6)

## Gráfico de líneas valor per cápita ----

lineas_valor_perca <- ggplot(
  filter(tabla_pvs, `Tipo de PVS` != "Total"),
  aes(x = Año, y = `Valor per-capita`, colour = `Tipo de PVS`)
) +
  geom_line(size = 1) +
  labs(x = "", y = "") +
  scale_colour_manual(values = colores, name = "") +
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "$",
    big.mark = ".",
    decimal.mark = ","
  )) +
  theme_minimal() +
  theme(legend.position = "bottom") #+
  # ggrepel::geom_text_repel(
  #   aes(
  #     label = scales::dollar(`Valor per-capita`,
  #                            prefix = "",
  #                            big.mark = ".",
  #                            decimal.mark = ",")
  #   ),
  #   position = position_dodge(width = 0.9),
  #   vjust = -0.7,
  #   size = 3,
  #   color = "#000000"
  # )

lineas_valor_perca <- rADRES::formatoADRES(lineas_valor_perca)

ggsave("plot/lineas_valor_perca.png",
       lineas_valor_perca,
       width = 10,
       height = 6,
       dpi = 300)

ggsave("plot/lineas_valor_perca.svg",
       lineas_valor_perca,
       width = 10,
       height = 6)

## Gráfico de líneas variación per cápita  ----

lineas_variacion_perca <- ggplot(
  filter(tabla_pvs, `Tipo de PVS` != "Total" & `Tipo de PVS` != "Sin PVS - Reg. Con."),
  aes(x = Año, y = `Variación porcentual percapita`, colour = `Tipo de PVS`)
) +
  geom_line(size = 1) +
  labs(x = "", y = "") +
  scale_colour_manual(values = colores, name = "") +
  scale_y_continuous(
    labels = scales::percent_format(# scale = 1,
      big.mark = ".", decimal.mark = ",")
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
ggrepel::geom_text_repel(
  aes(
    label = scales::percent(
      `Variación porcentual percapita`,
      suffix = "",
      big.mark = ".",
      decimal.mark = ",",
      accuracy = 0.1
    )
  ),
  position = position_dodge(width = 0.9),
  vjust = -0.7,
  size = 3,
  # angle = 90,
  color = "#000000",
  # hjust = 1.1
)

lineas_variacion_perca <- rADRES::formatoADRES(lineas_variacion_perca)

ggsave("plot/lineas_variacion_perca.png",
       lineas_variacion_perca,
       width = 10,
       height = 6,
       dpi = 300)

ggsave("plot/lineas_variacion_perca.svg",
       lineas_variacion_perca,
       width = 10,
       height = 6)

## Gráfico línea a precios constantes dic 2023 ----

ipc_2018_2023 <- ipc %>% 
  filter(Fecha >= "2018-01-01")

ipc_anual_base <- ipc_anual %>% 
  filter(year(Fecha) >= 2018 & year(Fecha) <= 2022) %>% 
  mutate(Año = year(Fecha))

tabla_pvs  <-  tabla_pvs %>% 
  left_join(ipc_anual_base, by = "Año") %>% 
  mutate(valor_per_cap_cons = `Valor per-capita` * Deflactor)

lineas_valor_perca_cons <- ggplot(
  filter(tabla_pvs, `Tipo de PVS` != "Total"),
  aes(x = Año, y = valor_per_cap_cons, colour = `Tipo de PVS`)
) +
  geom_line(size = 1) +
  labs(x = "", y = "") +
  scale_colour_manual(values = colores, name = "") +
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "$",
    big.mark = ".",
    decimal.mark = ","
  )) +
  theme_minimal() +
  theme(legend.position = "bottom") #+
# ggrepel::geom_text_repel(
#   aes(
#     label = scales::dollar(`Valor per-capita`,
#                            prefix = "",
#                            big.mark = ".",
#                            decimal.mark = ",")
#   ),
#   position = position_dodge(width = 0.9),
#   vjust = -0.7,
#   size = 3,
#   color = "#000000"
# )

lineas_valor_perca_cons <- rADRES::formatoADRES(lineas_valor_perca_cons)

ggsave("plot/lineas_valor_perca_cons.png",
       lineas_valor_perca_cons,
       width = 10,
       height = 6,
       dpi = 300)

ggsave("plot/lineas_valor_perca_cons.svg",
       lineas_valor_perca_cons,
       width = 10,
       height = 6)

## Gráfico barras per cápita a precios constantes dic-2023

graf_per_capita_cons <- ggplot(data = filter(tabla_pvs,`Tipo de PVS` != "Total"),
                               aes(x = factor(Año),
                                                     y = valor_per_cap_cons,
                                                fill = `Tipo de PVS`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "") +
  scale_fill_manual(values = colores, name = "") +
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "$",
    big.mark = ".",
    decimal.mark = ","
  )) +
  theme_classic() +
  theme(legend.position = "bottom") +
  geom_text(
    aes(
      label = scales::dollar(
        valor_per_cap_cons,
        prefix = "$",
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    position = position_dodge(width = 0.9),
    vjust = 0.7,
    size = 3,
    angle = 90,
    color = "#eeeeee",
    hjust = 1.1
  )


ggsave(
  "plot/graf_per_capita_cons.png",
  graf_per_capita_cons,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave(
  "plot/graf_per_capita_cons.svg",
  graf_per_capita_cons,
  width = 10,
  height = 6
)

## Gráfica regimen contributivo y PVS

mensual_pvs <- readr::read_csv("data/No_de_afiliados_mes_anno_PVS.csv",
                              locale = locale(encoding = "UTF-8"))

mensual_pvs <- mensual_pvs %>% 
  mutate(
    ANNO = str_extract(anio_mes,"\\d+(?=-)"),
    MES = str_extract(anio_mes, "(?<=-)\\d+"),
    ANNO = as.numeric(ANNO),
    MES = as.numeric(MES)
  ) %>% 
  dplyr::select(-anio_mes)

mensual_pvs <- mensual_pvs %>%
  mutate(fecha = as.Date(paste(ANNO, MES, 1, sep = "-"), format = "%Y-%m-%d")) %>% 
  filter(fecha <= "2023-12-01")

Afiliados_total_contributivo <- Afiliados_total_contributivo %>% 
  filter(ANNO <= 2023)

percent_pvs <- mensual_pvs %>% 
  left_join(Afiliados_total_contributivo, by = c("ANNO","MES")) %>% 
  mutate(per_pvs = num_afiliados / TOTAL_CANTIDAD) %>% 
  group_by(ANNO) %>% 
  dplyr::summarise(prom_pvs = mean(per_pvs, na.rm = T)) %>% 
  mutate(fecha = as.Date(paste(ANNO,1,1, sep = "-"), format = "%Y-%m-%d"))

Afiliados_total_contributivo <- Afiliados_total_contributivo %>%
  mutate(fecha = as.Date(paste(ANNO,MES,1, sep = "-"), format = "%Y-%m-%d"),
         Series = "No. de afiliados total\ndel régimen contributivo")

linea_mes_enero <- Afiliados_total_contributivo %>% 
  filter(MES == 1)
  
  
graf_mensual_pvs <- ggplot() +
  geom_area(data = Afiliados_total_contributivo,
            aes(x = fecha, y = TOTAL_CANTIDAD, fill = Series),
            alpha = 0.5) +
  geom_line(
    data = mensual_pvs,
    aes(x = fecha, y = num_afiliados, color = "No. de afiliados a PVS"),
    size = 1.1
  ) +
  geom_vline(
    data = linea_mes_enero,
    aes(xintercept = as.numeric(fecha)),
    color = "black",
    linetype = "dashed"
  ) +
  geom_text(
    data = percent_pvs,
    aes(x = fecha, y = 0, label = scales::percent(prom_pvs, accuracy = 0.1)),
    angle = 90,
    vjust = 1.2,
    hjust = -1.5, #estaba -0.02
    color = "gray23"
  ) +
  labs(x = "",
       y = "",
       fill = "",
       color = "") +
  scale_fill_manual(values = c("No. de afiliados total\ndel régimen contributivo" = "#79E9E8")) +
  scale_color_manual(values = c("No. de afiliados a PVS" = "#273656")) +
  scale_x_date(date_labels = "%b", date_breaks = "1 months") +
  scale_y_continuous(
    labels = scales::comma_format(big.mark = ".", decimal.mark = ",",),
    breaks = seq(0, max(Afiliados_total_contributivo$TOTAL_CANTIDAD,
                        na.rm = TRUE),
                 by = 2000000)
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(
    angle = 90,
    vjust = 0.5,
    hjust = 1
  ),
  legend.position = "bottom")

graf_mensual_pvs <- rADRES::formatoADRES(graf_mensual_pvs)


ggsave(
  "plot/graf_mensual_pvs.png",
  graf_mensual_pvs,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave("plot/graf_mensual_pvs.svg",
       graf_mensual_pvs,
       width = 10,
       height = 6)

## Grafica per cápita por sexo ----

sufi_sexo <- readr::read_csv("data/sufi_pvs_sexo.csv")

sufi_sexo <- sufi_sexo %>% 
  mutate(per_capita = valor_total/usuarios_unicos)

# Se grafica para los hombres

graf_per_capita_hombre <- ggplot(data = filter(sufi_sexo, sexo == "M")
                          , aes(x = factor(ANNO), y = per_capita, fill = Tipo_PVS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "") +
  scale_fill_manual(values = colores, name = "") +
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "$",
    big.mark = ".",
    decimal.mark = ","
  )) +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2)) +
  geom_text(
    aes(
      label = scales::dollar(
        per_capita,
        prefix = "$",
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    position = position_dodge(width = 0.9),
    vjust = 0.7,
    size = 3,
    angle = 90,
    color = "#eeeeee",
    hjust = 1.1
  )

graf_per_capita_hombre <- rADRES::formatoADRES(graf_per_capita_hombre)

ggsave(
  "plot/pvs_per_capita_hombre.png",
  graf_per_capita_hombre,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave(
  "plot/pvs_per_capita_hombre.svg",
  graf_per_capita_hombre,
  width = 10,
  height = 6
)

# Se grafica para mujeres

graf_per_capita_mujer <- ggplot(data = filter(sufi_sexo, sexo == "F")
                                 , aes(x = factor(ANNO), y = per_capita, fill = Tipo_PVS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "") +
  scale_fill_manual(values = colores, name = "") +
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "$",
    big.mark = ".",
    decimal.mark = ","
  )) +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2)) +
  geom_text(
    aes(
      label = scales::dollar(
        per_capita,
        prefix = "$",
        big.mark = ".",
        decimal.mark = ","
      )
    ),
    position = position_dodge(width = 0.9),
    vjust = 0.7,
    size = 3,
    angle = 90,
    color = "#eeeeee",
    hjust = 1.1
  )

graf_per_capita_mujer <- rADRES::formatoADRES(graf_per_capita_mujer)

ggsave(
  "plot/pvs_per_capita_mujer.png",
  graf_per_capita_mujer,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave(
  "plot/pvs_per_capita_mujer.svg",
  graf_per_capita_mujer,
  width = 10,
  height = 6
)

## Grafico por grupos etários -----

grpo_etario <- readr::read_csv("data/sufi_pvs_edad.csv")

grpo_etario <- grpo_etario %>% 
  mutate(per_capita = valor_total/usuarios_unicos,
         decada = if_else(decada == "+ 90", "Más de 90",decada))


graf_per_capita_grupoeta <- ggplot(grpo_etario, aes(x = ANNO,
                                                    y = per_capita,
                                                    fill = Tipo_PVS)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ decada,
             scales = "free_y") +
  scale_fill_manual(values = colores) +
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "$",
    big.mark = ".",
    decimal.mark = ","
  )) +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

graf_per_capita_grupoeta <- rADRES::formatoADRES(graf_per_capita_grupoeta)

ggsave(
  "plot/graf_per_capita_grupoeta.png",
  graf_per_capita_grupoeta,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave(
  "plot/graf_per_capita_grupoeta.svg",
  graf_per_capita_grupoeta,
  width = 10,
  height = 6
)

## Gráfico por grupos etarios, masculino ----

sufi_sexo_edad <- readr::read_csv("data/sufi_pvs_edad_sexo.csv")

sufi_sexo_edad <- sufi_sexo_edad %>% 
  mutate(per_capita = valor_total/usuarios_unicos,
         decada = if_else(decada == "+ 90", "Más de 90", decada))

graf_grupoeta_hombres <- ggplot(filter(sufi_sexo_edad,
                                               sexo == "M")
                                        ,aes(x = ANNO,
                                         y = per_capita,
                                         fill = Tipo_PVS)) +  
geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ decada,
             scales = "free_y") +
  scale_fill_manual(values = colores) +
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "$",
    big.mark = ".",
    decimal.mark = ","
  )) +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

graf_grupoeta_hombres <- rADRES::formatoADRES(graf_grupoeta_hombres)

ggsave(
  "plot/graf_grupoeta_hombres.png",
  graf_grupoeta_hombres,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave(
  "plot/graf_grupoeta_hombres.svg",
  graf_grupoeta_hombres,
  width = 10,
  height = 6
)

## Gráfico por grupos etarios, femenino ----

graf_grupoeta_mujeres <- ggplot(filter(sufi_sexo_edad,
                                       sexo == "F")
                                ,aes(x = ANNO,
                                     y = per_capita,
                                     fill = Tipo_PVS)) +  
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ decada,
             scales = "free_y") +
  scale_fill_manual(values = colores) +
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "$",
    big.mark = ".",
    decimal.mark = ","
  )) +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

graf_grupoeta_mujeres <- rADRES::formatoADRES(graf_grupoeta_mujeres)

ggsave(
  "plot/graf_grupoeta_mujeres.png",
  graf_grupoeta_mujeres,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave(
  "plot/graf_grupoeta_mujeres.svg",
  graf_grupoeta_mujeres,
  width = 10,
  height = 6
)

## Gráfica per cápita por EPS ----

sufi_eps <- readr::read_csv("data/sufi_pvs_eps.csv")

name_eps <- read.xlsx("xlsx/cod_eps.xlsx")

sufi_eps <- sufi_eps %>% 
  mutate(per_capita = valor_total/usuarios_unicos) %>% 
  left_join(name_eps, by = c("STR_COD_EPS" = "COD.EPS"))

graf_eps <- ggplot(sufi_eps,
                                aes(x = ANNO,
                                     y = per_capita,
                                     fill = Tipo_PVS)) +  
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ EPS,
             scales = "free_y") +
  scale_fill_manual(values = colores) +
  scale_y_continuous(labels = scales::dollar_format(
    prefix = "$",
    big.mark = ".",
    decimal.mark = ","
  )) +
  labs(title = "",
       x = "",
       y = "",
       fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2))

graf_eps <- rADRES::formatoADRES(graf_eps)

ggsave(
  "plot/graf_eps.png",
  graf_eps,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave(
  "plot/graf_eps.svg",
  graf_eps,
  width = 10,
  height = 6
)

## Gráfico per cápita por departamento (sin PVS) ----

sufi_dpto <- readr::read_csv("data/sufi_pvs_dpto.csv")

shape_dpto <- rADRES::mapaColombiaDept_sd

colores_dpto <- c(
  "Menor a $250.000" = "#838B8B",
  "Entre $250.001 y $500.000" = "#79E9E8",
  "Entre $500.001 y $750.000" = "#0BDAB2",
  "Entre $750.001 y $1.000.000" = "#4E67B4",
  "Más de $1.000.000" = "#273656",
  "Sin datos" = "white"
)

sufi_dpto <- sufi_dpto %>% 
  filter(!grepl("[a-zA-Z]", cod_dpto)) %>% 
  mutate(per_capita = valor_total/usuarios_unicos)
  
  shape_dpto <- st_as_sf(shape_dpto)
  
# sufi_dpto <- st_as_sf(sufi_dpto)
  
  # sufi_dpto_sf <- sufi_dpto %>%
  #   st_join(shape_dpto, by = c("cod_dpto" = "DPTO")) 

  
  # Filtrar y transformar los datos
  grafico_dpto_sin_pvs <- sufi_dpto %>% 
    filter(Tipo_PVS == "Sin PVS - Reg. Con.") %>% 
    group_by(ANNO, cod_dpto) %>% 
    summarize(tot_dpto = mean(per_capita, na.rm = TRUE)) %>% 
    mutate(clasificacion = case_when(
      tot_dpto <= 250000 ~ 1,
      tot_dpto > 250000 & tot_dpto <= 500000 ~ 2,
      tot_dpto > 500000 & tot_dpto <= 750000 ~ 3,
      tot_dpto > 750000 & tot_dpto <= 1000000 ~ 4,
      TRUE ~ 5
    )) %>%
    mutate(
      clasificacion = factor(
        clasificacion,
        levels = 1:5,
        labels = c(
          "Menor a $250.000",
          "Entre $250.001 y $500.000",
          "Entre $500.001 y $750.000",
          "Entre $750.001 y $1.000.000",
          "Más de $1.000.000"
        )
        )
    )
  
  # Asegurarse de que grafico_dpto_sin_pvs sea un objeto sf
  grafico_dpto_sin_pvs_sf <- left_join(grafico_dpto_sin_pvs, shape_dpto, by = c("cod_dpto" = "DPTO")) %>%
    st_as_sf()
  
  # Crear el gráfico
grafico_dpto_sinpvs <-  ggplot(grafico_dpto_sin_pvs_sf) +
    geom_sf(aes(fill = clasificacion)) +
    scale_fill_manual(values = colores_dpto) +
    facet_wrap(~ANNO) +
    theme_minimal() +
    labs(title = "",
         fill = "",
         x = "",
         y = "") 

  
grafico_dpto_sinpvs <- rADRES::formatoADRES(grafico_dpto_sinpvs) +
  theme_void() + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"), 
        plot.background = element_rect(fill = "white"),
        legend.position = c(0.98, 0.125),  
        legend.justification = c(1, 0),  
        legend.box.margin = margin(10, 10, 10, 10))

ggsave(
  "plot/grafico_dpto_sinpvs.png",
  grafico_dpto_sinpvs,
  width = 10,
  height = 6,
  dpi = 540
)

ggsave(
  "plot/grafico_dpto_sinpvs.svg",
  grafico_dpto_sinpvs,
  width = 10,
  height = 6
)

## Gráfico per cápita por departamento (mismo grupo económico) ----

# Filtrar y transformar los datos
grafico_dpto_mismo_ge <- sufi_dpto %>% 
  filter(Tipo_PVS == "Mismo Grupo Económico PMP - EPS") %>% 
  group_by(ANNO, cod_dpto) %>% 
  summarize(tot_dpto = mean(per_capita, na.rm = TRUE)) %>% 
  mutate(clasificacion = case_when(
    tot_dpto <= 250000 ~ 1,
    tot_dpto > 250000 & tot_dpto <= 500000 ~ 2,
    tot_dpto > 500000 & tot_dpto <= 750000 ~ 3,
    tot_dpto > 750000 & tot_dpto <= 1000000 ~ 4,
    TRUE ~ 5
  )) %>%
  mutate(
    clasificacion = factor(
      clasificacion,
      levels = 1:5,
      labels = c(
        "Menor a $250.000",
        "Entre $250.001 y $500.000",
        "Entre $500.001 y $750.000",
        "Entre $750.001 y $1.000.000",
        "Más de $1.000.000"
      )
    )
  )

# grafico_dpto_mismo_ge_completo <- departamentos_completos %>%
#   left_join(grafico_dpto_mismo_ge, by = c("ANNO", "DPTO" = "cod_dpto"))

# Crear una lista vacía para almacenar los shapes con la columna de año
shapes_por_año <- list()

# Bucle para crear una copia del shape por cada año de 2018 a 2022 y agregar la columna de año
for (año in 2018:2022) {
  shape_temp <- shape_dpto
  shape_temp$año <- año
  shapes_por_año[[as.character(año)]] <- shape_temp
}


# Combinar todos los shapes en uno solo
shape_final <- do.call(rbind, shapes_por_año)

grafico_dpto_mismo_ge_sf <- shape_final %>%
  left_join(grafico_dpto_mismo_ge, by = c("DPTO" = "cod_dpto",
                                          "año" = "ANNO")) %>% 
  st_as_sf()

grafico_dpto_mismo_ge_sf <- grafico_dpto_mismo_ge_sf %>%
  mutate(clasificacion = ifelse(is.na(clasificacion), "6", clasificacion))

grafico_dpto_mismo_ge_sf <- grafico_dpto_mismo_ge_sf %>% 
  mutate(clasificacion = factor(clasificacion,
                                 levels = 1:6,
                                 labels = c(
                                   "Menor a $250.000",
                                   "Entre $250.001 y $500.000",
                                   "Entre $500.001 y $750.000",
                                   "Entre $750.001 y $1.000.000",
                                   "Más de $1.000.000",
                                   "Sin datos"
                                 )))
# Crear el gráfico
grafico_dpto_mismoge <-  ggplot(grafico_dpto_mismo_ge_sf) +
  geom_sf(aes(fill = clasificacion)) +
  scale_fill_manual(values = colores_dpto) +
  facet_wrap(~año) +
  theme_minimal() +
  labs(title = "",
       fill = "",
       x = "",
       y = "") 


grafico_dpto_mismoge <- rADRES::formatoADRES(grafico_dpto_mismoge) +
  theme_void() + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"), 
        plot.background = element_rect(fill = "white"),
        legend.position = c(0.98, 0.125),  
        legend.justification = c(1, 0),  
        legend.box.margin = margin(10, 10, 10, 10))

ggsave("plot/grafico_dpto_mismoge.png",
       grafico_dpto_mismoge,
       width = 10,
       height = 6,
       dpi = 540)

ggsave(
  "plot/grafico_dpto_mismoge.svg",
  grafico_dpto_mismoge,
  width = 16,
  height = 9
)

## Gráfico per cápita por departamento (diferente grupo económico) ----

grafico_dpto_diferente_ge <- sufi_dpto %>% 
filter(Tipo_PVS == "Diferente Grupo Económico PMP - EPS") %>% 
  group_by(ANNO, cod_dpto) %>% 
  summarize(tot_dpto = mean(per_capita, na.rm = TRUE)) %>% 
  mutate(clasificacion = case_when(
    tot_dpto <= 250000 ~ 1,
    tot_dpto > 250000 & tot_dpto <= 500000 ~ 2,
    tot_dpto > 500000 & tot_dpto <= 750000 ~ 3,
    tot_dpto > 750000 & tot_dpto <= 1000000 ~ 4,
    TRUE ~ 5
  )) %>%
  mutate(
    clasificacion = factor(
      clasificacion,
      levels = 1:5,
      labels = c(
        "Menor a $250.000",
        "Entre $250.001 y $500.000",
        "Entre $500.001 y $750.000",
        "Entre $750.001 y $1.000.000",
        "Más de $1.000.000"
      )
    )
  )


# Asegurarse de que grafico_dpto_sin_pvs sea un objeto sf
grafico_dpto_diferente_ge_sf <- shape_final %>%
  left_join(grafico_dpto_diferente_ge, by = c("DPTO" = "cod_dpto",
                                              "año" = "ANNO")) %>% 
  st_as_sf()

grafico_dpto_diferente_ge_sf <- grafico_dpto_diferente_ge_sf %>%
  mutate(clasificacion = ifelse(is.na(clasificacion), "6", clasificacion))

grafico_dpto_diferente_ge_sf <- grafico_dpto_diferente_ge_sf %>% 
  mutate(clasificacion = factor(clasificacion,
                                levels = 1:6,
                                labels = c(
                                  "Menor a $250.000",
                                  "Entre $250.001 y $500.000",
                                  "Entre $500.001 y $750.000",
                                  "Entre $750.001 y $1.000.000",
                                  "Más de $1.000.000",
                                  "Sin datos"
                                )))
# Crear el gráfico
grafico_dpto_diferentege <-  ggplot(grafico_dpto_diferente_ge_sf) +
  geom_sf(aes(fill = clasificacion)) +
  scale_fill_manual(values = colores_dpto) +
  facet_wrap(~año) +
  theme_minimal() +
  labs(title = "",
       fill = "",
       x = "",
       y = "")


grafico_dpto_diferentege <- rADRES::formatoADRES(grafico_dpto_diferentege) +
  theme_void() +
  theme(plot.margin = unit(c(0,0,0,0), "cm"), 
      plot.background = element_rect(fill = "white"),
      legend.position = c(0.98, 0.125),  
      legend.justification = c(1, 0),  
      legend.box.margin = margin(10, 10, 10, 10))

ggsave("plot/grafico_dpto_diferentege.png",
       grafico_dpto_diferentege,
       width = 10,
       height = 6,
       dpi = 540)

ggsave(
  "plot/grafico_dpto_diferentege.svg",
  grafico_dpto_diferentege,
  width = 16,
  height = 9
)

## Gráfico per cápita por departamento (PAC) ----

grafico_dpto_pac <- sufi_dpto %>% 
  filter(Tipo_PVS == "PAC") %>% 
  group_by(ANNO, cod_dpto) %>% 
  summarize(tot_dpto = mean(per_capita, na.rm = TRUE)) %>% 
  mutate(clasificacion = case_when(
    tot_dpto <= 250000 ~ 1,
    tot_dpto > 250000 & tot_dpto <= 500000 ~ 2,
    tot_dpto > 500000 & tot_dpto <= 750000 ~ 3,
    tot_dpto > 750000 & tot_dpto <= 1000000 ~ 4,
    TRUE ~ 5
  )) %>%
  mutate(
    clasificacion = factor(
      clasificacion,
      levels = 1:5,
      labels = c(
        "Menor a $250.000",
        "Entre $250.001 y $500.000",
        "Entre $500.001 y $750.000",
        "Entre $750.001 y $1.000.000",
        "Más de $1.000.000"
      )
    )
  )


# Asegurarse de que grafico_dpto_sin_pvs sea un objeto sf
grafico_dpto_pac_sf <- shape_final %>%
  left_join(grafico_dpto_pac, by = c("DPTO" = "cod_dpto", "año" = "ANNO")) %>% 
  st_as_sf()

grafico_dpto_pac_sf <- grafico_dpto_pac_sf %>%
  mutate(clasificacion = ifelse(is.na(clasificacion), "6", clasificacion))

grafico_dpto_pac_sf <- grafico_dpto_pac_sf %>% 
  mutate(clasificacion = factor(clasificacion,
                                levels = 1:6,
                                labels = c(
                                  "Menor a $250.000",
                                  "Entre $250.001 y $500.000",
                                  "Entre $500.001 y $750.000",
                                  "Entre $750.001 y $1.000.000",
                                  "Más de $1.000.000",
                                  "Sin datos"
                                )))
# Crear el gráfico
grafico_dpto_pacc <-  ggplot(grafico_dpto_pac_sf) +
  geom_sf(aes(fill = clasificacion)) +
  scale_fill_manual(values = colores_dpto) +
  facet_wrap(~año) +
  theme_minimal() +
  labs(title = "",
       fill = "",
       x = "",
       y = "")

grafico_dpto_pacc <- rADRES::formatoADRES(grafico_dpto_pacc) +
  theme_void() + 
  theme(plot.margin = unit(c(0,0,0,0), "cm"), 
        plot.background = element_rect(fill = "white"),
        legend.position = c(0.98, 0.125),  
        legend.justification = c(1, 0),  
        legend.box.margin = margin(10, 10, 10, 10))

ggsave("plot/grafico_dpto_pacc.png",
       grafico_dpto_pacc,
       width = 10,
       height = 6,
       dpi = 540)

ggsave(
  "plot/grafico_dpto_pacc.svg",
  grafico_dpto_pacc,
  width = 10,
  height = 6
)

###### Ejercicio conteo

grupo_20_30 <- grpo_etario %>% 
  filter(Tipo_PVS != "Sin PVS - Reg. Con.",
         ANNO == 2022) %>%
  mutate(total = sum(usuarios_unicos)) %>% 
  group_by(decada) %>% 
  summarize(per_usu = (sum(usuarios_unicos)/total)*100) %>% 
  distinct(decada, per_usu)

## Distribución afiliados por EPS ----

cant_usu_eps <- sufi_eps %>% 
  group_by(ANNO, EPS) %>% 
  summarize(usuarios = sum(usuarios_unicos, na.rm = T)) %>% 
  ggplot(aes(x = reorder(EPS,usuarios), y = usuarios)) +
  geom_bar(stat = "identity", fill = "#4E67B4") +
  geom_text(
    aes(
      label = ifelse(
        usuarios > 700000,
        scales::dollar(usuarios, prefix = "$",
                       big.mark = ".",
                       decimal.mark = ","),
        ""
      )
    ),
    position = position_dodge(width = 0.9),
    vjust = 0.7,
    size = 3,
    color = "#eeeeee",
    hjust = 1.1
  ) +
  facet_wrap(~ANNO,
             scales = "free_y") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6,
                                                   suffix = "M")) +
  coord_flip() +
  theme_minimal() +
  labs(x = "",
       y = "")

# cant_usu_eps <- rADRES::formatoADRES(cant_usu_eps)

ggsave("plot/cantidad_usuarios_eps_con.png",
       cant_usu_eps,
       width = 16,
       height = 9,
       dpi = 540
)
