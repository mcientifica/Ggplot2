library(ggplot2)
library(plotly)
library(readr)
library(dplyr)
library(lubridate)
library(stringr)

clientes <- read_csv("../Desktop/clientes.csv")
ventas <- read_csv("../Desktop/ventas.csv")
comisiones <- read_csv("../Desktop/comisiones.csv")
modelos <- read_csv("../Desktop/modelos.csv")

# - -----------------------------------------------------------------------

# Ejercicio 1

ventas %>% 
  left_join(modelos) %>% 
  group_by(Marca) %>% 
  summarise(
    Facturacion = sum(Precio)
  ) %>% 
  ggplot(
    mapping = aes(x = Marca, y = Facturacion/1000000)
  ) + 
  geom_col(fill = "darkblue") +
  theme_classic() +
  ylab(label = "Facturacion (Mils)")


# Ejercicio 2

ventas %>% 
  left_join(modelos) %>% 
  group_by(Marca, Modelo) %>% 
  summarise(
    Facturacion = sum(Precio)
  ) %>% 
  ggplot(
    mapping = aes(x = Marca, y = Facturacion, fill = Modelo)
  ) + 
  geom_col() +
  theme_classic()


# Ejercicio 3

ventas %>% 
  mutate(
    fecha_mes = make_date(year(FechaVenta), month(FechaVenta))
  ) %>% 
  group_by(fecha_mes) %>% 
  summarise(
    Facturacion = sum(Precio)
  ) %>% 
  ggplot(
    mapping = aes(x = fecha_mes, y = Facturacion)
  ) +
  geom_line()+
  theme_classic()


# Ejercicio 4

ventas %>% 
  left_join(modelos) %>% 
  mutate(
    fecha_mes = floor_date(FechaVenta, "month")
  ) %>% 
  group_by(fecha_mes, Marca, Modelo) %>% 
  summarise(
    Facturacion = sum(Precio)
  ) %>% 
  ggplot(
    mapping = aes(
      x = fecha_mes, 
      y = Facturacion)
  ) +
  geom_col()+
  facet_wrap(Marca~Modelo)


# Ejercicio 5

ggplotly(
  ventas %>% 
    ggplot(mapping = aes(x = Precio/1000)) +
    geom_histogram(bins = 1000) +
    xlab("Precio (000)")+
    scale_x_continuous(n.breaks = 20)+
    scale_y_continuous(n.breaks = 10)
)


# Ejercicio 6

# Mensual

ventas %>% 
  mutate(
    Mes = month(FechaVenta, label = TRUE)
  ) %>% 
  group_by(Mes) %>% 
  summarise(
    Facturacion = sum(Precio)
  ) %>% 
  ggplot(
    mapping = aes(x = Mes, y = Facturacion)
  ) +
  geom_col()

# Anual

ventas %>% 
  mutate(
    Year = as.integer(year(FechaVenta))
  ) %>% 
  group_by(Year) %>% 
  summarise(
    Facturacion = sum(Precio)
  ) %>% 
  ggplot(
    mapping = aes(x = Year, y = Facturacion)
  ) +
  geom_col()
    
# Diario

ventas %>% 
  mutate(
    Dia = day(FechaVenta)
  ) %>% 
  group_by(Dia) %>% 
  summarise(
    Facturacion = sum(Precio)
  ) %>% 
  ggplot(
    mapping = aes(x = Dia, y = Facturacion)
  ) +
  geom_col()

# por dia de semana
ventas %>% 
  mutate(
    dia_semana = wday(
      FechaVenta, 
      label = TRUE, 
      abbr = FALSE,
      week_start = 1
      )
  ) %>% 
  group_by(dia_semana) %>% 
  summarise(
    Facturacion = sum(Precio)
  ) %>% 
  ggplot(
    mapping = aes(x = dia_semana, y = Facturacion)
  ) +
  geom_col()


# Ejercicio 7

ventas %>% 
  mutate(dif = FechaPrimerContacto - FechaCompraCoche) %>% 
  ggplot(mapping = aes(x = dif)) + 
    geom_histogram()

ventas %>% 
  mutate(dif = FechaVenta - FechaPrimerContacto) %>% 
  ggplot(mapping = aes(x = dif)) + 
  geom_histogram(bins = 20)

ventas %>% 
  mutate(dif = FechaEntregaCoche - FechaVenta) %>% 
  ggplot(mapping = aes(x = dif)) + 
  geom_histogram(bins = 15)


# Ejercicio 8

ventas %>% 
  filter(Estado == "Segunda Mano") %>% 
  mutate(
    antiguedad = factor(year(FechaVenta) - Year) # el factor es únicamente para tener todos los labels en el eje x del gráfico
  ) %>% 
  group_by(antiguedad) %>% 
  summarise(
    precio_promedio = mean(Precio)
  ) %>% 
  ggplot(mapping = aes(x = antiguedad, y = precio_promedio)) +
  geom_col()


# Ejercicio 9

# Facturacion diaria

ventas %>% 
  left_join(clientes, by = "IdCliente") %>% 
  mutate(Genero = str_replace_na(Genero,"Otro")) %>% 
  group_by(Genero, Estado, FechaVenta) %>% 
  summarise(
    Facturacion = sum(Precio)
  ) %>% 
  ggplot(mapping = aes(x = FechaVenta, y = Facturacion)) +
  geom_line() +
  facet_wrap(Estado ~ Genero)

# Facturacion mensual - no se pedía

ventas %>% 
  left_join(clientes, by = "IdCliente") %>% 
  mutate(
    Genero = str_replace_na(Genero,"Otro"),
    Mes = floor_date(FechaVenta, "month")
    ) %>% 
  group_by(Genero, Estado, Mes) %>% 
  summarise(
    Facturacion = sum(Precio)
  ) %>% 
  ggplot(mapping = aes(x = Mes, y = Facturacion)) +
  geom_line() +
  facet_wrap(Estado ~ Genero)

