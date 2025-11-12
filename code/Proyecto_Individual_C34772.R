
setwd("C:/Users/rober/OneDrive/Escritorio/Proyecto_C34772_files/data")
library(tidyr)
library(purrr)
library(scales)
library(dplyr)
library(readr)
library(ggplot2)
library(worldfootballR)



#Para iniciar mediante los datos de Transfermaket voy a poder acceder a los datos sobre todo los fichajes que se dieron en la Liga Espanola, inglesa y Alemana de la temporada 2024-2025


#Guardamos los URl como un csv asi cada vez que se quiera renderizar no tenemos que acceder desde la url sino desde un excel creado con esos mismos datos de forma es que solo se cargan una vez.

fichajes_espana <- read.csv("fichajes_laliga.csv")
fichajes_england <- read.csv("fichajes_premier.csv")
fichajes_germany <- read.csv("fichajes_bundesliga.csv")

fichajes_europa <- bind_rows(fichajes_espana, fichajes_england, fichajes_germany)
fichajes_europa <- fichajes_europa %>%
  mutate(monto_num = parse_number(as.character(transfer_fee)))



#fichajes top 3 ligas
fichajes_top3 <- fichajes_europa %>%
  filter(league %in% c("LaLiga", "Bundesliga", "Premier League"))

#fichajes acomodados de mayor a menor segun su monto de transferencia.    
fichajes_europa_mas_caros <- fichajes_europa %>%
  ungroup() %>%  
  filter(!is.na(transfer_fee)) %>%  
  arrange(desc(transfer_fee))  

#Top 10

Top10_fichajes <- head(fichajes_europa_mas_caros, 10)

#fichajes acomodados de mayor a menor segun su cantidad de minutos en la temporada anteior
fichajes_europa_mas_minutos <- fichajes_europa %>%
  ungroup() %>%  
  filter(!is.na(minutes_played)) %>%  
  arrange(desc(minutes_played))  

#fichajes acomodados de mayor a menor segun su cantidad de goles en la temporada anteior
fichajes_europa_mas_goles <- fichajes_europa %>%
  ungroup() %>%  
  filter(!is.na(goals)) %>%  
  arrange(desc(goals))  

#Equipos que mas gastaron 

gasto_por_equipo <- fichajes_europa %>%
  group_by(team_name) %>%  
  summarise(
    gasto_total = sum(transfer_fee, na.rm = TRUE),
    fichajes = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(gasto_total))

#Top 20 equipos que mas gastaron
head(gasto_por_equipo, 20)

# ver que liga gasto mas

gasto_por_liga <- fichajes_europa %>%
  group_by(league) %>%  
  summarise(
    gasto_total_por_liga = sum(transfer_fee, na.rm = TRUE),
    fichajes = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(gasto_total_por_liga))

# Filtrar solo las tres ligas principales
gasto_top3 <- gasto_por_liga %>%
  filter(league %in% c("LaLiga", "Premier League", "Bundesliga")) %>%
  mutate(league = factor(league, levels = c("LaLiga", "Bundesliga", "Premier League"))) %>%
  arrange(desc(gasto_total_por_liga))

# Ahora calcularemos el promedio de edad, el minimo y el maximo asi podemos notar que tendencia tienen los clubes

edad_promedio_por_equipo <- fichajes_europa %>%
  filter(!is.na(player_age)) %>%
  group_by(team_name) %>%  
  summarise(
    edad_promedio = mean(as.numeric(player_age), na.rm = TRUE),
    edad_min = min(player_age, na.rm = TRUE),
    edad_max = max(player_age, na.rm = TRUE),
    numero_fichajes = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(edad_promedio))

#Monto de fichajes por edad (la suma)

fichajes_por_edad <- fichajes_top3 %>%
  mutate(monto = parse_number(as.character(transfer_fee))) %>%
  filter(!is.na(player_age), !is.na(monto), monto > 0) %>%
  group_by(player_age, league) %>%
  summarise(gasto_promedio = mean(monto, na.rm = TRUE), .groups = "drop")

#Ahora un promedio de edad de los fichajes por liga

edad_promedio_por_equipo <- fichajes_europa %>%
  filter(!is.na(player_age)) %>%
  group_by(league) %>%  
  summarise(
    edad_promedio = mean(as.numeric(player_age), na.rm = TRUE),
    edad_min = min(player_age, na.rm = TRUE),
    edad_max = max(player_age, na.rm = TRUE),
    numero_fichajes = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(edad_promedio))

# Tambien podemos ver cuales son las nacionalidades que mas se repiten en los fichajes.

nacionalidades <- fichajes_europa %>%
  filter(!is.na(player_nationality)) %>%
  group_by(player_nationality) %>%
  summarise(
    cantidad_jugadores = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(cantidad_jugadores)) 



# Tambien podemos ver cuales son las nacionalidades que mas se repiten en los fichajes por liga.
nacionalidades <- fichajes_europa %>% 
  filter(!is.na(player_nationality)) %>%
  group_by(league, player_nationality) %>% 
  summarise(
    cantidad_jugadores = n(),
    .groups = "drop"
  ) 
nacionalidades_premier <- nacionalidades %>%
  filter(league == "Premier League")

nacionalidades_laliga <- nacionalidades %>%
  filter(league == "LaLiga")

nacionalidades_bundesliga <- nacionalidades %>%
  filter(league == "Bundesliga")

#Observar como afecta la posicion en el campo a su valor del mercado analizando cada posicion y sus referentes

datos_porteros <- filter(fichajes_europa, fichajes_europa$player_position == "Goalkeeper" )

datos_defensas <- fichajes_europa %>%
  filter(player_position %in% c("Centre-Back", "Left-Back", "Right-Back", "Defender"))

datos_mediocampistas <- fichajes_europa %>%
  filter(player_position %in% c("Attacking Midfield","Defensive Midfield","Central Midfield","Left Midfield","Right Midfield"))

datos_delanteros <- fichajes_europa %>%
  filter(player_position %in% c("Centre-Forward" ,"Left Winger","Right Winger", "Second Striker"))



#Gasto promedio por posicion del campo

gasto_porteros <- datos_porteros %>%
  mutate(monto = as.numeric(transfer_fee)) %>%
  summarise(zona = "Portero",
            gasto_total_eur = sum(monto, na.rm = TRUE),
            fichajes_con_monto = sum(!is.na(monto)),
            fichajes_totales = n())

gasto_defensas <- datos_defensas %>%
  mutate(monto = as.numeric(transfer_fee)) %>%
  summarise(zona = "Defensas",
            gasto_total_eur = sum(monto, na.rm = TRUE),
            fichajes_con_monto = sum(!is.na(monto)),
            fichajes_totales = n())

gasto_mediocampistas <- datos_mediocampistas %>%
  mutate(monto = as.numeric(transfer_fee)) %>%
  summarise(zona = "Mediocampistas",
            gasto_total_eur = sum(monto, na.rm = TRUE),
            fichajes_con_monto = sum(!is.na(monto)),
            fichajes_totales = n())

gasto_delanteros <- datos_delanteros %>%
  mutate(monto = as.numeric(transfer_fee)) %>%
  summarise(zona = "Delanteros",
            gasto_total_eur = sum(monto, na.rm = TRUE),
            fichajes_con_monto = sum(!is.na(monto)),
            fichajes_totales = n())

#gasto por posicion

gasto_pos <- bind_rows(gasto_porteros, gasto_defensas, gasto_mediocampistas, gasto_delanteros) %>%
  mutate(zona = factor(zona, levels = c("Portero","Defensas","Mediocampistas","Delanteros")))

#Top 10 mas caros por posicion dentro del campo:

top10_delanteros <- datos_delanteros %>%
  mutate(monto = as.numeric(transfer_fee)) %>% 
  filter(!is.na(monto)) %>%                       
  arrange(desc(monto)) %>%              
  select(player_name, team_name, league, monto) %>%  
  head(10)

top10_mediocampistas <- datos_mediocampistas %>%
  mutate(monto = as.numeric(transfer_fee)) %>% 
  filter(!is.na(monto)) %>%                       
  arrange(desc(monto)) %>%              
  select(player_name, team_name, league, monto) %>%  
  head(10)       

top10_defensas <- datos_defensas %>%
  mutate(monto = as.numeric(transfer_fee)) %>% 
  filter(!is.na(monto)) %>%                       
  arrange(desc(monto)) %>%              
  select(player_name, team_name, league, monto) %>%  
  head(10)       

top10_porteros <- datos_porteros %>%
  mutate(monto = as.numeric(transfer_fee)) %>% 
  filter(!is.na(monto)) %>%                       
  arrange(desc(monto)) %>%              
  select(player_name, team_name, league, monto) %>%  
  head(10)       

#comparar la ventana de invierno con la de verano

# Filtrar solo verano
fichajes_verano <- fichajes_europa %>%
  filter(window == "summer")

# Filtrar solo invierno
fichajes_invierno <- fichajes_europa %>%
  filter(window == "winter")

resumen_por_periodo <- fichajes_europa %>%
  mutate(monto = parse_number(as.character(transfer_fee))) %>%
  group_by(window) %>%
  summarise(
    gasto_total = sum(monto, na.rm = TRUE),
    fichajes = n(),
    .groups = "drop"
  )


#top 10 por liga

top10_premier <- fichajes_europa %>%
  filter(league == "Premier League") %>%
  arrange(desc(monto_num)) %>%
  slice_head(n = 10) %>%
  mutate(player_name = factor(player_name, levels = rev(unique(player_name))))

top10_bundesliga <- fichajes_europa %>%
  filter(league == "Bundesliga") %>%
  arrange(desc(monto_num)) %>%
  slice_head(n = 10) %>%
  mutate(player_name = factor(player_name, levels = rev(unique(player_name))))

top10_laliga <- fichajes_europa %>%
  filter(league == "LaLiga") %>%
  arrange(desc(monto_num)) %>%
  slice_head(n = 10) %>%
  mutate(player_name = factor(player_name, levels = rev(unique(player_name))))



#Grafico de cuanto se gasta por liga:
  
  
ggplot(gasto_top3, aes(x = league, y = gasto_total_por_liga / 1e6, fill = league)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = paste0("€", format(round(gasto_total_por_liga / 1e6, 1), big.mark = ".", decimal.mark = ","), " M")),
    vjust = -0.4,
    size = 5,
    color = "black",
    fontface = "bold"
  ) +
  labs(
    title = " Gasto por liga (Mercado de fichajes 2024-2025)",
    subtitle = "Comparativa entre LaLiga, Bundesliga y Premier League",
    x = NULL,
    y = "Gasto total (millones €)"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    labels = function(x) paste0("€", format(x, big.mark = ".", decimal.mark = ","))
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
    axis.text.x = element_text(face = "bold", size = 13),
    axis.text.y = element_text(size = 11),
    axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(size = 10, hjust = 0)
  ) +
  scale_fill_manual(values = c(
    "LaLiga" = "blue",         
    "Bundesliga" = "red",      
    "Premier League" = "purple"   
  ))

#Gasto por mercado:
  


ggplot(resumen_por_periodo, aes(x = window, y = gasto_total / 1e6, fill = window)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = paste0("€", format(round(gasto_total / 1e6, 1), big.mark = ".", decimal.mark = ","), " M")),
    vjust = -0.4, size = 5, fontface = "bold"
  ) +
  labs(
    title = "Gasto total por ventana de transferencias (2024-2025)",
    subtitle = "Comparación entre ventana de verano e invierno",
    x = NULL,
    y = "Gasto total (millones €)"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = c("Summer" = "yellow", "Winter" = "darkblue"))


#Top 10 fichajes:

Top10_fichajes %>%
  mutate(
    monto = as.numeric(transfer_fee),
    player_name = factor(player_name, levels = rev(unique(player_name)))
  ) %>%
  ggplot(aes(x = player_name, y = monto/1e6, fill = league)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("€", round(monto/1e6, 1), " M")),
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Top 10 fichajes por monto",
    x = NULL,
    y = "Millones de Euros"
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  scale_fill_manual(values = c(
    "LaLiga" = "blue",
    "Bundesliga" = "red",
    "Premier League" = "purple"
  )) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  )


#Top 10 nacionalidades en las 3 ligas 


nacionalidades %>%
  arrange(desc(cantidad_jugadores)) %>% 
  head(10) %>%
  ggplot(aes(x = reorder(player_nationality, -cantidad_jugadores), y = cantidad_jugadores)) +
  geom_col(width = 0.6, fill = "#1f77b4") +
  coord_flip() +
  labs(title = "Top 10 nacionalidades fichadas – Top 3 Ligas", x = NULL, y = "Jugadores") +
  theme_minimal(base_size = 13)






#Top 10 nacionalidades por Liga:
  

nacionalidades_premier %>%
  arrange(desc(cantidad_jugadores)) %>% 
  head(10) %>%
  ggplot(aes(x = reorder(player_nationality, cantidad_jugadores), y = cantidad_jugadores)) +
  geom_col(width = 0.6, fill = "purple") +
  coord_flip() +
  labs(title = "Top 10 nacionalidades fichadas – Premier", x = NULL, y = "Jugadores") +
  theme_minimal(base_size = 13)



nacionalidades_laliga %>%
  arrange(desc(cantidad_jugadores)) %>% 
  head(10) %>%
  ggplot(aes(x = reorder(player_nationality, cantidad_jugadores), y = cantidad_jugadores)) +
  geom_col(width = 0.6, fill = "blue") +
  coord_flip() +
  labs(title = "Top 10 nacionalidades fichadas – LaLiga", x = NULL, y = "Jugadores") +
  theme_minimal(base_size = 13)



nacionalidades_bundesliga %>%
  arrange(desc(cantidad_jugadores)) %>% 
  head(10) %>%
  ggplot(aes(x = reorder(player_nationality, cantidad_jugadores), y = cantidad_jugadores)) +
  geom_col(width = 0.6, fill = "red") +
  coord_flip() +
  labs(title = "Top 10 nacionalidades fichadas – Bundesliga", x = NULL, y = "Jugadores") +
  theme_minimal(base_size = 13)



ggplot(gasto_pos, aes(x = zona, y = gasto_total_eur/1e6, fill = zona)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = paste0(round(gasto_total_eur/1e6, 1), " M")),
            vjust = -0.4, size = 4, fontface = "bold") +
  labs(title = "Gasto total por zona del campo", x = NULL, y = "Millones de €") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))



#Como se repartieron estos montos:
  

top10_delanteros %>%
  mutate(
    player_name = factor(player_name, levels = rev(unique(player_name)))
  ) %>%
  ggplot(aes(x = player_name, y = monto/1e6, fill = league)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("€", round(monto/1e6, 1), " M")),
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Top 10 fichajes delanteros",
    x = NULL,
    y = "Millones de Euros"
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  scale_fill_manual(values = c(
    "LaLiga" = "blue",
    "Bundesliga" = "red",
    "Premier League" = "purple"
  )) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  )




top10_mediocampistas %>%
  mutate(
    player_name = factor(player_name, levels = rev(unique(player_name)))
  ) %>%
  ggplot(aes(x = player_name, y = monto/1e6, fill = league)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("€", round(monto/1e6, 1), " M")),
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Top 10 fichajes mediocampistas",
    x = NULL,
    y = "Millones de Euros"
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  scale_fill_manual(values = c(
    "LaLiga" = "blue",
    "Bundesliga" = "red",
    "Premier League" = "purple"
  )) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  )


top10_defensas %>%
  mutate(
    player_name = factor(player_name, levels = rev(unique(player_name)))
  ) %>%
  ggplot(aes(x = player_name, y = monto/1e6, fill = league)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("€", round(monto/1e6, 1), " M")),
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Top 10 fichajes defensas",
    x = NULL,
    y = "Millones de Euros"
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  scale_fill_manual(values = c(
    "LaLiga" = "blue",
    "Bundesliga" = "red",
    "Premier League" = "purple"
  )) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  )


top10_porteros %>%
  mutate(
    player_name = factor(player_name, levels = rev(unique(player_name)))
  ) %>%
  ggplot(aes(x = player_name, y = monto/1e6, fill = league)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("€", round(monto/1e6, 1), " M")),
            hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Top 10 fichajes Porteros",
    x = NULL,
    y = "Millones de Euros"
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(expand = expansion(mult = c(0, .15))) +
  scale_fill_manual(values = c(
    "LaLiga" = "blue",
    "Bundesliga" = "red",
    "Premier League" = "purple"
  )) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "bottom"
  )


#Como cambian los precios al paso del tiempo con respecto a su edad:
  

ggplot(fichajes_por_edad, aes(x = player_age, y = gasto_promedio/1e6, color = league)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("LaLiga" = "skyblue", "Bundesliga" = "red3", "Premier League" = "purple")) +
  labs(
    title = "Tendencia del gasto promedio según la edad del jugador (2024–2025)",
    subtitle = "Comparación entre LaLiga, Bundesliga y Premier League",
    x = "Edad del jugador (años)",
    y = "Gasto promedio (Millones de €)",
    color = "Liga"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

#Promedio de fichajes segun los minutos jugados anteriormente



fichajes_top3 %>%
  mutate(
    monto = parse_number(as.character(transfer_fee)),
    rango_minutos = cut(minutes_played,
                        breaks = c(0, 500, 1000, 2000, 3000, 4000, 5000),
                        labels = c("0–500", "500–1000", "1000–2000", "2000–3000", "3000–4000", "4000+"))
  ) %>%
  filter(!is.na(monto), monto > 0) %>%
  group_by(rango_minutos) %>%
  summarise(promedio_monto = mean(monto, na.rm = TRUE)) %>%
  ggplot(aes(x = rango_minutos, y = promedio_monto/1e6)) +
  geom_line(group = 1, color = "blue", linewidth = 1.2) +
  geom_point(size = 3, color = "darkblue") +
  labs(
    title = "Promedio del valor de fichajes según minutos jugados",
    x = "Rango de minutos jugados",
    y = "Monto promedio (Millones de €)",
    caption = "Fuente: Transfermarkt (via worldfootballR)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


#Gasto por equipos:
  

gasto_por_equipo %>%
  slice(1:20) %>%
  left_join(
    fichajes_europa %>% select(team_name, league) %>% distinct(),
    by = "team_name"
  ) %>%
  mutate(
    gasto_total = parse_number(as.character(gasto_total)),
    team_name   = factor(team_name, levels = rev(team_name))
  ) %>%
  ggplot(aes(x = team_name, y = gasto_total/1e6, color = league)) +
  geom_segment(aes(xend = team_name, y = 0, yend = gasto_total/1e6),
               linewidth = 1.1, color = "black") +
  geom_point(size = 3) +
  coord_flip() +
  labs(title = "Top 20 clubes por gasto total",
       x = NULL, y = "Millones de €") +
  theme_minimal(base_size = 13) +
  scale_color_manual(
    values = c("LaLiga" = "blue", "Bundesliga" = "red", "Premier League" = "purple"),
    na.value = "green",   
    name = "Liga"
  )


#Gasto por liga y ventana de traspasos


fichajes_top3 %>%
  mutate(monto =as.numeric(transfer_fee)) %>%
  filter(!is.na(monto), monto > 0) %>%
  group_by(league, window) %>%
  summarise(gasto = sum(monto, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = league, y = gasto/1e6, fill = window)) +
  geom_col(position = position_dodge(width = .65), width = .6) +
  labs(title = "Gasto por liga y ventana", x = NULL, y = "Millones de €", fill = "Ventana") +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c("Summer"="gold","Winter"="skyblue"))



fichajes_top3 %>%
  mutate(monto = as.numeric(transfer_fee)) %>%
  filter(!is.na(monto), monto > 0) %>%
  ggplot(aes(x = league, y = monto/1e6, fill = league)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = .35) +
  labs(title = "Distribución de montos por liga",
       x = NULL, y = "Millones de €") +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c("LaLiga"="blue","Bundesliga"="red","Premier League"="purple"))



edad_promedio_por_equipo %>%
  
  mutate(
    edad_promedio = as.numeric(edad_promedio),
    edad_min = as.numeric(edad_min),
    edad_max = as.numeric(edad_max)
  ) %>%
  filter(league %in% c("LaLiga", "Bundesliga", "Premier League")) %>%
  ggplot(aes(x = league, y = edad_promedio, color = league)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = edad_min, ymax = edad_max), 
                width = 0.2, linewidth = 1) +
  labs(
    title = "Rango de edad de fichajes por liga (2024–2025)",
    subtitle = "Comparativa entre LaLiga, Bundesliga y Premier League",
    x = NULL,
    y = "Edad promedio (años)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_color_manual(values = c(
    "LaLiga" = "skyblue",
    "Bundesliga" = "red3",
    "Premier League" = "purple"
  ))



fichajes_top3 %>%
  filter(player_position %in% c("Centre-Forward","Left Winger","Right Winger","Second Striker")) %>%
  mutate(monto = as.numeric(transfer_fee)) %>%
  filter(!is.na(goals), !is.na(monto), monto > 0) %>%
  ggplot(aes(x = goals, y = monto/1e6, color = league)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_smooth(se = FALSE, linewidth = 1.1) +
  scale_color_manual(values = c("LaLiga"="blue","Bundesliga"="red","Premier League"="purple")) +
  labs(
    title = "Relación entre goles y monto de transferencia (solo delanteros)",
    x = "Goles temporada anterior", y = "Monto (M€)", color = "Liga"
  ) +
  theme_minimal(base_size = 13)



fichajes_goles_precio <- fichajes_europa %>%
  mutate(monto = as.numeric(transfer_fee)) %>%
  filter(!is.na(goals), !is.na(monto), monto > 0)

ggplot(fichajes_goles_precio, aes(x = goals, y = monto/1e6, color = league)) +
  geom_point(alpha = 0.6, size = 2.5) +
  scale_color_manual(values = c("LaLiga"="blue","Bundesliga"="red","Premier League"="purple")) +
  labs(
    title = "Relación entre goles y monto de transferencia",
    subtitle = "Temporada 2024–2025",
    x = "Goles en la temporada anterior",
    y = "Monto del fichaje (Millones de €)",
    color = "Liga"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))



ggplot(top10_premier, aes(x = player_name, y = monto_num/1e6, fill = league)) +
  geom_col(width = 0.6, fill = "purple") +
  geom_text(aes(label = paste0("€", round(monto_num/1e6, 1), " M")),
            hjust = 0, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Top 10 fichajes más caros – Premier League (2024-2025)",
    x = NULL,
    y = "Millones de Euros"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

ggplot(top10_bundesliga, aes(x = player_name, y = monto_num/1e6, fill = league)) +
  geom_col(width = 0.6, fill = "red") +
  geom_text(aes(label = paste0("€", round(monto_num/1e6, 1), " M")),
            hjust = 0, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Top 10 fichajes más caros – Bundesliga(2024-2025)",
    x = NULL,
    y = "Millones de Euros"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )

ggplot(top10_laliga, aes(x = player_name, y = monto_num/1e6, fill = league)) +
  geom_col(width = 0.6, fill = "blue") +
  geom_text(aes(label = paste0("€", round(monto_num/1e6, 1), " M")),
            hjust = 0, size = 4, fontface = "bold") +
  coord_flip() +
  labs(
    title = "Top 10 fichajes más caros – La Liga(2024-2025)",
    x = NULL,
    y = "Millones de Euros"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )


#Ahora intentaremos como le fue a cada equipo en la temporada segun cuanto gastaron en cada fichaje:



tabla_premier <- read.csv("tabla_premier_2025.csv")
tabla_laliga <- read.csv("tabla_laliga_2025.csv")




ggplot(tabla_premier, aes(x=Pts, y= reorder(Squad, Pts))) + geom_col(fill= "Purple")+  geom_text(
  aes(label = Pts),
  hjust = -0.2,                
  color = "black",
  size = 4,
  fontface = "bold"
) + labs(
  title = "Clasificacion Premier League 2024-2025",
  x= "Puntos",
  y= NULL
)+
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
  )



ggplot(tabla_laliga, aes(x=Pts, y= reorder(Squad, Pts))) + geom_col(fill= "blue")+  geom_text(
  aes(label = Pts),
  hjust = -0.2,                
  color = "black",
  size = 4,
  fontface = "bold"
) + labs(
  title = "Clasificacion La Liga 2024-2025",
  x= "Puntos",
  y= NULL
)+
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.position = "none"
    
    
  )


#Ahora a la tabal de posiciones agregaremos una columna que se llame monto asi veremos cuanto gastaron en la temporada:
  

calcular_gasto_por_equipo <- function(fichajes_top3) {
  fichajes_top3 %>%
    mutate(monto = as.numeric(monto_num)) %>%
    filter(!is.na(monto), monto > 0) %>%
    group_by(team_name) %>%
    summarise(
      gasto_total = sum(monto, na.rm = TRUE),
      cantidad_fichajes = n(),
      gasto_promedio = mean(monto, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(gasto_total))
}

gasto_laliga <- calcular_gasto_por_equipo(
  fichajes_top3 %>% filter(league == "LaLiga")
)
gasto_premier <- calcular_gasto_por_equipo(
  fichajes_top3 %>% filter(league == "Premier League")
)

gasto_premier <- gasto_premier %>%
  mutate(
    team_name = case_when(
      team_name == "Liverpool FC" ~ "Liverpool",
      team_name == "Everton FC" ~ "Everton",
      team_name == "Newcastle United" ~ "Newcastle Utd",
      team_name == "Fulham FC" ~ "Fulham",
      team_name == "Brentford FC" ~ "Brentford",
      team_name == "Nottingham Forest" ~ "Nott'ham Forest",
      team_name == "Chelsea FC" ~ "Chelsea",
      team_name == "Brighton & Hove Albion" ~ "Brighton",
      team_name == "Man Utd" ~ "Manchester Utd",
      team_name == "Man City" ~ "Manchester City",
      team_name == "Wolverhampton Wanderers" ~ "Wolves",
      team_name == "AFC Bournemouth" ~ "Bournemouth",
      team_name == "Arsenal FC" ~ "Arsenal",
      team_name == "Tottenham Hotspur" ~ "Tottenham",
      team_name == "West Ham United" ~ "West Ham",
      
      TRUE ~ team_name 
    )
  )

gasto_laliga <- gasto_laliga %>%
  mutate(
    team_name = case_when(
      team_name == "Villarreal CF" ~ "Villarreal",
      team_name == "FC Barcelona" ~ "Barcelona",
      team_name == "Atlético de Madrid" ~ "Atlético Madrid",
      team_name == "Athletic Bilbao" ~ "Athletic Club",
      team_name == "Real Betis Balompié" ~ "Betis",
      team_name == "Getafe CF" ~ "Getafe",
      team_name == "Celta de Vigo" ~ "Celta Vigo",
      team_name == "Deportivo Alavés" ~ "Alavés",
      team_name == "CA Osasuna" ~ "Osasuna",
      team_name == "Valencia CF" ~ "Valencia",
      team_name == "RCD Mallorca" ~ "Mallorca",
      team_name == "Girona FC" ~ "Girona",
      team_name == "Sevilla FC" ~ "Sevilla",
      TRUE ~ team_name
    )
  )



#La tabla no toma en cuenta a los recien ascendidos por eso debemos anadirlos de manera manual mediante la busqueda en trasnfermarket
tabla_premier <- tabla_premier %>%
  mutate(
    monto = gasto_premier$gasto_total[
      match(Squad, gasto_premier$team_name)
    ]
  )%>% mutate(
    monto = ifelse(Squad %in% c("Leicester City"),91050000, monto)
  ) %>% mutate(
    monto = ifelse(Squad %in% c("Southampton"),55950000, monto)
  )%>% mutate(
    monto = ifelse(Squad %in% c("Ipswich Town"),59250000, monto)
  )%>% mutate(Liga = "Premier League") %>% mutate(
    monto = ifelse(Squad %in% c("Manchester Utd"),246300000	, monto)
  ) 


tabla_laliga <- tabla_laliga %>%
  mutate(
    monto = gasto_laliga$gasto_total[
      match(Squad, gasto_laliga$team_name)
    ] 
  )%>% mutate(
    monto = ifelse(Squad %in% c("Las Palmas", "Rayo Vallecano"), 0, monto)
  ) %>% 
  mutate(
    monto = ifelse(Squad %in% c("Valladolid"), 16680000, monto)
  ) %>% mutate(
    monto = ifelse(Squad %in% c("Leganés"),3810000, monto)
  ) %>% mutate(
    monto = ifelse(Squad %in% c("Espanyol"),400000, monto)
  )  %>% mutate(Liga = "LaLiga")




tabla_laliga %>%
  arrange(Rk) %>%
  ggplot(aes(y = reorder(Squad, Rk), x = monto/1e6)) +
  geom_segment(aes(x = 0, xend = monto/1e6, yend = reorder(Squad, Rk)), linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Gasto por equipo ordenado por posición (LaLiga)",
       x = "Gasto (Millones €)", y = NULL) +
  theme_minimal(base_size = 13)




tabla_premier %>%
  arrange(Rk) %>%
  ggplot(aes(y = reorder(Squad, Rk), x = monto/1e6)) +
  geom_segment(aes(x = 0, xend = monto/1e6, yend = reorder(Squad, Rk)), linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Gasto por equipo ordenado por posición (Premier League)",
       x = "Gasto (Millones €)", y = NULL) +
  theme_minimal(base_size = 13)



tabla_general <- bind_rows(
  tabla_premier,
  tabla_laliga  
)

top6 <- tabla_general %>%
  group_by(Liga) %>%
  slice_max(Pts, n =6, with_ties = FALSE) %>%
  ungroup()
ggplot(top6, aes(x = factor(Rk), y = monto/1e6, fill = Liga)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0("€", round(monto/1e6, 1))),
    position = position_dodge(width = 0.7),
    vjust = -0.25, size = 3.6
  ) +
  labs(
    title = "Gasto en millones de € para entrar al Top 6 (Acceso a UEFA)",
    x = "Posición en la tabla",
    y = "Gasto en fichajes",
    fill = "Liga"
  )+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c(
    "LaLiga" = "blue",
    "Premier League" = "purple"
  ))

#Otra buena pregunta es cuanto cuesta la permanencia tanto en la liga como en la Premier:

  

peores3 <- tabla_general %>%
  group_by(Liga) %>% 
  slice_min(Pts, n = 3, with_ties = FALSE) %>%
  ungroup()
ggplot(peores3, aes(x = factor(Rk), y = monto/1e6, fill = Liga)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.7) +
  geom_text(
    aes(label = paste0("€", round(monto/1e6, 1), "M")),
    position = position_dodge(width = 0.7),
    vjust = -0.25, size = 3.6
  ) +
  labs(
    title = "Gasto en millones para los equipos delegados",
    x = "Posición en la tabla",
    y = "Gasto en fichajes",
    fill = "Liga"
  )+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c(
    "LaLiga" = "blue",
    "Premier League" = "purple"
  ))

#Viendo los altos montos que invirtieron los equipos que bajaron de categoria en la premier, que nivel ocuparian entre los equipos que mas gastaron de la liga?
  

descendidos_premier <- tabla_general %>%
  filter(Liga == "Premier League") %>%
  arrange(Pts) %>%  
  slice_head(n = 3) 

comparar_descendidos_con_laliga <- bind_rows(descendidos_premier, tabla_laliga)

#Ahora los acomodo de mayor a menor en gastos para ver que posicion toman en comparacion a la liga
comparar_descendidos_con_laliga %>%
  arrange(desc(monto))

#sacar el top 10
top10_gasto_union <- comparar_descendidos_con_laliga %>%
  arrange(desc(monto)) %>%
  slice_head(n = 10)

ggplot(top10_gasto_union, aes(x = reorder(Squad, monto), y = monto / 1e6, fill = Liga)) +
  geom_col(width = 0.7, show.legend =TRUE) +
  geom_text(
    aes(label = paste0("€", round(monto / 1e6, 1), "M")),
    hjust = -0.10, size = 3.6, color = "black"
  ) +
  coord_flip() +
  labs(
    title = "Descendidos Premier vs La liga",
    x = "Equipo",
    y = "Gasto total (Millones de €)",
    fill = "Liga"
  ) +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c("LaLiga" = "blue", "Premier League" = "purple")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))


#Ahora me gustaria ver como quedan los 3 equipos con mejor rendimiento en espana y sus gastos contra los gastos de todos los equipos de la premier league


#filtramos los primeros equipos en la clasificacion espanola

top3_ligaespanola <- tabla_general %>% 
  filter(Liga== "LaLiga") %>% 
  arrange(desc(Pts)) %>% 
  slice_head(n =3)

superliga <- bind_rows(top3_ligaespanola, tabla_premier)

superliga<- superliga %>% arrange(desc(monto))

ggplot(superliga, aes(x = reorder(Squad, monto), y = monto / 1e6, fill = Liga)) +
  geom_col(width = 0.7, show.legend =TRUE) +
  geom_text(
    aes(label = paste0("€", round(monto / 1e6, 1), "M")),
    hjust = -0.10, size = 3.6, color = "black"
  ) +
  coord_flip() +
  labs(
    title = "Top 3 Espana vs Premier League",
    x = "Equipo",
    y = "Gasto total (Millones de €)",
    fill = "Liga"
  ) +
  theme_minimal(base_size = 13) +
  scale_fill_manual(values = c("LaLiga" = "blue", "Premier League" = "purple")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))




#Utilizaremos el indice de Pearson pero este para que sirve? El indice de correlacion de Pearson mide la relacion lineal entre dos variables numericas, si de 0 a 1 es que hay relacion positiva, 0 es que no hay relacion y -1 es que la relacion es negativa

# Aqui queremos ver la relacion entre monto y puntos obtenidos
Pearson_monto_pts <- cor(tabla_general$monto, tabla_general$Pts, method = "pearson", use = "complete.obs")

#Aqui queremos ver la relacion entre el monto pagado y la edad del jugador por liga

monto_edad_laliga <- cor(tabla_general$monto[tabla_general$Liga=="LaLiga"],
                         tabla_general$Pts[tabla_general$Liga=="LaLiga"],
                         method = "pearson")

monto_edad_premier <- cor(tabla_general$monto[tabla_general$Liga=="Premier League"],                tabla_general$Pts[tabla_general$Liga=="Premier League"],
                          method = "pearson")


#Queremos ver la relacion del valor de transferencia segun los goles que metió
Pearson_monto_goles <- cor(fichajes_goles_precio$monto, fichajes_goles_precio$goals, method = "pearson", use = "complete.obs")

#Queremos ver la relacion del valor de transferencia segun los goles que metió
Pearson_edad_monto <- cor(fichajes_europa$player_age, fichajes_europa$monto, method = "pearson", use = "complete.obs")

#Goles por delanteros
delanteros_goles <- fichajes_top3 %>%
  filter(player_position %in% c("Centre-Forward","Left Winger","Right Winger","Second Striker")) %>%
  mutate(monto = as.numeric(monto_num)) %>%
  filter(!is.na(goals), !is.na(monto), monto>0)

# Como esta relacionado cuanto se pagan por los delanteros y cantidad de goles
cor_goles_precio_liga <- delanteros_goles %>%
  group_by(league) %>%
  summarise(r = cor(goals, monto, use="complete.obs"),
            .groups="drop")
cor_goles_precio_liga
#¿Se paga más por jugadores con mucha continuidad?
cor(fichajes_top3$monto_num, fichajes_top3$minutes_played, use = "complete.obs")

# La relacion edad-precio varia segun la edad, en posiciones como la porteria o en la defensa es normal pagar mas por jugadores ya consagrados y con trayectoria.

fichajes_top3 %>%
  mutate(monto = as.numeric(monto_num)) %>%
  filter(!is.na(player_age), !is.na(monto), monto > 0) %>%
  group_by(player_position) %>%
  summarise(r = cor(player_age, monto, use = "complete.obs"), .groups = "drop")

#Se paga mas en verano?

fichajes_top3 %>%
  mutate(
    monto = as.numeric(monto_num),
    summer = if_else(tolower(window) == "summer", 1, 0)
  ) %>%
  summarise(r = cor(monto, summer, use = "complete.obs"))

#Los jugadores con mas minutos hacen más goles_
cor(fichajes_top3$goals, fichajes_top3$minutes_played, use = "complete.obs")


#relacion entre gasto y goles a favor

cor(tabla_general$monto, tabla_general$GF, use = "complete.obs")

#relacion entre gasto y diferencia de goles
cor(tabla_general$monto, tabla_general$GD, use = "complete.obs")

#de forma general todo lo anterior
tabla_general %>%
  group_by(Liga) %>%
  summarise(
    relacion_gasto_pts = cor(monto, Pts, use = "complete.obs"),
    relacion_gasto_GF = cor(monto, GF,  use = "complete.obs"),
    relacion_gasto_GD = cor(monto, GD,  use = "complete.obs"),
    relacion_gasto_GA = cor(monto, GA, "complete.obs" ) ,
    relacion_gasto_Rk = cor(monto, Rk,  use = "complete.obs")
  )

