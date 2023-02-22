library(here)
library(readxl)
library(tidyverse)

others <-
  readxl::read_xlsx(
    "www/defaults_rema_fip_13Ene23.xlsx",
    sheet = 2,
    na = c("", "N/A")
  ) %>%
  janitor::clean_names() %>%
  filter(concepto == "Otro",
         actividad == "Reuniones para presentar información de ZRP") %>% 
  mutate(section = "FIP") %>% 
  select(concepto, rubro, descripcion, unidades)

add_subfase_info <- function(data){
  data %>% 
    group_by(subfase) %>%
    nest() %>% 
    mutate(subfase_code = cur_group_id(),
           subfase_orden = subfase_code,
           data = map(data, add_activity_info)) %>% 
    unnest(cols = data) %>% 
    ungroup()
}

add_activity_info <- function(data){
  data %>% 
    group_by(actividad) %>%
    nest() %>% 
    mutate(actividad_code = cur_group_id(),
           actividad_orden = actividad_code,
           data = map(data, add_rubro_info)) %>% 
    unnest(cols = data)
}

add_rubro_info <- function(data){
  data %>% 
    bind_rows(others) %>% 
    mutate(code = 1:nrow(.)) %>% 
    ungroup()
}

metadata <- readxl::read_xlsx(
  "www/defaults_rema_fip_13Ene23.xlsx",
  sheet = 1,
  na = c("", "N/A"))

rema_data <- readxl::read_xlsx(
  "www/defaults_rema_fip_13Ene23.xlsx",
  sheet = 2,
  na = c("", "N/A")) %>%
  janitor::clean_names() %>%
  filter(!concepto == "Otro") %>% 
  select(-c(contains("orde"), contains("code"), responsable, cantidades, precio, id)) %>%
  group_by(fase) %>% 
  nest() %>% 
  mutate(fase_code = cur_group_id(),
         data = map(data, add_subfase_info)) %>% 
  unnest(cols = data) %>% 
  ungroup() %>% 
  mutate(responsable = "",
         precio = 0,
         cantidades = 0,
         fase_duracion = 12,
         unidades = ifelse(is.na(unidades), "$/unidad", unidades),
         actividad_frecuencia = "Anual",
         section = "REMA") %>% 
  arrange(fase_code, subfase_code, actividad_code, actividad_orden, code) %>% 
  select(section,
         fase, fase_code, fase_duracion,
         subfase, subfase_code, subfase_orden,
         concepto, actividad, actividad_code, actividad_orden, actividad_frecuencia, code,
         rubro, descripcion, responsable, unidades, cantidades, precio) %>% 
  mutate(id = paste(tolower(section), fase_code, subfase_code, actividad_code, code, sep = "_"))

fip_data <- readxl::read_xlsx(
  "www/defaults_rema_fip_13Ene23.xlsx",
  sheet = 3,
  na = c("", "N/A")) %>%
  janitor::clean_names() %>%
  select(-c(contains("orde"), contains("code"), responsable, cantidades, precio, id)) %>%
  group_by(fase) %>% 
  nest() %>% 
  mutate(fase_code = cur_group_id(),
         data = map(data, add_subfase_info)) %>% 
  unnest(cols = data) %>% 
  ungroup() %>% 
  mutate(responsable = "",
         precio = 0,
         cantidades = 0,
         fase_duracion = 12,
         unidades = ifelse(is.na(unidades), "$/unidad", unidades),
         actividad_frecuencia = "Anual",
         section = "FIP") %>% 
  arrange(fase_code, subfase_code, actividad_code, actividad_orden, code) %>% 
  select(section,
         fase, fase_code, fase_duracion,
         subfase, subfase_code, subfase_orden,
         concepto, actividad, actividad_code, actividad_orden, actividad_frecuencia, code,
         rubro, descripcion, responsable, unidades, cantidades, precio) %>% 
  mutate(id = paste(tolower(section), fase_code, subfase_code, actividad_code, code, sep = "_"))

writexl::write_xlsx(
  x = list(
    METADATA = metadata,
    REMA = rema_data,
    FIP = fip_data
    ),
  path = here("www", "test.xlsx"),
  col_names = TRUE
)
