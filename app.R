#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(magrittr)
library(tidyverse)
library(cowplot)
library(plotly)

cost_data <- read.csv("www/cost_data.csv",
                      stringsAsFactors = F,
                      strip.white = T,
                      na.strings = "N/A") %>% 
  janitor::clean_names() %>% 
  arrange(fase, tipo_de_inversion, periodicidad, concepto, subactividad, elemento) %>% 
  mutate(valor_unitario_usd = ifelse(
    is.na(valor_unitario_usd),
    0,
    valor_unitario_usd),
    estimacion_de_unidades_requeridas = ifelse(
      is.na(estimacion_de_unidades_requeridas),
      0,
      estimacion_de_unidades_requeridas))

boxHeaderUI <- function(){
  tagList(
    fluidRow(
      column(width = 6,
             h4("Costos")),
      column(width = 6,
             h4("Cantidades")))
  )
}

CostUnitUI <- function(titleId, pairId, costLabel, unitLabel){
  
  # Define inputId labels for cost and units
  costId <- paste0("c_", pairId)
  unitId <- paste0("u_", pairId)
  
  # Define cost and unit defaults for numeric inputs
  costDefault <- cost_data$valor_unitario_usd[cost_data$id == pairId]
  unitDefault <- cost_data$estimacion_de_unidades_requeridas[cost_data$id == pairId]

  tagList(
    titleId,
    fluidRow(
      column(width = 6,
             numericInput(inputId = costId,
                          label = costLabel,
                          value = costDefault,
                          min = 0)
      ),
      column(width = 6,
             numericInput(inputId = unitId,
                          label = unitLabel,
                          value = unitDefault,
                          min = 0)))
  )
}

# Define UI for application that draws a histogram
ui <- dashboardPage(title = "Costeo de COBI",
                    header = dashboardHeader(
                      title = "Costeo de Reservas",
                      dropdownMenu(type = "tasks",
                                   headerText = "Progreso por categoría",
                                   badgeStatus = "success",
                                   taskItem(value = 90,
                                            color = "blue",
                                            text = "Viáticos y campo"),
                                   taskItem(value = 17,
                                            color = "light-blue",
                                            text = "Salarios y Beneficios"),
                                   taskItem(value = 75,
                                            color = "blue",
                                            text = "Equipo"),
                                   taskItem(value = 80,
                                            color = "light-blue",
                                            text = "Costos Directos"),
                                   taskItem(value = 75,
                                            color = "blue",
                                            text = "Consultores"),
                                   taskItem(value = 80,
                                            color = "light-blue",
                                            text = "Comunicación")
                      )
                    ),
                    sidebar = dashboardSidebar(
                      width = 275,
                      sidebarMenu(id = "tabs", # Setting id makes input$tabs give the tabName of currently-selected tab
                                  menuItem(
                                    text = "Inicio",
                                    tabName = "inicio",
                                    icon = icon(name = "info")),
                                  menuItem(
                                    text = "Datos Generales",
                                           tabName = "datos_generales",
                                           icon = icon(name = "list")),
                                  menuItem(
                                    text = "Ingresar Costos",
                                    tabName = "costos",
                                    icon = icon(name = "usd"),
                                    menuSubItem(text = "Viáticos y Campo",
                                                tabName = "viaticos_campo"),
                                    menuSubItem(text = "Salarios y Beneficios",
                                                tabName = "salarios_beneficios"),
                                    menuSubItem(text = "Equipo",
                                                tabName = "equipo"),
                                    menuSubItem(text = "Costos Directos",
                                                tabName = "costos_directos"),
                                    menuSubItem(text = "Consultores",
                                                tabName = "consultores"),
                                    menuSubItem(text = "Comunicación",
                                                tabName = "comunicacion"),
                                    menuSubItem(text = "Otros",
                                                tabName = "otros")),
                                  menuItem(
                                    text = "Presupuesto",
                                    tabName = "presupuesto",
                                    icon = icon("bar-chart") # cambiar por hand-holding-usd
                                  ),
                                  fluidRow(
                                    infoBoxOutput(
                                      outputId = "totalUSD",
                                      width = 12)),
                                  fluidRow(
                                    infoBoxOutput(
                                      outputId = "totalMXP",
                                      width = 12))
                                  
                      )
                    ),
                    body = dashboardBody(
                      tabItems(
                        tabItem(tabName = "inicio",
                                "Informacion de la app aqui"),
                        tabItem(tabName = "datos_generales",
                                box(title = "Información del proyecto",
                                    width = 3,
                                    status = "info",
                                    numericInput(inputId = "anos",
                                                 label = "Duración del proyecto (años)",
                                                 value = 5,
                                                 min = 1,
                                                 max = 50),
                                    numericInput(inputId = "usd2mxp",
                                                 label = "Precio del dolar (1 USD = ? MXP)",
                                                 value = 17,
                                                 min = 0,
                                                 max = NA) #Let's home the economy doesn't crash and we don't really need an undefined maximum on the dollar price
                                    )),
                        tabItem(tabName = "viaticos_campo",
                                fluidRow(
                                  box(title = "Definición de objetivos",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Alimentos",
                                                 pairId = "vyc_do_alimentos",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Gasolina",
                                                 pairId = "vyc_do_gasolina",
                                                 costLabel = "$ / litro",
                                                 unitLabel = "Litros"),
                                      CostUnitUI(titleId = "Hospedaje",
                                                 pairId = "vyc_do_hospedaje",
                                                 costLabel = "$ / noche",
                                                 unitLabel = "Noches"),
                                      CostUnitUI(titleId = "Renta automovil",
                                                 pairId = "vyc_do_auto",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Taller preeliminar (objetivos)",
                                                 pairId = "vyc_do_taller",
                                                 costLabel = "$ / taller",
                                                 unitLabel = "Num. Talleres")
                                      ),
                                  box(title = "Entrenamiento de pescadores",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Certificación de buceo",
                                                 pairId = "vyc_certificacion",
                                                 costLabel = "$ / pescador",
                                                 unitLabel = "Num. Pescadores"),
                                      CostUnitUI(titleId = "Curso de buceo para pescadores",
                                                 pairId = "vyc_curso",
                                                 costLabel = "$ / pescador",
                                                 unitLabel = "Num. Pescadores")
                                      ),
                                  box(title = "Diseñar reservas",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Alimentos",
                                                 pairId = "vyc_dr_alimentos",
                                                 costLabel = "$ día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Hospedaje",
                                                 pairId = "vyc_dr_hospedaje",
                                                 costLabel = "$ / noche",
                                                 unitLabel = "Noches"),
                                      CostUnitUI(titleId = "Taller (diseño)",
                                                 pairId = "vyc_dr_taller",
                                                 costLabel = "$ / Taller",
                                                 unitLabel = "Num. Talleres")
                                      ),
                                  box(title = "Monitoreo",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Alimento",
                                                 pairId = "vyc_mo_alimento",
                                                 costLabel = "$ / pescador/ día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Alquiler embarcación",
                                                 pairId = "vyc_mo_embarcacion",
                                                 costLabel = "$ / hora",
                                                 unitLabel = "Horas"),
                                      CostUnitUI(titleId = "Aceite embarcación",
                                                 pairId = "vyc_mo_aceite",
                                                 costLabel = "$ / litro",
                                                 unitLabel = "Litros"),
                                      CostUnitUI(titleId = "Gasolina embarcación",
                                                 pairId = "vyc_mo_gasolina",
                                                 costLabel = "$ / litro",
                                                 unitLabel = "Litros"),
                                      CostUnitUI(titleId = "Hospedaje",
                                                 pairId = "vyc_mo_hospedaje",
                                                 costLabel = "$ / pescador / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Viaje",
                                                 pairId = "vyc_mo_viaje",
                                                 costLabel = "$ / pescador",
                                                 unitLabel = "Num. pescadores")
                                      )
                                  ),
                                fluidRow(
                                  box(title = "Presentación de resultados",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Alimentos",
                                                 pairId = "vyc_pr_alimentos",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Gasolina",
                                                 pairId = "vyc_pr_gasolina",
                                                 costLabel = "$ / litro",
                                                 unitLabel = "Litros"),
                                      CostUnitUI(titleId = "Hospedaje",
                                                 pairId = "vyc_pr_hospedaje",
                                                 costLabel = "$ / noche",
                                                 unitLabel = "Noches"),
                                      CostUnitUI(titleId = "Renta automovil",
                                                 pairId = "vyc_pr_auto",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")),
                                  box(title = "Renovación",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Alimentos",
                                                 pairId = "vyc_re_alimentos",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Encuestas",
                                                 pairId = "vyc_re_encuestas",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Gasolina",
                                                 pairId = "vyc_re_gasolina",
                                                 costLabel = "$ / litro",
                                                 unitLabel = "Litros"),
                                      CostUnitUI(titleId = "Hospedaje",
                                                 pairId = "vyc_re_hospedaje",
                                                 costLabel = "$ / noche",
                                                 unitLabel = "Noches"),
                                      CostUnitUI(titleId = "Renta automovil",
                                                 pairId = "vyc_re_auto",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Taller preliminar (renovación)",
                                                 pairId = "vyc_re_taller",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"))
                                )
                        ),
                        tabItem("salarios_beneficios",
                                fluidRow(
                                  box(title = "Definición de objetivos",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "syb_do_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "syb_do_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")),
                                  box(title = "Diseñar reservas",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "syb_dr_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "syb_dr_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")),
                                  box(title = "Elaboracón ETJ",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "syb_ej_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "syb_ej_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")),
                                  box(title = "Monitoreo",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "syb_mo_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "syb_mo_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")),
                                  box(title = "Análisis de datos",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "syb_ad_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "syb_ad_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")),
                                  box(title = "Presentación de resultados",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "syb_pr_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "syb_pr_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")),
                                  box(title = "Renovación",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "syb_re_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "syb_re_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")
                                      )
                                  )
                        ),
                        tabItem("equipo",
                                fluidRow(
                                  box(title = "Compra",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Cintas de transecto",
                                                 pairId = "equ_ce_cinta",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Tubos de PVC para monitoreo",
                                                 pairId = "equ_ce_tubo",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Chalecos buceo (BCD)",
                                                 pairId = "equ_ce_bcd",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Reguladores",
                                                 pairId = "equ_ce_reg",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Tanques",
                                                 pairId = "equ_ce_tanque",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Plomos",
                                                 pairId = "equ_ce_plomos",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Computadoras",
                                                 pairId = "equ_ce_computadora",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Equipo de snorkel",
                                                 pairId = "equ_ce_snorkel",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Brújula",
                                                 pairId = "equ_ce_brujula",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Tablas de monitoreo",
                                                 pairId = "equ_ce_tabla",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Sonda de profundidad",
                                                 pairId = "equ_ce_sonda",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Transductor de sonda",
                                                 pairId = "equ_ce_transductor",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "GPS",
                                                 pairId = "equ_ce_gps",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Radio marino",
                                                 pairId = "equ_ce_radio",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Botiquín de primeros auxilios",
                                                 pairId = "equ_ce_auxilios",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Botiquín oxígeno DAN",
                                                 pairId = "equ_ce_oxydan",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Caja pelican",
                                                 pairId = "equ_ce_pelican",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Reloj",
                                                 pairId = "equ_ce_reloj",
                                                 costLabel = "",
                                                 unitLabel = "")),
                                  box(title = "Mantenimiento",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "",
                                                 pairId = "",
                                                 costLabel = "",
                                                 unitLabel = "")),
                                  box(title = "Vigilancia",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "",
                                                 pairId = "",
                                                 costLabel = "",
                                                 unitLabel = ""))
                                  )
                                ),
                        tabItem("costos_directos",
                                fluidRow(
                                  box(title = "Elaboracón ETJ",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T),
                                  box(title = "Monitoreo",
                                      width = 3,
                                      status = "info",
                                      collapsible = T),
                                  box(title = "Renovación",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T))),
                        tabItem("consultores",
                                fluidRow(
                                  box(title = "Evaluación de pescadores",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T),
                                  box(title = "Monitoreo",
                                      width = 3,
                                      status = "info",
                                      collapsible = T))),
                        tabItem("comunicacion",
                                fluidRow(
                                  box(title = "Estrategias de comunicación",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T))),
                        tabItem("otros",
                                fluidRow(
                                  box(title = "Otros",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T))),
                        tabItem("presupuesto",
                                fluidRow(
                                  box(title = "Control de gráficas",
                                      status = "primary",
                                      width = 12,
                                      collapsible = T,
                                      collapsed = T,
                                  box(width = 2,
                                      status = "primary",
                                      checkboxInput(inputId = "costs_in_mxp",
                                                    label = "Gráficas en pesos",
                                                    value = F)
                                      
                                  ),
                                  box(width = 2,
                                      status = "primary",
                                      numericInput(inputId = "text_size",
                                                   label = "Tamaño del texto",
                                                   value = 10,
                                                   min = 10,
                                                   max = 20)
                                      
                                  ),
                                  box(width = 2,
                                      status = "primary",
                                      selectInput(inputId = "color_scheme",
                                                  label = "Esquema de colores",
                                                  choices = c("COBI" = "Blues",
                                                              "Pares" = "Paired",
                                                              "Set1" = "Set1",
                                                              "Set2" = "Set2",
                                                              "Set3" = "Set3"),
                                                  selected = "COBI")
                                      
                                  ))
                                ),
                                fluidRow(
                                  box(title = "Presupuesto",
                                      width = 6,
                                      status = "primary",
                                      plotlyOutput(outputId = "plot1")
                                  ),
                                  box(title = "Presupuesto 2",
                                      width = 6,
                                      status = "primary",
                                      plotlyOutput(outputId = "plot2")
                                  ),
                                  box(title = "Presupuesto3",
                                      width = 6,
                                      status = "primary",
                                      plotlyOutput(outputId = "plot3")
                                  ),
                                  box(title = "Presupuesto 4",
                                      width = 6,
                                      status = "primary",
                                      plotlyOutput(outputId = "plot4")
                                  )
                                )
                        )
                      )
                    )
)

# Define server logic
server <- function(input, output){
  
  
  inputs <- reactive({
    tibble(
      id = c(
        "vyc_certificacion",
        "vyc_curso",
        "vyc_do_alimentos",
        "vyc_do_auto",
        "vyc_do_gasolina",
        "vyc_do_hospedaje",
        "vyc_do_taller",
        "vyc_dr_alimentos",
        "vyc_dr_hospedaje",
        "vyc_dr_taller",
        "vyc_mo_aceite",
        "vyc_mo_alimento",
        "vyc_mo_embarcacion",
        "vyc_mo_gasolina",
        "vyc_mo_hospedaje",
        "vyc_mo_viaje",
        "vyc_pr_alimentos",
        "vyc_pr_auto",
        "vyc_pr_gasolina",
        "vyc_pr_hospedaje",
        "vyc_re_alimentos",
        "vyc_re_auto",
        "vyc_re_encuestas",
        "vyc_re_gasolina",
        "vyc_re_hospedaje",
        "vyc_re_taller",
        "syb_do_encargado",
        "syb_do_asistente",
        "syb_ej_encargado",
        "syb_ej_asistente",
        "syb_dr_encargado",
        "syb_dr_asistente",
        "syb_mo_encargado",
        "syb_mo_asistente",
        "syb_ad_encargado",
        "syb_ad_asistente",
        "syb_pr_encargado",
        "syb_pr_asistente",
        "syb_re_encargado",
        "syb_re_asistente",
        "equ_ce_cinta",
        "equ_ce_tubo",
        "equ_ce_bcd",
        "equ_ce_reg",
        "equ_ce_tanque",
        "equ_ce_plomos",
        "equ_ce_computadora",
        "equ_ce_snorkel",
        "equ_ce_brujula",
        "equ_ce_tabla",
        "equ_ce_sonda",
        "equ_ce_transductor",
        "equ_ce_gps",
        "equ_ce_radio",
        "equ_ce_auxilios",
        "equ_ce_oxydan",
        "equ_ce_pelican",
        "equ_ce_reloj"
      ),
      costs = c(
        input$c_vyc_certificacion,
        input$c_vyc_curso,
        input$c_vyc_do_alimentos,
        input$c_vyc_do_auto,
        input$c_vyc_do_gasolina,
        input$c_vyc_do_hospedaje,
        input$c_vyc_do_taller,
        input$c_vyc_dr_alimentos,
        input$c_vyc_dr_hospedaje,
        input$c_vyc_dr_taller,
        input$c_vyc_mo_aceite,
        input$c_vyc_mo_alimento,
        input$c_vyc_mo_embarcacion,
        input$c_vyc_mo_gasolina,
        input$c_vyc_mo_hospedaje,
        input$c_vyc_mo_viaje,
        input$c_vyc_pr_alimentos,
        input$c_vyc_pr_auto,
        input$c_vyc_pr_gasolina,
        input$c_vyc_pr_hospedaje,
        input$c_vyc_re_alimentos,
        input$c_vyc_re_auto,
        input$c_vyc_re_encuestas,
        input$c_vyc_re_gasolina,
        input$c_vyc_re_hospedaje,
        input$c_vyc_re_taller,
        input$c_syb_do_encargado,
        input$c_syb_do_asistente,
        input$c_syb_ej_encargado,
        input$c_syb_ej_asistente,
        input$c_syb_dr_encargado,
        input$c_syb_dr_asistente,
        input$c_syb_mo_encargado,
        input$c_syb_mo_asistente,
        input$c_syb_ad_encargado,
        input$c_syb_ad_asistente,
        input$c_syb_pr_encargado,
        input$c_syb_pr_asistente,
        input$c_syb_re_encargado,
        input$c_syb_re_asistente,
        input$c_equ_ce_cinta,
        input$c_equ_ce_tubo,
        input$c_equ_ce_bcd,
        input$c_equ_ce_reg,
        input$c_equ_ce_tanque,
        input$c_equ_ce_plomos,
        input$c_equ_ce_computadora,
        input$c_equ_ce_snorkel,
        input$c_equ_ce_brujula,
        input$c_equ_ce_tabla,
        input$c_equ_ce_sonda,
        input$c_equ_ce_transductor,
        input$c_equ_ce_gps,
        input$c_equ_ce_radio,
        input$c_equ_ce_auxilios,
        input$c_equ_ce_oxydan,
        input$c_equ_ce_pelican,
        input$c_equ_ce_reloj
      ),
      units = c(
        input$u_vyc_certificacion,
        input$u_vyc_curso,
        input$u_vyc_do_alimentos,
        input$u_vyc_do_auto,
        input$u_vyc_do_gasolina,
        input$u_vyc_do_hospedaje,
        input$u_vyc_do_taller,
        input$u_vyc_dr_alimentos,
        input$u_vyc_dr_hospedaje,
        input$u_vyc_dr_taller,
        input$u_vyc_mo_aceite,
        input$u_vyc_mo_alimento,
        input$u_vyc_mo_embarcacion,
        input$u_vyc_mo_gasolina,
        input$u_vyc_mo_hospedaje,
        input$u_vyc_mo_viaje,
        input$u_vyc_pr_alimentos,
        input$u_vyc_pr_auto,
        input$u_vyc_pr_gasolina,
        input$u_vyc_pr_hospedaje,
        input$u_vyc_re_alimentos,
        input$u_vyc_re_auto,
        input$u_vyc_re_encuestas,
        input$u_vyc_re_gasolina,
        input$u_vyc_re_hospedaje,
        input$u_vyc_re_taller,
        input$u_syb_do_encargado,
        input$u_syb_do_asistente,
        input$u_syb_ej_encargado,
        input$u_syb_ej_asistente,
        input$u_syb_dr_encargado,
        input$u_syb_dr_asistente,
        input$u_syb_mo_encargado,
        input$u_syb_mo_asistente,
        input$u_syb_ad_encargado,
        input$u_syb_ad_asistente,
        input$u_syb_pr_encargado,
        input$u_syb_pr_asistente,
        input$u_syb_re_encargado,
        input$u_syb_re_asistente,
        input$u_equ_ce_cinta,
        input$u_equ_ce_tubo,
        input$u_equ_ce_bcd,
        input$u_equ_ce_reg,
        input$u_equ_ce_tanque,
        input$u_equ_ce_plomos,
        input$u_equ_ce_computadora,
        input$u_equ_ce_snorkel,
        input$u_equ_ce_brujula,
        input$u_equ_ce_tabla,
        input$u_equ_ce_sonda,
        input$u_equ_ce_transductor,
        input$u_equ_ce_gps,
        input$u_equ_ce_radio,
        input$u_equ_ce_auxilios,
        input$u_equ_ce_oxydan,
        input$u_equ_ce_pelican,
        input$u_equ_ce_reloj
      )
    )
  })
  
  output$totalUSD <- renderInfoBox({
    
    total <- cost_data %>% 
      select(fase, concepto, subactividad, periodicidad, id) %>% 
      mutate(anos = case_when(periodicidad == "Anual" ~ 1 * input$anos,
                              periodicidad == "Bianal" ~ floor(0.5 * input$anos),
                              periodicidad == "Mensual" ~ 12 * input$anos,
                              TRUE ~ 1)) %>% 
      left_join(inputs(), by = "id") %>% 
      mutate(total = costs * units * anos) %$% 
      sum(total, na.rm = T)
    
    infoBox(title = "Costo total",
            value = total,
            subtitle = "USD",
            icon = icon("usd"),
            fill = T,
            color = "light-blue")
  })
  
  output$totalMXP <- renderInfoBox({
    
    total <- cost_data %>% 
      select(fase, concepto, subactividad, periodicidad, id) %>% 
      mutate(anos = case_when(periodicidad == "Anual" ~ 1 * input$anos,
                              periodicidad == "Bianal" ~ floor(0.5 * input$anos),
                              periodicidad == "Mensual" ~ 12 * input$anos,
                              TRUE ~ 1)) %>% 
      left_join(inputs(), by = "id") %>% 
      mutate(total = costs * units * anos) %$% 
      sum(total, na.rm = T)
    
    infoBox(title = "Costo total",
            value = round(total * input$usd2mxp),
            subtitle = "MXP",
            icon = icon("usd"),
            fill = T,
            color = "blue")
  })
  
  correct_fases <- tibble(fase = c("Implementacion",
                                   "Monitoreo",
                                   "Operacion",
                                   "Renovacion"),
                          Fase = c("Implementación",
                                   "Monitoreo",
                                   "Operación",
                                   "Renovación"))
  
    output$plot1 <- renderPlotly({
      
      plot1_data <- cost_data %>% 
        select(fase, concepto, subactividad, periodicidad, id) %>% 
        mutate(anos = case_when(periodicidad == "Anual" ~ 1 * input$anos,
                                periodicidad == "Bianal" ~ floor(0.5 * input$anos),
                                periodicidad == "Mensual" ~ 12 * input$anos,
                                TRUE ~ 1)) %>% 
        left_join(inputs(), by = "id") %>% 
        left_join(correct_fases, by = "fase") %>% 
        mutate(total = costs * units * anos) %>% 
        group_by(Fase, concepto, subactividad) %>% 
        summarize(total = sum(total, na.rm = T)) %>% 
        ungroup()
      
      if(input$costs_in_mxp){plot1_data$total <- plot1_data$total * input$usd2mxp / 1e3}
      
      y_label <- ifelse(input$costs_in_mxp, "Costo total (K MXP)", "Costo total (USD)")
      
      plot1 <- plot1_data %>% 
        rename(Concepto = concepto, Subactividad = subactividad, Total = total) %>% 
        ggplot(aes(x = Fase, y = Total, fill = Concepto, label = Subactividad)) +
        geom_col(color = "black") +
        theme_cowplot() +
        scale_fill_brewer(palette = input$color_scheme) +
        labs(x = "Fase del proyecto", y = y_label) +
        theme(text = element_text(size = input$text_size),
              axis.text = element_text(size = input$text_size - 2))
      
      p <- ggplotly(plot1)
      
      # Fix from https://github.com/ropensci/plotly/issues/985#issuecomment-328575761
      p$elementId <- NULL
      p
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

