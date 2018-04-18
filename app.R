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
                                      outputId = "total",
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
                                                 max = 50)
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
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Alquiler embarcación",
                                                 pairId = "vyc_mo_embarcacion",
                                                 costLabel = "$ / hora",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Aceite embarcación",
                                                 pairId = "vyc_mo_aceite",
                                                 costLabel = "$ / litro",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Gasolina embarcación",
                                                 pairId = "vyc_mo_gasolina",
                                                 costLabel = "$ / litro",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Hospedaje",
                                                 pairId = "vyc_mo_hospedaje",
                                                 costLabel = "$ / pescador / día",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Viaje",
                                                 pairId = "vyc_mo_viaje",
                                                 costLabel = "$ / pescador",
                                                 unitLabel = "")
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
                                      collapsible = T),
                                  box(title = "Diseñar reservas",
                                      width = 3,
                                      status = "info",
                                      collapsible = T),
                                  box(title = "Elaboracón ETJ",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T),
                                  box(title = "Monitoreo",
                                      width = 3,
                                      status = "info",
                                      collapsible = T),
                                  box(title = "Análisis de datos",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T),
                                  box(title = "Presentación de resultados",
                                      width = 3,
                                      status = "info",
                                      collapsible = T),
                                  box(title = "Renovación",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T))
                        ),
                        tabItem("equipo",
                                fluidRow(
                                  box(title = "Compra",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T),
                                  box(title = "Mantenimiento",
                                      width = 3,
                                      status = "info",
                                      collapsible = T),
                                  box(title = "Vigilancia",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T))),
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
                                  box(title = "Presupuesto",
                                      width = 6,
                                      status = "primary",
                                      plotOutput(outputId = "plot1"))))
                      ))
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
        "vyc_re_taller"),
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
        input$c_vyc_re_taller),
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
        input$u_vyc_re_taller)
    )
  })
  
  output$total <- renderInfoBox({
    
    total <- cost_data %>% 
      select(fase, concepto, subactividad, periodicidad, id) %>% 
      mutate(anos = case_when(periodicidad == "Anual" ~ 1 * input$anos,
                              periodicidad == "Bianal" ~ 0.5 * input$anos,
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
  
  output$plot1 <- renderPlot({
    cost_data %>% 
      select(fase, concepto, subactividad, periodicidad, id) %>% 
      mutate(anos = case_when(periodicidad == "Anual" ~ 1 * input$anos,
                              periodicidad == "Bianal" ~ 0.5 * input$anos,
                              periodicidad == "Mensual" ~ 12 * input$anos)) %>% 
      left_join(inputs(), by = "id") %>% 
      mutate(total = costs * units * anos) %>% 
      group_by(fase, concepto, subactividad) %>% 
      summarize(total = sum(total, na.rm = T)) %>% 
      ungroup() %>% 
      ggplot(aes(x = fase, y = total, fill = concepto)) +
      geom_col() +
      theme_cowplot()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

