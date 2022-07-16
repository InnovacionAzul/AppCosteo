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
library(xlsx)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(plotly)
library(shinyBS)


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
  tooltipText <- cost_data$descripcion[cost_data$id == pairId]

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
                          min = 0))),
    bsTooltip(id = costId,
              title = tooltipText,
              placement = "right",
              trigger = "hover",
              options = list(container = "body"))
  )
}

makeUnit <- function(data, phase, subactivity){
  
  fluidRow(
    box(title = subactivity,
        width = 3,
        status = "primary",
        collapsible = T,
        boxHeaderUI(),
        
        filter(data,
               fase == phase,
               subactividad == subactivity) %$%
          pmap(.l = list(elemento,
                         id,
                         unidad,
                         stringr::str_remove_all(unidad, "[$/]")),
               .f = CostUnitUI)
    )
  )
  
}

# Define UI for application that draws a histogram
ui <- dashboardPage(title = "Costeo de Intervenciones",
                    header = dashboardHeader(
                      title = img(src = "img/COBI_logo.png", height = "52px")
                    ),
                    sidebar = dashboardSidebar(
                      width = 275,
                      sidebarMenu(id = "tabs", # Setting id makes input$tabs give the tabName of currently-selected tab
                                  menuItem(
                                    text = "Inicio",
                                    tabName = "inicio",
                                    icon = icon(name = "info")),
                                  menuItem(
                                    text = "Costeo de Reservas",
                                    tabName = "rema",
                                    icon = icon("dollar-sign")#,
                                    # menuSubItem(text = "Implementación",
                                                # tabName = "implementacion"),
                                    # menuSubItem(text = "Monitoreo",
                                                # tabName = "monitoreo"),
                                    # menuSubItem(text = "Operación",
                                                # tabName = "operacion"),
                                    # menuSubItem(text = "Renovación",
                                                # tabName = "renovacion")
                                    ),
                                  menuItem(
                                    text = "Costeo de FIP",
                                    tabName = "fip",
                                    icon = icon("dollar-sign")
                                  ),
                                  
                                  menuItem(
                                    text = "Presupuesto",
                                    tabName = "presupuesto",
                                    icon = icon("chart-bar") # cambiar por hand-holding-usd
                                  ),
                                  fluidRow(
                                    infoBoxOutput(
                                      outputId = "totalMXP",
                                      width = 12)),
                                  fluidRow(
                                    box(title = "Herramientas",
                                        status = "primary",
                                        background = "blue",
                                        width = 12,
                                        collapsible = T,
                                        collapsed = T,
                                        downloadButton(outputId = "download_total",
                                                       label = "Descargar presupuesto"),
                                        bookmarkButton(title = "Compartir",
                                                       label = "Compartir")
                                    )
                                  )
                      )
                    ),
                    body = dashboardBody(
                      tabItems(
                        tabItem(tabName = "inicio",
                                box(title = h1("Costeo de Reservas Marinas"),
                                    width = 12,
                                    status = "primary",
                                    p("Las reservas marinas completamente protegidas son áreas del océano restringidas a cualquier actividad extractiva, incluyendo la pesca. Las reservas marinas exitosas crean condiciones en las que las poblaciones de especies previamente capturadas se pueden recuperar y restaurar el equilibrio trófico en el ecosistema. La recuperación de la biomasa pesquera dentro de la reserva puede causar efectos colaterales en las zonas de pesca adyacentes, tanto desde el traslado de especímenes adultos, como en la exportación de larvas. Este efecto de desbordamiento puede ayudar a los usuarios a compensar algunos de los costos de oportunidad al ceder las zonas de pesca. La reserva marina funciona como una cuenta bancaria, la cual se repobla con el interés al capital con el desbordamiento."),
                                    p(a("COBI", href = "www.cobi.org.mx", target = "_blank"),"ha trabajado durante 19 años para establecer, evaluar y mantener las reservas marinas en colaboración con las comunidades pesqueras de México. Nuestro modelo de reservas marinas consiste de cuatro fases:"),
                                    tags$ul(
                                      tags$li(tags$b("1) Implementación - "),"procesos inclusivos en el que las partes interesadas participen en el proceso de diseño, definición de objetivos y la selección del sitio"),
                                      tags$li(tags$b("2) Monitoreo - "),"después de que la reserva se ha creado, miembros de la comunidad están capacitados para recopilar datos para evaluar la reserva"),
                                      tags$li(tags$b("3) Operación – "),"acciones relacionadas a la vigilancia comunitaria, señalización y comunicación de resultados"),
                                      tags$li(tags$b("4) Renovación – "),"manejo adaptativo basado en los datos recogidos por la comunidad para garantizar el funcionamiento eficaz de la reserva.")
                                    ),
                                    p("El número de iniciativas para establecer redes de reservas marinas está en aumento, y la gran mayoría de los esfuerzos se realizan con fondos filantrópicos. Sin embargo, al iniciar el proceso para establecer reservas marinas es común que los costos proyectados al futuro no estén claramente definidos. Esto puede afectar la sustentabilidad de la reserva marina al largo plazo, sobre todo si el costo de mantenerla y operarla es mayor al beneficio que puede proporcionar a la comunidad."),
                                    p("Este calculador de costos contempla todos los pasos necesarios para establecer una reserva marina utilizado el modelo COBI, con el objetivo de ayudar a comunidades, organizaciones de la sociedad civil y tomadores de decisiones de planear sus inversiones con mayor claridad y transparencia."),
                                    p("Para cualquier pregunta o comentario sobre este producto escribe al correo rema@cobi.org.mx Tus observaciones nos ayudarán a mejorar nuestras herramientas."),
                                    p("© COBI 2018")
                                )
                        ),
                        tabItem("rema",
                                tabBox(id = "rema",
                                       width = 12,
                                       title = "REMA",
                                       tabPanel(
                                         title = "Implementación",
                                         fluidRow(
                                           box(title = "Definir objetivos",
                                               width = 3,
                                               status = "primary",
                                               collapsible = T,
                                               boxHeaderUI(),
                                               
                                               filter(cost_data,
                                                      fase == "Implementacion",
                                                      subactividad == "Definir objetivos de la reserva") %$%
                                                 pmap(.l = list(elemento,
                                                                id,
                                                                unidad,
                                                                "dias"),
                                                      .f = CostUnitUI)
                                           )
                                         )
                                       ),
                                       tabPanel(
                                         title = "Diseño de Reservas",
                                         makeUnit(data = cost_data,
                                                  phase = "Implementacion",
                                                  subactivity = "Definir objetivos de la reserva"
                                                  )
                                       ),
                                       tabPanel("C"),
                                       tabPanel("D"),
                                       tabPanel("E"),
                                       
                                ),
                                ),
                        tabItem(tabName = "implementacion",
                                fluidRow(
                                  box(title = "Definición de objetivos",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Alimentos",
                                                 pairId = "imp_do_alimentos",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Gasolina",
                                                 pairId = "imp_do_gasolina",
                                                 costLabel = "$ / litro",
                                                 unitLabel = "Litros"),
                                      CostUnitUI(titleId = "Hospedaje",
                                                 pairId = "imp_do_hospedaje",
                                                 costLabel = "$ / noche",
                                                 unitLabel = "Noches"),
                                      CostUnitUI(titleId = "Renta automovil",
                                                 pairId = "imp_do_auto",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Taller preeliminar (objetivos)",
                                                 pairId = "imp_do_taller",
                                                 costLabel = "$ / taller",
                                                 unitLabel = "Num. Talleres"),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "imp_do_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "imp_do_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")
                                      ),
                                  box(title = "Diseño de reservas",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Alimentos",
                                                 pairId = "imp_dr_alimentos",
                                                 costLabel = "$ día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Hospedaje",
                                                 pairId = "imp_dr_hospedaje",
                                                 costLabel = "$ / noche",
                                                 unitLabel = "Noches"),
                                      CostUnitUI(titleId = "Taller (diseño)",
                                                 pairId = "imp_dr_taller",
                                                 costLabel = "$ / Taller",
                                                 unitLabel = "Num. Talleres"),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "imp_dr_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "imp_dr_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")
                                      ),
                                  box(title = "Elaboración de propuesta",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "imp_ej_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "imp_ej_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Trabajo de oficina",
                                                 pairId = "imp_ej_oficina",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")
                                      ),
                                  box(title = "Comunicación",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Estrategia de Comunicación",
                                                 pairId = "imp_ec_estrategia",
                                                 costLabel = "$ / programa",
                                                 unitLabel = "Programas")
                                      )
                                  ),
                                fluidRow(
                                  box(title = "Otros costos",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "imp_ot_otro1",
                                                 costLabel = "$",
                                                 unitLabel = "Unidades"),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "imp_ot_otro2",
                                                 costLabel = "$",
                                                 unitLabel = "Unidades"),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "imp_ot_otro3",
                                                 costLabel = "$",
                                                 unitLabel = "Unidades"),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "imp_ot_otro4",
                                                 costLabel = "$",
                                                 unitLabel = "Unidades"),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "imp_ot_otro5",
                                                 costLabel = "$",
                                                 unitLabel = "Unidades")
                                      )
                                )
                        ),
                        tabItem("monitoreo",
                                fluidRow(
                                  box(title = "Información del proyecto",
                                      width = 3,
                                      status = "info",
                                      numericInput(inputId = "mon_dur",
                                                   label = "Duración de la fase de Monitoreo (años)",
                                                   value = 5,
                                                   min = 1,
                                                   max = 50)
                                  )
                                ),
                                fluidRow(
                                  box(title = "Actividades de monitoreo",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Alimento",
                                                 pairId = "mon_mo_alimento",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Alquiler embarcación",
                                                 pairId = "mon_mo_embarcacion",
                                                 costLabel = "$ / hora",
                                                 unitLabel = "Horas"),
                                      CostUnitUI(titleId = "Aceite embarcación",
                                                 pairId = "mon_mo_aceite",
                                                 costLabel = "$ / litro",
                                                 unitLabel = "Litros"),
                                      CostUnitUI(titleId = "Gasolina embarcación",
                                                 pairId = "mon_mo_gasolina",
                                                 costLabel = "$ / litro",
                                                 unitLabel = "Litros"),
                                      CostUnitUI(titleId = "Hospedaje",
                                                 pairId = "mon_mo_hospedaje",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Seguros de buceo",
                                                 pairId = "mon_mo_seguro",
                                                 costLabel = "$ / seguro",
                                                 unitLabel = "Num. seguros"),
                                      CostUnitUI(titleId = "Salarios pescadores",
                                                 pairId = "mon_mo_salarios",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Viaje",
                                                 pairId = "mon_mo_viaje",
                                                 costLabel = "$ / pescador",
                                                 unitLabel = "Num. pescadores"),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "mon_mo_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "mon_mo_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")
                                      ),
                                  box(title = "Análisis de datos",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "mon_ad_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "mon_ad_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")
                                      ),
                                  box(title = "Capacitación de pescadores",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Certificación de buceo",
                                                 pairId = "mon_certificacion",
                                                 costLabel = "$ / pescador",
                                                 unitLabel = "Num. Pescadores"),
                                      CostUnitUI(titleId = "Curso de buceo para pescadores",
                                                 pairId = "mon_curso",
                                                 costLabel = "$ / pescador",
                                                 unitLabel = "Num. Pescadores")
                                      ),
                                  box(title = "Compra de equipo",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Cintas de transecto",
                                                 pairId = "mon_ce_cinta",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Tubos de PVC para monitoreo",
                                                 pairId = "mon_ce_tubo",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Chalecos buceo (BCD)",
                                                 pairId = "mon_ce_bcd",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Reguladores",
                                                 pairId = "mon_ce_reg",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Tanques",
                                                 pairId = "mon_ce_tanque",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Plomos",
                                                 pairId = "mon_ce_plomos",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Computadoras",
                                                 pairId = "mon_ce_computadora",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Equipo de snorkel",
                                                 pairId = "mon_ce_snorkel",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Brújula",
                                                 pairId = "mon_ce_brujula",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Tablas de monitoreo",
                                                 pairId = "mon_ce_tabla",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Sonda de profundidad",
                                                 pairId = "mon_ce_sonda",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Transductor de sonda",
                                                 pairId = "mon_ce_transductor",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "GPS",
                                                 pairId = "mon_ce_gps",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Radio marino",
                                                 pairId = "mon_ce_radio",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Botiquín de primeros auxilios",
                                                 pairId = "mon_ce_auxilios",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Botiquín oxígeno DAN",
                                                 pairId = "mon_ce_oxydan",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Caja pelican",
                                                 pairId = "mon_ce_pelican",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Reloj",
                                                 pairId = "mon_ce_reloj",
                                                 costLabel = "",
                                                 unitLabel = "")
                                  )
                                  ),
                                fluidRow(
                                  box(title = "Mantenimiento",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Reemplazo de equipo",
                                                 pairId = "mon_me_equiporeemplazo",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Equipo de mantenimiento",
                                                 pairId = "mon_me_equipomantenimiento",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Formatos de datos",
                                                 pairId = "mon_me_polypap",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Impresión de formatos",
                                                 pairId = "mon_me_impresion",
                                                 costLabel = "",
                                                 unitLabel = "")),
                                  box(title = "Otros",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Evaluación de pescadores",
                                                 pairId = "mon_evaluacion_pescadores",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "mon_ot_otro1",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "mon_ot_otro2",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "mon_ot_otro3",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "mon_ot_otro4",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "mon_ot_otro5",
                                                 costLabel = "",
                                                 unitLabel = ""))
                                  )
                        ),
                        tabItem("operacion",
                                fluidRow(
                                  box(title = "Información del proyecto",
                                      width = 3,
                                      status = "info",
                                      numericInput(inputId = "ope_dur",
                                                   label = "Duración de la fase de Operación (años)",
                                                   value = 5,
                                                   min = 1,
                                                   max = 50)
                                  )
                                ),
                                fluidRow(
                                  box(title = "Presentación de resultados",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Alimentos",
                                                 pairId = "ope_pr_alimentos",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Gasolina",
                                                 pairId = "ope_pr_gasolina",
                                                 costLabel = "$ / litro",
                                                 unitLabel = "Litros"),
                                      CostUnitUI(titleId = "Hospedaje",
                                                 pairId = "ope_pr_hospedaje",
                                                 costLabel = "$ / noche",
                                                 unitLabel = "Noches"),
                                      CostUnitUI(titleId = "Renta automovil",
                                                 pairId = "ope_pr_auto",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "ope_pr_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "ope_pr_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")
                                      ),
                                  box(title = "Vigilancia",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Boya",
                                                 pairId = "ope_vi_boya",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Instalación de boya",
                                                 pairId = "ope_vi_boyainst",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Cámara",
                                                 pairId = "ope_vi_camara",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Gasolina",
                                                 pairId = "ope_vi_gasolina",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "GPS",
                                                 pairId = "ope_vi_gps",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Radio marino",
                                                 pairId = "ope_vi_radio",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Salario vigilancia",
                                                 pairId = "ope_vi_salariovig",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Embarcación",
                                                 pairId = "ope_vi_embarcacion",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Mantenimiento embarcación",
                                                 pairId = "ope_vi_mantenimiento",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Motor embarcación",
                                                 pairId = "ope_vi_motor",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Registro embarcación",
                                                 pairId = "ope_vi_registro",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Seguro embarcación",
                                                 pairId = "ope_vi_seguro",
                                                 costLabel = "",
                                                 unitLabel = "")
                                      ),
                                  box(title = "Estrategia de comunicación",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Materiales de comunicación",
                                                 pairId = "ope_ec_comunicacion",
                                                 costLabel = "$ / unidad",
                                                 unitLabel = "Unidades"),
                                      CostUnitUI(titleId = "Señalización de la reserva",
                                                 pairId = "ope_ec_senalizacion",
                                                 costLabel = "$ / unidad",
                                                 unitLabel = "Unidades")
                                      ),
                                  box(title = "Otros costos",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "ope_ot_otro1",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "ope_ot_otro2",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "ope_ot_otro3",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "ope_ot_otro4",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "ope_ot_otro5",
                                                 costLabel = "",
                                                 unitLabel = "")
                                  )
                                  )
                                ),
                        tabItem("renovacion",
                                fluidRow(
                                  box(title = "Elaboracón ETJ",
                                      width = 3,
                                      status = "primary",
                                      collapsible = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Alimentos",
                                                 pairId = "ren_re_alimentos",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Encuestas",
                                                 pairId = "ren_re_encuestas",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Gasolina",
                                                 pairId = "ren_re_gasolina",
                                                 costLabel = "$ / litro",
                                                 unitLabel = "Litros"),
                                      CostUnitUI(titleId = "Hospedaje",
                                                 pairId = "ren_re_hospedaje",
                                                 costLabel = "$ / noche",
                                                 unitLabel = "Noches"),
                                      CostUnitUI(titleId = "Renta automovil",
                                                 pairId = "ren_re_auto",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Taller preliminar (renovación)",
                                                 pairId = "ren_re_taller",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Encargado del proyecto",
                                                 pairId = "ren_re_encargado",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Asistente del proyecto",
                                                 pairId = "ren_re_asistente",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días"),
                                      CostUnitUI(titleId = "Trabajo de oficina",
                                                 pairId = "ren_re_oficina",
                                                 costLabel = "$ / día",
                                                 unitLabel = "Días")
                                      ),
                                  box(title = "Otros costos",
                                      width = 3,
                                      status = "info",
                                      collapsible = T,
                                      collapsed = T,
                                      boxHeaderUI(),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "ren_ot_otro1",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "ren_ot_otro2",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "ren_ot_otro3",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "ren_ot_otro4",
                                                 costLabel = "",
                                                 unitLabel = ""),
                                      CostUnitUI(titleId = "Otros costos",
                                                 pairId = "ren_ot_otro5",
                                                 costLabel = "",
                                                 unitLabel = "")
                                  )
                                )
                        ),
                        tabItem("presupuesto",
                                fluidRow(
                                  box(title = "Control de gráficas",
                                      status = "primary",
                                      width = 12,
                                      collapsible = T,
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
                                  box(title = "Presupuesto por fases",
                                      width = 6,
                                      status = "primary",
                                      plotlyOutput(outputId = "plot1")
                                  ),
                                  box(title = "Presupuesto por conceptos",
                                      width = 6,
                                      status = "primary",
                                      plotlyOutput(outputId = "plot2")
                                  )
                                )
                        )
                      )
                    )
)


# Define server logic
server <- function(input, output){
  
  periods <- reactive({
    tibble(
      etapa = c("imp", "mon", "ope", "ren"),
      duracion_fase = c(1,
                        input$mon_dur,
                        input$ope_dur,
                        1)
    )
  })
  
  inputs <- reactive({
    tibble(
      id = c(
        "imp_do_alimentos",
        "imp_do_auto",
        "imp_do_gasolina",
        "imp_do_hospedaje",
        "imp_do_taller",
        "imp_do_asistente",
        "imp_do_encargado",
        "imp_dr_alimentos",
        "imp_dr_asistente",
        "imp_dr_encargado",
        "imp_dr_hospedaje",
        "imp_dr_taller",
        "imp_ec_estrategia",
        "imp_ej_asistente",
        "imp_ej_encargado",
        "imp_ej_oficina",
        "imp_ot_otro1",
        "imp_ot_otro2",
        "imp_ot_otro3",
        "imp_ot_otro4",
        "imp_ot_otro5",
        "mon_certificacion",
        "mon_curso",
        "mon_ce_auxilios",
        "mon_ce_bcd",
        "mon_ce_brujula",
        "mon_ce_cinta",
        "mon_ce_computadora",
        "mon_ce_gps",
        "mon_ce_oxydan",
        "mon_ce_pelican",
        "mon_ce_plomos",
        "mon_ce_radio",
        "mon_ce_reg",
        "mon_ce_reloj",
        "mon_ce_snorkel",
        "mon_ce_sonda",
        "mon_ce_tabla",
        "mon_ce_tanque",
        "mon_ce_transductor",
        "mon_ce_tubo",
        "mon_evaluacion_pescadores",
        "mon_me_equipomantenimiento",
        "mon_me_equiporeemplazo",
        "mon_me_impresion",
        "mon_me_polypap",
        "mon_mo_aceite",
        "mon_mo_alimento",
        "mon_mo_embarcacion",
        "mon_mo_gasolina",
        "mon_mo_hospedaje",
        "mon_mo_salarios",
        "mon_mo_seguro",
        "mon_mo_viaje",
        "mon_ot_otro1",
        "mon_ot_otro2",
        "mon_ot_otro3",
        "mon_ot_otro4",
        "mon_ot_otro5",
        "ope_ec_comunicacion",
        "ope_ec_senalizacion",
        "ope_ot_otro1",
        "ope_ot_otro2",
        "ope_ot_otro3",
        "ope_ot_otro4",
        "ope_ot_otro5",
        "ope_pr_alimentos",
        "ope_pr_asistente",
        "ope_pr_auto",
        "ope_pr_encargado",
        "ope_pr_gasolina",
        "ope_pr_hospedaje",
        "ope_vi_boya",
        "ope_vi_boyainst",
        "ope_vi_camara",
        "ope_vi_embarcacion",
        "ope_vi_gasolina",
        "ope_vi_gps",
        "ope_vi_mantenimiento",
        "ope_vi_motor",
        "ope_vi_radio",
        "ope_vi_registro",
        "ope_vi_salariovig",
        "ope_vi_seguro",
        "ren_ot_otro1",
        "ren_ot_otro2",
        "ren_ot_otro3",
        "ren_ot_otro4",
        "ren_ot_otro5",
        "ren_re_alimentos",
        "ren_re_asistente",
        "ren_re_auto",
        "ren_re_encargado",
        "ren_re_encuestas",
        "ren_re_gasolina",
        "ren_re_hospedaje",
        "ren_re_taller",
        "ren_re_oficina",
        "mon_ad_asistente",
        "mon_ad_encargado",
        "mon_mo_asistente",
        "mon_mo_encargado"
      ),
      costos = c(
        input$c_imp_do_alimentos,
        input$c_imp_do_auto,
        input$c_imp_do_gasolina,
        input$c_imp_do_hospedaje,
        input$c_imp_do_taller,
        input$c_imp_do_asistente,
        input$c_imp_do_encargado,
        input$c_imp_dr_alimentos,
        input$c_imp_dr_asistente,
        input$c_imp_dr_encargado,
        input$c_imp_dr_hospedaje,
        input$c_imp_dr_taller,
        input$c_imp_ec_estrategia,
        input$c_imp_ej_asistente,
        input$c_imp_ej_encargado,
        input$c_imp_ej_oficina,
        input$c_imp_ot_otro1,
        input$c_imp_ot_otro2,
        input$c_imp_ot_otro3,
        input$c_imp_ot_otro4,
        input$c_imp_ot_otro5,
        input$c_mon_certificacion,
        input$c_mon_curso,
        input$c_mon_ce_auxilios,
        input$c_mon_ce_bcd,
        input$c_mon_ce_brujula,
        input$c_mon_ce_cinta,
        input$c_mon_ce_computadora,
        input$c_mon_ce_gps,
        input$c_mon_ce_oxydan,
        input$c_mon_ce_pelican,
        input$c_mon_ce_plomos,
        input$c_mon_ce_radio,
        input$c_mon_ce_reg,
        input$c_mon_ce_reloj,
        input$c_mon_ce_snorkel,
        input$c_mon_ce_sonda,
        input$c_mon_ce_tabla,
        input$c_mon_ce_tanque,
        input$c_mon_ce_transductor,
        input$c_mon_ce_tubo,
        input$c_mon_evaluacion_pescadores,
        input$c_mon_me_equipomantenimiento,
        input$c_mon_me_equiporeemplazo,
        input$c_mon_me_impresion,
        input$c_mon_me_polypap,
        input$c_mon_mo_aceite,
        input$c_mon_mo_alimento,
        input$c_mon_mo_embarcacion,
        input$c_mon_mo_gasolina,
        input$c_mon_mo_hospedaje,
        input$c_mon_mo_salarios,
        input$c_mon_mo_seguro,
        input$c_mon_mo_viaje,
        input$c_mon_ot_otro1,
        input$c_mon_ot_otro2,
        input$c_mon_ot_otro3,
        input$c_mon_ot_otro4,
        input$c_mon_ot_otro5,
        input$c_ope_ec_comunicacion,
        input$c_ope_ec_senalizacion,
        input$c_ope_ot_otro1,
        input$c_ope_ot_otro2,
        input$c_ope_ot_otro3,
        input$c_ope_ot_otro4,
        input$c_ope_ot_otro5,
        input$c_ope_pr_alimentos,
        input$c_ope_pr_asistente,
        input$c_ope_pr_auto,
        input$c_ope_pr_encargado,
        input$c_ope_pr_gasolina,
        input$c_ope_pr_hospedaje,
        input$c_ope_vi_boya,
        input$c_ope_vi_boyainst,
        input$c_ope_vi_camara,
        input$c_ope_vi_embarcacion,
        input$c_ope_vi_gasolina,
        input$c_ope_vi_gps,
        input$c_ope_vi_mantenimiento,
        input$c_ope_vi_motor,
        input$c_ope_vi_radio,
        input$c_ope_vi_registro,
        input$c_ope_vi_salariovig,
        input$c_ope_vi_seguro,
        input$c_ren_ot_otro1,
        input$c_ren_ot_otro2,
        input$c_ren_ot_otro3,
        input$c_ren_ot_otro4,
        input$c_ren_ot_otro5,
        input$c_ren_re_alimentos,
        input$c_ren_re_asistente,
        input$c_ren_re_auto,
        input$c_ren_re_encargado,
        input$c_ren_re_encuestas,
        input$c_ren_re_gasolina,
        input$c_ren_re_hospedaje,
        input$c_ren_re_taller,
        input$c_ren_re_oficina,
        input$c_mon_ad_asistente,
        input$c_mon_ad_encargado,
        input$c_mon_dr_asistente,
        input$c_mon_dr_encargado,
        input$c_mon_mo_asistente,
        input$c_mon_mo_encargado
      ),
      unidades = c(
        input$u_imp_do_alimentos,
        input$u_imp_do_auto,
        input$u_imp_do_gasolina,
        input$u_imp_do_hospedaje,
        input$u_imp_do_taller,
        input$u_imp_do_asistente,
        input$u_imp_do_encargado,
        input$u_imp_dr_alimentos,
        input$u_imp_dr_asistente,
        input$u_imp_dr_encargado,
        input$u_imp_dr_hospedaje,
        input$u_imp_dr_taller,
        input$u_imp_ec_estrategia,
        input$u_imp_ej_asistente,
        input$u_imp_ej_encargado,
        input$u_imp_ej_oficina,
        input$u_imp_ot_otro1,
        input$u_imp_ot_otro2,
        input$u_imp_ot_otro3,
        input$u_imp_ot_otro4,
        input$u_imp_ot_otro5,
        input$u_mon_certificacion,
        input$u_mon_curso,
        input$u_mon_ce_auxilios,
        input$u_mon_ce_bcd,
        input$u_mon_ce_brujula,
        input$u_mon_ce_cinta,
        input$u_mon_ce_computadora,
        input$u_mon_ce_gps,
        input$u_mon_ce_oxydan,
        input$u_mon_ce_pelican,
        input$u_mon_ce_plomos,
        input$u_mon_ce_radio,
        input$u_mon_ce_reg,
        input$u_mon_ce_reloj,
        input$u_mon_ce_snorkel,
        input$u_mon_ce_sonda,
        input$u_mon_ce_tabla,
        input$u_mon_ce_tanque,
        input$u_mon_ce_transductor,
        input$u_mon_ce_tubo,
        input$u_mon_evaluacion_pescadores,
        input$u_mon_me_equipomantenimiento,
        input$u_mon_me_equiporeemplazo,
        input$u_mon_me_impresion,
        input$u_mon_me_polypap,
        input$u_mon_mo_aceite,
        input$u_mon_mo_alimento,
        input$u_mon_mo_embarcacion,
        input$u_mon_mo_gasolina,
        input$u_mon_mo_hospedaje,
        input$u_mon_mo_salarios,
        input$u_mon_mo_seguro,
        input$u_mon_mo_viaje,
        input$u_mon_ot_otro1,
        input$u_mon_ot_otro2,
        input$u_mon_ot_otro3,
        input$u_mon_ot_otro4,
        input$u_mon_ot_otro5,
        input$u_ope_ec_comunicacion,
        input$u_ope_ec_senalizacion,
        input$u_ope_ot_otro1,
        input$u_ope_ot_otro2,
        input$u_ope_ot_otro3,
        input$u_ope_ot_otro4,
        input$u_ope_ot_otro5,
        input$u_ope_pr_alimentos,
        input$u_ope_pr_asistente,
        input$u_ope_pr_auto,
        input$u_ope_pr_encargado,
        input$u_ope_pr_gasolina,
        input$u_ope_pr_hospedaje,
        input$u_ope_vi_boya,
        input$u_ope_vi_boyainst,
        input$u_ope_vi_camara,
        input$u_ope_vi_embarcacion,
        input$u_ope_vi_gasolina,
        input$u_ope_vi_gps,
        input$u_ope_vi_mantenimiento,
        input$u_ope_vi_motor,
        input$u_ope_vi_radio,
        input$u_ope_vi_registro,
        input$u_ope_vi_salariovig,
        input$u_ope_vi_seguro,
        input$u_ren_ot_otro1,
        input$u_ren_ot_otro2,
        input$u_ren_ot_otro3,
        input$u_ren_ot_otro4,
        input$u_ren_ot_otro5,
        input$u_ren_re_alimentos,
        input$u_ren_re_asistente,
        input$u_ren_re_auto,
        input$u_ren_re_encargado,
        input$u_ren_re_encuestas,
        input$u_ren_re_gasolina,
        input$u_ren_re_hospedaje,
        input$u_ren_re_taller,
        input$u_ren_re_oficina,
        input$u_mon_ad_asistente,
        input$u_mon_ad_encargado,
        input$u_mon_dr_asistente,
        input$u_mon_dr_encargado,
        input$u_mon_mo_asistente,
        input$u_mon_mo_encargado
      )
    )
  })
  
  totals <- reactive({cost_data %>% 
      select(fase, concepto, subactividad, periodicidad, id) %>% 
      left_join(inputs(), by = "id") %>% 
      mutate(etapa = substr(x = id, start = 1, stop = 3)) %>% 
      left_join(periods(), by = "etapa") %>% 
      mutate(eventos = case_when(periodicidad == "Anual" ~ 1 * duracion_fase,
                            periodicidad == "Bianual" ~ floor(0.5 * duracion_fase),
                            periodicidad == "Mensual" ~ 12 * duracion_fase,
                            TRUE ~ 1)) %>% 
      mutate(total = costos * unidades * eventos) %>% 
      select(id, fase, concepto, subactividad, duracion_fase, periodicidad, eventos, costos, unidades, total, -etapa)
    })
  
  output$totalUSD <- renderInfoBox({
    
    total <- sum(totals()$total)
    
    infoBox(title = "Costo total",
            value = total,
            subtitle = "USD",
            icon = icon("dollar-sign"),
            fill = T,
            color = "light-blue")
  })
  
  output$totalMXP <- renderInfoBox({
    
    total <- sum(totals()$total)
    
    infoBox(title = "Costo total",
            value = round(total * input$usd2mxp),
            subtitle = "MXP",
            icon = icon("dollar-sign"),
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
      
      plot1_data <- totals()  %>% 
        filter(total > 0) %>% 
        group_by(fase, concepto, subactividad) %>% 
        summarize(total = sum(total, na.rm = T) / 1e3) %>% 
        ungroup()
      
      if(input$costs_in_mxp){plot1_data$total <- plot1_data$total * input$usd2mxp}
      
      y_label <- ifelse(input$costs_in_mxp, "Costo total (K MXP)", "Costo total (K USD)")
      
      plot1 <- plot1_data %>% 
        rename(Concepto = concepto,
               Subactividad = subactividad,
               Total = total,
               Fase = fase) %>% 
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
    
    output$plot2 <- renderPlotly({
      
      plot2_data <- totals()  %>% 
        filter(total > 0) %>% 
        group_by(fase, concepto, subactividad) %>% 
        summarize(total = sum(total, na.rm = T) / 1e3) %>% 
        ungroup()
      
      if(input$costs_in_mxp){plot2_data$total <- plot2_data$total * input$usd2mxp}
      
      y_label <- ifelse(input$costs_in_mxp, "Costo total (K MXP)", "Costo total (K USD)")
      
      plot2 <- plot2_data %>% 
        rename(Concepto = concepto,
               Subactividad = subactividad,
               Total = total,
               Fase = fase) %>% 
        ggplot(aes(x = Concepto, y = Total, fill = Fase, label = Subactividad)) +
        geom_col(color = "black") +
        theme_cowplot() +
        scale_fill_brewer(palette = input$color_scheme) +
        labs(x = "Fase del proyecto", y = y_label) +
        theme(text = element_text(size = input$text_size),
              axis.text = element_text(size = input$text_size - 2)) +
        coord_flip()
      
      p <- ggplotly(plot2)
      
      # Fix from https://github.com/ropensci/plotly/issues/985#issuecomment-328575761
      p$elementId <- NULL
      p
    })
    
    output$download_total <- downloadHandler(filename = "Presupuesto.xlsx",
                                             content = function(file){
                                               write.xlsx(x = totals(),
                                                          file = file,
                                                          sheetName = "Presupuesto",
                                                          row.names = F,
                                                          showNA = T)
                                             }
      # This function should write data to a file given to it by
      # the argument 'file'.
      # content = function(file) {
      #   # Write to a file specified by the 'file' argument
      #   write.table(datasetInput(), file, sep = sep,
      #               row.names = FALSE)
      # }
    )
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

