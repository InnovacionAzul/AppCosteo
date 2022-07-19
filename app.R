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
library(purrr)
library(readxl)

rema_data <- readxl::read_xlsx("www/defaults_rema_fip.xlsx",
                               sheet = 1,
                               na = c("", "N/A")) %>%
  janitor::clean_names() %>%
  arrange(fase, subfase, concepto, actividad) %>%
  mutate(valor_unitario = ifelse(is.na(valor_unitario), 0, valor_unitario),
         estimacion_de_unidades_requeridas = ifelse(is.na(estimacion_de_unidades_requeridas), 0, estimacion_de_unidades_requeridas)) %>%
  mutate(section = "REMA")

fip_data <- readxl::read_xlsx("www/defaults_rema_fip.xlsx",
                               sheet = 2,
                               na = c("", "N/A")) %>%
  janitor::clean_names() %>%
  arrange(fase, subfase, concepto, actividad) %>%
  mutate(valor_unitario = ifelse(is.na(valor_unitario), 0, valor_unitario),
         estimacion_de_unidades_requeridas = ifelse(is.na(estimacion_de_unidades_requeridas), 0, estimacion_de_unidades_requeridas)) %>%
  mutate(section = "FIP")

cost_data <- rema_data %>%
  bind_rows(fip_data)

# cost_data <- read.csv("www/cost_data.csv",
#                       stringsAsFactors = F,
#                       strip.white = T,
#                       na.strings = "N/A") %>% 
#   janitor::clean_names() %>% 
#   arrange(fase, tipo_de_inversion, periodicidad, concepto, subactividad, elemento) %>% 
#   mutate(valor_unitario_usd = ifelse(
#     is.na(valor_unitario_usd),
#     0,
#     valor_unitario_usd),
#     estimacion_de_unidades_requeridas = ifelse(
#       is.na(estimacion_de_unidades_requeridas),
#       0,
#       estimacion_de_unidades_requeridas))

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
  costDefault <- cost_data$valor_unitario[cost_data$id == pairId]
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

makeUnit <- function(activity, data_subphase){
  
  # fluidRow(
    box(title = activity,
        width = 4,
        status = "primary",
        collapsible = T,
        collapsed = T,
        boxHeaderUI(),
        
        filter(data_subphase,
               actividad == activity) %$%
          pmap(.l = list(rubro,
                         id,
                         unidades,
                         stringr::str_remove_all(unidades, "[$/]")),
               .f = CostUnitUI)
    )
  #)
  
}

### Makes a row with a header and varying numbers of boxes for each subphase
activityWrapper <- function(subphase, number, data_fase){
  
  data_subphase <- data_fase %>%
    dplyr::filter(subfase == subphase)
  
  activities <- unique(data_subphase$actividad)
  
  tagList(
    fluidRow(
      column(3,
             valueBox(value = number, subtitle = subphase, icon = NULL, color = "aqua", width = 12,
                      href = NULL)
      ),
      column(9,
             fluidRow(
               map(.x = activities,
                   .f = makeUnit,
                   data_subphase = data_subphase)
             )
      )
    ),
    tags$hr()
  )
  
}

### Filters the data by section and phase and finds all subphases to iterate over 
subphaseWrapper <- function(data, phase, section){

  data_fase <- filter(data,
                      fase == phase,
                      section == section)
  
  subfases <- unique(data_fase$subfase)
  numbers <- seq(1:length(subfases))
  
  tagList(
    map2(.x = subfases,
         .y = numbers,
        .f = activityWrapper,
        data_fase = data_fase)
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
                        tabItem(tabName = "rema",
                                tabBox(id = "rema_tabs",
                                       width = 12,
                                       title = "REMA",
                                       ### Design
                                       tabPanel(title = "Diseño de Reservas",
                                                subphaseWrapper(data = rema_data,
                                                                phase = "Diseño",
                                                                section = "REMA")),
                                       ### Implementation
                                       tabPanel(title = "Implementación",
                                                subphaseWrapper(data = rema_data,
                                                                phase = "Implementación",
                                                                section = "REMA")),
                                       ### Reporting
                                       tabPanel(title = "Seguimiento",
                                                subphaseWrapper(data = rema_data,
                                                                phase = "Seguimiento",
                                                                section = "REMA")),
                                )
                        ),
                        tabItem(tabName = "fip",
                                tabBox(id = "fip_tabs",
                                       width = 12,
                                       title = "FIP",
                                       ### Design
                                       tabPanel(title = "Diseño de FIP",
                                                subphaseWrapper(data = fip_data,
                                                                phase = "Diseño",
                                                                section = "FIP")),
                                       ### Implementation
                                       tabPanel(title = "Implementación",
                                                subphaseWrapper(data = fip_data,
                                                                phase = "Implementación",
                                                                section = "FIP")),
                                       ### Reporting
                                       tabPanel(title = "Seguimiento",
                                                subphaseWrapper(data = fip_data,
                                                                phase = "Seguimiento",
                                                                section = "FIP")),
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
  
  ### Reactive object for REMA inputs
  rema_inputs <- reactiveValues(id = rema_data$id,
                                costos = rema_data$valor_unitario,
                                unidades = rema_data$estimacion_de_unidades_requeridas)
  
  ### Look for any changes to inputs in this 
  # observe({
  #   
  #   rema_ids <- rema_data$id
  #   rema_cost_inputs <- paste0("c_", rema_data$id)
  #   rema_unit_inputs <- paste0("u_", rema_data$id)
  #   browser()
  #   
  #   rema_inputs$costos <- map_dbl(rema_cost_inputs, function(x){eval(parse(text=paste0("input$", x)))})
  #   rema_inputs$unidades <- map_dbl
  #   sapply(function(x){eval(parse(text="input$c_dis_def_1"))})
  #   input[[rema_unit_inputs]]
  #   
  #   str(reactiveValuesToList(input)) 
  #   
  # })
  
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

