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
#library(xlsx)
library(magrittr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(plotly)
library(shinyBS)
library(purrr)
library(readxl)

# Source functions
source("helpers.R")

### Read in data 
rema_data <- readxl::read_xlsx("www/defaults_rema_fip.xlsx",
                               sheet = 1,
                               na = c("", "N/A")) %>%
  janitor::clean_names() %>%
  arrange(fase, subfase_orden, actividad_orden) %>%
  mutate(valor_unitario = ifelse(is.na(valor_unitario), 0, valor_unitario),
         estimacion_de_unidades_requeridas = ifelse(is.na(estimacion_de_unidades_requeridas), 0, estimacion_de_unidades_requeridas),
         unidades = ifelse(is.na(unidades), "$/unidad", unidades)) %>%
  mutate(section = "REMA")

fip_data <- readxl::read_xlsx("www/defaults_rema_fip.xlsx",
                               sheet = 2,
                               na = c("", "N/A")) %>%
  janitor::clean_names() %>%
  arrange(fase, subfase_orden, actividad_orden) %>%
  mutate(valor_unitario = ifelse(is.na(valor_unitario), 0, valor_unitario),
         estimacion_de_unidades_requeridas = ifelse(is.na(estimacion_de_unidades_requeridas), 0, estimacion_de_unidades_requeridas),
         unidades = ifelse(is.na(unidades), "$/unidad", unidades)) %>%
  mutate(section = "FIP")

cost_data <- rema_data %>%
  bind_rows(fip_data)

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
                                      outputId = "REMAtotalUSD",
                                      width = 12)),
                                  fluidRow(
                                    infoBoxOutput(
                                      outputId = "FIPtotalUSD",
                                      width = 12)),
                                  fluidRow(
                                    box(title = "Herramientas",
                                        status = "primary",
                                        background = "blue",
                                        width = 12,
                                        collapsible = T,
                                        collapsed = T,
                                        fileInput(
                                          inputId = "budget_upload",
                                          label = "Cargar archivo",
                                          multiple = F,
                                          accept = c(".xls", ".xlsx"),
                                          buttonLabel = "Cargar"
                                        ),
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
                        ### Marine reserves
                        tabItem(tabName = "rema",
                                tabBox(id = "rema_tabs",
                                       width = 12,
                                       title = "REMA",
                                       tags$br(),
                                       ### Design
                                       tabPanel(title = "Diseño de Reservas",
                                                makePhaseDuration(phase = "Diseño",
                                                                  section = "REMA"),
                                                subphaseWrapper(data = rema_data,
                                                                phase = "Diseño",
                                                                section = "REMA")),
                                       ### Implementation
                                       tabPanel(title = "Implementación",
                                                makePhaseDuration(phase = "Implementación",
                                                                  section = "REMA"),                                  
                                                subphaseWrapper(data = rema_data,
                                                                phase = "Implementación",
                                                                section = "REMA")),
                                       ### Follow up
                                       tabPanel(title = "Seguimiento",
                                                makePhaseDuration(phase = "Seguimiento",
                                                                  section = "REMA"),
                                                subphaseWrapper(data = rema_data,
                                                                phase = "Seguimiento",
                                                                section = "REMA")),
                                )
                        ),
                        ### FIPs
                        tabItem(tabName = "fip",
                                tabBox(id = "fip_tabs",
                                       width = 12,
                                       title = "FIP",
                                       tags$br(),
                                       ### Design
                                       tabPanel(title = "Diseño de FIP",
                                                makePhaseDuration(phase = "Diseño",
                                                                  section = "FIP"),
                                                subphaseWrapper(data = fip_data,
                                                                phase = "Diseño",
                                                                section = "FIP")),
                                       ### Implementation
                                       tabPanel(title = "Implementación",
                                                makePhaseDuration(phase = "Implementación",
                                                                  section = "FIP"),
                                                uiOutput("imp_fip")
# 
#                                                 subphaseWrapper(data = fip_data,
#                                                                 phase = "Implementación",
#                                                                 section = "FIP")
                                                ),
                                       ### Follow up
                                       tabPanel(title = "Seguimiento",
                                                makePhaseDuration(phase = "Seguimiento",
                                                                  section = "FIP"),
                                                subphaseWrapper(data = fip_data,
                                                                phase = "Seguimiento",
                                                                section = "FIP")),
                                )
                        ),
                        ### Plots
                        tabItem("presupuesto",
                                fluidRow(
                                  box(title = "Control de gráficas",
                                      status = "primary",
                                      width = 12,
                                      collapsible = T,
                                      # box(width = 2,
                                      #     status = "primary",
                                      #     checkboxInput(inputId = "costs_in_mxp",
                                      #                   label = "Gráficas en pesos",
                                      #                   value = F)
                                      #     
                                      # ),
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
                                      width = 12,
                                      status = "primary",
                                      plotlyOutput(outputId = "plot1"))
                                ),
                                fluidRow(
                                  box(title = "Presupuesto por conceptos",
                                      width = 12,
                                      status = "primary",
                                      plotlyOutput(outputId = "plot2"))
                                )
                        ) #/tabItem
                      ) #/tabItems
                    ) #/dashboardBody
) #/ui


# Define server logic
server <- function(input, output){
  
  ### Reactive UI for FIP Implementation stage
  output$imp_fip <- renderUI({
    subphaseWrapper(data = fip_data,
                    phase = "Implementación",
                    section = "FIP",
                    subphases_to_include = input$choices_imp_fip)
  })
  
  outputOptions(output, "imp_fip", suspendWhenHidden = FALSE, priority = 10)
  
  ### Durations of each stage
  periods <- reactiveValues(df = tibble(etapa = rep(c("dis", "imp", "seg"), times = 2),
                                        section = rep(c("REMA", "FIP"), each = 3),
                                        duracion_fase = rep(1, length.out = 6)))
  
  observe({
    periods$df$duracion_fase <- c(input$d_dis_rema,
                                  input$d_imp_rema,
                                  input$d_seg_rema,
                                  input$d_dis_fip,
                                  input$d_imp_fip,
                                  input$d_seg_fip)
  })
  
  # periods <- reactive({
  #   tibble(etapa = c("dis", "imp", "seg", "mon", "ope", "ren"),
  #          duracion_fase = c(1,
  #                            1,
  #                            1,
  #                            1,#input$mon_dur,
  #                            1,#input$ope_dur,
  #                            1))
  # })
  
  ### Reactive object for REMA and FIP inputs
  input_rv <- reactiveValues(rema = tibble(id = rema_data$id,
                                         costos = rema_data$valor_unitario,
                                         unidades = rema_data$estimacion_de_unidades_requeridas),
                             fip = tibble(id = fip_data$id,
                                          costos = fip_data$valor_unitario,
                                          unidades = fip_data$estimacion_de_unidades_requeridas))
  
  ## Look for any changes to inputs in the REMA section
  observe({
    
    valid_c_rema_inputs <- rema_data$id[which(paste0("c_", rema_data$id) %in% names(input))]
    
    rema_costos <- purrr::map2_dfr(.x = valid_c_rema_inputs,
                                  .y = paste0("c_", valid_c_rema_inputs),
                                  ~ {
                                    tibble(id = .x,
                                           costos = input[[.y]])
                                  })
    
    input_rv$rema$costos[input_rv$rema$id %in% rema_costos$id] <- rema_costos$costos
    
    valid_u_rema_inputs <- rema_data$id[which(paste0("u_", rema_data$id) %in% names(input))]
    
    rema_unidades <- purrr::map2_dfr(.x = valid_u_rema_inputs,
                                    .y = paste0("u_", valid_u_rema_inputs),
                                    ~ {
                                      tibble(id = .x,
                                             unidades = input[[.y]])
                                    })
    
    input_rv$rema$unidades[input_rv$rema$id %in% rema_unidades$id] <- rema_unidades$unidades

  })
  
  ## Look for any changes to inputs in the REMA section
  observe({
    
    valid_c_fip_inputs <- fip_data$id[which(paste0("c_", fip_data$id) %in% names(input))]
    
    fip_costos <- purrr::map2_dfr(.x = valid_c_fip_inputs,
                                  .y = paste0("c_", valid_c_fip_inputs),
                                  ~ {
                                    tibble(id = .x,
                                           costos = input[[.y]])
                                  })
    
    input_rv$fip$costos[input_rv$fip$id %in% fip_costos$id] <- fip_costos$costos
    
    valid_u_fip_inputs <- fip_data$id[which(paste0("u_", fip_data$id) %in% names(input))]
    
    fip_unidades <- purrr::map2_dfr(.x = valid_u_fip_inputs,
                                    .y = paste0("u_", valid_u_fip_inputs),
                                    ~ {
                                      tibble(id = .x,
                                             unidades = input[[.y]])
                                    })
    
    input_rv$fip$unidades[input_rv$fip$id %in% fip_unidades$id] <- fip_unidades$unidades
    
  })
  
  ### Get totals for each activity
  totals <- reactive({
    
    dat <- input_rv$rema %>%
      bind_rows(input_rv$fip)
    
   cost_data %>% 
      select(fase, subfase, concepto, actividad, actividad_frecuencia, rubro, id, section) %>% 
      left_join(dat, by = "id") %>% 
      mutate(etapa = tolower(substr(x = fase, start = 1, stop = 3))) %>% 
      left_join(periods$df, by = c("etapa", "section")) %>% 
      mutate(eventos = case_when(actividad_frecuencia == "Anual" ~ 1 * duracion_fase,
                                 actividad_frecuencia == "Bianual" ~ floor(0.5 * duracion_fase),
                                 actividad_frecuencia == "Mensual" ~ 12 * duracion_fase,
                                 TRUE ~ 1)) %>%
      mutate(total = costos * unidades * eventos) %>% 
      select(section, id, concepto, fase, subfase, actividad, rubro, duracion_fase, actividad_frecuencia, eventos, costos, unidades, total, -etapa) 
    
    })
  
  ### Value box with total cost in USD for marine reserves
  output$REMAtotalUSD <- renderInfoBox({
    
    total <- sum(totals()$total[totals()$section == "REMA"], na.rm = T)
    
    infoBox(title = "Costo total (REMA)",
            value = total,
            subtitle = "USD",
            icon = icon("dollar-sign"),
            fill = T,
            color = "light-blue")
  })
  
  ### Value box with total cost in USD for FIP
  output$FIPtotalUSD <- renderInfoBox({
    
    total <- sum(totals()$total[totals()$section == "FIP"], na.rm = T)
    
    infoBox(title = "Costo total (FIP)",
            value = total,
            subtitle = "USD",
            icon = icon("dollar-sign"),
            fill = T,
            color = "light-blue")
  })
  
  # output$REMAtotalMXP <- renderInfoBox({
  #   
  #   browser()
  #   total <- sum(totals()$total)
  #   
  #   infoBox(title = "Costo total (REMA)",
  #           value = round(total * input$usd2mxp),
  #           subtitle = "MXP",
  #           icon = icon("dollar-sign"),
  #           fill = T,
  #           color = "blue")
  # })
  # 
  # output$FIPtotalMXP <- renderInfoBox({
  #   
  #   browser()
  #   total <- sum(totals()$total)
  #   
  #   infoBox(title = "Costo total (FIP)",
  #           value = round(total * input$usd2mxp),
  #           subtitle = "MXP",
  #           icon = icon("dollar-sign"),
  #           fill = T,
  #           color = "blue")
  # })
  
  ### Plot 
  output$plot1 <- renderPlotly({
      
      plot1_data <- totals()  %>% 
        filter(total > 0) %>% 
        group_by(section, fase, concepto, subfase) %>% 
        summarize(total = sum(total, na.rm = T) / 1e3) %>% 
        ungroup()
      
      req(nrow(plot1_data) > 0)
      
      #if(input$costs_in_mxp){plot1_data$total <- plot1_data$total * input$usd2mxp}
      
      #y_label <- ifelse(input$costs_in_mxp, "Costo total (K MXP)", "Costo total (K USD)")
      y_label <- "Costo total (K USD)"
      
      plot1 <- plot1_data %>% 
        rename(Intervención = section,
               Concepto = concepto,
               Subfase = subfase,
               Total = total,
               Fase = fase) %>% 
        ggplot(aes(x = Fase, y = Total, fill = Concepto, label = Subfase, group = Intervención)) +
        geom_col(color = "black") +
        theme_cowplot() +
        scale_fill_brewer(palette = input$color_scheme) +
        labs(x = "Fase del proyecto", y = y_label) +
        theme(text = element_text(size = input$text_size),
              axis.text = element_text(size = input$text_size - 2)) +
        facet_wrap(~Intervención, ncol = 2) +
        theme(legend.position = "none")
      
      p <- ggplotly(plot1)
      
      # Fix from https://github.com/ropensci/plotly/issues/985#issuecomment-328575761
      p$elementId <- NULL
      p
      
    })
    
  output$plot2 <- renderPlotly({
      
      plot2_data <- totals()  %>% 
        filter(total > 0) %>% 
        group_by(section, fase, concepto, subfase) %>% 
        summarize(total = sum(total, na.rm = T) / 1e3) %>% 
        ungroup()
      
      req(nrow(plot2_data) > 0)
      
      #if(input$costs_in_mxp){plot2_data$total <- plot2_data$total * input$usd2mxp}
      
      #y_label <- ifelse(input$costs_in_mxp, "Costo total (K MXP)", "Costo total (K USD)")
      y_label <- "Costo total (K USD)"

      plot2 <- plot2_data %>% 
        rename(Intervención = section,
               Concepto = concepto,
               Subfase = subfase,
               Total = total,
               Fase = fase) %>% 
        ggplot(aes(x = Concepto, y = Total, fill = Fase, label = Subfase)) +
        geom_col(color = "black") +
        theme_cowplot() +
        scale_fill_brewer(palette = input$color_scheme) +
        labs(x = "Concepto", y = y_label) +
        theme(text = element_text(size = input$text_size),
              axis.text = element_text(size = input$text_size - 2)) +
        coord_flip() +
        facet_wrap(~Intervención, ncol = 1)
      
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

