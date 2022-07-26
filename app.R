######################################################
# COST APP #
# V2.0
######################################################
#
# Developer info:
# Name: Juan Carlos Villaseñor-Derbez
# email: juancarlos@ucsb.ed; juancarlos.villader@gmail.com
# GitHub: jcvdav
#
# Contributors:
# - Stuart Fulton
# - Katherine Millage
# - Lorena Rocha
# - Jacobo Caamal
#
######################################################

################################################################################
#### SET UP ####################################################################
################################################################################

# Load packages ----------------------------------------------------------------
# App
library(shiny)
library(shinydashboard)
library(shinyBS)

# Visualization
library(cowplot)
library(plotly)

# Excel file handling
library(readxl)
library(writexl)

# Programming
library(magrittr)
library(tidyverse)

# Source functions -------------------------------------------------------------
source("helpers.R")

# Read in data -----------------------------------------------------------------
# REMA sheet
rema_data <-
  readxl::read_xlsx(
    "www/defaults_rema_fip.xlsx",
    sheet = 1,
    na = c("", "N/A")
  ) %>%
  janitor::clean_names() %>%
  arrange(fase, subfase_orden, actividad_orden) %>%
  mutate(
    precio = ifelse(is.na(precio), 0, precio),
    cantidades = ifelse(is.na(cantidades), 0, cantidades),
    unidades = ifelse(is.na(unidades), "$/unidad", unidades)
  )

# FIP sheet
fip_data <- readxl::read_xlsx("www/defaults_rema_fip.xlsx",
                              sheet = 2,
                              na = c("", "N/A")) %>%
  janitor::clean_names() %>%
  arrange(fase, subfase_orden, actividad_orden) %>%
  mutate(
    precio = ifelse(is.na(precio), 0, precio),
    cantidades = ifelse(is.na(cantidades), 0, cantidades),
    unidades = ifelse(is.na(unidades), "$/unidad", unidades)
  )

# Combine sheets into a single tibble
cost_data <- rema_data %>%
  bind_rows(fip_data)

################################################################################
#### DEFINE APP ################################################################
################################################################################

## DEFINE UI ###################################################################
ui <- 
  dashboardPage(
    title = "Costeo de Intervenciones",
    header = dashboardHeader(
      title = img(src = "img/COBI_logo.png", height = "52px")
    ),
    sidebar = dashboardSidebar(
      width = 275,
      sidebarMenu(
        id = "tabs",
        # Setting id makes input$tabs give the tabName of currently-selected tab
        menuItem(
          text = "Inicio",
          tabName = "inicio",
          icon = icon(name = "info")
        ),
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
          text = "Gráficos",
          tabName = "presupuesto",
          icon = icon("chart-bar")
        ),
        fluidRow(
          infoBoxOutput(
            outputId = "REMAtotalUSD",
            width = 12
          )
        ),
        fluidRow(
          infoBoxOutput(
            outputId = "FIPtotalUSD",
            width = 12
          )
        ),
        fluidRow(
          box(
            title = "Herramientas",
            status = "primary",
            background = "blue",
            width = 12,
            collapsible = TRUE,
            collapsed = FALSE,
            
            column(12,
                   
                   # UPLOAD BUTTON
                   fileInput(inputId = "budget_upload",
                             label = "Cargar archivo",
                             multiple = F,
                             accept = c(".xls", ".xlsx"),
                             buttonLabel = "Cargar",
                             width = "100%"),
                   
                   # MANUAL
                   actionButton("manual",
                                a("Manual de Usuario", 
                                  href = "https://jcvdav.github.io/CostApp_manual/", 
                                  target = "_blank",
                                  style = "color: black;"),
                                style = "width: 100%; color: black; margin-left: 0;"),
                   
                   # DOWNLOAD BUTTON
                   downloadButton(outputId = "download_total",
                                  label = "Descargar presupuesto",
                                  style = "width: 100%; color: black; margin-left:0;"),
                   
                   # BOOKMARK BUTTON
                   bookmarkButton(title = "Compartir el estado actual",
                                  label = "Compartir",
                                  style = "width: 100%; color: black; margin-left: 0;")
                   
            )
          )
        )
      )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(
          tabName = "inicio",
          box(
            title = h1("Costeo de Intervenciones de Conservación (V2.0 beta)"),
            width = 12,
            status = "primary",
            p("Las reservas marinas completamente protegidas son áreas del océano restringidas a cualquier actividad extractiva, incluyendo la pesca. Las reservas marinas exitosas crean condiciones en las que las poblaciones de especies previamente capturadas se pueden recuperar y restaurar el equilibrio trófico en el ecosistema. La recuperación de la biomasa pesquera dentro de la reserva puede causar efectos colaterales en las zonas de pesca adyacentes, tanto desde el traslado de especímenes adultos, como en la exportación de larvas. Este efecto de desbordamiento puede ayudar a los usuarios a compensar algunos de los precio de oportunidad al ceder las zonas de pesca. La reserva marina funciona como una cuenta bancaria, la cual se repobla con el interés al capital con el desbordamiento."),
            p(a("COBI", href = "www.cobi.org.mx", target = "_blank"),
              "ha trabajado durante 19 años para establecer, evaluar y mantener las reservas marinas en colaboración con las comunidades pesqueras de México. Nuestro modelo de reservas marinas consiste de cuatro fases:"),
            tags$ul(
              tags$li(
                tags$b("1) Implementación - "),
                "procesos inclusivos en el que las partes interesadas participen en el proceso de diseño, definición de objetivos y la selección del sitio"
              ),
              tags$li(
                tags$b("2) Monitoreo - "),
                "después de que la reserva se ha creado, miembros de la comunidad están capacitados para recopilar datos para evaluar la reserva"
              ),
              tags$li(
                tags$b("3) Operación – "),
                "acciones relacionadas a la vigilancia comunitaria, señalización y comunicación de resultados"
              ),
              tags$li(
                tags$b("4) Renovación – "),
                "manejo adaptativo basado en los datos recogidos por la comunidad para garantizar el funcionamiento eficaz de la reserva."
              )
            ),
            p("El número de iniciativas para establecer redes de reservas marinas está en aumento, y la gran mayoría de los esfuerzos se realizan con fondos filantrópicos. Sin embargo, al iniciar el proceso para establecer reservas marinas es común que los precio proyectados al futuro no estén claramente definidos. Esto puede afectar la sustentabilidad de la reserva marina al largo plazo, sobre todo si el costo de mantenerla y operarla es mayor al beneficio que puede proporcionar a la comunidad."),
            p("Este calculador de precio contempla todos los pasos necesarios para establecer una reserva marina utilizado el modelo COBI, con el objetivo de ayudar a comunidades, organizaciones de la sociedad civil y tomadores de decisiones de planear sus inversiones con mayor claridad y transparencia."),
            p("Para cualquier pregunta o comentario sobre este producto escribe al correo rema@cobi.org.mx Tus observaciones nos ayudarán a mejorar nuestras herramientas."),
            p("© COBI 2018")
          )
        ),
        ### Marine reserves
        tabItem(
          tabName = "rema",
          tabBox(
            id = "rema_tabs",
            width = 12,
            title = "REMA",
            tags$br(),
            ### Design
            tabPanel(
              title = "Diseño",
              uiOutput(outputId = "rema_dis")
            ),
            ### Implementation
            tabPanel(
              title = "Implementación",
              uiOutput(outputId = "rema_imp")
            ),
            ### Follow up
            tabPanel(
              title = "Seguimiento",
              uiOutput(outputId = "rema_seg")
            ),
          )
        ),
        ### FIPs
        tabItem(
          tabName = "fip",
          tabBox(
            id = "fip_tabs",
            width = 12,
            title = "FIP",
            tags$br(),
            ### Design
            tabPanel(
              title = "Diseño",
              uiOutput("fip_dis")
            ),
            ### Implementation
            tabPanel(
              title = "Implementación",
              uiOutput(outputId = "fip_imp")
            ),
            ### Follow up
            tabPanel(
              title = "Seguimiento",
              uiOutput(outputId = "fip_seg")
            ),
          )
        ),
        ### Plots
        tabItem("presupuesto",
                fluidRow(
                  box(
                    title = "Control de gráficas",
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
                    box(
                      width = 2,
                      status = "primary",
                      numericInput(
                        inputId = "text_size",
                        label = "Tamaño del texto",
                        value = 10,
                        min = 10,
                        max = 20
                      )
                      
                    ),
                    box(
                      width = 2,
                      status = "primary",
                      selectInput(
                        inputId = "color_scheme",
                        label = "Esquema de colores",
                        choices = c(
                          "COBI" = "Blues",
                          "Pares" = "Paired",
                          "Set1" = "Set1",
                          "Set2" = "Set2",
                          "Set3" = "Set3"
                        ),
                        selected = "COBI"
                      )
                      
                    )
                  )
                ),
                fluidRow(
                  box(
                    title = "Presupuesto por fases",
                    width = 12,
                    status = "primary",
                    plotlyOutput(outputId = "plot1")
                  )
                ),
                fluidRow(
                  box(
                    title = "Presupuesto por conceptos",
                    width = 12,
                    status = "primary",
                    plotlyOutput(outputId = "plot2")
                  )
                ),
                fluidRow(
                  box(
                    title = "División del presupuesto",
                    width = 12,
                    status = "primary",
                    collapsible = T,
                    column(
                      width = 4,
                      numericInput(
                        inputId = "n_actors",
                        label = "Número de actores",
                        value = 2,
                        min = 1),
                      uiOutput(outputId = "actors")
                    ),
                    column(
                      width = 8,
                      plotlyOutput(
                        outputId = "split_budget_plot"
                      )
                    )
                  )
                )
        ) #/tabItem
      ) #/tabItems
    ) #/dashboardBody
  ) #/ui


# Define server logic
server <- function(input, output) {
  
  # user-defined cost data from the uploaded file ##############################
  # REMA -----------------------------------------------------------------------
  user_rema_data <- reactive({
    req(input$budget_upload)
    file <- input$budget_upload
    
    user_rema <- readxl::read_xlsx(file$datapath,
                                   sheet = 1,
                                   na = c("", "N/A")) %>%
      janitor::clean_names() %>%
      arrange(fase, subfase_orden, actividad_orden) %>%
      mutate(
        precio = ifelse(is.na(precio), 0, precio),
        cantidades = ifelse(is.na(cantidades), 0, cantidades),
        unidades = ifelse(is.na(unidades), "$/unidad", unidades)
      )
  })
  
  # FIP ------------------------------------------------------------------------
  user_fip_data <- reactive({
    req(input$budget_upload)
    file <- input$budget_upload
    
    readxl::read_xlsx(file$datapath,
                      sheet = 2,
                      na = c("", "N/A")) %>%
      janitor::clean_names() %>%
      arrange(fase, subfase_orden, actividad_orden) %>%
      mutate(
        precio = ifelse(is.na(precio), 0, precio),
        cantidades = ifelse(is.na(cantidades), 0, cantidades),
        unidades = ifelse(is.na(unidades), "$/unidad", unidades)
      )
  })
  
  
  ##############################################################################
  ##### POPULATE UI ############################################################
  ##############################################################################
  
  
  # REMA #########################################################################
  # Reactive UI for REMA Design phase ------------------------------------------
  # output$rema_dis_dur <- renderUI({
  #   if (!is.null(input$budget_upload)) {
  #     rema_data <- user_rema_data()
  #   }
  #   
  #   duration <- rema_data %>% 
  #     filter(fase == "Diseño") %>% 
  #     pull(fase_duracion) %>% 
  #     unique()
  #   
  #   makePhaseDuration(phase = "Diseño",
  #                     section = "REMA",
  #                     duration = duration)
  # })
  
  output$rema_dis <- renderUI({
    section <- "REMA"
    phase <- "Diseño"
    if (!is.null(input$budget_upload)) {
      rema_data <- user_rema_data()
    }
    
    duration <- rema_data %>% 
      filter(fase == phase) %>% 
      pull(fase_duracion) %>% 
      unique()
    
    tagList(
      makePhaseDuration(
        phase = phase,
        section = section,
        duration = duration),
      subphaseWrapper(
        data = rema_data,
        phase = phase,
        section = section)
    )
    
  })
  
  # Reactive UI for REMA Implementation phase  ---------------------------------
  output$rema_imp <- renderUI({
    section <- "REMA"
    phase <- "Implementación"
    if (!is.null(input$budget_upload)) {
      rema_data <- user_rema_data()
    }
    
    duration <- rema_data %>% 
      filter(fase == phase) %>% 
      pull(fase_duracion) %>% 
      unique()
    
    tagList(
      makePhaseDuration(
        phase = phase,
        section = section,
        duration = duration),
      
      subphaseWrapper(
        data = rema_data,
        phase = phase,
        section = section)
    )
    
  })
  
  # Reactive UI for REMA Follow-up phase ---------------------------------------
  output$rema_seg <- renderUI({
    section <- "REMA"
    phase <- "Seguimiento"
    if (!is.null(input$budget_upload)) {
      rema_data <- user_rema_data()
    }
    
    duration <- rema_data %>% 
      filter(fase == phase) %>% 
      pull(fase_duracion) %>% 
      unique()
    
    tagList(
      makePhaseDuration(
        phase = phase,
        section = section,
        duration = duration),
      
      subphaseWrapper(
        data = rema_data,
        phase = phase,
        section = section)
    )
    
  })
  
  # FIP ########################################################################
  # Reactive UI for FIP Design phase  ------------------------------------------
  output$fip_dis <- renderUI({
    section <- "FIP"
    phase <- "Diseño"
    if (!is.null(input$budget_upload)) {
      fip_data <- user_fip_data()
    }
    
    duration <- fip_data %>% 
      filter(fase == phase) %>% 
      pull(fase_duracion) %>% 
      unique()
    
    tagList(
      makePhaseDuration(
        phase = phase,
        section = section,
        duration = duration),
      subphaseWrapper(
        data = fip_data,
        phase = phase,
        section = section)
    )
  })
  
  # Reactive UI for FIP Implementation phase  ----------------------------------
  output$fip_imp <- renderUI({
    section <- "FIP"
    phase <- "Implementación"
    if (!is.null(input$budget_upload)) {
      fip_data <- user_fip_data()
    }
    
    duration <- fip_data %>% 
      filter(fase == phase) %>% 
      pull(fase_duracion) %>% 
      unique()
    
    tagList(
      makePhaseDuration(
        phase = phase,
        section = section,
        duration = duration,
        fip_data = fip_data,
        selected_phases = input$choices_imp_fip
      ),
      subphaseWrapper(
        data = fip_data,
        phase = phase,
        section = section,
        subphases_to_include = input$choices_imp_fip
      )
    )
  })
  
  # Reactive UI for FIP Follow-up phase  ---------------------------------------
  output$fip_seg <- renderUI({
    section <- "FIP"
    phase <- "Seguimiento"
    if (!is.null(input$budget_upload)) {
      fip_data <- user_fip_data()
    }
    
    duration <- rema_data %>% 
      filter(fase == phase) %>% 
      pull(fase_duracion) %>% 
      unique()
    
    tagList(
      makePhaseDuration(
        phase = phase,
        section = section,
        duration = duration),
      subphaseWrapper(
        data = fip_data,
        phase = phase,
        section = section)
    )
  })
  
  # Duration and frequencies of phases and activities ##########################
  # Phase duration -------------------------------------------------------------
  periods <- reactive({
    expand_grid(
      etapa = c("dis", "imp", "seg"),
      section = c("REMA", "FIP")
    ) %$% 
    map2_dfr(
      .x = etapa,
      .y = section,
      .f = ~{
        tibble(etapa = .x,
               section = .y,
               fase_duracion = input[[paste("d", etapa, tolower(section), sep = "_")]])
      }
    )
  })
  
  # Activity frequency ---------------------------------------------------------
  frequencies <- reactive({
    # Frequency of REMA part
    req(input$freq_rema_1_1_1)
    if (!is.null(input$budget_upload)) {
      rema_data <- user_rema_data()
    }
    
    rema_freq <- map_dfr(
      paste0("freq_", unique(str_extract(string = rema_data$id, pattern = "[:alpha:]+_[:digit:]+_[:digit:]+_[:digit:]+"))),
      .f = ~{
        tibble(
          activity_id = .x,
          actividad_frecuencia = input[[.x]]
        )
      }
    )
    
    # Frequency of FIP part
    if (!is.null(input$budget_upload)) {
      fip_data <- user_fip_data()
    }
    
    fip_freq <- map_dfr(
      paste0("freq_", unique(str_extract(string = fip_data$id, pattern = "[:alpha:]+_[:digit:]+_[:digit:]+_[:digit:]+"))),
      .f = ~{
        tibble(
          activity_id= .x,
          actividad_frecuencia = input[[.x]]
        )
      }
    )
    
    bind_rows(rema_freq, fip_freq)
  })
  
  
  ### Reactive object for REMA and FIP inputs
  input_rv <- reactiveValues(
    rema = tibble(
      id = rema_data$id,
      precio = rema_data$precio,
      cantidades = rema_data$cantidades
    ),
    fip = tibble(
      id = fip_data$id,
      precio = fip_data$precio,
      cantidades = fip_data$cantidades
    )
  )
  
  ### Look for any changes to price inputs in the REMA section
  observe({
    
    # Check which REMA price inputs have been created
    valid_c_rema_inputs <-
      rema_data$id[which(paste0("c_", rema_data$id) %in% names(input))]
    
    # Update reactive values with REMA price inputs
    req(length(valid_c_rema_inputs) > 0)
    
    rema_precio <- purrr::map2_dfr(.x = valid_c_rema_inputs,
                                   .y = paste0("c_", valid_c_rema_inputs),
                                   ~ {
                                     tibble(id = .x,
                                            precio = input[[.y]])
                                   })
    
    input_rv$rema$precio[input_rv$rema$id %in% rema_precio$id] <-
      rema_precio$precio
    
  })
  
  ### Look for any changes to quantity inputs in the REMA section
  observe({
    
    # Check  which REMA quantity inputs have been created
    valid_u_rema_inputs <-
      rema_data$id[which(paste0("u_", rema_data$id) %in% names(input))]
    
    # Update reactive values with REMA quantity inputs
    req(length(valid_u_rema_inputs) > 0)
    
    rema_unidades <- purrr::map2_dfr(.x = valid_u_rema_inputs,
                                     .y = paste0("u_", valid_u_rema_inputs),
                                     ~ {
                                       tibble(id = .x,
                                              cantidades = input[[.y]])
                                     })
    
    input_rv$rema$cantidades[input_rv$rema$id %in% rema_unidades$id] <-
      rema_unidades$cantidades
    
  })
  
  ### Look for any changes to price inputs in the FIP section
  observe({
    
    # Check which FIP price inputs have been created
    valid_c_fip_inputs <- fip_data$id[which(paste0("c_", fip_data$id) %in% names(input))]
    
    # Update reactive values with FIP price inputs
    req(length(valid_c_fip_inputs) > 0)
    
    fip_precio <- purrr::map2_dfr(.x = valid_c_fip_inputs,
                                  .y = paste0("c_", valid_c_fip_inputs),
                                  ~ {
                                    tibble(id = .x,
                                           precio = input[[.y]])
                                  })
    
    input_rv$fip$precio[input_rv$fip$id %in% fip_precio$id] <-
      fip_precio$precio
    
  })
  
  ### Look for any changes to quantity inputs in the FIP section
  observe({
    
    # Check which FIP quantity inputs have been created
    valid_u_fip_inputs <- fip_data$id[which(paste0("u_", fip_data$id) %in% names(input))]
    
    # Update reactive values with FIP quantity inputs
    req(length(valid_u_fip_inputs) > 0)
    
    fip_unidades <- purrr::map2_dfr(.x = valid_u_fip_inputs,
                                    .y = paste0("u_", valid_u_fip_inputs),
                                    ~ {
                                      tibble(id = .x,
                                             cantidades = input[[.y]])
                                    })
    
    input_rv$fip$cantidades[input_rv$fip$id %in% fip_unidades$id] <-
      fip_unidades$cantidades

  })
  
  ### Reactive object for totals - fixes summary boxes not appearing on start
  totals_rv <- reactiveValues(rema = 0,
                              fip = 0)
  
  ### Get totals for each activity
  totals <- reactive({
    
    dat <- input_rv$rema %>%
      bind_rows(input_rv$fip)
    
    req(nrow(dat) > 0)
    
   cost_data %>%
      select(section,
             fase,
             subfase,
             concepto,
             actividad,
             rubro,
             id,
             descripcion,
             unidades,
             contains("orden")) %>%
      inner_join(dat, by = "id") %>%
      mutate(etapa = tolower(substr(x = fase, start = 1, stop = 3))) %>%
      inner_join(periods(), by = c("etapa", "section")) %>%
      mutate(activity_id = paste0("freq_", str_extract(string = id, pattern = "[:alpha:]+_[:digit:]+_[:digit:]+_[:digit:]+"))) %>% 
      inner_join(frequencies(), by = "activity_id") %>% 
      select(-activity_id) %>% 
      mutate(
        eventos = pmax(
          fase_duracion * case_when(
            actividad_frecuencia == "Mensual" ~ 12,
            actividad_frecuencia == "Trimestral" ~ 4,
            actividad_frecuencia == "Semestral" ~ 2,
            actividad_frecuencia == "Anual" ~ 1,
            actividad_frecuencia == "Trienal" ~ 1/3,
            TRUE ~ 0),
          1),
        total = precio * cantidades * eventos) %>%
      select(
        section,
        fase,
        fase_duracion,
        subfase,
        subfase_orden,
        concepto,
        actividad,
        actividad_orden,
        actividad_frecuencia,
        rubro,
        descripcion,
        unidades,
        cantidades,
        precio,
        id,
        total,
        -c(eventos, etapa)
      )
    
  })
  
  ### Update totals rv
  observe({
    
    totals_rv$rema <- test <- totals() %>% 
      dplyr::filter(section == "REMA") %>% 
      pull(total) %>% 
      sum(na.rm = T)
    
    totals_rv$fip <- totals() %>% 
      filter(section == "FIP") %>% 
      pull(total) %>% 
      sum(na.rm = T)
    
  })
  
  # VALUE BOXES ################################################################
  # Value box with total cost in USD for marine reserves -----------------------
  output$REMAtotalUSD <- renderInfoBox({

    infoBox(
      title = "Costo total (REMA)",
      value = totals_rv$rema,
      subtitle = "USD",
      icon = icon("dollar-sign"),
      fill = T,
      color = "light-blue")
    
  })
  
  # Value box with total cost in USD for FIP -----------------------------------
  output$FIPtotalUSD <- renderInfoBox({
    
    infoBox(
      title = "Costo total (FIP)",
      value = totals_rv$fip,
      subtitle = "USD",
      icon = icon("dollar-sign"),
      fill = T,
      color = "light-blue")
    
  })
  
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
      rename(
        Intervención = section,
        Concepto = concepto,
        Subfase = subfase,
        Total = total,
        Fase = fase
      ) %>%
      ggplot(aes(
        x = Fase,
        y = Total,
        fill = Concepto,
        label = Subfase,
        group = Intervención
      )) +
      geom_col(color = "black") +
      theme_cowplot() +
      scale_fill_brewer(palette = input$color_scheme) +
      labs(x = "Fase del proyecto", y = y_label) +
      theme(
        text = element_text(size = input$text_size),
        axis.text = element_text(size = input$text_size - 2)
      ) +
      facet_wrap( ~ Intervención, ncol = 2) +
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
      rename(
        Intervención = section,
        Concepto = concepto,
        Subfase = subfase,
        Total = total,
        Fase = fase
      ) %>%
      ggplot(aes(
        x = Concepto,
        y = Total,
        fill = Fase,
        label = Subfase
      )) +
      geom_col(color = "black") +
      theme_cowplot() +
      scale_fill_brewer(palette = input$color_scheme) +
      labs(x = "Concepto", y = y_label) +
      theme(
        text = element_text(size = input$text_size),
        axis.text = element_text(size = input$text_size - 2)
      ) +
      coord_flip() +
      facet_wrap( ~ Intervención, ncol = 1)
    
    p <- ggplotly(plot2)
    
    # Fix from https://github.com/ropensci/plotly/issues/985#issuecomment-328575761
    p$elementId <- NULL
    p
  })
  
  # BUDGET SPLITTING ###########################################################
  # Define number of actors ----------------------------------------------------
  output$actors <- renderUI({
    n <- input$n_actors
    
    map2(.x = 1:n,
         .y = 100/n,
         .f = make_funder)
  })
  
  # Assamble a tibble of actors ------------------------------------------------
  actors_tibble <- reactive({
    req(input$funder_1)
    
    purrr::map2_dfr(
      paste0("funder_", 1:input$n_actors),
      paste0("pct_funder_", 1:input$n_actors),
      ~{
        tibble(
          actor = input[[.x]],
          pct = input[[.y]]
        )
      }
    )
    
  })
  
  # Plot -----------------------------------------------------------------------
  output$split_budget_plot <- renderPlotly({
    
    
    p <-
      ggplot(
        data = actors_tibble(),
        mapping = aes(
          x = actor,
          y = pct * sum(totals()$total, na.rm = T) / 100,
          fill = actor
        )
      ) +
      geom_col(
        color = "black"
      ) +
      theme_bw() +
      labs(x = "Actor",
           y = "Contribución total ($)") +
      scale_fill_brewer(palette = input$color_scheme) +
      guides(fill = guide_legend(title = "Actor"))
    
    ggplotly(p)
  })
  
  # EXPORTING ------------------------------------------------------------------
  output$download_total <- downloadHandler(
    filename = "Presupuesto.xlsx",
    content = function(file) {
      writexl::write_xlsx(
        x = list(
          REMA = totals() %>% 
            filter(section == "REMA"),
          FIP = totals() %>% 
            filter(section == "FIP")
        ),
        path = file,
        col_names = TRUE
      )
    }
  )
  
  output$test <- renderTable({
    frequencies()
  })
  
}

# Run the application
shinyApp(ui = ui,
         server = server,
         enableBookmarking = "url")
