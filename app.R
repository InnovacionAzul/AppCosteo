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
library(grid)
library(gridtext)
library(glue)
library(gridExtra)

# Excel file handling
library(readxl)
library(writexl)

# Programming
library(magrittr)
library(tidyverse)

# Source functions -------------------------------------------------------------
source("helpers.R")

# Read in data -----------------------------------------------------------------
# DEFAULTS
default_actors <-
  readxl::read_xlsx(
    "www/defaults_rema_fip.xlsx",
    sheet = 1,
    na = c("", "N/A")
  ) %>%
  janitor::clean_names() %>%
  pull(actores)

default_n <- length(default_actors)

# REMA sheet
rema_data <-
  readxl::read_xlsx(
    "www/defaults_rema_fip.xlsx",
    sheet = 2,
    na = c("", "N/A")
  ) %>%
  janitor::clean_names() %>%
  arrange(fase, subfase_orden, actividad_orden) %>%
  mutate(
    precio = 0, #ifelse(is.na(precio), 0, precio),
    cantidades = 0, #ifelse(is.na(cantidades), 0, cantidades),
    unidades = ifelse(is.na(unidades), "$/unidad", unidades)
  )

# FIP sheet
fip_data <- readxl::read_xlsx(
  "www/defaults_rema_fip.xlsx",
  sheet = 3,
  na = c("", "N/A")) %>%
  janitor::clean_names() %>%
  arrange(fase, subfase_orden, actividad_orden) %>%
  mutate(
    precio = 0, #ifelse(is.na(precio), 0, precio),
    cantidades = 0, #ifelse(is.na(cantidades), 0, cantidades),
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
    title = "Sistema de Costeo",
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
                   fluidRow(
                     actionButton(
                       inputId = "ventana",
                       label = "Crear proyecto",
                       icon = icon("dollar-sign"),
                       style = "width: 100%; color: black; margin-left: 0;"
                     )
                   ),
                   # DOWNLOAD BUTTON
                   fluidRow(
                     downloadButton(
                       outputId = "download_total",
                       icon = icon("save"),
                       label = "Guardar proyecto",
                       style = "width: 100%; color: black; margin-left: 0;"
                     )
                   ),
                   tags$br(),
                   # DOWNLOAD BUTTON
                   fluidRow(
                     downloadButton(
                       outputId = "download_pdf",
                       label = "Descargar resumen (PDF)",
                       style = "width: 100%; color: black; margin-left: 0;"
                     )
                   ),
                   # MANUAL
                   fluidRow(
                     actionButton(
                       inputId = "manual",
                       label = a("Manual de Usuario", 
                                 href = "https://jcvdav.github.io/CostApp_manual/", 
                                 target = "_blank",
                                 style = "color: black;"),
                       icon = icon("book"),
                       style = "width: 100%; color: black; margin-left: 0;"
                     )
                   ),
                   # ELEMNT INDEX
                   fluidRow(
                     actionButton(
                       inputId = "index",
                       label = a("Índice de Elementos", 
                                 href = "https://jcvdav.github.io/CostApp_manual/indice.html", 
                                 target = "_blank",
                                 style = "color: black;"),
                       icon = icon("list"),
                       style = "width: 100%; color: black; margin-left: 0;"
                     )
                   )
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
            width = 12,
            h2("Sistema de Costeo para Herramientas de Reservas Marinas y Proyectos de Mejora Pesquera"),
            status = "primary",
            h3("¿Cuánto cuesta mi reserva marina o proyecto de mejora pesquera?"),
            p("Para lograr la conservación y uso responsable de los recursos marinos y la seguridad alimentaria de las comunidades pesqueras es esencial tomar en cuenta ambos elementos, por ejemplo la aplicación de herramientas de manejo pesquero como las reservas marinas, o para lograr una pesca sostenible a través de los proyectos de mejora pesquera."),
            fluidRow(
              column(
                width = 6,
                box(
                  width = 12,
                  status = "primary",
                  h4("Reservas Marinas"),
                  p("- Crean condiciones que permiten recuperar especies y restaurar el equilibrio del ecosistema."),
                  p("- Pueden generar un efecto desborde de biomasa pesquera en adultos y larvas."),
                  p("- Esto puede ayudar a tener una mejor pesca fuera de las reservas, compensando el costo de oportunidad por crear la reserva."),
                )
              ),
              column(
                width = 6,
                box(
                  width = 12,
                  status = "primary",
                  h4("Proyectos de Mejora Pesquera"),
                  p("- Los Proyectos de Mejora Pesquera (FIP, por sus siglas en inglés) guían hacia las buenas prácticas siguiendo el estándar del MSC."),
                  p("- Contribuyen a mejorar la salud de los recursos, cuidar el medio ambiente, y tener una mejor gobernanza y cumplimiento de la normativa pesquera.")
                )
              )
            ),
            h3("¿Por qué es útil esta herramienta?"),
            p("Financiar por largos periodos las reservas marinas o pesquerías sostenibles siempre ha sido un reto. Esta aplicación permitirá a los usuarios y usuarias ir documentando y monitoreando los costos de diseño, implementación y seguimiento de cada proyecto, que ayuden a planear mejor las contribuciones actuales y futuras de las personas participantes."),
            h3("¿Qué modelo sigue?"),
            p("El modelo para monitorear el financiamiento en estos esquemas consiste en tres etapas:"),
            p("1) Diseño – mediante procesos inclusivos en el que las partes interesadas participen en el proceso de diseño y definición de objetivos y tiempos."),
            p("2) Implementación - después de que se da inicio al esquema se toman en cuentan todas las acciones para poder ejecutar el proyecto contemplando a todas las personas/organizaciones que participan."),
            p("3) Seguimiento – acciones relacionadas al seguimiento y/o mantenimiento del esquema."),
            p("Para cualquier pregunta o comentario sobre este producto escribe al correo cobi@cobi.org.mx. Tus observaciones nos ayudarán a mejorar esta herramienta"),
            p("© COBI 2022")
          )
        ),
        ### Marine reserves
        tabItem(
          tabName = "rema",
          tabBox(
            id = "rema_tabs",
            width = 12,
            title = "Reservas Marinas",
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
            title = "Proyecto de Mejora Pesquera (FIP)",
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
                    title = "Control de gráficas y opciones",
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
                      width = 4,
                      background = "blue",
                      numericInput(
                        inputId = "text_size",
                        label = "Tamaño del texto",
                        value = 10,
                        min = 10,
                        max = 20
                      )
                    ),
                    box(
                      width = 4,
                      background = "blue",
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
                    title = "Presupuesto por usuarios",
                    width = 12,
                    plotlyOutput(
                      outputId = "split_budget_plot"
                    )
                  )
                )
        ) #/tabItem
      ) #/tabItems
    ) #/dashboardBody
  ) #/ui


## DEFINE SERVER ###############################################################
server <- function(input, output) {
  # Welcome window to gather project metadata ##################################
  query_modal <- modalDialog(
    title = "Bienvenido!",
    size = "l",
    fluidRow(
      box(
        title = "Continúa un proyecto",
        width = 4,
        status = "primary",
        fileInput(
          inputId = "budget_upload",
          label = "Cargar archivo",
          placeholder = "Ningún archivo seleccionado",
          multiple = F,
          accept = c(".xls", ".xlsx"),
          buttonLabel = "Cargar",
          width = "100%")
      ),
      box(
        title = "Inicia un nuevo proyecto",
        width = 8,
        status = "primary",
        column(
          width = 12,
          textInput(
            inputId = "title",
            label = "Título del proyecto"
          ),
          textInput(
            inputId = "author",
            label = "Autor del proyecto"
          ),
          textAreaInput(
            inputId = "notes",
            label = "Notas",
            resize = "both"
          ),
          uiOutput(outputId = "n_actors_ui"),
          uiOutput(outputId = "actors_ui")
        )
      )
    ),
    easyClose = F,
    footer = tagList(
      actionButton(
        inputId = "go",
        label = "Confirmar")
    )
  )
  
  # Show the model on start up ...
  # showModal(query_modal)
  
  observeEvent(input$ventana,
               {
                 showModal(query_modal)
               })
  
  observeEvent(input$go, {
    removeModal()
  })
  
  # User-defined cost data from the uploaded file ##############################
  # Extract actors from the file uploaded by the user --------------------------
  user_actors <- reactive({
    req(input$budget_upload)
    
    file <- input$budget_upload
    
    readxl::read_xlsx(file$datapath,
                      sheet = 1,
                      na = c("", "N/A")) %>%
      janitor::clean_names() %>%
      pull(actores) %>% 
      unique()
  })
  
  # REMA -----------------------------------------------------------------------
  user_rema_data <- reactive({
    req(input$budget_upload)
    file <- input$budget_upload
    
    user_rema <- readxl::read_xlsx(file$datapath,
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
  
  # FIP ------------------------------------------------------------------------
  user_fip_data <- reactive({
    req(input$budget_upload)
    file <- input$budget_upload
    
    readxl::read_xlsx(file$datapath,
                      sheet = 3,
                      na = c("", "N/A")) %>%
      janitor::clean_names() %>%
      arrange(fase, subfase_orden, actividad_orden) %>%
      mutate(
        precio = ifelse(is.na(precio), 0, precio),
        cantidades = ifelse(is.na(cantidades), 0, cantidades),
        unidades = ifelse(is.na(unidades), "$/unidad", unidades)
      )
  })
  
  # Define some values ---------------------------------------------------------
  values <- reactiveValues(
    n = default_n,
    actors = default_actors
  )
  
  # Define number of actors ----------------------------------------------------
  output$n_actors_ui <- renderUI({
    if (!is.null(input$budget_upload)) {
      values$n <- length(user_actors())
    }

    numericInput(
      inputId = "n_actors",
      label = "Número de actores",
      value = isolate(values$n),
      min = 1)
  })
  
  # Define actors --------------------------------------------------------------
  output$actors_ui <- renderUI({
    req(input$n_actors)
    values$n <- input$n_actors
    values$actors <- paste0("Grupo ", 1:isolate(values$n))

    if (!is.null(input$budget_upload)) {
      values$n <- length(user_actors())
      values$actors <- user_actors()
    }
    
    map2(.x = 1:length(values$actors),
         .y = values$actors,
         .f = get_funder)
  })
  
  # Get list of actors
  ui_actors <- reactive({
    map_chr(
      paste0("funder_", 1:values$n),
      ~{input[[.x]]}
    )
  })

  ##############################################################################
  ##### POPULATE UI ############################################################
  ##############################################################################

  
  # REMA UI ####################################################################
  # Reactive UI for REMA Design phase ------------------------------------------
  output$rema_dis <- renderUI({
    if(!is.null(input$funder_1)){
      values$actors <- ui_actors()
    }
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
      makeSubphases(
        data = rema_data,
        phase = phase,
        section = section,
        actors = values$actors)
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
      
      makeSubphases(
        data = rema_data,
        phase = phase,
        section = section,
        actors = values$actors)
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
      
      makeSubphases(
        data = rema_data,
        phase = phase,
        section = section,
        actors = values$actors)
    )
    
  })
  
  # FIP UI #####################################################################
  # Reactive UI for FIP Design phase -------------------------------------------
  output$fip_dis <- renderUI({
    if(!is.null(input$funder_1)){
      values$actors <- ui_actors()
    }
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
      makeSubphases(
        data = fip_data,
        phase = phase,
        section = section,
        actors = values$actors)
    )
  })
  
  ### Reactive UI for FIP Implementation phase
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
      makeSubphases(
        data = fip_data,
        phase = phase,
        section = section,
        subphases_to_include = input$choices_imp_fip,
        actors = values$actors
      )
    )
  })
  
  ### Reactive UI for FIP Follow-up phase
  output$fip_seg <- renderUI({
    section <- "FIP"
    phase <- "Seguimiento"
    if (!is.null(input$budget_upload)) {
      fip_data <- user_fip_data()
      actors <- ui_actors()
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
      makeSubphases(
        data = fip_data,
        phase = phase,
        section = section,
        actors = values$actors)
    )
  })
  
  # PHASE DURATION -------------------------------------------------------------
  # Reactive object for phase duration -----------------------------------------
  duration_rv <- reactiveValues(
    df = tibble(
      etapa = rep(c("dis", "imp", "seg"), each = 2),
      section = rep(c("REMA", "FIP"), times = 3)
      ) %>%
      mutate(input_id = paste("d", etapa, tolower(section), sep = "_"),
             fase_duracion = 1)
  )
  
  ### Update phase duration from inputs
  observe({
    # Check which duration inputs have been created
    valid_d_inputs <- duration_rv$df$input_id[which(duration_rv$df$input_id %in% names(input))]
    
    # Update reactive values with duration inputs
    req(length(valid_d_inputs) > 0)
    
    phase_duration <- purrr::map_dfr(.x = valid_d_inputs,
                                   ~ {
                                     tibble(input_id = .x,
                                            fase_duracion = input[[.x]])
                                   })
    
    duration_rv$df$fase_duracion[duration_rv$df$input_id %in% valid_d_inputs] <- phase_duration$fase_duracion
  })
  
  # ACTIVITY FREQUENCY ---------------------------------------------------------
  # Reactive object for activity frequency 
  frequency_rv <- reactiveValues(
    rema = tibble(
      activity_id = paste0("freq_", unique(str_extract(string = rema_data$id, pattern = "[:alpha:]+_[:digit:]+_[:digit:]+_[:digit:]+"))),
      actividad_frecuencia = rep("Anual")),
    fip = tibble(
      activity_id = paste0("freq_", unique(str_extract(string = fip_data$id, pattern = "[:alpha:]+_[:digit:]+_[:digit:]+_[:digit:]+"))),
      actividad_frecuencia = rep("Anual"))
  )
  
  ### Update REMA activity frequencies from inputs
  observe({
    # Check for user input data
    if (!is.null(input$budget_upload)) {
      rema_data <- user_rema_data()
    }
    # Check which duration inputs have been created
    valid_freq_inputs <- frequency_rv$rema$activity_id[which(frequency_rv$rema$activity_id %in% names(input))]
    
    # Update reactive values with duration inputs
    req(length(valid_freq_inputs) > 0)
    
    activity_frequency <- purrr::map_dfr(.x = valid_freq_inputs,
                                     ~ {
                                       tibble(activity_id = .x,
                                              actividad_frecuencia = input[[.x]])
                                     })
    
    frequency_rv$rema$actividad_frecuencia[frequency_rv$rema$activity_id %in% valid_freq_inputs] <- activity_frequency$actividad_frecuencia
  })

  ### Update FIP activity frequencies from inputs
  observe({
    # # Check for user input data
    # if (!is.null(input$budget_upload)) {
    #   fip_data <- user_fip_data()
    # }
    # Check which duration inputs have been created
    valid_freq_inputs <- frequency_rv$fip$activity_id[which(frequency_rv$fip$activity_id %in% names(input))]
    
    # Update reactive values with duration inputs
    req(length(valid_freq_inputs) > 0)
    
    activity_frequency <- purrr::map_dfr(.x = valid_freq_inputs,
                                         ~ {
                                           tibble(activity_id = .x,
                                                  actividad_frecuencia = input[[.x]])
                                         })
    
    frequency_rv$fip$actividad_frecuencia[frequency_rv$fip$activity_id %in% valid_freq_inputs] <- activity_frequency$actividad_frecuencia
  })
  
  # SUBPHASE RESPONSIBLES ------------------------------------------------------
  # Reactive object for responsible actors
  actors_rv <- reactiveValues(
    rema = tibble(
      subphase_id = paste0(unique(str_extract(string = rema_data$id, pattern = "[:alpha:]+_[:digit:]+_[:digit:]+")), "_resp"),
      responsable = NA_character_
    ),
    fip = tibble(
      subphase_id = paste0(unique(str_extract(string = fip_data$id, pattern = "[:alpha:]+_[:digit:]+_[:digit:]+")), "_resp"),
      responsable = NA_character_
    )
  )
  # Check for changes to responsible actors in REMA
  observe({
    # Check for user input data
    if (!is.null(input$budget_upload)) {
      rema_data <- user_rema_data()
    }
    # Check which duration inputs have been created
    valid_actors_inputs <- actors_rv$rema$subphase_id[which(actors_rv$rema$subphase_id %in% names(input))]

    # Update reactive values with duration inputs
    req(length(valid_actors_inputs) > 0)

    responsable <- purrr::map_dfr(.x = valid_actors_inputs,
                                         ~ {
                                           tibble(subphase_id = .x,
                                                  responsable = input[[.x]])
                                         })

    actors_rv$rema$responsable[actors_rv$rema$subphase_id %in% valid_actors_inputs] <- responsable$responsable
  })
  # Check for changes to responsible actors in FIP
  observe({
    # Check for user input data
    if (!is.null(input$budget_upload)) {
      fip_data <- user_fip_data()
    }
    # Check which duration inputs have been created
    valid_actors_inputs <- actors_rv$fip$subphase_id[which(actors_rv$fip$subphase_id %in% names(input))]
    
    # Update reactive values with duration inputs
    req(length(valid_actors_inputs) > 0)
    
    responsable <- purrr::map_dfr(.x = valid_actors_inputs,
                                  ~ {
                                    tibble(subphase_id = .x,
                                           responsable = input[[.x]])
                                  })
    
    actors_rv$fip$responsable[actors_rv$fip$subphase_id %in% valid_actors_inputs] <- responsable$responsable
  })
  
  # COSTS AND QUANTITIES -------------------------------------------------------
  # Reactive object for REMA and FIP inputs
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
  
  # TOTALS ---------------------------------------------------------------------
  # Reactive object for totals - fixes summary boxes not appearing on start
  totals_rv <- reactiveValues(rema = 0,
                              fip = 0)
  
  ### Get totals for each activity
  totals <- reactive({
    # browser()
    
    # Combine rema and fip cost and quantity data
    dat <- input_rv$rema %>%
      bind_rows(input_rv$fip)
    
    req(nrow(dat) > 0)
    
    # Combine rema and fip frequency data
    frequency <- bind_rows(frequency_rv$rema, frequency_rv$fip)
    
    # Combine responsible actors
    responsible <- bind_rows(actors_rv$rema, actors_rv$fip) 
    
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
     inner_join(duration_rv$df %>% dplyr::select(-input_id), by = c("etapa", "section")) %>%
     mutate(activity_id = paste0("freq_", str_extract(string = id, pattern = "[:alpha:]+_[:digit:]+_[:digit:]+_[:digit:]+")),
            subphase_id = paste0(str_extract(string = id, pattern = "[:alpha:]+_[:digit:]+_[:digit:]+"), "_resp")) %>% 
     inner_join(frequency, by = "activity_id") %>% 
     inner_join(responsible, by = "subphase_id") %>% 
     select(-c(activity_id, subphase_id)) %>% 
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
       responsable,
       unidades,
       cantidades,
       precio,
       id,
       total,
       -c(eventos, etapa)
     )
   
  })
  
  ### Update totals (REMA)
  observe({
    
    totals_rv$rema <- totals() %>% 
      dplyr::filter(section == "REMA") %>% 
      pull(total) %>% 
      sum(na.rm = T)
    
  })
  
  ### Update totals (FIP)
  observe({
    
    totals_rv$fip <- totals() %>% 
      filter(section == "FIP") %>% 
      pull(total) %>% 
      sum(na.rm = T)
    
  })
  
  # VALUE BOXES ----------------------------------------------------------------
  # Value box with total cost in USD for marine reserves
  output$REMAtotalUSD <- renderInfoBox({

    infoBox(
      title = "Costo total (Reserva)",
      value = prettyNum(totals_rv$rema, big.mark = ","),
      subtitle = "MXN",
      icon = icon("dollar-sign"),
      fill = T,
      color = "light-blue")
    
  })
  
  ### Value box with total cost in USD for FIP
  output$FIPtotalUSD <- renderInfoBox({
    
    infoBox(
      title = "Costo total (FIP)",
      value = prettyNum(totals_rv$fip, big.mark = ","),
      subtitle = "MXN",
      icon = icon("dollar-sign"),
      fill = T,
      color = "light-blue")
    
  })
  
  ### Plots --------------------------------------------------------------------
  ### Reactive values for plots
  output_rv <- reactiveValues(summary_dat = NULL,
                              plot1 = NULL,
                              plot2 = NULL,
                              plot3 = NULL)
  
  ### Summary data for tables
  observe({

    table_dat <- totals() %>%
      dplyr::filter(total > 0)
    
    output_rv$summary_dat <- table_dat

  })
  
  ### Plot 1: Presupuesto por fases
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
        fill = Fase,
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
    
    output_rv$plot1 <- plot1
    
    p <- ggplotly(plot1)
    
    # Fix from https://github.com/ropensci/plotly/issues/985#issuecomment-328575761
    p$elementId <- NULL
    p
    
  })
  
  ### Plot 2: Presupuesto por conceptos
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
    
    output_rv$plot2 <- plot2
    
    p <- ggplotly(plot2)
    
    # Fix from https://github.com/ropensci/plotly/issues/985#issuecomment-328575761
    p$elementId <- NULL
    p
  })
  
  ### Plot 3: Presupuesto por usuarios
  output$split_budget_plot <- renderPlotly({
  
    plot_data <- totals() %>% 
      group_by(responsable, section, fase, subfase) %>% 
      summarize(total = sum(total, na.rm = T) / 1e3) %>% 
      mutate(step = paste(fase, subfase, sep = "-")) %>% 
      # select(section, responsable, step, total) %>% 
      drop_na(responsable) %>% 
      filter(total > 0)
      
      
    p <-
      ggplot(
        data = plot_data,
        mapping = aes(
          x = step,
          y = responsable,
          fill = total
        )
      ) +
      geom_tile(color = "black") +
      theme_bw() +
      labs(x = "",
           y = "Actor") +
      scale_fill_gradient(low = "lightblue", high = "steelblue") +
      facet_wrap(~section) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    output_rv$plot3 <- p
    
    ggplotly(p)
  })
  
  ### EXPORTING ----------------------------------------------------------------
  ### Download progress (XLSX)
  output$download_total <- downloadHandler(
    filename = "Presupuesto.xlsx",
    content = function(file) {
      writexl::write_xlsx(
        x = list(
          METAADATA = tibble(Titulo = c(input$title, rep("", values$n -1)),
                             Autor = c(input$author, rep("", values$n -1)),
                             Notas = c(input$notes, rep("", values$n -1)),
                             Actores = ui_actors()),
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
  
  ### Download summary PDF
  output$download_pdf <- downloadHandler(

    filename = function(){paste0("AppCosteo_resumen_de_resultados.pdf")},
    content = function(file){
      
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "summary_report.Rmd")
      file.copy("summary_report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(title = input$title,
                     author = input$author,
                     actors = paste0(ui_actors(), collapse = ", "),
                     notes = input$notes,
                     summary_dat = output_rv$summary_dat,
                     plot1 = output_rv$plot1,
                     plot2 = output_rv$plot2,
                     plot3 = output_rv$plot3)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  
  
  
}

# Run the application
shinyApp(ui = ui,
         server = server,
         enableBookmarking = "url")
