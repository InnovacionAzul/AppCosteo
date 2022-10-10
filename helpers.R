######################################################
#helper_functions#
######################################################
# 
# Helper functions
#
######################################################

### Cost and quantity header for every activity
boxHeaderUI <- function(activity_id, default, actors, selected_actor){
  
  fluidRow(
    # Frequency selector
    selectInput(
      inputId = paste0("freq_", activity_id),
      label = "Frecuencia de la actividad",
      choices = c("Mensual", "Trimestral", "Semestral", "Anual", "Trienal", "Única"),
      selected = default
    ),
    
    # Financial responsible selector
    selectInput(
      inputId = paste0("resp_", activity_id),
      label = "Responsable financiero",
      choices = actors,
      selected = selected_actor
    ),
    
    # Make headers
    column(
      width = 6,
      h4("Precio")),
    column(
      width = 6,
      h4("Cantidad")
    )
  )
}

### Create cost and quantity input row for every activity
makeElement <- function(titleId, pairId, costLabel, unitLabel, costDefault = NULL, unitDefault = NULL, tooltipText = NULL, cost_data = NULL){
  # Define inputId labels for cost and units
  costId <- paste0("p_", pairId)
  unitId <- paste0("c_", pairId)
  
  # Define cost and unit defaults for numeric inputs
  if(is.null(costDefault)) {
    costDefault <- cost_data$precio[cost_data$id == pairId]
  }
  if(is.null(unitDefault)){
    unitDefault <- cost_data$cantidades[cost_data$id == pairId]
  }
  if(is.null(tooltipText)){
    tooltipText <- cost_data$descripcion[cost_data$id == pairId]
  }
  
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

makeActivity <- function(activity, data_subphase, actors){
  
  act_data <- filter(data_subphase,
                     actividad == activity) 
  
  activity_id <- act_data %>% 
    pull(id) %>% 
    str_extract(pattern = "[:alpha:]+_[:digit:]+_[:digit:]+_[:digit:]+") %>% 
    unique()
  
  default <- act_data %>% 
    pull(actividad_frecuencia) %>% 
    unique()
  
  selected_actor <- act_data %>%
    pull(responsable) %>%
    unique()
  
  if(selected_actor %in% actors){
    out <- selected_actor
  }else{
    out <- actors[1]
  }
  
  box(
    title = activity,
    width = 12,
    status = "primary",
    collapsible = T,
    collapsed = T,
    
    # Insert header
    boxHeaderUI(activity_id = activity_id,
                default = default,
                actors = actors,
                selected_actor = out),
    
    # Insert columns for price and quantities
    act_data %$%
      pmap(.l = list(rubro,
                     id,
                     unidades,
                     stringr::str_remove_all(unidades, "[$/]"),
                     cantidades,
                     precio),
           .f = makeElement
      )
  )
}

### Makes a row with a header and varying numbers of boxes for each subphase
makeSubphase <- function(subphase, number, data_fase, actors = NULL){
  
  data_subphase <- data_fase %>%
    dplyr::filter(subfase == subphase) %>%
    arrange(actividad_orden)
  
  subphase_code <- paste0(unique(str_extract(string = data_subphase$id, pattern = "[:alpha:]+_[:digit:]+_[:digit:]+")), "_resp")
  
  activities <- unique(data_subphase$actividad)
  
  # selected_actor <- unique(data_subphase$responsable)
  
  tagList(
    tags$div(
      style = "border: 1px solid lightgray; margin: 15px; padding: 15px 15px 0px 0px; border-radius: 5px;",
      fluidRow(
        column(
          width = 4,
          box(
            width = 12,
            background = "blue",
            valueBox(
              value = number,
              width = 12,
              subtitle = subphase, 
              icon = NULL, 
              color = "blue"
            )
            # column(width = 12,
            #        selectInput(
            #          inputId = subphase_code,
            #          label = "Responsable financiero",
            #          choices = actors,
            #          selected = selected_actor
            #        )
            # )
          )
        ),
        column(
          width = 8,
          map(.x = activities,
              .f = makeActivity,
              data_subphase = data_subphase,
              actors = actors)
        )
      )
    )
  )
}

### Filters the data by section and phase and finds all subphases to iterate over 
makeSubphases <- function(data, phase, section, subphases_to_include = NULL, actors = NULL){
  
  data_fase <- filter(data,
                      fase == phase,
                      section == section) %>%
    arrange(subfase_orden)
  
  if(phase == "Implementación" & section == "FIP"){
    subfases <- subphases_to_include
    numbers <- seq(1:length(subfases))
  } else {
    subfases <- unique(data_fase$subfase)
    numbers <- seq(1:length(subfases))
  }
  
  tagList(
    map2(.x = subfases,
         .y = numbers,
         .f = makeSubphase,
         data_fase = data_fase,
         actors = actors)
  )
}

### Duration
makePhaseDuration <- function(phase, section, duration = 0, fip_data = NULL, selected_phases = NULL){
  
  # Define inputId labels for cost and units
  durationId <- paste0("d_", tolower(substr(x = phase, start = 1, stop = 3)), "_", tolower(section))
  
  if(phase == "Implementación" & section == "FIP"){
    tagList(
      tags$div(
        style = "margin: 15px; padding: 15px 15px 15px 0px; border-radius: 5px;",
        fluidRow(
          column(
            width = 6,
            valueBox(
              value = NULL,
              subtitle = numericInput(
                durationId,
                label = "Duración de la fase (Meses)",
                value = duration,
                min = 0,
                max = 72,
                width = "100%"),
              width = 12,
              color = "aqua")
          ),
          box(
            title = "Intervenciones",
            collapsible = T,
            collapsed = T,
            width = 6,
            status = "info",
            checkboxGroupInput(
              inputId = "choices_imp_fip",
              label = "Selecciona tus intervenciones:",
              choices = unique(fip_data$subfase[fip_data$fase == "Implementación"]),
              selected = selected_phases,
              width = "100%")
          )
        )
      )
    )
  } else {
    tagList(
      tags$div(
        style = "margin: 15px; padding: 15px 15px 15px 0px; border-radius: 5px;",
        fluidRow(
          column(
            width = 6,
            valueBox(
              value = NULL,
              subtitle = numericInput(
                durationId,
                label = "Duración de fase (meses)",
                value = duration,
                min = 0,
                max = 60,
                width = "100%"),
              width = 12,
              color = "aqua")
          )
        )
      )
    )
  }
}

# ## Visualization hepers
# make_funder <- function(funders) {
#   fluidRow(
#     # Funder NAME
#     # column(
#     #   width = 12,
#     #   textInput(
#     #     inputId = paste0("funder_", funder),
#     #     label = "Nombre del grupo",
#     #     value = paste("Grupo", funder)
#     #   )
#     # ),
#     column(
#       #Funder CONTRIBUTION %
#       width = 6,
#       numericInput(
#         inputId = paste0("pct_funder_", funders),
#         label = paste0("% de contribución de ", funder),
#         value = pct,
#         min = 0,
#         max = 100
#       )
#     )
#   )
# }
# Function to ask for number and names of funders
# in the modeal window.
get_funder <- function(n_funder, funder_name) {
  fluidRow(
    # Funder NAME
    column(
      width = 12,
      textInput(
        inputId = paste0("funder_", n_funder),
        label = "Nombre del grupo",
        value = funder_name
      )
    )
  )
}