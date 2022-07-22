######################################################
#helper_functions#
######################################################
# 
# Helper functions
#
######################################################

### Cost and quantity header for every activity
boxHeaderUI <- function(){
  
  fluidRow(
    # Frequency selector
    selectInput(
      inputId = "test",
      label = "Frecuencia de la actividad",
      choices = c("Mensual", "Trimestral", "Anual", "Bienal", "Trienal")
    ),
    column(
      width = 6,
      h4("Costos")),
    column(
      width = 6,
      h4("Cantidades")
    )
  )
}

### Create cost and quantity input row for every activity
CostUnitUI <- function(titleId, pairId, costLabel, unitLabel, costDefault = NULL, unitDefault = NULL, tooltipText = NULL, cost_data = NULL){
  # Define inputId labels for cost and units
  costId <- paste0("c_", pairId)
  unitId <- paste0("u_", pairId)
  
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

makeUnit <- function(activity, data_subphase){
  
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
                       stringr::str_remove_all(unidades, "[$/]"),
                       cantidades,
                       precio),
             .f = CostUnitUI)
  )
  
}

### Makes a row with a header and varying numbers of boxes for each subphase
activityWrapper <- function(subphase, number, data_fase){
  
  data_subphase <- data_fase %>%
    dplyr::filter(subfase == subphase) %>%
    arrange(actividad_orden)
  
  activities <- unique(data_subphase$actividad)
  
  tagList(
    tags$div(style = "border: 1px solid lightgray; margin: 15px; padding: 15px 15px 0px 0px; border-radius: 5px;",
             fluidRow(
               column(3,
                      valueBox(value = number, 
                               subtitle = subphase, 
                               icon = NULL, 
                               color = "blue", 
                               width = 12,
                               href = NULL)
               ),
               column(9,
                      fluidRow(
                        map(.x = activities,
                            .f = makeUnit,
                            data_subphase = data_subphase)
                      )
               )
             )
    )
  )
  
}

### Filters the data by section and phase and finds all subphases to iterate over 
subphaseWrapper <- function(data, phase, section, subphases_to_include = NULL){
  
  data_fase <- filter(data,
                      fase == phase,
                      section == section) %>%
    arrange(subfase_orden)
  
  if(phase == "Implementación" & section == "FIP"){
    subfases <- subphases_to_include
    numbers <- seq(1:length(subfases))
    
  }else{
    subfases <- unique(data_fase$subfase)
    numbers <- seq(1:length(subfases))
  }
  
  #req(length(subfases) >= 1)
  
  tagList(
    map2(.x = subfases,
         .y = numbers,
         .f = activityWrapper,
         data_fase = data_fase)
  )
}

### Duration
makePhaseDuration <- function(phase, section, fip_data = NULL){
  
  # Define inputId labels for cost and units
  durationId <- paste0("d_", tolower(substr(x = phase, start = 1, stop = 3)), "_", tolower(section))
  
  if(phase == "Implementación" & section == "FIP"){
    tagList(
      tags$div(style = "margin: 15px; padding: 15px 15px 15px 0px; border-radius: 5px;",
               fluidRow(
                 column(width = 6,
                        valueBox(value = NULL,
                                 subtitle = numericInput(durationId,
                                                         label = "Duración de la fase (años)",
                                                         value = 1,
                                                         min = 1,
                                                         max = 20,
                                                         width = "100%"),
                                 width = 12,
                                 color = "aqua")
                 ),
                 valueBox(
                   value = NULL,
                   subtitle = selectInput(
                     "choices_imp_fip",
                     label = "Selecciona tus intervenciones:",
                     choices = unique(fip_data$subfase[fip_data$fase == "Implementación"]),
                     multiple = T,
                     width = "100%"),
                   width = 6,
                   color = "aqua")
               )
      )
    )
  }else{
    tagList(
      tags$div(style = "margin: 15px; padding: 15px 15px 15px 0px; border-radius: 5px;",
               fluidRow(
                 column(width = 6,
                        valueBox(value = NULL,
                                 subtitle = numericInput(durationId,
                                                         label = "Duración de fase (años)",
                                                         value = 1,
                                                         min = 1,
                                                         max = 20,
                                                         width = "100%"),
                                 width = 12,
                                 color = "aqua")
                 )
               )
      )
    )
  }
}

## Visualization hepers
make_funder <- function(funder, pct) {
  fluidRow(
    # Funder NAME
    column(
      width = 6,
      textInput(
        inputId = paste0("funder_", funder),
        label = "Nombre del grupo",
        value = paste("Grupo", funder)
      )
    ),
    column(
      #Funder CONTRIBUTION %
      width = 6,
      numericInput(
        inputId = paste0("pct_funder_", funder),
        label = "% de contribución",
        value = pct,
        min = 0,
        max = 100
      )
    )
  )
}