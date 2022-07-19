
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)

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

ui <- dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(),
  body = dashboardBody(
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
          uiOutput(outputId = "actors"),
          actionButton(
            inputId = "update_split_budget",
            label = "Actualizar"
          )
        ),
        column(
          width = 8,
          plotlyOutput(
            outputId = "split_budget_plot"
          )
        )
      )
    )
  )
)
    
server <- function(input, output){
  # Budget splitting server logic ##############################################
  # Create flexible UI input space determined by the number of actors
  output$actors <- renderUI({
    n <- input$n_actors
    
    map2(.x = 1:n,
         .y = 100/n,
         .f = make_funder)
  })
  
  # Assamble a tibble of actors
  actors_tibble <- reactive({
    
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

  
  # Plot
  output$split_budget_plot <- renderPlotly({
    # browser()
    p <-
      ggplot(
        data = actors_tibble(),
        mapping = aes(
          x = actor,
          y = pct,
          fill = actor
        )
      ) +
      geom_col(
        color = "black"
        ) +
      theme_bw()

    ggplotly(p)
  })
}
    
shinyApp(ui = ui, server = server)
