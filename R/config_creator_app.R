library(shiny)


ui <- fluidPage(

  tags$head(
    tags$style(
      "body{
    min-height: 611px;
    height: auto;
    min-width: 600px;
    max-width: 800px;
    margin: auto;
        }"
    )
  ),

  h4(strong("Create config file for experiment")),
  hr(),
  column(
    width = 6,
    textInput("exp_name", label="Experiment Name", placeholder = "Enter experiment name"),
    dateRangeInput('hab_date_range',
                   label = 'Habituation Date range input: yyyy-mm-dd',
                   start = Sys.Date() - 2, end = Sys.Date() + 2
    ),
    dateRangeInput('exp_date_range',
                   label = 'Experiment Date range input: yyyy-mm-dd',
                   start = Sys.Date() - 2, end = Sys.Date() + 2
    ),
    textInput("animal_prefix", value = NULL,
              label = "Prefix for the Animal ID (defaults to nothing)"),
    numericInput("min_ID", value = NULL,
                 label = 'Min Numeric Animal ID'),
    numericInput("max_ID", value = NULL,
                 label = 'Max Numeric Animal ID'),
    selectizeInput("to_remove", label = "Animal(s) to remove",
                   choices = NULL, selected = NULL, multiple = TRUE,
                   options = NULL)
  ),
  # add lights: ['07:00:00', '19:00:00'] somewhere as a slider
  column(width=8,
   fluidRow(
          column(
            width=4,
            textInput("treatment", label="Treatment level", placeholder = "Enter treatment")
          ),
          column(
            width=2, style = "margin-top: 25px",
            actionButton("add_treatment", label="Add")
          ),
          column(
            width=2, style = "margin-top: 25px",
            actionButton("remove_treatment", label="Remove")
          )
      ),
   fluidRow(
     h5("Current Treatments:"),
     span(textOutput("treat_level"), style="color:red"),
     sliderInput("lights", label="Hours Lights ON",
                 value=c(7,19),
                 min = 0, max = 24,
                 ),
     downloadButton('generate_config',
                  label="Create", class = "btn-primary")
   )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(treatments = NULL)

  get_date_range <- function(date_range){
    out <- list(from = date_range[1],
                to = date_range[2])
    return(out)
  }

  create_date_sequence <- function(list_range) {
    out <- seq(list_range$from, list_range$to, by = "1 day")
    return(out)
  }


  make_IDs <- reactive({
    validate(
      need(is.null(input$min_ID) == FALSE, label = "No min ID selected. Please select one"),
      need(is.null(input$max_ID) == FALSE, label = "No min ID selected. Please select one"),
      need(input$min_ID < input$max_ID, label = "min ID must be less than max ID. Please check entries.")
    )
    animal_ids <- seq(input$min_ID, input$max_ID, by = 1)
    # pad with zeros
    animal_ids <- stringr::str_pad(as.character(animal_ids), pad = 0, width = 3)
    # join with prefix
    animal_ids <- paste0(input$animal_prefix, animal_ids)
    return(animal_ids)
  })

  observe({
   input$min_ID
   animal_ids <- make_IDs()
   updateSelectizeInput(session,"to_remove", choices = animal_ids, options = list())
  }
  )

  generate <- function() {
    validate(
      need(is.null(input$min_ID) == FALSE, label = "No min ID selected. Please select one"),
      need(is.null(input$max_ID) == FALSE, label = "No min ID selected. Please select one"),
      need(input$min_ID < input$max_ID, label = "min ID must be less than max ID. Please check entries.")
    )
    # get the ranges and create the values for the list
    hab_dates <- create_date_sequence(get_date_range(input$hab_date_range))
    exp_dates <- create_date_sequence(get_date_range(input$exp_date_range))
    animal_ids <- make_IDs()

    animal_list <- list(min_ID = input$min_ID,
                        max_ID = input$max_ID,
                        ID_prefix = input$animal_prefix,
                        ID_list = animal_ids,
                        to_remove = input$to_remove)

    lights <- data.table::as.ITime(input$lights*3600)
    config <- list(
      exp_name = input$exp_name,
      habituation_dates = as.character(hab_dates),
      experimental_dates = as.character(exp_dates),
      treatment = rv$treatment,
      animals = animal_list,
      lights = as.character(lights))
    
    #shinyFiles::shinyDirChoose(input, id = 'folder', ...)
    #yaml::write_yaml(config, "config.yaml")

    return(config)
  }

  #observeEvent(input$hab_date_range, print(get_date_range(input$hab_date_range)))
  #observeEvent(input$exp_date_range, print(get_date_range(input$exp_date_range)))

  # observe Add/Remove treatment -----
  observeEvent(input$add_treatment, {
    # with unique, we can't add twice the same level 
    rv$treatment <- unique(c(rv$treatment, input$treatment))
  }
  )
  observeEvent(input$remove_treatment, {
    rv$treatment <- rv$treatment[-length(rv$treatment)]
  }
  )
  
  #  display treatments
  output$treat_level <- renderText({
    rv$treatment
  })
  
  # save the file with the download handler
  output$generate_config <- downloadHandler(
    filename = "config.yaml",
    content = function(file) {
      yaml::write_yaml(generate(), file=file)
    }
  )
  
  
  #observeEvent(input$generate_config, generate())

}

shinyApp(ui, server)
