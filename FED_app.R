library(tidyverse)
library(shiny)
library(DT)


# Initialize things -------------------

training_programs <- data.frame(FED = "FED",
                                FR = "FR") 


ui_upload <- sidebarLayout(
  sidebarPanel(
    fileInput("file", "Data", buttonLabel = "Upload .csv", multiple = TRUE),
    dateRangeInput('dateRange',
                   label = 'Date range input: yyyy-mm-dd',
                   start = Sys.Date() - 2, end = Sys.Date() + 2
    ),
    varSelectInput("x_var",
                   "X variable:",
                   NULL),
    varSelectInput("y_var",
                   "Y variable:",
                   NULL),
    varSelectInput("color_var",
                   "Color variable:",
                   NULL),
    varSelectInput("program", "Training program", training_programs)
  ),
  mainPanel(
  tabsetPanel(
    #h3("Raw data"),
    tabPanel("Data", value = "data",
             column(12,
             h4("Raw Data"),
             DT::dataTableOutput("preview1"),
             h4("Filtered Data"),
             DT::dataTableOutput("preview2")
                    )
             ),
    tabPanel("Plot", value = "plot",
             radioButtons("geoms", "Type of plot", inline = TRUE,
                          c("point" = "point",
                            "line" = "line",
                            "boxplot" = "boxplot"),
                          selected = "point", ),
             h6(tags$em("Hint1: If an empty plot is displayed, check whether you have the correct program selected.")),
             h6(tags$em("Hint2: Boxplot option treats x variable as a factor, might yield meaningless plot. Please avoid using it with `date` variable.")),
             hr(),
             plotOutput("plot_panel")),
    tabPanel("Histogram", value = "histogram",
             radioButtons("dodged", "Position of the bars",
                                c("stack" = "stack",
                                  "dodge" = "dodge"),
                                selected = "stack", ),
             hr(),
             plotOutput("histogram_panel"))
    )
  )
)


ui_download <- fluidRow(
  column(width = 6,
         downloadButton("download_data",
                        label = "Download data",
                        class = "btn-block")),
  column(width = 6,
         downloadButton("download_plot",
                        label = "Download plot",
                        class = "btn-block"))
)



ui <- fluidPage(
  ui_upload,
  #ui_var_selector,
  ui_download
)

server <- function(input, output, session) {
  # Upload ---------------------------------------------------------------
  raw <- reactive({
    req(input$file)
    #delim <- if (input$delim == "") NULL else input$delim
    # we leave as NULL to guess
    delim <- NULL
    
    # col classes
    FED3_classes <-  cols(
      `MM:DD:YYYY hh:mm:ss` = col_character(),
      Device_Number = col_double(),
      Battery_Voltage = col_double(),
      Motor_Turns = col_double(),
      #FR_Ratio = col_character(),
      Active_Poke = col_character(),
      Left_Poke_Count = col_double(),
      Right_Poke_Count = col_double(),
      Pellet_Count = col_double(),
      Retrieval_Time = col_double()
    )
    
    li <- lapply(input$file$datapath,
           function(tt)
    vroom::vroom(tt, delim = delim, skip = 0,
                 col_types = FED3_classes)
    )
    names(li) <- input$file$datapath
    return(li)
  })
  
  # Clean ----------------------------------------------------------------
  tidied <- reactive({
    req(input$file)
    # get raw data
    li <- raw()
    # give them names
    names(li) <- get_fed_names()
    # code for a plot
    df <- bind_rows(li, .id = "FED") %>%
      rename(date = `MM:DD:YYYY hh:mm:ss`) %>%
      mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M:%S"),
             day = lubridate::day(date),
             month = lubridate::month(date),
             year = lubridate::year(date))
    
    # filter dates 
    #filter_dates <- get_date_range()
    #df <- df %>%
    #  mutate(date = lubridate::as_datetime(date)) %>% 
    #  filter(between(date, filter_dates$from, filter_dates$to)) 
    
    ## Add FR_Ratio (make compatible with FED2)
    if("FR_Ratio" %in% names(df) == FALSE){
      df <- df %>% mutate(FR_Ratio = 1)
    }
    print(df)
    return(df)
  })
  
  # Observe and react to events --------
  
  get_fed_names <- reactive({
    req(input$file)
    fed <- stringr::str_extract(string = input$file$name,
                         pattern = "FED[0-9]{3}")
    #fed <- stringr::str_remove(fed, "FED")
    
    return(fed)
  })
  

  # Update column selectors -------------------------------------------------
  # whenever we upload to raw, we change the columns
  # Change x_var
  observe({
  updateSelectInput(
    session,
    "x_var",
    choices = c("NA", names(tidied()))
  )
})
  observe({
    updateSelectInput(
      session,
      "y_var",
      choices = c("NA", names(tidied()))
    )
  })
  observe({
    updateSelectInput(
      session,
      "color_var",
      choices = c("none", names(tidied()))
    )
  })
  
  # Get current tab ------

  get_current_tab <- reactive({
    print(input$tabs)
  })
  
  # Get dates --------
  
  get_date_range <- reactive({
    out <- list(from = input$dateRange[1],
                to = input$dateRange[2])
    out <- lapply(out, as.POSIXct)
    return(out)
  })


  # Table Outputs -----------------------
  
  table_render_options <- list(pageLength = 5,
                               lengthMenu = list(c(5, 15, -1),
                                                 list('5', '15', 'All'))
  )
  
  output$preview1 <- DT::renderDataTable(raw()[[1]],
                                     options = table_render_options)

  output$preview2 <- DT::renderDataTable(tidied(), options = table_render_options)

  # Plot --------------------------------
  
  get_program <- reactive({
    filter_value <- case_when(input$program == "FED" ~ "FED",
                              input$program == "FR" ~ "FR",
                              TRUE ~ "FED")
    return(filter_value)
  })
  
  
  plot_with_requirements <- reactive({
    df <- tidied()
    training_program <- get_program()
    
    # get type of plot
    plot_type <- input$geoms
    
    if(training_program == "FR"){
      # There might be a problem if not unique
      training_program <- unique(df$FR_Ratio)
    }
    
    df <- filter(df, FR_Ratio == training_program) %>%
      # aggregate in time
      mutate(floor_minute = lubridate::floor_date(date, unit = "1 minute"))
    
    
    p1 <- ggplot(df, aes(!!input$x_var, !!input$y_var)) +
      labs(title = paste(input$y_var, "vs", input$x_var), 
           subtitle = paste("Fixed Ratio is", unique(df$FR_Ratio),
           ifelse(training_program == "FED", "-- Freely available pellets", ""))
           )
    
    if(input$color_var == "none"){
      p1 <- p1 +
        switch(plot_type,
               'point' = geom_point(),
               'line' = geom_line(),
               'boxplot' = geom_boxplot(aes(x=factor(!!input$x_var))))
    } else {
      p1 <- p1 +
        switch(plot_type,
               'point' = geom_point(aes(color = !!input$color_var)),
               'line' = geom_line(aes(color = !!input$color_var)),
               # maps to fill in boxplot
               'boxplot' = geom_boxplot(aes(x=factor(!!input$x_var),
                                            fill = !!input$color_var)))+
        theme(legend.position = "bottom")
    }
    return(p1)
  })
  
  histogram_with_requirements <- reactive({
    df <- tidied()
    training_program <- get_program()
    
    
    p1 <- ggplot(df, aes(!!input$x_var)) +
      labs(title = paste("Histogram of", input$x_var), 
           subtitle = paste("Fixed Ratio is :", unique(df$FR_Ratio),
                            ifelse(training_program == "FED", "-- Freely available pellets", ""))
      )
    
    if(input$color_var == "none"){
      p1 <- p1 +
        geom_histogram()
      
    } else {
      p1 <- p1 +
        # we will use fill instead of color
        geom_histogram(aes(fill = !!input$color_var), 
                       position = input$dodged)+
        theme(legend.position = "bottom")
      
    }
    
    
    return(p1)
    
  })
  
  output$plot_panel <- renderPlot({
    req(input$file)
    validate(
      need(nrow(tidied()) > 0, "Please make sure your dates are correct"),
      # treating NA as character
      need(input$x_var != "NA", 'Please choose x coord'),
      need(input$y_var != "NA", 'Please choose y coord')
    )
    plot_with_requirements()
    })
  
  output$histogram_panel <- renderPlot({
    req(input$file)
    validate(
      need(nrow(tidied()) > 0, "Please make sure your dates are correct"),
      # treating NA as character
      need(input$x_var != "NA", 'Please choose x coord'),
      need(input$y_var == "NA", 'Please make sure y coord is NA')
    )
    histogram_with_requirements()
  })
  
  
  # Download -------------------------------------------------------------
output$download_data <- downloadHandler(
    filename = function() {
      paste0(input$dateRange[1],"_to_",input$dateRange[2], "_clean_data.csv")
    },
    content = function(file) {
      vroom::vroom_write(tidied(), file)
    }
  )

output$download_plot = downloadHandler(
# do we need this ?
  # decide which plot to save 
#  plot_to save <- case_when(input$tabs == ...)
    filename = function() {"plots.png"},
    content = function(file) {
      ggsave(file, device = "png", width=11, height=8.5)
      
    }
  )

}

shinyApp(ui, server)