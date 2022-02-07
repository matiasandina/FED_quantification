library(tidyverse)
library(shiny)
library(DT)

# source functions
source("R/create_treatment_key.R")

# Initialize things -------------------
training_programs <- data.frame(FreeFeed = "FreeFeed",
                                FR1 = "FR1",
                                FR3 = "FR3",
                                FR5 = "FR5") 


ui_upload <- sidebarLayout(
  sidebarPanel(
    fileInput("file", "Upload or drag/drop one or multiple FED3 files",
              buttonLabel = "Upload .csv",
              multiple = TRUE),
    fileInput("treatment_key", "Upload or drag/drop config.yaml",
              buttonLabel = "Upload config.yaml",
              multiple = FALSE),
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
             h4("Auto Assignment"),
             h5("Check that the animal assignment to FEDs is correct"),
             DT::dataTableOutput("preview1"),
             h4("Manual Assignment"),
             h5("Assign FED to Animal."),
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
  # this will return the address of the local copy of the files
  # the $datapath column of input$file contains the temp files created
  filelist <- reactive({
    return(input$file$datapath)
  })
  
  # use read_fed to get raw data in
  raw_df <- reactive({
    req(input$file)
    files <- filelist()

    li <- lapply(files,
                 # using fed3 package
           function(tt) fed3::read_fed(tt)
    )
    df <- bind_rows(li) %>% 
      mutate(FED = paste0("FED", str_pad(Device_Number, width=3, pad=0)))
    return(df)
  })
  
  # Clean ----------------------------------------------------------------
  tidied <- reactive({
    req(input$file)
    # get raw data
    df <- raw_df()
  
    # get treatment key
    req(input$treatment_key)
    params <- yaml::read_yaml(input$treatment_key$datapath)
    treatment_key <- create_treatment_key(params)

    # bind
    df <- left_join(df, treatment_key, by = "FED")
    
    # filter dates 
    #filter_dates <- get_date_range()
    #df <- df %>%
    #  mutate(date = lubridate::as_datetime(date)) %>% 
    #  filter(between(date, filter_dates$from, filter_dates$to)) 
    
    # calculate cumulative pellets
    df <- 
      df %>% fed3::recalculate_pellets()

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
  
  output$preview1 <- DT::renderDataTable(
    tidied() %>% select(ID, FED, Treatment) %>% distinct(),
    options = table_render_options)

  output$preview2 <- DT::renderDataTable(tidied(),
                                         options = table_render_options)

  # Plot --------------------------------
  
  get_program <- reactive({
    filter_value <- case_when(input$program == "FreeFeed" ~ "FreeFeed",
                              input$program == "FR1" ~ "FR1",
                              input$program == "FR3" ~ "FR3",
                              input$program == "FR5" ~ "FR5",
                              TRUE ~ "FreeFeed")
    return(filter_value)
  })
  
  
  plot_with_requirements <- reactive({
    df <- tidied()
    # fix if df comes with misspell
    if("Sessiontype" %in% names(df)){df <- rename(df, Session_Type = Sessiontype)} 
    
    training_program <- get_program()
    
    # get type of plot
    plot_type <- input$geoms
    
    if (training_program == "FR") {
      # There might be a problem if not unique
      training_program <- unique(df$FR_Ratio)
    }
    
    # FED_Version > 1.1.44 saves Session_Type variable 
    df <- filter(df, Session_Type == training_program) %>%
      # aggregate in time
      mutate(floor_minute = lubridate::floor_date(datetime, unit = "1 minute"))
    
    
    p1 <- ggplot(df, aes(!!input$x_var, !!input$y_var)) +
      labs(title = paste(input$y_var, "vs", input$x_var), 
           subtitle = paste("Session Type is", unique(df$Session_Type),
           ifelse(training_program == "FreeFeed", "-- Freely available pellets", ""))
           )
    
    if(input$color_var == "none"){
      p1 <- p1 +
        switch(plot_type,
               'point' = geom_point(),
               'line' = geom_line(aes(group=ID)),
               'boxplot' = geom_boxplot(aes(x=factor(!!input$x_var))))
    } else {
      p1 <- p1 +
        switch(plot_type,
               'point' = geom_point(aes(color = !!input$color_var)),
               'line' = geom_line(aes(group=ID, color = !!input$color_var)),
               # color maps to fill in boxplot
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
      # TODO: change this garbage name 
      paste0(input$dateRange[1],"_to_",input$dateRange[2], "_pooled_FED_data.csv")
    },
    content = function(file) {
      vroom::vroom_write(tidied(), file, delim=",")
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