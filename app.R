# app.R
library(shiny)
library(plotly)
library(dplyr)
library(readr)

# Sample data for download
generate_sample_data <- function(n = 300, phi = 0.8, meanlog = 0, sdlog = 0.2) {
  
  # simulate AR(1) process using arima.sim()
  ar_process <- arima.sim(n = n, list(ar = phi), sd = 0.5)
  
  # general log-normal tx
  lognorm_series <- exp(meanlog + sdlog * ar_process)
  
  out <- data.frame(
    datetime = seq(as.POSIXct("2024-01-01"), by = "hour", length.out = n),
    value = lognorm_series
  )
  
  return(out)
  
}

# UI
ui <- fluidPage(
  titlePanel("Interactive Time Series Plot with CSV Upload and Selection Tools"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Download sample data to upload"),
      downloadButton("downloadSample", "Download", class = "btn-info"),
      hr(),
      
      h4("Upload CSV"),
      fileInput("file_upload", "Upload CSV File", accept = c(".csv")),
      actionButton("clear_dataset", "Clear Uploaded Dataset", class = "btn-secondary"),
      
      h4("Manage Selections"),
      actionButton("clear_all", "Clear All Selections", class = "btn-danger"),
      actionButton("undo_last", "Remove Last Selected Point", class = "btn-warning"),
      downloadButton("downloadData", "Download Filtered Data"),
      
      h4("Selected Points:"),
      verbatimTextOutput("clicked_points")
    ),
    
    mainPanel(
      plotlyOutput("timeSeriesPlot")
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive value to store either uploaded or cleared data
  uploaded_data <- reactiveVal(NULL)  # Store uploaded data (if any)
  
  # Reactive data to use in the plot (uploaded or empty)
  data <- reactive({
    out <- uploaded_data()
    req(out)
    out |> 
      mutate(
        datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S"),
        value = as.numeric(value)
      )
  })
  
  # Store selected points
  selected_points <- reactiveVal(data.frame(datetime = as.POSIXct(character()), value = numeric()))
  
  # Render Plotly time series plot when data is available
  output$timeSeriesPlot <- renderPlotly({
    req(data())  # Ensure data exists before plotting
    
    plot_ly(
      data = data(), 
      x = ~datetime, 
      y = ~value, 
      type = 'scatter', 
      mode = 'lines+markers',
      marker = list(size = 8)
    ) %>%
      layout(
        xaxis = list(title = NA),
        dragmode = 'select',  # Enable selection with mouse drag
        clickmode = 'event+select'
      )
  })
  
  # Update uploaded data when a new file is uploaded
  observeEvent(input$file_upload, {
    uploaded_data(read_csv(input$file_upload$datapath, col_types = 'Tn'))
  })
  
  # Clear uploaded data and reset selections
  observeEvent(input$clear_dataset, {
    uploaded_data(NULL)
    selected_points(data.frame(datetime = as.POSIXct(character()), value = numeric()))
  })
  
  # Handle box/lasso selection
  observeEvent(event_data("plotly_selected"), {
    selection <- event_data("plotly_selected")
    if (!is.null(selection)) {
      new_points <- data.frame(datetime = as.POSIXct(selection$x), value = selection$y)
      updated_points <- bind_rows(selected_points(), new_points) %>%
        distinct()  # Avoid duplicates
      selected_points(updated_points)
    }
  })
  
  # Handle single point click
  observeEvent(event_data("plotly_click"), {
    click <- event_data("plotly_click")
    new_point <- data.frame(datetime = as.POSIXct(click$x), value = click$y)
    updated_points <- bind_rows(selected_points(), new_point) %>%
      distinct()  # Avoid duplicates
    selected_points(updated_points)
  })
  
  # Clear all selected points
  observeEvent(input$clear_all, {
    selected_points(data.frame(datetime = as.POSIXct(character()), value = numeric()))
  })
  
  # Remove the last selected point
  observeEvent(input$undo_last, {
    current_points <- selected_points()
    if (nrow(current_points) > 0) {
      updated_points <- current_points[-nrow(current_points), ]
      selected_points(updated_points)
    }
  })
  
  # Display selected points
  output$clicked_points <- renderPrint({
    selected_points()
  })
  
  # Download filtered data
  output$downloadData <- downloadHandler(
    filename = function() { "filtered_time_series.csv" },
    content = function(file) {
      req(data())  # Ensure data is loaded before exporting
      filtered_data <- anti_join(data(), selected_points(), by = "datetime")
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  # Download sample dataset
  output$downloadSample <- downloadHandler(
    filename = function() { "sample_time_series.csv" },
    content = function(file) {
      sample_data <- generate_sample_data()
      write.csv(sample_data, file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
