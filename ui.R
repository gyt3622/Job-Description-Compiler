ui <- fluidPage(
  
  titlePanel("Job Description Compiler"),
  
  sidebarLayout(
    
    sidebarPanel(
      textInput("query", "Enter a job title", placeholder = "Data Analyst", value="Data Analyst"),
      selectInput("num", "Choose how many jobs to return", choices = c(50, 100, 150, 200, 250),
                  selected = 50),
      actionButton("button", "Search"),
      br(),
      br(),
      downloadButton("downloadData", "Download as CSV (all information about the job)"),
      br(),
      br(),
      downloadButton("downloadData2", "Download as CSV (only the title and the description)")
    ),
    
    mainPanel(
      
      DTOutput(outputId = "table")
      
    )
    
    
  )
)

server <- function(input, output) {
  
  
  
  
  datasetInput <- eventReactive(input$button, {
    dat <- searchJob(input$query, as.numeric(input$num))
    dat
  })
  
  
  output$table <- renderDT({
    input$button
    datasetInput()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("information", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
  
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("description", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput()[, c(1, 3)], file, row.names = FALSE)
    }
  )
  
}

shinyApp(ui = ui, server = server)

