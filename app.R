library(shiny)
library(DT)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Display search directory
      actionButton("dirButton", "Search directory", icon = icon("folder-open")),
      br(),
      verbatimTextOutput("dir"),
      br(),
      # Applies a filter on file extension
      textInput("fileExtension", label = "File extension", value = "R"),
      br(),
      textInput("term", label = "Term to find"),
      align = "center"
    ),
    mainPanel(
      # Display the table of usage
      DT::dataTableOutput("usageTable")
    )
  )
)

server <- function(input, output, session) {
  #----- Update search directory based on user input
  searchDirectory <- reactiveValues(path = path.expand("~"))
  observeEvent(input$dirButton, {
    selectedDir <- rstudioapi::selectDirectory(path = searchDirectory$path)
    selectedDir <- normalizePath(selectedDir, winslash = "/")
    if (is.null(selectedDir)) {
      return()
    }
    searchDirectory$path <- selectedDir
    return()
  })
  output$dir <- renderText({
    searchDirectory$path
  })
  #----- Display usage for search term in selected files
  output$usageTable <- DT::renderDataTable({
    # If no searched term, do not try anything
    if (input$term %in% "") {
      return()
    }
    # Define the file extension to search terms in
    filePattern <- NULL
    if (!(input$fileExtension %in% c(NA, ""))) {
      # because of the regular expression needs \\ before .
      filePattern <- paste0("\\.", input$fileExtension)
    }

    # Initialize usage table and loop through all files to check the usage
    usage <- data.frame()
    for (fileName in list.files(searchDirectory$path, pattern = filePattern)) {
      fileContent <- readLines(
        file.path(searchDirectory$path, fileName),
        warn = FALSE
      )
      linesWithTerm <- grep(pattern = input$term, x = fileContent)

      usage <- rbind.data.frame(
        usage,
        data.frame(
          File = rep(fileName, length(linesWithTerm)),
          Line = linesWithTerm,
          Content = fileContent[linesWithTerm]
        )
      )
    }
    return(usage)
  })
}

shinyApp(ui, server)
