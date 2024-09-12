pacman::p_load(shiny, readxl, dplyr, tidyr, ggplot2, lubridate, miceadds)
source.all("anc/")
addResourcePath("sty", "sty")

ui <- navbarPage(
  title = div(
    class = "navbar-brand",
    tags$img(src = "sty/logo.png", height = "50px"),  
    span("Validatietool Samplers")
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "sty/styles.css")
  ),

  tabPanel("Data Invoer", 
           fluidPage(

             titlePanel("Importeer uw ruwe datafile"),

             sidebarLayout(
               sidebarPanel(
                selectInput("meetnet", "Selecteer Meetnet:",
                             choices = c("59")),
                fileInput("file", "Upload Excel File", 
                           accept = c(".xls", ".xlsx")),
                uiOutput("meetpostopstelling_ui")
               ),

               mainPanel(
                 tableOutput("data_summary"),
                 tags$style(HTML("
                   table {
                   width: 100%;
                   table-layout: auto;
                   }
                   th, td {
                   white-space: nowrap;
                   }
                 "))
               )
             )
           )
  ),
  tabPanel("Validatie", 
fluidPage(
  h4(textOutput("validation_title"))
          )
  ),
  tabPanel("Data Export", 
         fluidPage(
         )
  )
)


server <- function(input, output, session) {

  uploaded_data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath)
    return(df)  
  })

  processed_data <- reactive({
    df <- uploaded_data()  
    processed_df <- get(paste0("process_meetnet_", input$meetnet))(df)
    return(processed_df)
  })

  output$meetpostopstelling_ui <- renderUI({
    req(processed_data())
    selectInput("meetpostopstelling", "Selecteer Meetpost:",
                choices = names(processed_data()))
  })

  output$data_summary <- renderTable({
    req(input$meetpostopstelling)
    processed_data()[[input$meetpostopstelling]] 
  })

  output$validation_title <- renderText({
    paste("Validatie voor Meetnet ", input$meetnet, " en Meetpostopstelling ", input$meetpostopstelling, ":", sep = "")
  })

}

shinyApp(ui = ui, server = server)
